{-# LANGUAGE TypeSynonymInstances, MultiParamTypeClasses, PatternGuards, BangPatterns #-}
module Moves.Board (
    posFromFen, initPos,
    isCheck, inCheck,
    goPromo, hasMoves,
    genmv, genmvT,
    genMoveCapt, genMoveCast, genMoveNCapt, genMoveTransf, genMovePCapt, genMovePNCapt, genMoveFCheck,
    genMoveNCaptToCheck,
    updatePos, kingsOk, checkOk,
    legalMove, alternateMoves, nonCapt,
    doFromToMove, reverseMoving
    ) where

import Prelude hiding ((++), foldl, filter, map, concatMap, concat, head, tail, repeat, zip,
                       zipWith, null, words, foldr, elem, lookup, any)
-- import Control.Exception (assert)
import Data.Bits
import Data.List.Stream
import Data.Char
import Data.Maybe
import Data.Ord (comparing)

import Struct.Struct
import Moves.Moves
import Moves.BitBoard
import Moves.ShowMe
import Moves.SEE
import Eval.BasicEval
import Hash.Zobrist

startFen :: String
startFen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR/ w KQkq - 0 1"

fenToTable :: String -> MyPos
fenToTable fen = foldr setp emptyPos $ fenToAssocs fen
    where setp (sq, (c, p)) = setPiece sq c p

fenToAssocs :: String -> [(Square, (Color, Piece))]
fenToAssocs str = go 56 str []
    where go _ [] acc = acc
          go sq (c:cs) acc
              | sq < 0 = acc
              | c `elem` "PRNBQK" = go (sq+1) cs $ (sq, fcw):acc
              | c `elem` "prnbqk" = go (sq+1) cs $ (sq, fcb):acc
              -- | c == '/'  = go (nextline sq) cs acc
              | isDigit c = go (skip sq c) cs acc
              -- | otherwise = go sq cs acc	-- silently ignore other chars
              | otherwise = go (nextline sq) cs acc	-- treat like /
              where fcw = (White, toPiece c)
                    fcb = (Black, toPiece $ toUpper c)
          skip f c = f + fromIntegral (ord c - ord '0')
          nextline f = f - 16
          toPiece c = fromJust $ lookup c letterToPiece

letterToPiece :: [(Char, Piece)]
letterToPiece = [('P', Pawn), ('R', Rook), ('N', Knight), ('B', Bishop),
                    ('Q', Queen), ('K', King)]

initPos :: MyPos
initPos = posFromFen startFen

-- TODO: en passant
posFromFen :: String -> MyPos
posFromFen fen = p { basicPos = bp, zobkey = zk }
    where fen1:fen2:fen3:_:fen5:_ = fenFromString fen
          p  = fenToTable fen1
          bp = (basicPos p) { bpepcas = x }
          x  = fyInit . castInit . epInit $ epcas0
          (epcas0, z) = case fen2 of
              'w':_ -> (0, 0)
              'b':_ -> (mvMask, zobMove)
              _     -> error "posFromFen: expect w or b"
          (cK, z1) = if 'K' `elem` fen3 then ((.|. caRKiw), zobCastKw) else (id, 0)
          (cQ, z2) = if 'Q' `elem` fen3 then ((.|. caRQuw), zobCastQw) else (id, 0)
          (ck, z3) = if 'k' `elem` fen3 then ((.|. caRKib), zobCastKb) else (id, 0)
          (cq, z4) = if 'q' `elem` fen3 then ((.|. caRQub), zobCastQb) else (id, 0)
          castInit = cQ . cK . cq . ck
          epInit   = id					-- TODO: ep field
          fyInit = set50Moves $ read fen5
          zk = zobkey p `xor` z `xor` z1 `xor` z2 `xor` z3 `xor` z4		-- also for ep

-- A primitive decomposition of the fen string
fenFromString :: String -> [String]
fenFromString fen = zipWith ($) fenfuncs fentails
    where fentails = tails $ words fen
          fenfuncs = [ getFenPos, getFenMv, getFenCast, getFenEp, getFenHalf, getFenMvNo ]
          headOrDefault a0 as = if null as then a0 else head as
          getFenPos  = headOrDefault ""
          getFenMv   = headOrDefault "w"
          getFenCast = headOrDefault "-"
          getFenEp   = headOrDefault "-"
          getFenHalf = headOrDefault "-"
          getFenMvNo = headOrDefault "-"

{--
-- find rook-like possibly pinning pieces for a position & color
-- that is: rooks or queens, which possibly pin oponent pieces in regard to the (oponent) king
findPKAr p c = rAttacs defksq 0 .&. rs .&. atp
    where (atp, defp) = if c == White then (white p, black p) else (black p, white p)
          rs = rooks p .|. queens p
          defksq = firstOne $ defp .&. kings p

-- find bishop-like possibly pinning pieces for a position & color
-- that is: bishops or queens, which possibly pin oponent pieces in regard to the (oponent) king
findPKAb p c = bAttacs defksq 0 .&. bs .&. atp
    where (atp, defp) = if c == White then (white p, black p) else (black p, white p)
          bs = bishops p .|. queens p
          defksq = firstOne $ defp .&. kings p

-- find all possibly pining pieces and lines in a given position
-- this has to be calculated per position, and recalculated
-- only when the king or one of the pinning pieces move or is captured
allPLKAs p = (lwr ++ lwb, lbr ++ lbb)
    where pkaswr = findPKAr p White
          pkaswb = findPKAb p White
          pkasbr = findPKAr p Black
          pkasbb = findPKAb p Black
          kwsq = firstOne $ kings p .&. white p
          kbsq = firstOne $ kings p .&. black p
          lwr = filter f $ map (findLKA Rook kbsq) $ bbToSquares pkaswr
          lwb = filter f $ map (findLKA Bishop kbsq) $ bbToSquares pkaswb
          lbr = filter f $ map (findLKA Rook kwsq) $ bbToSquares pkasbr
          lbb = filter f $ map (findLKA Bishop kwsq) $ bbToSquares pkasbb
          f = (/= 0) . snd
--}

-- For pinned pieces the move generation is restricted to the pinned line
-- so the same attacs .&. direction
pinningDir :: MyPos -> Color -> Square -> BBoard
pinningDir p c sq = let ds = filter (exactOne . (.&. bit sq)) $ map snd
                                $ if c == White then bpindirs p else wpindirs p
                    in if null ds then error "pinningDir" else head ds

pinningCapt :: MyPos -> Color -> Square -> BBoard
pinningCapt p c sq = let ds = filter (exactOne . (.&. bit sq) . snd)
                                  $ if c == White then bpindirs p else wpindirs p
                     in if null ds then error "pinningCapt" else bit . fst . head $ ds

-- Is color c in check in position p?
isCheck :: MyPos -> Color -> Bool
isCheck p c = (ckp /= 0) && (ckp .&. colp /= 0)
    where colp = if c == White then white p else black p
          ckp = check p

{-# INLINE inCheck #-}
inCheck :: MyPos -> Bool
inCheck = (/= 0) . check

goPromo :: MyPos -> Move -> Bool
goPromo p m
    | moveIsTransf m = True
    | otherwise      = case tabla p t of
                           Busy White Pawn -> ppw
                           Busy Black Pawn -> ppb
                           _               -> False
    where t = toSquare m
          ppw = t >= 48	-- 40
          ppb = t < 16		-- 24

-- {-# INLINE genmv #-}
genmv :: Bool -> MyPos -> (Square, Square) -> Move
genmv spec _ (f, t) = if spec then makeSpecial m else m
    where !m = moveFromTo f t

-- Used only with transformation pawns
genmvT :: MyPos -> (Square, Square) -> Move
genmvT _ (f, t) = makeTransf Queen f t

-- Here it seems we have a problem when we are not in check but could move
-- only a pinned piece: then we are stale mate but don't know (yet)
-- In the next ply, when we try to find a move, we see that all moves are illegal
-- In this case we should take care in search that the score is 0!
hasMoves :: MyPos -> Color -> Bool
hasMoves !p c
    | chk       = not . null $ genMoveFCheck p c
    | otherwise = anyMove
    where hasPc = any (/= 0) $ map (pcapt . pAttacs c)
                     $ bbToSquares $ pawns p .&. myfpc
          hasPm = not . null $ pAll1Moves c (pawns p .&. mypc) (occup p)
          hasN = any (/= 0) $ map (legmv . nAttacs) $ bbToSquares $ knights p .&. myfpc
          hasB = any (/= 0) $ map (legmv . bAttacs (occup p))
                     $ bbToSquares $ bishops p .&. myfpc
          hasR = any (/= 0) $ map (legmv . rAttacs (occup p))
                     $ bbToSquares $ rooks p .&. myfpc
          hasQ = any (/= 0) $ map (legmv . qAttacs (occup p))
                     $ bbToSquares $ queens p .&. myfpc
          !hasK = 0 /= (legal . kAttacs $ firstOne $ kings p .&. mypc)
          !anyMove = hasK || hasN || hasPm || hasPc || hasQ || hasR || hasB
          chk = inCheck p
          (!mypc, !yopi) = thePieces p c
          -- myfpc = mypc `less` pinned p
          myfpc = mypc
          !yopiep = yopi .|. (epcas p .&. epMask)
          legmv x = x `less` mypc
          pcapt x = x .&. yopiep
          legal x = x `less` oppAt
          !oppAt = if c == White then blAttacs p else whAttacs p

-- Move generation generates legal moves
genMoveCapt :: MyPos -> Color -> [(Square, Square)]
genMoveCapt !p c = sortByMVVLVA p allp
    where !pGenC = concatMap (srcDests (pcapt . pAttacs c))
                     $ bbToSquares $ pawns p .&. myfpc `less` traR
          !nGenC = concatMap (srcDests (capt . nAttacs)) 
                     $ bbToSquares $ knights p .&. myfpc
          !bGenC = concatMap (srcDests (capt . bAttacs (occup p)))
                     $ bbToSquares $ bishops p .&. myfpc
          !rGenC = concatMap (srcDests (capt . rAttacs (occup p)))
                     $ bbToSquares $ rooks p .&. myfpc
          !qGenC = concatMap (srcDests (capt . qAttacs (occup p)))
                     $ bbToSquares $ queens p .&. myfpc
          !kGenC =            srcDests (capt . legal . kAttacs)
                     $ firstOne $ kings p .&. myfpc
          allp = concat [ pGenC, nGenC, bGenC, rGenC, qGenC, kGenC ]
          (!mypc, !yopi) = thePieces p c
          -- myfpc = mypc `less` pinned p
          myfpc = mypc
          -- yopi  = yoPieces p c
          !yopiep = yopi .|. (epcas p .&. epMask)
          capt x = x .&. yopi
          pcapt x = x .&. yopiep
          legal x = x `less` oppAt
          !oppAt = if c == White then blAttacs p else whAttacs p
          !traR = if c == White then 0x00FF000000000000 else 0xFF00

-- For quiescent search we generate only winning captures
-- This is just an approximation
{-
genMoveWCapt :: MyPos -> Color -> [(Square, Square)]
genMoveWCapt !p !c = concat [ pGenC, nGenC, bGenC, rGenC, qGenC, kGenC ]
    where pGenC = concatMap (srcDests (pcapt . pAttacs c))
                     $ bbToSquares $ pawns p .&. mypc `less` traR
          nGenC = concatMap (srcDests (wcapt yopfornb . nAttacs)) 
                     $ bbToSquares $ knights p .&. mypc
          bGenC = concatMap (srcDests (wcapt yopfornb . bAttacs (occup p)))
                     $ bbToSquares $ bishops p .&. mypc
          rGenC = concatMap (srcDests (wcapt yopforr . rAttacs (occup p)))
                     $ bbToSquares $ rooks p .&. mypc
          qGenC = concatMap (srcDests (wcapt yopforq . qAttacs (occup p)))
                     $ bbToSquares $ queens p .&. mypc
          kGenC =            srcDests (capt . legal . kAttacs)
                     $ firstOne $ kings p .&. mypc
          mypc = myPieces p c `less` pinned p
          yopi  = yoPieces p c
          yopiep = yopi .|. (epcas p .&. epMask)
          capt x = x .&. yopi
          wcapt y x = x .&. y
          pcapt x = x .&. yopiep
          legal x = x `less` oppAt
          oppAt = if c == White then blAttacs p else whAttacs p
          traR = if c == White then 0x00FF000000000000 else 0xFF00
          hanging  = yopi `less` oppAt
          yopfornb = hanging .|. (yopi `less` pawns p)
          yopforr  = hanging .|. (yopfornb `less` knights p `less` bishops p)
          yopforq  = hanging .|. (yopi .&. queens p)
-}

genMoveNCapt :: MyPos -> Color -> [(Square, Square)]
-- genMoveNCapt p c = concat [ pGenNC2, qGenNC, rGenNC, bGenNC, nGenNC, pGenNC1, kGenNC ]
-- genMoveNCapt p c = concat [ pGenNC1, nGenNC, bGenNC, rGenNC, qGenNC, pGenNC2, kGenNC ]
genMoveNCapt !p c = concat [ nGenNC, bGenNC, rGenNC, qGenNC, pGenNC1, pGenNC2, kGenNC ]
    -- where pGenNCT = concatMap (srcDests True (ncapt . \s -> pMovs s c ocp)) 
    --                  $ bbToSquares $ pawns p .&. mypc .&. traR
    --       pGenNC = concatMap (srcDests False (ncapt . \s -> pMovs s c ocp)) 
    --                  $ bbToSquares $ pawns p .&. mypc `less` traR
    where pGenNC1 = pAll1Moves c (pawns p .&. mypc `less` traR) (occup p)
          pGenNC2 = pAll2Moves c (pawns p .&. mypc) (occup p)
          nGenNC = concatMap (srcDests (ncapt . nAttacs))
                      $ bbToSquares $ knights p .&. mypc
          bGenNC = concatMap (srcDests (ncapt . bAttacs (occup p)))
                      $ bbToSquares $ bishops p .&. mypc
          rGenNC = concatMap (srcDests (ncapt . rAttacs (occup p)))
                      $ bbToSquares $ rooks p .&. mypc
          qGenNC = concatMap (srcDests (ncapt . qAttacs (occup p)))
                      $ bbToSquares $ queens p .&. mypc
          kGenNC =            srcDests (ncapt . legal . kAttacs)
                      $ firstOne $ kings p .&. mypc
          -- mypc = myPieces p c `less` pinned p
          mypc = myPieces p c
          ncapt x = x `less` occup p
          legal x = x `less` oppAt
          oppAt = if c == White then blAttacs p else whAttacs p
          traR = if c == White then 0x00FF000000000000 else 0xFF00
          -- mypawns = pawns p .&. mypc

-- Generate only transformations (now only to queen) - captures and non captures
genMoveTransf :: MyPos -> Color -> [(Square, Square)]
genMoveTransf !p c = pGenC ++ pGenNC
    where pGenC = concatMap (srcDests (pcapt . pAttacs c))
                     $ bbToSquares $ pawns p .&. myfpc
    --       pGenNC = concatMap (srcDests False (ncapt . \s -> pMovs s c ocp)) 
    --                  $ bbToSquares $ pawns p .&. myfpc .&. traR
          pGenNC = pAll1Moves c (pawns p .&. myfpc) (occup p)
          (!mypc, !yopi) = thePieces p c
          -- myfpc = mypc .&. traR `less` pinned p
          !myfpc = mypc .&. traR
          !yopiep = yopi .|. (epcas p .&. epMask)
          pcapt x = x .&. yopiep
          !traR = if c == White then 0x00FF000000000000 else 0xFF00

-- Generate the captures with pinned pieces
genMovePCapt :: MyPos -> Color -> [(Square, Square)]
genMovePCapt !p !c = concat [ pGenC, nGenC, bGenC, rGenC, qGenC ]
    where pGenC = concatMap (srcDests $ pinCapt p c (pcapt . pAttacs c))
                     $ bbToSquares $ pawns p .&. myfpc `less` traR
          nGenC = concatMap (srcDests $ pinCapt p c (capt . nAttacs)) 
                     $ bbToSquares $ knights p .&. myfpc
          bGenC = concatMap (srcDests $ pinCapt p c (capt . bAttacs (occup p)))
                     $ bbToSquares $ bishops p .&. myfpc
          rGenC = concatMap (srcDests $ pinCapt p c (capt . rAttacs (occup p)))
                     $ bbToSquares $ rooks p .&. myfpc
          qGenC = concatMap (srcDests $ pinCapt p c (capt . qAttacs (occup p)))
                     $ bbToSquares $ queens p .&. myfpc
          (mypc, yopi) = thePieces p c
          myfpc = mypc .&. pinned p
          -- yopi  = yoPieces p c
          yopiep = yopi .|. (epcas p .&. epMask)
          capt x = x .&. yopi
          pcapt x = x .&. yopiep
          traR = if c == White then 0x00FF000000000000 else 0xFF00

-- Generate the non-captures with pinned pieces
genMovePNCapt :: MyPos -> Color -> [(Square, Square)]
genMovePNCapt !p !c = concat [ pGenNC, qGenNC, rGenNC, bGenNC, nGenNC ]
    where pGenNC = concatMap (srcDests $ pinMove p c (ncapt . \s -> pMovs s c (occup p))) 
                     $ bbToSquares $ pawns p .&. mypc `less` traR
          nGenNC = concatMap (srcDests $ pinMove p c (ncapt . nAttacs))
                      $ bbToSquares $ knights p .&. mypc
          bGenNC = concatMap (srcDests $ pinMove p c (ncapt . bAttacs (occup p)))
                      $ bbToSquares $ bishops p .&. mypc
          rGenNC = concatMap (srcDests $ pinMove p c (ncapt . rAttacs (occup p)))
                      $ bbToSquares $ rooks p .&. mypc
          qGenNC = concatMap (srcDests $ pinMove p c (ncapt . qAttacs (occup p)))
                      $ bbToSquares $ queens p .&. mypc
          mypc = myPieces p c .&. pinned p
          ncapt x = x `less` occup p
          traR = if c == White then 0x00FF000000000000 else 0xFF00
          -- mypawns = pawns p .&. mypc

-- {-# INLINE pinMove #-}
pinMove :: MyPos -> Color -> (Square -> BBoard) -> Square -> BBoard
pinMove p c f sq = f sq .&. pinningDir p c sq

-- {-# INLINE pinCapt #-}
pinCapt :: MyPos -> Color -> (Square -> BBoard) -> Square -> BBoard
pinCapt p c f sq = f sq .&. pinningCapt p c sq

-- {-# INLINE srcDests #-}
srcDests :: (Square -> BBoard) -> Square -> [(Square, Square)]
srcDests f !s = zip (repeat s) $ bbToSquares $ f s

-- Because finding the blocking square for a queen check is so hard,
-- we define a data type and, in case of a queen check, we give also
-- the piece type (rook or bishop) in which direction the queen checks
data CheckInfo = NormalCheck Piece !Square
               | QueenCheck Piece !Square

-- Finds pieces which check
findChecking :: MyPos -> Color -> [CheckInfo]
findChecking !p !c = concat [ pChk, nChk, bChk, rChk, qbChk, qrChk ]
    where pChk = map (NormalCheck Pawn) $ filter ((/= 0) . kattac . pAttacs c)
                               $ bbToSquares $ pawns p .&. mypc
          nChk = map (NormalCheck Knight) $ filter ((/= 0) . kattac . nAttacs)
                               $ bbToSquares $ knights p .&. mypc
          bChk = map (NormalCheck Bishop) $ filter ((/= 0) . kattac . bAttacs (occup p))
                               $ bbToSquares $ bishops p .&. mypc
          rChk = map (NormalCheck Rook) $ filter ((/= 0) . kattac . rAttacs (occup p))
                               $ bbToSquares $ rooks p .&. mypc
          qbChk = map (QueenCheck Bishop) $ filter ((/= 0) . kattac . bAttacs (occup p))
                               $ bbToSquares $ queens p .&. mypc
          qrChk = map (QueenCheck Rook) $ filter ((/= 0) . kattac . rAttacs (occup p))
                               $ bbToSquares $ queens p .&. mypc
          -- mypc = myPieces p c
          -- yopi  = yoPieces p c
          (!mypc, !yopi) = thePieces p c
          kattac x = x .&. kings p .&. yopi

-- Generate move when in check
genMoveFCheck :: MyPos -> Color -> [(Square, Square)]
genMoveFCheck !p c
    | null chklist = error "genMoveFCheck"
    | null $ tail chklist = r1 ++ kGen ++ r2	-- simple check
    | otherwise = kGen				-- double check, only king moves help
    where !chklist = findChecking p $ other c
          !kGen = srcDests (legal . kAttacs) ksq
          !ksq = firstOne kbb
          !kbb = kings p .&. mypc
          !ocp1 = occup p `less` kbb
          legal x = x `less` alle
          !alle = mypc .|. oppAt .|. excl
          !mypc = myPieces p c
          !oppAt = if c == White then blAttacs p else whAttacs p
          !excl = foldl' (.|.) 0 $ map chkAtt chklist
          chkAtt (NormalCheck f s) = fAttacs s f ocp1
          chkAtt (QueenCheck f s)  = fAttacs s f ocp1
          (r1, r2) = case head chklist of	-- this is needed only when simple check
                 NormalCheck Pawn sq   -> (beatAt p c (bit sq), [])  -- cannot block pawn
                 NormalCheck Knight sq -> (beatAt p c (bit sq), [])  -- or knight check
                 NormalCheck Bishop sq -> beatOrBlock Bishop p c sq
                 NormalCheck Rook sq   -> beatOrBlock Rook p c sq
                 QueenCheck pt sq      -> beatOrBlock pt p c sq
                 _                     -> error "genMoveFCheck: what check?"

-- Generate moves ending on a given square (used to defend a check by capture or blocking)
-- This part is only for queens, rooks, bishops and knights (no pawns and, of course, no kings)
defendAt :: MyPos -> Color -> BBoard -> [(Square, Square)]
defendAt p c bb = concat [ nGenC, bGenC, rGenC, qGenC ]
    where nGenC = concatMap (srcDests (target . nAttacs))
                     $ bbToSquares $ knights p .&. mypc `less` pinned p
          bGenC = concatMap (srcDests (target . bAttacs (occup p)))
                     $ bbToSquares $ bishops p .&. mypc `less` pinned p
          rGenC = concatMap (srcDests (target . rAttacs (occup p)))
                     $ bbToSquares $ rooks p .&. mypc `less` pinned p
          qGenC = concatMap (srcDests (target . qAttacs (occup p)))
                     $ bbToSquares $ queens p .&. mypc `less` pinned p
          target x = x .&. bb
          mypc = myPieces p c

-- Generate capture pawn moves ending on a given square (used to defend a check by capture)
-- TODO: Here: the promotion is not correct (does not promote!)
pawnBeatAt :: MyPos -> Color -> BBoard -> [(Square, Square)]
pawnBeatAt p c bb = concatMap (srcDests (pcapt . pAttacs c))
                           $ bbToSquares $ pawns p .&. mypc `less` pinned p
    where -- yopi  = yoPieces p c
          yopiep = bb .&. (yopi .|. (epcas p .&. epMask))
          pcapt x = x .&. yopiep
          -- mypc = myPieces p c
          (mypc, yopi) = thePieces p c

-- Generate blocking pawn moves ending on given squares (used to defend a check by blocking)
-- TODO: Here: the promotion is not correct (does not promote!)
pawnBlockAt :: MyPos -> Color -> BBoard -> [(Square, Square)]
pawnBlockAt p c bb = concatMap (srcDests (block . \s -> pMovs s c (occup p))) 
                            $ bbToSquares $ pawns p .&. mypc `less` pinned p
    where block x = x .&. bb
          mypc = myPieces p c

beatAt :: MyPos -> Color -> BBoard -> [(Square, Square)]
beatAt p c bb = pawnBeatAt p c bb ++ defendAt p c bb

blockAt :: MyPos -> Color -> BBoard -> [(Square, Square)]
blockAt p c bb = pawnBlockAt p c bb ++ defendAt p c bb

-- Defend a check from a sliding piece: beat it or block it
beatOrBlock :: Piece -> MyPos -> Color -> Square -> ([(Square, Square)], [(Square, Square)])
beatOrBlock f p c sq = (beat, block)
    where !beat = beatAt p c $ bit sq
          atp = if c == White then white p else black p
          aksq = firstOne $ atp .&. kings p
          (_, line) = findLKA f aksq sq
          !block = blockAt p c line

genMoveNCaptToCheck :: MyPos -> Color -> [(Square, Square)]
genMoveNCaptToCheck p c = genMoveNCaptDirCheck p c ++ genMoveNCaptIndirCheck p c

-- Todo: check with pawns (should be also without transformations)
genMoveNCaptDirCheck :: MyPos -> Color -> [(Square, Square)]
-- genMoveNCaptDirCheck p c = concat [ nGenC, bGenC, rGenC, qGenC ]
genMoveNCaptDirCheck p c = concat [ qGenC, rGenC, bGenC, nGenC ]
    where nGenC = concatMap (srcDests (target nTar . nAttacs))
                     $ bbToSquares $ knights p .&. mypc `less` pinned p
          bGenC = concatMap (srcDests (target bTar . bAttacs (occup p)))
                     $ bbToSquares $ bishops p .&. mypc `less` pinned p
          rGenC = concatMap (srcDests (target rTar . rAttacs (occup p)))
                     $ bbToSquares $ rooks p .&. mypc `less` pinned p
          qGenC = concatMap (srcDests (target qTar . qAttacs (occup p)))
                     $ bbToSquares $ queens p .&. mypc `less` pinned p
          target b x = x .&. b
          (mypc, yopc) = thePieces p c
          ksq  = firstOne $ yopc .&. kings p
          nTar = fAttacs ksq Knight (occup p) `less` yopc
          bTar = fAttacs ksq Bishop (occup p) `less` yopc
          rTar = fAttacs ksq Rook   (occup p) `less` yopc
          qTar = bTar .|. rTar

-- TODO: indirect non capture checking moves
genMoveNCaptIndirCheck :: MyPos -> Color -> [(Square, Square)]
genMoveNCaptIndirCheck _ _ = []

sortByMVVLVA :: MyPos -> [(Square, Square)] -> [(Square, Square)]
sortByMVVLVA p = map snd . sortBy (comparing fst) . map va
    where va ft@(f, t) | Busy _ f1 <- tabla p f, Busy _ f2 <- tabla p t
                       = let !vic = - matPiece White f2
                             !agr =   matPiece White f1
                         in ((vic, agr), ft)
          va _ = error "sortByMVVLVA: not a capture"

-- {-# INLINE updatePos #-}
updatePos :: MyPos -> MyPos
updatePos = updatePosCheck . updatePosAttacs . updatePosOccup

updatePosOccup :: MyPos -> MyPos
updatePosOccup p = p {
                  occup = toccup, white = twhite, kings   = tkings,
                  pawns   = tpawns, knights = tknights, queens  = tqueens,
                  rooks   = trooks, bishops = tbishops
               }
    where !toccup = kkrq p .|. diag p
          !tkings = kkrq p .&. diag p `less` slide p
          !twhite = toccup `less` black p
          !tpawns   = diag p `less` (kkrq p .|. slide p)
          !tknights = kkrq p `less` (diag p .|. slide p)
          !tqueens  = slide p .&. kkrq p .&. diag p
          !trooks   = slide p .&. kkrq p `less` diag p
          !tbishops = slide p .&. diag p `less` kkrq p

updatePosAttacs :: MyPos -> MyPos
updatePosAttacs p = p {
        whPAttacs = twhPAtt, whNAttacs = twhNAtt, whBAttacs = twhBAtt,
        whRAttacs = twhRAtt, whQAttacs = twhQAtt, whKAttacs = twhKAtt,
        -- whAttacs = twhPAtt .|. twhNAtt .|. twhBAtt .|. twhRAtt .|. twhQAtt .|. twhKAtt,
        blPAttacs = tblPAtt, blNAttacs = tblNAtt, blBAttacs = tblBAtt,
        blRAttacs = tblRAtt, blQAttacs = tblQAtt, blKAttacs = tblKAtt,
        -- blAttacs = tblPAtt .|. tblNAtt .|. tblBAtt .|. tblRAtt .|. tblQAtt .|. tblKAtt
        whAttacs = twhAttacs, blAttacs = tblAttacs
    }
    where !twhPAtt = bbToSquaresBB (pAttacs White) $ pawns p .&. white p
          !twhNAtt = bbToSquaresBB nAttacs $ knights p .&. white p
          -- !twhBAtt = foldl' (\w s -> w .|. bAttacs s (occup p)) 0 $ bbToSquares $ bishops p .&. white p
          -- !twhRAtt = foldl' (\w s -> w .|. rAttacs s (occup p)) 0 $ bbToSquares $ rooks p .&. white p
          -- !twhQAtt = foldl' (\w s -> w .|. qAttacs s (occup p)) 0 $ bbToSquares $ queens p .&. white p
          !twhBAtt = bbToSquaresBB (bAttacs ocp) $ bishops p .&. white p
          !twhRAtt = bbToSquaresBB (rAttacs ocp) $ rooks p .&. white p
          !twhQAtt = bbToSquaresBB (qAttacs ocp) $ queens p .&. white p
          !twhKAtt = kAttacs $ firstOne $ kings p .&. white p
          !tblPAtt = bbToSquaresBB (pAttacs Black) $ pawns p .&. black p
          !tblNAtt = bbToSquaresBB nAttacs $ knights p .&. black p
          -- !tblBAtt = foldl' (\w s -> w .|. bAttacs s (occup p)) 0 $ bbToSquares $ bishops p .&. black p
          -- !tblRAtt = foldl' (\w s -> w .|. rAttacs s (occup p)) 0 $ bbToSquares $ rooks p .&. black p
          -- !tblQAtt = foldl' (\w s -> w .|. qAttacs s (occup p)) 0 $ bbToSquares $ queens p .&. black p
          !tblBAtt = bbToSquaresBB (bAttacs ocp) $ bishops p .&. black p
          !tblRAtt = bbToSquaresBB (rAttacs ocp) $ rooks p .&. black p
          !tblQAtt = bbToSquaresBB (qAttacs ocp) $ queens p .&. black p
          !tblKAtt = kAttacs $ firstOne $ kings p .&. black p
          !twhAttacs = twhPAtt .|. twhNAtt .|. twhBAtt .|. twhRAtt .|. twhQAtt .|. twhKAtt
          !tblAttacs = tblPAtt .|. tblNAtt .|. tblBAtt .|. tblRAtt .|. tblQAtt .|. tblKAtt
          ocp = occup p

updatePosCheck :: MyPos -> MyPos
updatePosCheck p = p {
                  check = tcheck
                  -- pinned = calcPinned p wpind bpind,
                  -- wpindirs = wpind, bpindirs = bpind
               }
    where !whcheck = white p .&. kings p .&. blAttacs p
          !blcheck = black p .&. kings p .&. whAttacs p
          !tcheck = blcheck .|. whcheck

-- compute the actually pinned pieces based on pining directions and occupancy
-- {-# INLINE calcPinned #-}
{-
calcPinned p wpind bpind = wpi .|. bpi
    where wpi = foldl' (.|.) 0 $ filter ((/= 0) . (.&. white p))
                    $ filter exactOne $ map ((.&. occup p) . snd) bpind
          bpi = foldl' (.|.) 0 $ filter ((/= 0) . (.&. black p))
                    $ filter exactOne $ map ((.&. occup p) . snd) wpind
-}

-- Generate the castle moves
-- Here we could optimize a bit by defining constants separately for White and Black
-- and test anyway kingmoved first (or even a more general pattern for all moved)
genMoveCast :: MyPos -> Color -> [Move]
genMoveCast p c
    | inCheck p || kingmoved = []
    | otherwise = kingside ++ queenside
    where (ksq, cmidk, cmidq, opAtt) =
             if c == White then (4,  caRMKw, caRMQw, blAttacs p)
                           else (60, caRMKb, caRMQb, whAttacs p)
          kingmoved = not (epcas p `testBit` ksq)
          rookk = ksq + 3
          rookq = ksq - 4
          kingside  = if (epcas p `testBit` rookk) && (occup p .&. cmidk == 0) && (opAtt .&. cmidk == 0)
                        then [caks] else []
          queenside = if (epcas p `testBit` rookq) && (occup p .&. cmidq == 0) && (opAtt .&. cmidq == 0)
                        then [caqs] else []
          caks = makeCastleFor c True
          caqs = makeCastleFor c False

-- Set a piece on a square of the table
setPiece :: Square -> Color -> Piece -> MyPos -> MyPos
setPiece sq c f p = p { basicPos = nbp, zobkey = nzob, mater = nmat }
    where setCond cond = if cond then (.|. bsq) else (.&. nbsq)
          nbp = (basicPos p) {
                    bpblack = setCond (c == Black) $ black p,
                    bpslide = setCond (isSlide f)  $ slide p,
                    bpkkrq  = setCond (isKkrq f)   $ kkrq p,
                    bpdiag  = setCond (isDiag f)   $ diag p
                }
          nzob = zobkey p `xor` zold `xor` znew
          nmat = mater p - mold + mnew
          (!zold, !mold) = case tabla p sq of
                             Empty      -> (0, 0)
                             Busy co fo -> (zobPiece co fo sq, matPiece co fo)
          !znew = zobPiece c f sq
          !mnew = matPiece c f
          bsq = 1 `unsafeShiftL` sq
          !nbsq = complement bsq

kingsOk, checkOk :: MyPos -> Bool
{-# INLINE kingsOk #-}
{-# INLINE checkOk #-}
kingsOk p = exactOne (kings p .&. white p)
         && exactOne (kings p .&. black p)
checkOk p = if nextmovewhite then blincheck == 0 else whincheck == 0
    where nextmovewhite = (epcas p .&. mvMask) == 0
          whincheck = white p .&. kings p .&. blAttacs p
          blincheck = black p .&. kings p .&. whAttacs p

data ChangeAccum = CA !ZKey !Int

-- Accumulate a set of changes in MyPos (except BBoards) due to setting a piece on a square
accumSetPiece :: Square -> Color -> Piece -> MyPos -> ChangeAccum -> ChangeAccum
accumSetPiece sq c f p (CA z m)
    = case tabla p sq of
        Empty      -> CA znew mnew
        Busy co fo -> accumCapt sq co fo znew mnew
    where !znew = z `xor` zobPiece c f sq
          !mnew = m + matPiece c f

-- Accumulate a set of changes in MyPos (except BBoards) due to clearing a square
accumClearSq :: Square -> MyPos -> ChangeAccum -> ChangeAccum
accumClearSq sq p i@(CA z m)
    = case tabla p sq of
        Empty      -> i
        Busy co fo -> accumCapt sq co fo z m

accumCapt :: Square -> Color -> Piece -> ZKey -> Int -> ChangeAccum
accumCapt sq co fo z m = CA (z `xor` zco) (m - mco)
    where !zco = zobPiece co fo sq
          !mco = matPiece co fo

accumMoving :: MyPos -> ChangeAccum -> ChangeAccum
accumMoving _ (CA z m) = CA (z `xor` zobMove) m

-- Take an initial accumulation and a list of functions accum to accum
-- and compute the final accumulation
chainAccum :: ChangeAccum -> [ChangeAccum -> ChangeAccum] -> ChangeAccum
chainAccum = foldl (flip ($))

{-
changePining :: MyPos -> Square -> Square -> Bool
changePining p src dst = kings p `testBit` src	-- king is moving
                      || slide p `testBit` src -- pining piece is moving
                      || slide p `testBit` dst -- pining piece is captured
-}

{-# INLINE clearCast #-}
clearCast :: Square -> BBoard -> BBoard
clearCast sq bb
    = case caRiMa .&. bsq of
        0 -> bb
        _ -> bb .&. nbsq
    where bsq = 1 `unsafeShiftL` sq
          nbsq = complement bsq

-- Just for a dumb debug: a quick check if two consecutive moves
-- can be part of a move sequence
alternateMoves :: MyPos -> Move -> Move -> Bool
alternateMoves p m1 m2
    | Busy c1 _ <- tabla p src1,
      Busy c2 _ <- tabla p src2 = c1 /= c2
    | otherwise = True	-- means: we cannot say...
    where src1 = fromSquare m1
          src2 = fromSquare m2

-- This is used to filter the illegal moves coming from killers or hash table
-- but we must treat special moves (en-passant, castle and promotion) differently,
-- because they are more complex
legalMove :: MyPos -> Move -> Bool
legalMove p m
    | Busy col fig <- tabla p src,
      colp <- moving p =
        let mtype = if moveIsNormal m
                       then not owndst && canMove fig p src dst
                       else not owndst && specialMoveIsLegal p m
            owndst = myPieces p col `uTestBit` dst
        in colp == col && mtype
    | otherwise = False
    where src = fromSquare m
          dst = toSquare m

-- currently we assume for simplicity that special moves coming from killers or hash
-- are illegal, so they will be tried after the regular generation, and not as killers
specialMoveIsLegal :: MyPos -> Move -> Bool
specialMoveIsLegal _ _ = False

nonCapt :: MyPos -> Move -> Bool
nonCapt p m
    | Busy _ _ <- tabla p (toSquare m) = False
    | otherwise                        = True

canMove :: Piece -> MyPos -> Square -> Square -> Bool
canMove Pawn p src dst
    | (src - dst) .&. 0x7 == 0 = elem dst $
         map snd $ pAll1Moves col pw (occup p) ++ pAll2Moves col pw (occup p)
    | otherwise = pAttacs col src `uTestBit` dst
    where col = moving p
          pw = bit src
canMove fig p src dst = fAttacs src fig (occup p) `uTestBit` dst

mvBit :: Square -> Square -> BBoard -> BBoard
mvBit !src !dst !w	-- = w `xor` ((w `xor` (shifted .&. nbsrc)) .&. mask)
    | wsrc == 0 = case wdst of
                      0 -> w
                      _ -> w .&. nbdst
    | otherwise = case wdst of
                      0 -> w .&. nbsrc .|. bdst
                      _ -> w .&. nbsrc
    where bsrc = 1 `unsafeShiftL` src
          !bdst = 1 `unsafeShiftL` dst
          wsrc = w .&. bsrc
          wdst = w .&. bdst
          nbsrc = complement bsrc
          nbdst = complement bdst

-- Copy one square to another and clear the source square
-- doFromToMove :: Square -> Square -> MyPos -> Maybe MyPos
doFromToMove :: Move -> MyPos -> MyPos
doFromToMove m p | moveIsNormal m = updatePos p {
                                        basicPos = nbp, zobkey = tzobkey, mater = tmater
                                    }
    where nbp = BPos {
              bpblack = tblack, bpslide = tslide, bpkkrq  = tkkrq,  bpdiag  = tdiag,
              bpepcas = tepcas
          }
          src = fromSquare m
          dst = toSquare m
          tblack = mvBit src dst $ black p
          tslide = mvBit src dst $ slide p
          tkkrq  = mvBit src dst $ kkrq p
          tdiag  = mvBit src dst $ diag p
          pawnmoving = case tabla p src of
                       Busy _ fig -> fig == Pawn
                       _          -> False	-- actually this is an error!
          iscapture  = case tabla p dst of
                       Empty -> False
                       _     -> True
          irevers = pawnmoving || iscapture
          -- Here: we have to xor with the zobrist keys for casts! Only when rights change!
          tepcas' = clearCast src $ clearCast dst $ epcas p `xor` mvMask	-- to do: ep
          tepcas  = if irevers then reset50Moves tepcas' else addHalfMove tepcas'
          CA tzobkey tmater = case tabla p src of	-- identify the moving piece
               Busy col fig -> chainAccum (CA (zobkey p) (mater p)) [
                                   accumClearSq src p,
                                   accumSetPiece dst col fig p,
                                   accumMoving p
                               ]
               _ -> error $ "Src field empty: " ++ show src ++ " dst " ++ show dst ++ " in pos\n"
                                 ++ showTab (black p) (slide p) (kkrq p) (diag p)
                                 ++ "resulting pos:\n"
                                 ++ showTab tblack tslide tkkrq tdiag
doFromToMove m p | moveIsEnPas m = updatePos p {
                                       basicPos = nbp, zobkey = tzobkey, mater = tmater
                                   }
    where nbp = BPos {
              bpblack = tblack, bpslide = tslide, bpkkrq  = tkkrq,  bpdiag  = tdiag,
              bpepcas = tepcas
          }
          src = fromSquare m
          dst = toSquare m
          del = moveEnPasDel m
          bdel = 1 `unsafeShiftL` del
          nbdel = complement bdel
          tblack = mvBit src dst (black p) .&. nbdel
          tslide = mvBit src dst (slide p) .&. nbdel
          tkkrq  = mvBit src dst (kkrq p) .&. nbdel
          tdiag  = mvBit src dst (diag p) .&. nbdel
          tepcas = reset50Moves $ epcas p `xor` mvMask	-- to do: ep
          Busy col fig  = tabla p src	-- identify the moving piece
          Busy _   Pawn = tabla p del	-- identify the captured piece (pawn)
          CA tzobkey tmater = chainAccum (CA (zobkey p) (mater p)) [
                                accumClearSq src p,
                                accumClearSq del p,
                                accumSetPiece dst col fig p,
                                accumMoving p
                            ]
doFromToMove m p | moveIsTransf m = updatePos p0 {
                                        basicPos = nbp, zobkey = tzobkey, mater = tmater
                                    }
    where nbp = BPos {
              bpblack = tblack, bpslide = tslide, bpkkrq = tkkrq, bpdiag = tdiag,
              bpepcas = tepcas
          }
          src = fromSquare m
          dst = toSquare m
          Busy col Pawn = tabla p src	-- identify the moving color (piece must be pawn)
          pie = moveTransfPiece m
          p0 = setPiece src (moving p) pie p
          tblack = mvBit src dst $ black p0
          tslide = mvBit src dst $ slide p0
          tkkrq  = mvBit src dst $ kkrq p0
          tdiag  = mvBit src dst $ diag p0
          tepcas = reset50Moves $ epcas p `xor` mvMask	-- to do: ep
          CA tzobkey tmater = chainAccum (CA (zobkey p0) (mater p0)) [
                                accumClearSq src p0,
                                accumSetPiece dst col pie p0,
                                accumMoving p0
                            ]
doFromToMove m p | moveIsCastle m = updatePos p {
                                        basicPos = nbp, zobkey = tzobkey, mater = tmater
                                    }
    where nbp = BPos {
              bpblack = tblack, bpslide = tslide, bpkkrq  = tkkrq,  bpdiag  = tdiag,
              bpepcas = tepcas
          }
          src = fromSquare m
          dst = toSquare m
          (csr, cds) = case src of
              4  -> case dst of
                  6 -> (7, 5)
                  2 -> (0, 3)
                  _ -> error $ "Wrong destination for castle move " ++ show m
              60 -> case dst of
                  62 -> (63, 61)
                  58 -> (56, 59)
                  _ -> error $ "Wrong destination for castle move " ++ show m
              _  -> error $ "Wrong source for castle move " ++ show m
          tblack = mvBit csr cds $ mvBit src dst $ black p
          tslide = mvBit csr cds $ mvBit src dst $ slide p
          tkkrq  = mvBit csr cds $ mvBit src dst $ kkrq p
          tdiag  = mvBit csr cds $ mvBit src dst $ diag p
          -- Here: we have to xor with the zobrist keys for casts! Only when rights change!
          tepcas = reset50Moves $ clearCast src $ epcas p `xor` mvMask	-- to do: ep
          Busy col King = tabla p src	-- identify the moving piece (king)
          Busy co1 Rook = tabla p csr	-- identify the moving rook
          CA tzobkey tmater = chainAccum (CA (zobkey p) (mater p)) [
                                accumClearSq src p,
                                accumSetPiece dst col King p,
                                accumClearSq csr p,
                                accumSetPiece cds co1 Rook p,
                                accumMoving p
                            ]
doFromToMove _ _ = error "doFromToMove: wrong move type"

reverseMoving :: MyPos -> MyPos
reverseMoving p = p { basicPos = nbp, zobkey = z }
    where nbp = (basicPos p) { bpepcas = tepcas }
          tepcas = epcas p `xor` mvMask
          CA z _ = chainAccum (CA (zobkey p) (mater p)) [
                       accumMoving p
                   ]
-- Here is not clear what to do with castle and en passant...
