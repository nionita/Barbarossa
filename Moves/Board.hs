{-# LANGUAGE TypeSynonymInstances, MultiParamTypeClasses, PatternGuards, BangPatterns #-}
module Moves.Board (
    posFromFen, initPos,
    isCheck, inCheck,
    goPromo, hasMoves,
    genmv, genmvT, kingMoved, castKingRookOk, castQueenRookOk,
    genMoveCapt, genMoveCast, genMoveNCapt, genMoveTransf, genMoveFCheck, genMoveCaptWL,
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

-- Is color c in check in position p?
isCheck :: MyPos -> Color -> Bool
isCheck p White | check p .&. white == 0 = False
                | otherwise              = True
    where !white = occup p `less` black p
isCheck p Black | check p .&. black p == 0 = False
                | otherwise                = True

{-# INLINE inCheck #-}
inCheck :: MyPos -> Bool
inCheck = (/= 0) . check

goPromo :: MyPos -> Move -> Bool
goPromo p m
    | moveIsTransf m   = True
    | not (ppw || ppb) = False
    | otherwise = case tabla p t of
                      Busy White Pawn -> ppw
                      Busy Black Pawn -> ppb
                      _               -> False
    where !t = toSquare m
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
    | chk       = not . null $ genMoveFCheck p
    | otherwise = anyMove
    where hasPc = any (/= 0) $ map (pcapt . pAttacs c)
                     $ bbToSquares $ pawns p .&. me p
          hasPm = not . null $ pAll1Moves c (pawns p .&. me p) (occup p)
          hasN = any (/= 0) $ map (legmv . nAttacs) $ bbToSquares $ knights p .&. me p
          hasB = any (/= 0) $ map (legmv . bAttacs (occup p))
                     $ bbToSquares $ bishops p .&. me p
          hasR = any (/= 0) $ map (legmv . rAttacs (occup p))
                     $ bbToSquares $ rooks p .&. me p
          hasQ = any (/= 0) $ map (legmv . qAttacs (occup p))
                     $ bbToSquares $ queens p .&. me p
          !hasK = 0 /= (legal . kAttacs $ firstOne $ kings p .&. me p)
          !anyMove = hasK || hasN || hasPm || hasPc || hasQ || hasR || hasB
          chk = inCheck p
          !yopiep = yo p .|. (epcas p .&. epMask)
          legmv = (`less` me p)
          pcapt = (.&. yopiep)
          legal = (`less` yoAttacs p)

-- Move generation generates pseudo-legal moves
genMoveCapt :: MyPos -> [(Square, Square)]
genMoveCapt !p = sortByMVVLVA p allp
    where !pGenC = concatMap (srcDests (pcapt . pAttacs (moving p)))
                     $ bbToSquares $ pawns p .&. me p `less` traR
          !nGenC = concatMap (srcDests (capt . nAttacs)) 
                     $ bbToSquares $ knights p .&. me p
          !bGenC = concatMap (srcDests (capt . bAttacs (occup p)))
                     $ bbToSquares $ bishops p .&. me p
          !rGenC = concatMap (srcDests (capt . rAttacs (occup p)))
                     $ bbToSquares $ rooks p .&. me p
          !qGenC = concatMap (srcDests (capt . qAttacs (occup p)))
                     $ bbToSquares $ queens p .&. me p
          !kGenC =            srcDests (capt . legal . kAttacs)
                     $ firstOne $ kings p .&. me p
          allp = concat [ pGenC, nGenC, bGenC, rGenC, qGenC, kGenC ]
          !yopiep = yo p .|. (epcas p .&. epMask)
          capt = (.&. yo p)
          pcapt = (.&. yopiep)
          legal = (`less` yoAttacs p)
          !traR = if moving p == White then 0x00FF000000000000 else 0xFF00

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

genMoveNCapt :: MyPos -> [(Square, Square)]
-- genMoveNCapt p c = concat [ pGenNC2, qGenNC, rGenNC, bGenNC, nGenNC, pGenNC1, kGenNC ]
-- genMoveNCapt p c = concat [ pGenNC1, nGenNC, bGenNC, rGenNC, qGenNC, pGenNC2, kGenNC ]
genMoveNCapt !p = concat [ nGenNC, bGenNC, rGenNC, qGenNC, pGenNC1, pGenNC2, kGenNC ]
    -- where pGenNCT = concatMap (srcDests True (ncapt . \s -> pMovs s c ocp)) 
    --                  $ bbToSquares $ pawns p .&. mypc .&. traR
    --       pGenNC = concatMap (srcDests False (ncapt . \s -> pMovs s c ocp)) 
    --                  $ bbToSquares $ pawns p .&. mypc `less` traR
    where pGenNC1 = pAll1Moves c (pawns p .&. me p `less` traR) (occup p)
          pGenNC2 = pAll2Moves c (pawns p .&. me p) (occup p)
          nGenNC = concatMap (srcDests (ncapt . nAttacs))
                      $ bbToSquares $ knights p .&. me p
          bGenNC = concatMap (srcDests (ncapt . bAttacs (occup p)))
                      $ bbToSquares $ bishops p .&. me p
          rGenNC = concatMap (srcDests (ncapt . rAttacs (occup p)))
                      $ bbToSquares $ rooks p .&. me p
          qGenNC = concatMap (srcDests (ncapt . qAttacs (occup p)))
                      $ bbToSquares $ queens p .&. me p
          kGenNC =            srcDests (ncapt . legal . kAttacs)
                      $ firstOne $ kings p .&. me p
          ncapt = (`less` occup p)
          legal = (`less` yoAttacs p)
          traR = if c == White then 0x00FF000000000000 else 0xFF00
          !c = moving p

-- Generate only transformations (now only to queen) - captures and non captures
genMoveTransf :: MyPos -> [(Square, Square)]
genMoveTransf !p = pGenC ++ pGenNC
    where pGenC = concatMap (srcDests (pcapt . pAttacs c))
                     $ bbToSquares $ pawns p .&. myfpc
          pGenNC = pAll1Moves c (pawns p .&. myfpc) (occup p)
          !myfpc = me p .&. traR
          !yopiep = yo p .|. (epcas p .&. epMask)
          pcapt = (.&. yopiep)
          !traR = if c == White then 0x00FF000000000000 else 0xFF00
          !c = moving p

{--
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
          mypc = me p c .&. pinned p
          ncapt x = x `less` occup p
          traR = if c == White then 0x00FF000000000000 else 0xFF00
          -- mypawns = pawns p .&. mypc
--}

-- {-# INLINE pinMove #-}
pinMove :: MyPos -> Color -> (Square -> BBoard) -> Square -> BBoard
pinMove p c f sq = f sq .&. pinningDir p c sq

-- {-# INLINE pinCapt #-}
pinCapt :: MyPos -> Color -> (Square -> BBoard) -> Square -> BBoard
pinCapt p c f sq = f sq .&. pinningCapt p c sq

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
--}

{-# INLINE srcDests #-}
srcDests :: (Square -> BBoard) -> Square -> [(Square, Square)]
srcDests f !s = zip (repeat s) $ bbToSquares $ f s

-- Because finding the blocking square for a queen check is so hard,
-- we define a data type and, in case of a queen check, we give also
-- the piece type (rook or bishop) in which direction the queen checks
data CheckInfo = NormalCheck Piece !Square
               | QueenCheck Piece !Square

-- Finds pieces which check
findChecking :: MyPos -> [CheckInfo]
findChecking !p = concat [ pChk, nChk, bChk, rChk, qbChk, qrChk ]
    where pChk = map (NormalCheck Pawn) $ filter ((/= 0) . kattac . pAttacs (other $ moving p))
                               $ bbToSquares $ pawns p .&. yo p
          nChk = map (NormalCheck Knight) $ filter ((/= 0) . kattac . nAttacs)
                               $ bbToSquares $ knights p .&. yo p
          bChk = map (NormalCheck Bishop) $ filter ((/= 0) . kattac . bAttacs (occup p))
                               $ bbToSquares $ bishops p .&. yo p
          rChk = map (NormalCheck Rook) $ filter ((/= 0) . kattac . rAttacs (occup p))
                               $ bbToSquares $ rooks p .&. yo p
          qbChk = map (QueenCheck Bishop) $ filter ((/= 0) . kattac . bAttacs (occup p))
                               $ bbToSquares $ queens p .&. yo p
          qrChk = map (QueenCheck Rook) $ filter ((/= 0) . kattac . rAttacs (occup p))
                               $ bbToSquares $ queens p .&. yo p
          !myk = kings p .&. me p
          kattac = (.&. myk)

-- Generate move when in check
genMoveFCheck :: MyPos -> [(Square, Square)]
genMoveFCheck !p
    | null chklist = error "genMoveFCheck"
    | null $ tail chklist = r1 ++ kGen ++ r2	-- simple check
    | otherwise = kGen				-- double check, only king moves help
    where !chklist = findChecking p
          !kGen = srcDests (legal . kAttacs) ksq
          !ksq = firstOne kbb
          !kbb = kings p .&. me p
          !ocp1 = occup p `less` kbb
          legal = (`less` alle)
          !alle = me p .|. yoAttacs p .|. excl
          !excl = foldl' (.|.) 0 $ map chkAtt chklist
          chkAtt (NormalCheck f s) = fAttacs s f ocp1
          chkAtt (QueenCheck f s)  = fAttacs s f ocp1
          (r1, r2) = case head chklist of	-- this is needed only when simple check
                 NormalCheck Pawn sq   -> (beatAt p (bit sq), [])  -- cannot block pawn
                 NormalCheck Knight sq -> (beatAt p (bit sq), [])  -- or knight check
                 NormalCheck Bishop sq -> beatOrBlock Bishop p sq
                 NormalCheck Rook sq   -> beatOrBlock Rook p sq
                 QueenCheck pt sq      -> beatOrBlock pt p sq
                 _                     -> error "genMoveFCheck: what check?"

-- Generate moves ending on a given square (used to defend a check by capture or blocking)
-- This part is only for queens, rooks, bishops and knights (no pawns and, of course, no kings)
defendAt :: MyPos -> BBoard -> [(Square, Square)]
defendAt p !bb = concat [ nGenC, bGenC, rGenC, qGenC ]
    where nGenC = concatMap (srcDests (target . nAttacs))
                     $ bbToSquares $ knights p .&. me p `less` pinned p
          bGenC = concatMap (srcDests (target . bAttacs (occup p)))
                     $ bbToSquares $ bishops p .&. me p `less` pinned p
          rGenC = concatMap (srcDests (target . rAttacs (occup p)))
                     $ bbToSquares $ rooks p .&. me p `less` pinned p
          qGenC = concatMap (srcDests (target . qAttacs (occup p)))
                     $ bbToSquares $ queens p .&. me p `less` pinned p
          target = (.&. bb)

-- Generate capture pawn moves ending on a given square (used to defend a check by capture)
-- TODO: Here: the promotion is not correct (does not promote!)
pawnBeatAt :: MyPos -> BBoard -> [(Square, Square)]
pawnBeatAt !p bb = concatMap (srcDests (pcapt . pAttacs (moving p)))
                           $ bbToSquares $ pawns p .&. me p `less` pinned p
    where !yopiep = bb .&. (yo p .|. (epcas p .&. epMask))
          pcapt = (.&. yopiep)

-- Generate blocking pawn moves ending on given squares (used to defend a check by blocking)
-- TODO: Here: the promotion is not correct (does not promote!)
pawnBlockAt :: MyPos -> BBoard -> [(Square, Square)]
pawnBlockAt p !bb = concatMap (srcDests (block . \s -> pMovs s (moving p) (occup p))) 
                            $ bbToSquares $ pawns p .&. me p `less` pinned p
    where block = (.&. bb)

beatAt :: MyPos -> BBoard -> [(Square, Square)]
beatAt p !bb = pawnBeatAt p bb ++ defendAt p bb

blockAt :: MyPos -> BBoard -> [(Square, Square)]
blockAt p !bb = pawnBlockAt p bb ++ defendAt p bb

-- Defend a check from a sliding piece: beat it or block it
beatOrBlock :: Piece -> MyPos -> Square -> ([(Square, Square)], [(Square, Square)])
beatOrBlock f !p sq = (beat, block)
    where !beat = beatAt p $ bit sq
          !aksq = firstOne $ me p .&. kings p
          !line = findLKA f aksq sq
          !block = blockAt p line

genMoveNCaptToCheck :: MyPos -> [(Square, Square)]
genMoveNCaptToCheck p = genMoveNCaptDirCheck p ++ genMoveNCaptIndirCheck p

-- Todo: check with pawns (should be also without transformations)
genMoveNCaptDirCheck :: MyPos -> [(Square, Square)]
-- genMoveNCaptDirCheck p c = concat [ nGenC, bGenC, rGenC, qGenC ]
genMoveNCaptDirCheck p = concat [ qGenC, rGenC, bGenC, nGenC ]
    where nGenC = concatMap (srcDests (target nTar . nAttacs))
                     $ bbToSquares $ knights p .&. me p `less` pinned p
          bGenC = concatMap (srcDests (target bTar . bAttacs (occup p)))
                     $ bbToSquares $ bishops p .&. me p `less` pinned p
          rGenC = concatMap (srcDests (target rTar . rAttacs (occup p)))
                     $ bbToSquares $ rooks p .&. me p `less` pinned p
          qGenC = concatMap (srcDests (target qTar . qAttacs (occup p)))
                     $ bbToSquares $ queens p .&. me p `less` pinned p
          target b = (.&. b)
          !ksq  = firstOne $ yo p .&. kings p
          !nTar = fAttacs ksq Knight (occup p) `less` yo p
          !bTar = fAttacs ksq Bishop (occup p) `less` yo p
          !rTar = fAttacs ksq Rook   (occup p) `less` yo p
          !qTar = bTar .|. rTar

-- TODO: indirect non capture checking moves
genMoveNCaptIndirCheck :: MyPos -> [(Square, Square)]
genMoveNCaptIndirCheck _ = []

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
updatePosOccup !p = p {
                  occup = toccup, me = tme, yo = tyo, kings   = tkings,
                  pawns = tpawns, knights = tknights, queens  = tqueens,
                  rooks = trooks, bishops = tbishops
               }
    where !toccup = kkrq p .|. diag p
          !tkings = kkrq p .&. diag p `less` slide p
          !twhite = toccup `less` black p
          (!tme, !tyo) | moving p == White = (twhite, black p)
                       | otherwise         = (black p, twhite)
          !tpawns   = diag p `less` (kkrq p .|. slide p)
          !tknights = kkrq p `less` (diag p .|. slide p)
          !tqueens  = slide p .&. kkrq p .&. diag p
          !trooks   = slide p .&. kkrq p `less` diag p
          !tbishops = slide p .&. diag p `less` kkrq p

updatePosAttacs :: MyPos -> MyPos
updatePosAttacs !p
    | moving p == White = p {
                      myPAttacs = twhPAtt, myNAttacs = twhNAtt, myBAttacs = twhBAtt,
                      myRAttacs = twhRAtt, myQAttacs = twhQAtt, myKAttacs = twhKAtt,
                      yoPAttacs = tblPAtt, yoNAttacs = tblNAtt, yoBAttacs = tblBAtt,
                      yoRAttacs = tblRAtt, yoQAttacs = tblQAtt, yoKAttacs = tblKAtt,
                      myAttacs = twhAttacs, yoAttacs = tblAttacs
                  }
    | otherwise         = p {
                      myPAttacs = tblPAtt, myNAttacs = tblNAtt, myBAttacs = tblBAtt,
                      myRAttacs = tblRAtt, myQAttacs = tblQAtt, myKAttacs = tblKAtt,
                      yoPAttacs = twhPAtt, yoNAttacs = twhNAtt, yoBAttacs = twhBAtt,
                      yoRAttacs = twhRAtt, yoQAttacs = twhQAtt, yoKAttacs = twhKAtt,
                      myAttacs = tblAttacs, yoAttacs = twhAttacs
                  }
    where !twhPAtt = bbToSquaresBB (pAttacs White) $ pawns p .&. white
          !twhNAtt = bbToSquaresBB nAttacs $ knights p .&. white
          !twhBAtt = bbToSquaresBB (bAttacs ocp) $ bishops p .&. white
          !twhRAtt = bbToSquaresBB (rAttacs ocp) $ rooks p .&. white
          !twhQAtt = bbToSquaresBB (qAttacs ocp) $ queens p .&. white
          !twhKAtt = kAttacs $ firstOne $ kings p .&. white
          !tblPAtt = bbToSquaresBB (pAttacs Black) $ pawns p .&. black p
          !tblNAtt = bbToSquaresBB nAttacs $ knights p .&. black p
          !tblBAtt = bbToSquaresBB (bAttacs ocp) $ bishops p .&. black p
          !tblRAtt = bbToSquaresBB (rAttacs ocp) $ rooks p .&. black p
          !tblQAtt = bbToSquaresBB (qAttacs ocp) $ queens p .&. black p
          !tblKAtt = kAttacs $ firstOne $ kings p .&. black p
          !twhAttacs = twhPAtt .|. twhNAtt .|. twhBAtt .|. twhRAtt .|. twhQAtt .|. twhKAtt
          !tblAttacs = tblPAtt .|. tblNAtt .|. tblBAtt .|. tblRAtt .|. tblQAtt .|. tblKAtt
          ocp = occup p
          white = ocp `less` black p

updatePosCheck :: MyPos -> MyPos
updatePosCheck p = p {
                  check = tcheck
               }
    where !mecheck = me p .&. kings p .&. yoAttacs p
          !yocheck = yo p .&. kings p .&. myAttacs p
          !tcheck = mecheck .|. yocheck

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
genMoveCast :: MyPos -> [Move]
genMoveCast p
    | inCheck p || kingMoved p c = []
    | otherwise = kingside ++ queenside
    where (cmidk, cmidq, opAtt) =
             if c == White then (caRMKw, caRMQw, yoAttacs p)
                           else (caRMKb, caRMQb, myAttacs p)
          kingside  = if castKingRookOk  p c && (occup p .&. cmidk == 0) && (yoAttacs p .&. cmidk == 0)
                        then [caks] else []
          queenside = if castQueenRookOk p c && (occup p .&. cmidq == 0) && (yoAttacs p .&. cmidq == 0)
                        then [caqs] else []
          caks = makeCastleFor c True
          caqs = makeCastleFor c False
          !c = moving p

{-# INLINE kingMoved #-}
kingMoved :: MyPos -> Color -> Bool
kingMoved !p White = not (epcas p `testBit` 4)
kingMoved !p Black = not (epcas p `testBit` 60)

{-# INLINE castKingRookOk #-}
castKingRookOk :: MyPos -> Color -> Bool
castKingRookOk !p White = epcas p `testBit` 7
castKingRookOk !p Black = epcas p `testBit` 63

{-# INLINE castQueenRookOk #-}
castQueenRookOk :: MyPos -> Color -> Bool
castQueenRookOk !p White = epcas p `testBit` 0
castQueenRookOk !p Black = epcas p `testBit` 56

-- Set a piece on a square of the table
setPiece :: Square -> Color -> Piece -> MyPos -> MyPos
setPiece sq c f !p = p { basicPos = nbp, zobkey = nzob, mater = nmat }
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
kingsOk p = exactOne (kings p .&. me p)
         && exactOne (kings p .&. yo p)
checkOk p = yo p .&. kings p .&. myAttacs p == 0

data ChangeAccum = CA !ZKey !Int

-- Accumulate a set of changes in MyPos (except BBoards) due to setting a piece on a square
accumSetPiece :: Square -> Color -> Piece -> MyPos -> ChangeAccum -> ChangeAccum
accumSetPiece sq c f !p (CA z m)
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
accumCapt sq !co !fo !z !m = CA (z `xor` zco) (m - mco)
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
            owndst = me p `uTestBit` dst
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
doFromToMove m !p | moveIsNormal m = updatePos p {
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
doFromToMove m !p | moveIsEnPas m = updatePos p {
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
doFromToMove m !p | moveIsTransf m = updatePos p0 {
                                        basicPos = nbp, zobkey = tzobkey, mater = tmater
                                    }
    where nbp = BPos {
              bpblack = tblack, bpslide = tslide, bpkkrq = tkkrq, bpdiag = tdiag,
              bpepcas = tepcas
          }
          src = fromSquare m
          dst = toSquare m
          Busy col Pawn = tabla p src	-- identify the moving color (piece must be pawn)
          !pie = moveTransfPiece m
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
doFromToMove m !p | moveIsCastle m = updatePos p {
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
reverseMoving p = updatePos p { basicPos = nbp, zobkey = z }
    where nbp = (basicPos p) { bpepcas = tepcas }
          tepcas = epcas p `xor` mvMask
          CA z _ = chainAccum (CA (zobkey p) (mater p)) [
                       accumMoving p
                   ]
-- Here is not clear what to do with castle and en passant...

-- find pinning lines for a piece type, given the king & piece squares
-- the queen is very hard, so we solve it as a composition of rook and bishop
-- and when we call findLKA we always know as which piece the queen checks
{-# INLINE findLKA #-}
findLKA :: Piece -> Square -> Int -> BBoard
findLKA Queen !ksq !psq
    | rAttacs bpsq ksq .&. bpsq == 0 = findLKA0 Bishop ksq psq
    | otherwise                      = findLKA0 Rook   ksq psq
    where !bpsq = bit psq
findLKA pt !ksq !psq = findLKA0 pt ksq psq

findLKA0 :: Piece -> Square -> Int -> BBoard
findLKA0 pt ksq psq
    | pt == Bishop = go bAttacs
    | pt == Rook   = go rAttacs
    | otherwise    = 0	-- it will not be called with other pieces
    where go f = bb
              where !kp = f (bit psq) ksq
                    !pk = f (bit ksq) psq
                    !bb = kp .&. pk

-- The new SEE functions (swap-based)
-- Choose the cheapest of a set of pieces
chooseAttacker :: MyPos -> BBoard -> (BBoard, Int)
chooseAttacker pos !frompieces
    | p /= 0 = p1 `seq` (p1, value Pawn)
    | n /= 0 = n1 `seq` (n1, value Knight)
    | b /= 0 = b1 `seq` (b1, value Bishop)
    | r /= 0 = r1 `seq` (r1, value Rook)
    | q /= 0 = q1 `seq` (q1, value Queen)
    | k /= 0 = k1 `seq` (k1, value King)
    | otherwise = (0, 0)
    where p = frompieces .&. pawns pos
          n = frompieces .&. knights pos
          b = frompieces .&. bishops pos
          r = frompieces .&. rooks pos
          q = frompieces .&. queens pos
          k = frompieces .&. kings pos
          p1 = lsb p
          n1 = lsb n
          b1 = lsb b
          r1 = lsb r
          q1 = lsb q
          k1 = lsb k

newAttacs :: MyPos -> Square -> BBoard -> BBoard
newAttacs pos sq moved = bAttacs occ sq .&. (b .|. q)
                     .|. rAttacs occ sq .&. (r .|. q)
                     .|. nAttacs     sq .&. n
                     .|. kAttacs     sq .&. k
                     .|. (pAttacs White sq .&. black pos .|. pAttacs Black sq .&. white) .&. p
    where !occ = occup pos `less` moved
          !b = bishops pos  `less` moved
          !r = rooks pos    `less` moved
          !q = queens pos  `less` moved
          !n = knights pos `less` moved
          !k = kings pos   `less` moved
          !p = pawns pos   `less` moved
          !white = occup pos `less` black pos

slideAttacs :: Square -> BBoard -> BBoard -> BBoard -> BBoard -> BBoard
slideAttacs sq b r q occ = bAttacs occ sq .&. (b .|. q)
                       .|. rAttacs occ sq .&. (r .|. q)

xrayAttacs :: MyPos -> Square -> Bool
xrayAttacs pos sq = sa1 /= sa0
    where sa1 = slideAttacs sq (bishops pos) (rooks pos) (queens pos) (occup pos)
          sa0 = slideAttacs sq (bishops pos) (rooks pos) (queens pos) 0

unimax :: Int -> [Int] -> Int
unimax = foldl' (\a g -> min g (-a))

value :: Piece -> Int
value = matPiece White

usePosXRay :: Bool
usePosXRay = False

data SEEPars = SEEPars {
                   seeGain, seeVal :: !Int,
                   seeAtts, seeFrom, seeMovd, seeDefn, seeAgrs :: !BBoard
               }

-- Calculate the value of a move per SEE, given the position,
-- the source square of the first capture, the destination of the captures
-- and the value of the first captured piece
seeMoveValue :: MyPos -> Square -> Square -> Int -> Int
seeMoveValue pos sqfirstmv sqto gain0 = v
    where v = go sp0 [gain0]
          go :: SEEPars -> [Int] -> Int
          go seepars acc =
             let !gain'   = seeVal  seepars -     seeGain seepars
                 !moved'  = seeMovd seepars .|.   seeFrom seepars
                 !attacs1 = seeAtts seepars `xor` seeFrom seepars
                 (!from', !val') = chooseAttacker pos (attacs1 .&. seeAgrs seepars)
                 attacs2  = newAttacs pos sqto moved'
                 acc' = gain' : acc
                 seepars1 = SEEPars { seeGain = gain', seeVal = val', seeAtts = attacs1,
                                      seeFrom = from', seeMovd = moved', seeDefn = seeAgrs seepars,
                                      seeAgrs = seeDefn seepars }
                 seepars2 = SEEPars { seeGain = gain', seeVal = val', seeAtts = attacs2,
                                      seeFrom = from', seeMovd = moved', seeDefn = seeAgrs seepars,
                                      seeAgrs = seeDefn seepars }
             in if from' == 0
                   then unimax (minBound+2) acc
                   else if usePosXRay
                           then if posXRay && seeFrom seepars .&. mayXRay /= 0
                                   then go seepars2 acc'
                                   else go seepars1 acc'
                           else if seeFrom seepars .&. mayXRay /= 0
                                   then go seepars2 acc'
                                   else go seepars1 acc'
          !mayXRay = pawns pos .|. bishops pos .|. rooks pos .|. queens pos
          posXRay = xrayAttacs pos sqto
          !moved0 = bit sqfirstmv
          attacs0 = newAttacs pos sqto moved0
          (!from0, !valfrom) = chooseAttacker pos (attacs0 .&. yo pos)
          sp0 = SEEPars { seeGain = gain0, seeVal = valfrom, seeAtts = newAttacs pos sqto moved0,
                          seeFrom = from0, seeMovd = moved0, seeDefn = yo pos, seeAgrs = me pos }

-- This function can produce illegal captures with the king!
genMoveCaptWL :: MyPos -> ([(Square, Square)], [(Square, Square)])
genMoveCaptWL !pos
    = foldr (perCaptFieldWL pos (me pos) (yoAttacs pos)) ([],[]) $ squaresByMVV pos capts
    where capts = myAttacs pos .&. yo pos

perCaptFieldWL :: MyPos -> BBoard -> BBoard -> Square
          -> ([(Square, Square)], [(Square, Square)])
          -> ([(Square, Square)], [(Square, Square)])
perCaptFieldWL pos mypc advdefence sq mvlst
    | hanging   = foldr (addHanging sq) mvlst agrsqs
    | otherwise = foldr (perCaptWL pos valto sq) mvlst agrsqs
    where myattacs = mypc .&. newAttacs pos sq 0
          Busy _ pcto = tabla pos sq
          valto = value pcto
          hanging = not (advdefence `testBit` sq)
          agrsqs = squaresByLVA pos myattacs

approximateEasyCapts :: Bool
approximateEasyCapts = True	-- when capturing a better piece: no SEE, it is always winning

perCaptWL :: MyPos -> Int -> Square -> Square
          -> ([(Square, Square)], [(Square, Square)])
          -> ([(Square, Square)], [(Square, Square)])
perCaptWL pos gain0 sq sqfa (wsqs, lsqs)
    = if approx || adv <= gain0
         then (ss:wsqs, lsqs)
         else (wsqs, ss:lsqs)
    where ss = (sqfa, sq)
          approx = approximateEasyCapts && gain1 >= 0
          Busy _ pcfa = tabla pos sqfa
          v0 = value pcfa
          gain1 = gain0 - v0
          adv = seeMoveValue pos sqfa sq v0

-- Captures of hanging pieces are always winning
addHanging :: Square -> Square
          -> ([(Square, Square)], [(Square, Square)])
          -> ([(Square, Square)], [(Square, Square)])
addHanging to from (wsqs, lsqs) = ((from, to):wsqs, lsqs)

squaresByMVV :: MyPos -> BBoard -> [Square]
squaresByMVV pos bb = map snd $ sortBy (comparing fst)
                              $ map (mostValuableFirst pos) $ bbToSquares bb

squaresByLVA :: MyPos -> BBoard -> [Square]
squaresByLVA pos bb = map snd $ sortBy (comparing fst)
                              $ map (mostValuableLast pos) $ bbToSquares bb

-- Sort by value in order to get the most valuable last
mostValuableLast :: MyPos -> Square -> (Int, Square)
mostValuableLast pos sq | Busy _ f <- tabla pos sq = let !v = value f in (v, sq)
mostValuableLast _   _                             = error "mostValuableLast: Empty"

-- Sort by negative value in order to get the most valuable first
mostValuableFirst :: MyPos -> Square -> (Int, Square)
mostValuableFirst pos sq | Busy _ f <- tabla pos sq = let !v = - value f in (v, sq)
mostValuableFirst _   _                             = error "mostValuableFirst: Empty"
