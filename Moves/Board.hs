{-# LANGUAGE TypeSynonymInstances, MultiParamTypeClasses, PatternGuards, BangPatterns #-}
{-# LANGUAGE CPP #-}
module Moves.Board (
    posFromFen, initPos,
    isCheck, inCheck,
    goPromo, movePassed, hasMoves,
    genmv, genmvT, kingMoved, castKingRookOk, castQueenRookOk,
    -- genMoveCapt,
    genMoveCast, genMoveNCapt, genMoveTransf, genMoveFCheck, genMoveCaptWL,
    genMoveNCaptToCheck,
    updatePos, kingsOk, checkOk,
    legalMove, alternateMoves, nonCapt,
    doFromToMove, reverseMoving
    ) where

import Prelude hiding ((++), foldl, filter, map, concatMap, concat, head, tail, repeat, zip,
                       zipWith, null, words, foldr, elem, lookup, any, takeWhile, iterate)
-- import Control.Exception (assert)
import Data.Array.Base (unsafeAt)
import Data.Array.Unboxed
import Data.Bits
import Data.List.Stream
import Data.Char
import Data.Maybe
import Data.Ord (comparing)

import Struct.Struct
import Moves.Moves
import Moves.BitBoard
-- import Moves.Muster
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

posFromFen :: String -> MyPos
posFromFen fen = p { epcas = x, zobkey = zk }
    where fen1:fen2:fen3:fen4:fen5:_ = fenFromString fen
          p  = fenToTable fen1
          -- bp = (basicPos p) { bpepcas = x }
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
          (epInit, ze) = case fen4 of
              f:r:_ | f `elem` "abcdefgh" && r `elem` "36"
                    -> let fn  = ord f - ord 'a'
                           ms' = case r of
                                     '3' -> 0x10000
                                     _   -> 0x10000000000
                           ms = ms' `shiftL` fn
                           zz = zobEP fn
                       in ((.|.) ms, zz)
              _     -> (id, 0)
          fyInit = set50Moves $ read fen5
          zk = zobkey p `xor` z `xor` z1 `xor` z2 `xor` z3 `xor` z4 `xor` ze

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
{-# INLINE isCheck #-}
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

movePassed :: MyPos -> Move -> Bool
movePassed p m = passed p .&. (unsafeShiftL 1 $ fromSquare m) /= 0

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

{--
-- This one is not used anymore - see genMoveCaptWL
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
--}

genMoveNCapt :: MyPos -> [Move]
-- genMoveNCapt p c = concat [ pGenNC2, qGenNC, rGenNC, bGenNC, nGenNC, pGenNC1, kGenNC ]
-- genMoveNCapt p c = concat [ pGenNC1, nGenNC, bGenNC, rGenNC, qGenNC, pGenNC2, kGenNC ]
genMoveNCapt !p = map (uncurry moveFromTo)
                      $ concat [ nGenNC, bGenNC, rGenNC, qGenNC, pGenNC1, pGenNC2, kGenNC ]
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

-- Generate only promotions (now only to queen) - captures and non captures
genMoveTransf :: MyPos -> [Move]
genMoveTransf !p = map (uncurry (makeTransf Queen)) $ pGenC ++ pGenNC
    where pGenC = concatMap (srcDests (pcapt . pAttacs c))
                     $ bbToSquares $ pawns p .&. myfpc
          pGenNC = pAll1Moves c (pawns p .&. myfpc) (occup p)
          !myfpc = me p .&. traR
          -- !yopiep = yo p .|. (epcas p .&. epMask)	-- e.p. cannot promote
          pcapt = (.&. yo p)
          !traR = if c == White then 0x00FF000000000000 else 0xFF00
          !c = moving p

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
genMoveFCheck :: MyPos -> [Move]
genMoveFCheck !p
    | null chklist = error "genMoveFCheck"
    | null $ tail chklist = r1 ++ kGen ++ r2	-- simple check
    | otherwise = kGen				-- double check, only king moves help
    where !chklist = findChecking p
          !kGen = map (uncurry moveFromTo) $ srcDests (legal . kAttacs) ksq
          !ksq = firstOne kbb
          !kbb = kings p .&. me p
          !ocp1 = occup p `less` kbb
          legal = (`less` alle)
          !alle = me p .|. yoAttacs p .|. excl
          !excl = foldl' (.|.) 0 $ map chkAtt chklist
          chkAtt (NormalCheck f s) = fAttacs s f ocp1
          chkAtt (QueenCheck f s)  = fAttacs s f ocp1
          (r1, r2) = case head chklist of	-- this is needed only when simple check
                 NormalCheck Pawn sq   -> (beatAtP p (bit sq), [])  -- cannot block pawn
                 NormalCheck Knight sq -> (beatAt  p (bit sq), [])  -- or knight check
                 NormalCheck Bishop sq -> beatOrBlock Bishop p sq
                 NormalCheck Rook sq   -> beatOrBlock Rook p sq
                 QueenCheck pt sq      -> beatOrBlock pt p sq
                 _                     -> error "genMoveFCheck: what check?"

-- Generate moves ending on a given square (used to defend a check by capture or blocking)
-- This part is only for queens, rooks, bishops and knights (no pawns and, of course, no kings)
defendAt :: MyPos -> BBoard -> [Move]
defendAt p !bb = map (uncurry moveFromTo) $ concat [ nGenC, bGenC, rGenC, qGenC ]
    where nGenC = concatMap (srcDests (target . nAttacs))
                     $ bbToSquares $ knights p .&. me p
          bGenC = concatMap (srcDests (target . bAttacs (occup p)))
                     $ bbToSquares $ bishops p .&. me p
          rGenC = concatMap (srcDests (target . rAttacs (occup p)))
                     $ bbToSquares $ rooks p .&. me p
          qGenC = concatMap (srcDests (target . qAttacs (occup p)))
                     $ bbToSquares $ queens p .&. me p
          target = (.&. bb)

-- Generate capture pawn moves ending on a given square (used to defend a check by capture)
pawnBeatAt :: MyPos -> BBoard -> [Move]
pawnBeatAt !p bb = map (uncurry (makeTransf Queen))
                       (concatMap
                           (srcDests (pcapt . pAttacs (moving p)))
                           (bbToSquares promo))
                ++ map (uncurry moveFromTo)
                       (concatMap
                           (srcDests (pcapt . pAttacs (moving p)))
                           (bbToSquares rest))
    where !yopi = bb .&. yo p
          pcapt = (.&. yopi)
          (promo, rest) = promoRest p

-- Generate blocking pawn moves ending on given squares (used to defend a check by blocking)
pawnBlockAt :: MyPos -> BBoard -> [Move]
pawnBlockAt p !bb = map (uncurry (makeTransf Queen))
                        (concatMap
                              (srcDests (block . \s -> pMovs s (moving p) (occup p)))
                              (bbToSquares promo))
                 ++ map (uncurry moveFromTo)
                        (concatMap
                              (srcDests (block . \s -> pMovs s (moving p) (occup p)))
                              (bbToSquares rest))
    where block = (.&. bb)
          (promo, rest) = promoRest p

promoRest :: MyPos -> (BBoard, BBoard)
promoRest p
    | moving p == White
                  = let prp = mypawns .&. 0x00FF000000000000
                        rea = mypawns `less` prp
                    in (prp, rea)
    | otherwise   = let prp = mypawns .&. 0xFF00
                        rea = mypawns `less` prp
                    in (prp, rea)
    where !mypawns = pawns p .&. me p

beatAt :: MyPos -> BBoard -> [Move]
beatAt p !bb = pawnBeatAt p bb ++ defendAt p bb

-- Here we generate a possible en passant capture of a pawn which maybe checks
beatAtP :: MyPos -> BBoard -> [Move]
beatAtP p !bb = genEPCapts p ++ defendAt p bb

blockAt :: MyPos -> BBoard -> [Move]
blockAt p !bb = pawnBlockAt p bb ++ defendAt p bb

-- Defend a check from a sliding piece: beat it or block it
beatOrBlock :: Piece -> MyPos -> Square -> ([Move], [Move])
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
                     $ bbToSquares $ knights p .&. me p
          bGenC = concatMap (srcDests (target bTar . bAttacs (occup p)))
                     $ bbToSquares $ bishops p .&. me p
          rGenC = concatMap (srcDests (target rTar . rAttacs (occup p)))
                     $ bbToSquares $ rooks p .&. me p
          qGenC = concatMap (srcDests (target qTar . qAttacs (occup p)))
                     $ bbToSquares $ queens p .&. me p
          target b = (.&. b)
          !ksq  = firstOne $ yo p .&. kings p
          !nTar = fAttacs ksq Knight (occup p) `less` yo p
          !bTar = fAttacs ksq Bishop (occup p) `less` yo p
          !rTar = fAttacs ksq Rook   (occup p) `less` yo p
          !qTar = bTar .|. rTar

-- TODO: indirect non capture checking moves
genMoveNCaptIndirCheck :: MyPos -> [(Square, Square)]
genMoveNCaptIndirCheck _ = []

{--
-- This one is not used anymore
sortByMVVLVA :: MyPos -> [(Square, Square)] -> [(Square, Square)]
sortByMVVLVA p = map snd . sortBy (comparing fst) . map va
    where va ft@(f, t) | Busy _ f1 <- tabla p f, Busy _ f2 <- tabla p t
                       = let !vic = - matPiece White f2
                             !agr =   matPiece White f1
                         in ((vic, agr), ft)
          va _ = error "sortByMVVLVA: not a capture"
--}

-- Small optimisation: .&. instead `less` below
-- This one should be done in Muster.hs and used elsewhere too
notFileA, notFileH :: BBoard
notFileA = 0xFEFEFEFEFEFEFEFE
notFileH = 0x7F7F7F7F7F7F7F7F

-- Passed pawns: only with bitboard operations
whitePassed :: BBoard -> BBoard -> BBoard
whitePassed !wp !bp = wpa
    where !wp3 = shadowDown wp	-- shadow back of the own pawns for back doubles
          !bpL = (bp .&. notFileA) `unsafeShiftR` 9	-- left down
          !bpR = (bp .&. notFileH) `unsafeShiftR` 7	-- and right down
          !bp0 = bpL .|. bp .|. bpR
          !bp3 = shadowDown bp0	-- erase same and neighbours files
          !wpa = wp `less` (wp3 .|. bp3)

blackPassed :: BBoard -> BBoard -> BBoard
blackPassed !wp !bp = bpa
    where !bp3 = shadowUp bp	-- shadow back of the own pawns for back doubles
          !wpL = (wp .&. notFileA) `unsafeShiftL` 7	-- left up
          !wpR = (wp .&. notFileH) `unsafeShiftL` 9	-- and right up
          !wp0 = wpL .|. wp .|. wpR
          !wp3 = shadowUp wp0	-- erase same and neighbours files
          !bpa = bp `less` (bp3 .|. wp3)

{-# INLINE shadowDown #-}
shadowDown :: BBoard -> BBoard
shadowDown !wp = wp3
    where !wp0 =          wp  `unsafeShiftR`  8
          !wp1 = wp0 .|. (wp0 `unsafeShiftR`  8)
          !wp2 = wp1 .|. (wp1 `unsafeShiftR` 16)
          !wp3 = wp2 .|. (wp2 `unsafeShiftR` 32)

{-# INLINE shadowUp #-}
shadowUp :: BBoard -> BBoard
shadowUp !wp = wp3
    where !wp0 =          wp  `unsafeShiftL`  8
          !wp1 = wp0 .|. (wp0 `unsafeShiftL`  8)
          !wp2 = wp1 .|. (wp1 `unsafeShiftL` 16)
          !wp3 = wp2 .|. (wp2 `unsafeShiftL` 32)

updatePos :: MyPos -> MyPos
updatePos = updatePosCheck . updatePosAttacs . updatePosOccup

updatePosOccup :: MyPos -> MyPos
updatePosOccup !p = p {
                  occup = toccup, me = tme, yo = tyo, kings   = tkings,
                  pawns = tpawns, knights = tknights, queens  = tqueens,
                  rooks = trooks, bishops = tbishops, passed = tpassed
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
          !twpawns = tpawns .&. twhite
          !tbpawns = tpawns .&. black p
          !tpassed = whitePassed twpawns tbpawns .|. blackPassed twpawns tbpawns
          -- Further ideas:
          -- 1. The old method could be faster for afew pawns! Tests!!
          -- 2. This is necessary only after a pawn move, otherwise passed remains the same
          -- 3. Unify updatePos: one function with basic fields as parameter and eventually
          --    the old position, then everything in one go - should avoid copying

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

-- Generate the castle moves
-- Here we could optimize a bit by defining constants separately for White and Black
-- and test anyway kingmoved first (or even a more general pattern for all moved)
genMoveCast :: MyPos -> [Move]
genMoveCast p
    | inCheck p || kingMoved p c = []
    | otherwise = kingside ++ queenside
    where (cmidk, cmidq) = if c == White then (caRMKw, caRMQw)
                                         else (caRMKb, caRMQb)
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
setPiece sq c f !p
    = p { black = setCond (c == Black) $ black p,
          slide = setCond (isSlide f)  $ slide p,
          kkrq  = setCond (isKkrq f)   $ kkrq p,
          diag  = setCond (isDiag f)   $ diag p,
          zobkey = nzob, mater = nmat }
    where setCond cond = if cond then (.|. bsq) else (.&. nbsq)
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
clearCast :: BBoard -> BBoard -> BBoard
clearCast sqbb cas = xor (caRiMa .&. cas .&. sqbb) cas

{-# INLINE isClearingCast #-}
isClearingCast :: BBoard -> BBoard -> Bool
isClearingCast sqbb cas = (caRiMa .&. cas .&. sqbb) /= 0

-- When we know we have a move which clears some castling rights
-- we call this function to compute a start zobrist key from
-- the one of the current position, xoring the Zobrist keys
-- for the rights which are lost
-- These ones are precalculated in an array with most elements =0,
-- except for rooks and kings start places - see next array
clearCastZob :: BBoard -> Square -> Square -> BBoard
clearCastZob !zob !src !dst
    = zob `xor` (castZobArray `unsafeAt` src) `xor` (castZobArray `unsafeAt` dst)

castZobArray :: UArray Square ZKey
castZobArray = listArray (0, 63) (repeat 0) // [
        ( 0, zobCastQw), ( 4, zobCastQw `xor` zobCastKw), ( 7, zobCastKw),
        (56, zobCastQb), (60, zobCastQb `xor` zobCastKb), (63, zobCastKb)
    ]

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

{-# INLINE moveAndClearEp #-}
moveAndClearEp :: BBoard -> BBoard
moveAndClearEp bb = bb `xor` (bb .&. epMask) `xor` mvMask

{-# INLINE epToFile #-}
epToFile :: BBoard -> BBoard
epToFile bb = 0xFF .&. (bb `unsafeShiftR` 16 .|. bb `unsafeShiftR` 32)

{-# INLINE epClrZob #-}
epClrZob :: BBoard -> BBoard
epClrZob bb
    | epLastBB' == 0 = 0
    | otherwise      = zobEP epLastSq
    where !epLastBB  = bb .&. epMask
          !epLastBB' = epToFile epLastBB
          epLastSq   = head $ bbToSquares epLastBB'

genEP :: Bool
#ifdef GENEP
genEP = True
#else
genEP = False
#endif

-- Copy one square to another and clear the source square
-- doFromToMove :: Square -> Square -> MyPos -> Maybe MyPos
doFromToMove :: Move -> MyPos -> MyPos
doFromToMove m !p | moveIsNormal m
    = updatePos p {
          black = tblack, slide = tslide, kkrq  = tkkrq,  diag  = tdiag,
          epcas = tepcas, zobkey = tzobkey, mater = tmater
      }
    where src = fromSquare m
          dst = toSquare m
          tblack = mvBit src dst $ black p
          tslide = mvBit src dst $ slide p
          tkkrq  = mvBit src dst $ kkrq p
          tdiag  = mvBit src dst $ diag p
          !srcbb = 1 `unsafeShiftL` src
          !dstbb = 1 `unsafeShiftL` dst
          !pawnmoving = pawns p .&. srcbb /= 0	-- the correct color is
          !iscapture  = occup p .&. dstbb /= 0	-- checked somewhere else
          !isclc = isClearingCast srcbb (epcas p) || isClearingCast dstbb (epcas p)
          !irevers = pawnmoving || iscapture || isclc
          !tepcas' = clearCast srcbb $ clearCast dstbb $ moveAndClearEp $ epcas p
          !tepcas  = setEp $! if irevers then reset50Moves tepcas' else addHalfMove tepcas'
          -- For e.p. zob key:
          !epcl = epClrZob $ epcas p
          (setEp, !epSetZob)
              | genEP && pawnmoving && (src - dst == 16 || dst - src == 16)
                  = let epFld = (src + dst) `unsafeShiftR` 1
                        epBit = 1 `unsafeShiftL` epFld
                    in ((.|.) epBit, zobEP epFld)
              | otherwise = (id, 0)
          -- !zob = if isclc then clearCastZob (zobkey p) src dst else zobkey p
          -- What is faster? Alwas, or only when isclc?
          !zob = clearCastZob (zobkey p `xor` epcl `xor` epSetZob) src dst
          CA tzobkey tmater = case tabla p src of	-- identify the moving piece
               Busy col fig -> chainAccum (CA zob (mater p)) [
                                   accumClearSq src p,
                                   accumSetPiece dst col fig p,
                                   accumMoving p
                               ]
               _ -> error $ "Src field empty: " ++ show src ++ " dst " ++ show dst ++ " in pos\n"
                                 ++ showTab (black p) (slide p) (kkrq p) (diag p)
                                 ++ "resulting pos:\n"
                                 ++ showTab tblack tslide tkkrq tdiag
doFromToMove m !p | moveIsEnPas m
    = updatePos p {
          black = tblack, slide = tslide, kkrq  = tkkrq,  diag  = tdiag,
          epcas = tepcas, zobkey = tzobkey, mater = tmater
      }
    where src = fromSquare m
          dst = toSquare m
          del = moveEnPasDel m
          bdel = 1 `unsafeShiftL` del
          nbdel = complement bdel
          tblack = mvBit src dst (black p) .&. nbdel
          tslide = mvBit src dst (slide p) .&. nbdel
          tkkrq  = mvBit src dst (kkrq p) .&. nbdel
          tdiag  = mvBit src dst (diag p) .&. nbdel
          tepcas = reset50Moves $ moveAndClearEp $ epcas p
          Busy col fig  = tabla p src	-- identify the moving piece
          Busy _   Pawn = tabla p del	-- identify the captured piece (pawn)
          !epcl = epClrZob $ epcas p
          !zk = zobkey p `xor` epcl
          CA tzobkey tmater = chainAccum (CA zk (mater p)) [
                                accumClearSq src p,
                                accumClearSq del p,
                                accumSetPiece dst col fig p,
                                accumMoving p
                            ]
doFromToMove m !p | moveIsTransf m
    = updatePos p0 {
          black = tblack, slide = tslide, kkrq = tkkrq, diag = tdiag,
          epcas = tepcas, zobkey = tzobkey, mater = tmater
      }
    where src = fromSquare m
          dst = toSquare m
          Busy col Pawn = tabla p src	-- identify the moving color (piece must be pawn)
          !pie = moveTransfPiece m
          p0 = setPiece src (moving p) pie p
          tblack = mvBit src dst $ black p0
          tslide = mvBit src dst $ slide p0
          tkkrq  = mvBit src dst $ kkrq p0
          tdiag  = mvBit src dst $ diag p0
          tepcas = reset50Moves $ moveAndClearEp $ epcas p
          !epcl = epClrZob $ epcas p0
          !zk = zobkey p0 `xor` epcl
          CA tzobkey tmater = chainAccum (CA zk (mater p0)) [
                                accumClearSq src p0,
                                accumSetPiece dst col pie p0,
                                accumMoving p0
                            ]
doFromToMove m !p | moveIsCastle m
    = updatePos p {
          black = tblack, slide = tslide, kkrq  = tkkrq,  diag  = tdiag,
          epcas = tepcas, zobkey = tzobkey, mater = tmater
      }
    where src = fromSquare m
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
          srcbb = 1 `unsafeShiftL` src
          tepcas = reset50Moves $ clearCast srcbb $ moveAndClearEp $ epcas p
          Busy col King = tabla p src	-- identify the moving piece (king)
          Busy co1 Rook = tabla p csr	-- identify the moving rook
          !epcl = epClrZob $ epcas p
          !zob = clearCastZob (zobkey p `xor` epcl) src dst
          CA tzobkey tmater = chainAccum (CA zob (mater p)) [
                                accumClearSq src p,
                                accumSetPiece dst col King p,
                                accumClearSq csr p,
                                accumSetPiece cds co1 Rook p,
                                accumMoving p
                            ]
doFromToMove _ _ = error "doFromToMove: wrong move type"

reverseMoving :: MyPos -> MyPos
reverseMoving p = updatePos p { epcas = tepcas, zobkey = z }
    where tepcas = moveAndClearEp $ epcas p
          !epcl = epClrZob $ epcas p
          !zk = zobkey p `xor` epcl
          CA z _ = chainAccum (CA zk (mater p)) [
                       accumMoving p
                   ]

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
genMoveCaptWL :: MyPos -> ([Move], [Move])
genMoveCaptWL !pos
    = foldr (perCaptFieldWL pos (me pos) (yoAttacs pos)) (epcs,[]) $ squaresByMVV pos capts
    where capts = myAttacs pos .&. yo pos
          epcs  = genEPCapts pos

genEPCapts :: MyPos -> [Move]
genEPCapts !pos
    | epBB == 0 = []
    | otherwise = map (\s -> makeEnPas s dst del) $ bbToSquares srcBB
    where !epBB = epcas pos .&. epMask
          dst = head $ bbToSquares epBB
          srcBB = pAttacs (other $ moving pos) dst .&. me pos .&. pawns pos
          del = if moving pos == White then dst + 1 else dst - 1

perCaptFieldWL :: MyPos -> BBoard -> BBoard -> Square -> ([Move], [Move]) -> ([Move], [Move])
perCaptFieldWL pos mypc advdefence sq mvlst
    | hanging   = let mvlst1 = foldr (addHanging  sq) mvlst reAgrsqs
                  in foldr (addHangingP sq) mvlst1 prAgrsqs	-- for promotions
    | otherwise = let mvlst1 = foldr (perCaptWL pos False valto sq) mvlst reAgrsqs
                  in foldr (perCaptWL pos True  valto sq) mvlst1 prAgrsqs	-- promotions
    where myattacs = mypc .&. newAttacs pos sq 0
          Busy _ pcto = tabla pos sq
          valto = value pcto
          hanging = not (advdefence `testBit` sq)
          prAgrsqs = bbToSquares prPawns
          reAgrsqs = squaresByLVA pos $ reAtts
          (prPawns, reAtts)
              | sq >= 56 && moving pos == White
                  = let prp = myattacs .&. pawns pos .&. 0x00FF000000000000
                        rea = myattacs `less` prp
                    in (prp, rea)
              | sq <=  7 && moving pos == Black
                  = let prp = myattacs .&. pawns pos .&. 0xFF00
                        rea = myattacs `less` prp
                    in (prp, rea)
              | otherwise = (0, myattacs)

approximateEasyCapts :: Bool
approximateEasyCapts = True	-- when capturing a better piece: no SEE, it is always winning

perCaptWL :: MyPos -> Bool -> Int -> Square -> Square -> ([Move], [Move]) -> ([Move], [Move])
perCaptWL pos promo gain0 sq sqfa (wsqs, lsqs)
    | promo = (makeTransf Queen sqfa sq : wsqs, lsqs)
    | approx || adv <= gain0 = (ss:wsqs, lsqs)
    | otherwise = (wsqs, ss:lsqs)
    where ss = moveFromTo sqfa sq
          approx = approximateEasyCapts && gain1 >= 0
          Busy _ pcfa = tabla pos sqfa
          v0 = value pcfa
          gain1 = gain0 - v0
          adv = seeMoveValue pos sqfa sq v0

-- Captures of hanging pieces are always winning
addHanging :: Square -> Square -> ([Move], [Move]) -> ([Move], [Move])
addHanging to from (wsqs, lsqs) = (moveFromTo from to : wsqs, lsqs)

addHangingP :: Square -> Square -> ([Move], [Move]) -> ([Move], [Move])
addHangingP to from (wsqs, lsqs) = (makeTransf Queen from to : wsqs, lsqs)

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
