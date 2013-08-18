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
import Moves.Muster
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
posFromFen fen = p { epcas = x, zobkey = zk }
    where fen1:fen2:fen3:_:fen5:_ = fenFromString fen
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
{-# INLINE isCheck #-}
isCheck :: MyPos -> Color -> Bool
isCheck p White = white p .&. kings p .&. blAttacs p /= 0
isCheck p Black = black p .&. kings p .&. whAttacs p /= 0

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
    | chk       = not . null $ genMoveFCheck p c
    | otherwise = anyMove
    where hasPc = any (/= 0) $ map (pcapt . pAttacs c)
                     $ bbToSquares $ pawns p .&. mypc
          hasPm = not . null $ pAll1Moves c (pawns p .&. mypc) (occup p)
          hasN = any (/= 0) $ map (legmv . nAttacs) $ bbToSquares $ knights p .&. mypc
          hasB = any (/= 0) $ map (legmv . bAttacs (occup p))
                     $ bbToSquares $ bishops p .&. mypc
          hasR = any (/= 0) $ map (legmv . rAttacs (occup p))
                     $ bbToSquares $ rooks p .&. mypc
          hasQ = any (/= 0) $ map (legmv . qAttacs (occup p))
                     $ bbToSquares $ queens p .&. mypc
          !hasK = 0 /= (legal . kAttacs $ firstOne $ kings p .&. mypc)
          !anyMove = hasK || hasN || hasPm || hasPc || hasQ || hasR || hasB
          chk = inCheck p
          (!mypc, !yopi) = thePieces p c
          !yopiep = yopi .|. (epcas p .&. epMask)
          legmv x = x `less` mypc
          pcapt x = x .&. yopiep
          legal x = x `less` oppAt
          !oppAt = if c == White then blAttacs p else whAttacs p

-- Move generation generates legal moves
genMoveCapt :: MyPos -> Color -> [(Square, Square)]
genMoveCapt !p c = sortByMVVLVA p allp
    where !pGenC = concatMap (srcDests (pcapt . pAttacs c))
                     $ bbToSquares $ pawns p .&. mypc `less` traR
          !nGenC = concatMap (srcDests (capt . nAttacs)) 
                     $ bbToSquares $ knights p .&. mypc
          !bGenC = concatMap (srcDests (capt . bAttacs (occup p)))
                     $ bbToSquares $ bishops p .&. mypc
          !rGenC = concatMap (srcDests (capt . rAttacs (occup p)))
                     $ bbToSquares $ rooks p .&. mypc
          !qGenC = concatMap (srcDests (capt . qAttacs (occup p)))
                     $ bbToSquares $ queens p .&. mypc
          !kGenC =            srcDests (capt . legal . kAttacs)
                     $ firstOne $ kings p .&. mypc
          allp = concat [ pGenC, nGenC, bGenC, rGenC, qGenC, kGenC ]
          (!mypc, !yopi) = thePieces p c
          !yopiep = yopi .|. (epcas p .&. epMask)
          capt x = x .&. yopi
          pcapt x = x .&. yopiep
          legal x = x `less` oppAt
          !oppAt = if c == White then blAttacs p else whAttacs p
          !traR = if c == White then 0x00FF000000000000 else 0xFF00

genMoveNCapt :: MyPos -> Color -> [(Square, Square)]
-- genMoveNCapt p c = concat [ pGenNC2, qGenNC, rGenNC, bGenNC, nGenNC, pGenNC1, kGenNC ]
-- genMoveNCapt p c = concat [ pGenNC1, nGenNC, bGenNC, rGenNC, qGenNC, pGenNC2, kGenNC ]
genMoveNCapt !p c = concat [ nGenNC, bGenNC, rGenNC, qGenNC, pGenNC1, pGenNC2, kGenNC ]
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
          mypc = myPieces p c
          ncapt x = x `less` occup p
          legal x = x `less` oppAt
          oppAt = if c == White then blAttacs p else whAttacs p
          traR = if c == White then 0x00FF000000000000 else 0xFF00

-- Generate only transformations (now only to queen) - captures and non captures
genMoveTransf :: MyPos -> Color -> [(Square, Square)]
genMoveTransf !p c = pGenC ++ pGenNC
    where pGenC = concatMap (srcDests (pcapt . pAttacs c))
                     $ bbToSquares $ pawns p .&. myfpc
    --       pGenNC = concatMap (srcDests False (ncapt . \s -> pMovs s c ocp)) 
    --                  $ bbToSquares $ pawns p .&. myfpc .&. traR
          pGenNC = pAll1Moves c (pawns p .&. myfpc) (occup p)
          (!mypc, !yopi) = thePieces p c
          !myfpc = mypc .&. traR
          !yopiep = yopi .|. (epcas p .&. epMask)
          pcapt x = x .&. yopiep
          !traR = if c == White then 0x00FF000000000000 else 0xFF00

{-# INLINE srcDests #-}
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
defendAt p c !bb = concat [ nGenC, bGenC, rGenC, qGenC ]
    where nGenC = concatMap (srcDests (target . nAttacs))
                     $ bbToSquares $ knights p .&. mypc
          bGenC = concatMap (srcDests (target . bAttacs (occup p)))
                     $ bbToSquares $ bishops p .&. mypc
          rGenC = concatMap (srcDests (target . rAttacs (occup p)))
                     $ bbToSquares $ rooks p .&. mypc
          qGenC = concatMap (srcDests (target . qAttacs (occup p)))
                     $ bbToSquares $ queens p .&. mypc
          target = (.&. bb)
          mypc = myPieces p c

-- Generate capture pawn moves ending on a given square (used to defend a check by capture)
-- TODO: Here: the promotion is not correct (does not promote!)
pawnBeatAt :: MyPos -> Color -> BBoard -> [(Square, Square)]
pawnBeatAt !p c bb = concatMap (srcDests (pcapt . pAttacs c))
                           $ bbToSquares $ pawns p .&. mypc
    where !yopiep = bb .&. (yopi .|. (epcas p .&. epMask))
          pcapt   = (.&. yopiep)
          (mypc, yopi) = thePieces p c

-- Generate blocking pawn moves ending on given squares (used to defend a check by blocking)
-- TODO: Here: the promotion is not correct (does not promote!)
pawnBlockAt :: MyPos -> Color -> BBoard -> [(Square, Square)]
pawnBlockAt p c !bb = concatMap (srcDests (block . \s -> pMovs s c (occup p))) 
                            $ bbToSquares $ pawns p .&. mypc
    where block = (.&. bb)
          mypc = myPieces p c

beatAt :: MyPos -> Color -> BBoard -> [(Square, Square)]
beatAt p c !bb = pawnBeatAt p c bb ++ defendAt p c bb

blockAt :: MyPos -> Color -> BBoard -> [(Square, Square)]
blockAt p c !bb = pawnBlockAt p c bb ++ defendAt p c bb

-- Defend a check from a sliding piece: beat it or block it
beatOrBlock :: Piece -> MyPos -> Color -> Square -> ([(Square, Square)], [(Square, Square)])
beatOrBlock f p c sq = (beat, block)
    where !beat = beatAt p c $ bit sq
          atp = if c == White then white p else black p
          aksq = firstOne $ atp .&. kings p
          line = findLKA f aksq sq
          !block = blockAt p c line

-- Todo: check with pawns (should be also without transformations)
genMoveNCaptDirCheck :: MyPos -> Color -> [(Square, Square)]
-- genMoveNCaptDirCheck p c = concat [ nGenC, bGenC, rGenC, qGenC ]
genMoveNCaptDirCheck p c = concat [ qGenC, rGenC, bGenC, nGenC ]
    where nGenC = concatMap (srcDests (target nTar . nAttacs))
                     $ bbToSquares $ knights p .&. mypc
          bGenC = concatMap (srcDests (target bTar . bAttacs (occup p)))
                     $ bbToSquares $ bishops p .&. mypc
          rGenC = concatMap (srcDests (target rTar . rAttacs (occup p)))
                     $ bbToSquares $ rooks p .&. mypc
          qGenC = concatMap (srcDests (target qTar . qAttacs (occup p)))
                     $ bbToSquares $ queens p .&. mypc
          target b x = x .&. b
          (mypc, yopc) = thePieces p c
          ksq  = firstOne $ yopc .&. kings p
          nTar = fAttacs ksq Knight (occup p) `less` yopc
          bTar = fAttacs ksq Bishop (occup p) `less` yopc
          rTar = fAttacs ksq Rook   (occup p) `less` yopc
          qTar = bTar .|. rTar

genMoveNCaptToCheck :: MyPos -> Color -> [(Square, Square)]
genMoveNCaptToCheck p c = genMoveNCaptDirCheck p c ++ genMoveNCaptIndirCheck p c

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

whitePassPBBs, blackPassPBBs :: UArray Square BBoard
whitePassPBBs = array (0, 63) [(sq, wPassPBB sq) | sq <- [0 .. 63]]
blackPassPBBs = array (0, 63) [(sq, bPassPBB sq) | sq <- [0 .. 63]]

wPassPBB :: Square -> BBoard
wPassPBB sq = foldl' (.|.) 0 $ takeWhile (/= 0) $ iterate (`shiftL` 8) bsqs
    where bsq = bit sq
          bsq8 = bsq `shiftL` 8
          bsq7 = if bsq .&. fileA /= 0 then 0 else bsq `shiftL` 7
          bsq9 = if bsq .&. fileH /= 0 then 0 else bsq `shiftL` 9
          bsqs = bsq7 .|. bsq8 .|. bsq9

bPassPBB :: Square -> BBoard
bPassPBB sq = foldl' (.|.) 0 $ takeWhile (/= 0) $ iterate (`shiftR` 8) bsqs
    where bsq = bit sq
          bsq8 = bsq `shiftR` 8
          bsq7 = if bsq .&. fileH /= 0 then 0 else bsq `shiftR` 7
          bsq9 = if bsq .&. fileA /= 0 then 0 else bsq `shiftR` 9
          bsqs = bsq7 .|. bsq8 .|. bsq9

updatePos :: MyPos -> MyPos
updatePos = updatePosCheck . updatePosAttacs . updatePosOccup

updatePosOccup :: MyPos -> MyPos
updatePosOccup p = p {
                  occup = toccup, white = twhite, kings = tkings,
                  pawns = tpawns, knights = tknights, queens = tqueens,
                  rooks = trooks, bishops = tbishops, passed = tpassed
               }
    where !toccup = kkrq p .|. diag p
          !tkings = kkrq p .&. diag p `less` slide p
          !twhite = toccup `less` black p
          !tpawns   = diag p `less` (kkrq p .|. slide p)
          !tknights = kkrq p `less` (diag p .|. slide p)
          !tqueens  = slide p .&. kkrq p .&. diag p
          !trooks   = slide p .&. kkrq p `less` diag p
          !tbishops = slide p .&. diag p `less` kkrq p
          !twpawns = tpawns .&. twhite
          !tbpawns = tpawns .&. black p
          !wfpbb = foldr (.|.) 0 $ map ubit $ filter wpIsPass $ bbToSquares twpawns1
          !bfpbb = foldr (.|.) 0 $ map ubit $ filter bpIsPass $ bbToSquares tbpawns1
          !tpassed = wfpbb .|. bfpbb
          wpIsPass = (== 0) . (.&. tbpawns) . (unsafeAt whitePassPBBs)
          bpIsPass = (== 0) . (.&. twpawns) . (unsafeAt blackPassPBBs)
          ubit = unsafeShiftL 1
          -- we make an approximation for the passed pawns: when more than 10 pawns
          -- we consider only such in the opponent side (rows 5 to 8)
          -- to speed up the calculation in the opening and middle game
          passedApprox = 10
          np = popCount1 tpawns
          !twpawns1 | np < passedApprox = twpawns
                    | otherwise         = twpawns .&. 0xFFFFFFFF00000000
          !tbpawns1 | np < passedApprox = tbpawns
                    | otherwise         = tbpawns .&. 0x00000000FFFFFFFF

updatePosAttacs :: MyPos -> MyPos
updatePosAttacs p = p {
        whPAttacs = twhPAtt, whNAttacs = twhNAtt, whBAttacs = twhBAtt,
        whRAttacs = twhRAtt, whQAttacs = twhQAtt, whKAttacs = twhKAtt,
        blPAttacs = tblPAtt, blNAttacs = tblNAtt, blBAttacs = tblBAtt,
        blRAttacs = tblRAtt, blQAttacs = tblQAtt, blKAttacs = tblKAtt,
        whAttacs = twhAttacs, blAttacs = tblAttacs
    }
    where !twhPAtt = bbToSquaresBB (pAttacs White) $ pawns p .&. white p
          !twhNAtt = bbToSquaresBB nAttacs $ knights p .&. white p
          !twhBAtt = bbToSquaresBB (bAttacs ocp) $ bishops p .&. white p
          !twhRAtt = bbToSquaresBB (rAttacs ocp) $ rooks p .&. white p
          !twhQAtt = bbToSquaresBB (qAttacs ocp) $ queens p .&. white p
          !twhKAtt = kAttacs $ firstOne $ kings p .&. white p
          !tblPAtt = bbToSquaresBB (pAttacs Black) $ pawns p .&. black p
          !tblNAtt = bbToSquaresBB nAttacs $ knights p .&. black p
          !tblBAtt = bbToSquaresBB (bAttacs ocp) $ bishops p .&. black p
          !tblRAtt = bbToSquaresBB (rAttacs ocp) $ rooks p .&. black p
          !tblQAtt = bbToSquaresBB (qAttacs ocp) $ queens p .&. black p
          !tblKAtt = kAttacs $ firstOne $ kings p .&. black p
          !twhAttacs = twhPAtt .|. twhNAtt .|. twhBAtt .|. twhRAtt .|. twhQAtt .|. twhKAtt
          !tblAttacs = tblPAtt .|. tblNAtt .|. tblBAtt .|. tblRAtt .|. tblQAtt .|. tblKAtt
          ocp = occup p

updatePosCheck :: MyPos -> MyPos
updatePosCheck p = p { check = tcheck }
    where !whcheck = white p .&. kings p .&. blAttacs p
          !blcheck = black p .&. kings p .&. whAttacs p
          !tcheck = blcheck .|. whcheck

-- Generate the castle moves
-- Here we could optimize a bit by defining constants separately for White and Black
-- and test anyway kingmoved first (or even a more general pattern for all moved)
genMoveCast :: MyPos -> Color -> [Move]
genMoveCast p c
    | inCheck p || kingMoved p c = []
    | otherwise = kingside ++ queenside
    where (cmidk, cmidq, opAtt) =
             if c == White then (caRMKw, caRMQw, blAttacs p)
                           else (caRMKb, caRMQb, whAttacs p)
          kingside  = if castKingRookOk  p c && (occup p .&. cmidk == 0) && (opAtt .&. cmidk == 0)
                        then [caks] else []
          queenside = if castQueenRookOk p c && (occup p .&. cmidq == 0) && (opAtt .&. cmidq == 0)
                        then [caqs] else []
          caks = makeCastleFor c True
          caqs = makeCastleFor c False

{-# INLINE kingMoved #-}
kingMoved :: MyPos -> Color -> Bool
kingMoved p White = not (epcas p `testBit` 4)
kingMoved p Black = not (epcas p `testBit` 60)

{-# INLINE castKingRookOk #-}
castKingRookOk :: MyPos -> Color -> Bool
castKingRookOk p White = epcas p `testBit` 7
castKingRookOk p Black = epcas p `testBit` 63

{-# INLINE castQueenRookOk #-}
castQueenRookOk :: MyPos -> Color -> Bool
castQueenRookOk p White = epcas p `testBit` 0
castQueenRookOk p Black = epcas p `testBit` 56

-- Set a piece on a square of the table
setPiece :: Square -> Color -> Piece -> MyPos -> MyPos
setPiece sq c f p
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
doFromToMove m p | moveIsNormal m
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
doFromToMove m p | moveIsEnPas m
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
          tepcas = reset50Moves $ epcas p `xor` mvMask	-- to do: ep
          Busy col fig  = tabla p src	-- identify the moving piece
          Busy _   Pawn = tabla p del	-- identify the captured piece (pawn)
          CA tzobkey tmater = chainAccum (CA (zobkey p) (mater p)) [
                                accumClearSq src p,
                                accumClearSq del p,
                                accumSetPiece dst col fig p,
                                accumMoving p
                            ]
doFromToMove m p | moveIsTransf m
    = updatePos p0 {
          black = tblack, slide = tslide, kkrq = tkkrq, diag = tdiag,
          epcas = tepcas, zobkey = tzobkey, mater = tmater
      }
    where src = fromSquare m
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
doFromToMove m p | moveIsCastle m
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
reverseMoving p = p { epcas = tepcas, zobkey = z }
    where tepcas = epcas p `xor` mvMask
          CA z _ = chainAccum (CA (zobkey p) (mater p)) [
                       accumMoving p
                   ]
-- Here is not clear what to do with castle and en passant...

-- find pinning lines for a piece type, given the king & piece squares
-- the queen is very hard, so we solve it as a composition of rook and bishop
-- and when we call findLKA we always know as which piece the queen checks
{-# INLINE findLKA #-}
findLKA :: Piece -> Square -> Int -> BBoard
findLKA Queen ksq psq
    | rAttacs bpsq ksq .&. bpsq == 0 = findLKA0 Bishop ksq psq
    | otherwise                      = findLKA0 Rook   ksq psq
    where bpsq = bit psq
findLKA pt ksq psq = findLKA0 pt ksq psq

findLKA0 :: Piece -> Square -> Int -> BBoard
findLKA0 pt ksq psq
    | pt == Bishop = go bAttacs
    | pt == Rook   = go rAttacs
    | otherwise    = 0	-- it will not be called with other pieces
    where go f = bb
              where !kp = f (bit psq) ksq
                    !pk = f (bit ksq) psq
                    !bb = kp .&. pk

{-# INLINE myPieces #-}
myPieces :: MyPos -> Color -> BBoard
myPieces !p !c = if c == White then white p else black p

-- {-# INLINE yoPieces #-}
-- yoPieces :: MyPos -> Color -> BBoard
-- yoPieces !p !c = if c == White then black p else white p

{-# INLINE thePieces #-}
thePieces :: MyPos -> Color -> (BBoard, BBoard)
thePieces p c = if c == White then (white p, black p) else (black p, white p)

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
                     .|. (pAttacs White sq .&. black pos .|. pAttacs Black sq .&. white pos) .&. p
    where !occ = occup pos `less` moved
          !b = bishops pos  `less` moved
          !r = rooks pos    `less` moved
          !q = queens pos  `less` moved
          !n = knights pos `less` moved
          !k = kings pos   `less` moved
          !p = pawns pos   `less` moved

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

-- Calculate the value of a move per SEE, given the position, the color to move,
-- the source square of the first capture, the destination of the captures
-- and the value of the first captured piece
seeMoveValue :: MyPos -> Color -> Square -> Square -> Int -> Int
seeMoveValue pos col sqfirstmv sqto gain0 = v
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
          (mypc, yopc) = thePieces pos col
          !moved0 = bit sqfirstmv
          attacs0 = newAttacs pos sqto moved0
          (!from0, !valfrom) = chooseAttacker pos (attacs0 .&. yopc)
          sp0 = SEEPars { seeGain = gain0, seeVal = valfrom, seeAtts = newAttacs pos sqto moved0,
                          seeFrom = from0, seeMovd = moved0, seeDefn = yopc, seeAgrs = mypc }

-- This function can produce illegal captures with the king!
genMoveCaptWL :: MyPos -> Color -> ([(Square, Square)], [(Square, Square)])
genMoveCaptWL pos col = (wl, ll)
    where (wl, ll) = foldr (perCaptFieldWL pos col mypc yoAtt) ([],[]) $ squaresByMVV pos capts
          (mypc, yopc) = thePieces pos col
          (myAtt, yoAtt) = if col == White
                              then (whAttacs pos, blAttacs pos)
                              else (blAttacs pos, whAttacs pos)
          capts = myAtt .&. yopc

perCaptFieldWL :: MyPos -> Color -> BBoard -> BBoard -> Square
          -> ([(Square, Square)], [(Square, Square)])
          -> ([(Square, Square)], [(Square, Square)])
perCaptFieldWL pos col mypc advdefence sq mvlst
    | hanging   = foldr (addHanging sq) mvlst agrsqs
    | otherwise = foldr (perCaptWL pos col valto sq) mvlst agrsqs
    where myattacs = mypc .&. newAttacs pos sq 0
          Busy _ pcto = tabla pos sq
          valto = value pcto
          hanging = not (advdefence `testBit` sq)
          agrsqs = squaresByLVA pos myattacs

approximateEasyCapts :: Bool
approximateEasyCapts = True	-- when capturing a better piece: no SEE, it is always winning

perCaptWL :: MyPos -> Color -> Int -> Square -> Square
          -> ([(Square, Square)], [(Square, Square)])
          -> ([(Square, Square)], [(Square, Square)])
perCaptWL pos col gain0 sq sqfa (wsqs, lsqs)
    = if approx || adv <= gain0
         then (ss:wsqs, lsqs)
         else (wsqs, ss:lsqs)
    where ss = (sqfa, sq)
          approx = approximateEasyCapts && gain1 >= 0
          Busy _ pcfa = tabla pos sqfa
          v0 = value pcfa
          gain1 = gain0 - v0
          adv = seeMoveValue pos col sqfa sq v0

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
