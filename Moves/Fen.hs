{-# LANGUAGE BangPatterns #-}
module Moves.Fen (
    posFromFen, initPos, updatePos, setPiece, isCheckSquare
    ) where

import Data.Bits
import Data.Char
import Data.List (tails)
import Data.Maybe (fromJust)

import Struct.Struct
import Moves.Moves
import Moves.BitBoard
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
              | c `elem` "PRNBQK" = go (sq+1) cs $ (sq, fcw) : acc
              | c `elem` "prnbqk" = go (sq+1) cs $ (sq, fcb) : acc
              | isDigit c = go (skip sq c) cs acc
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
posFromFen fen = updatePos p { epcas = x, zobkey = zk }
    where fen1:fen2:fen3:fen4:fen5:_ = fenFromString fen
          p  = fenToTable fen1
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

{-# INLINE isCheckSquare #-}
isCheckSquare :: BBoard -> BBoard -> BBoard -> BBoard -> BBoard -> BBoard -> BBoard -> BBoard
              -> Color -> Square -> BBoard
isCheckSquare !occ !kin !que !roo !bis !kni !paw !yop !c !sq
    |    bAttacs occ sq .&. di  /= 0
      || rAttacs occ sq .&. rf  /= 0
      || nAttacs     sq .&. kn  /= 0
      || kAttacs     sq .&. kin /= 0	-- cant attack itself
      || pAttacs c   sq .&. ps  /= 0 = uBit sq
    | otherwise                      = 0
    where rf = yop .&. (roo .|. que)
          di = yop .&. (bis .|. que)
          kn = yop .&. kni
          ps = yop .&. paw

updatePos :: MyPos -> MyPos
updatePos = updatePosAttacs . updatePosOccup

updatePosOccup :: MyPos -> MyPos
updatePosOccup !p = p {
                  occup = toccup, me = tme, yo = tyo, kings  = tkings,
                  pawns = tpawns, knights = tknights, queens = tqueens,
                  rooks = trooks, bishops = tbishops, passed = tpassed,
                  check = tcheck1 .|. tcheck2
               }
    where !toccup = kkrq p .|. diag p
          !tkings = kkrq p .&. diag p `less` slide p
          !twhite = toccup `less` black p
          (!tme, !tyo, !c) | moving p == White = (twhite, black p, White)
                           | otherwise         = (black p, twhite, Black)
          !tpawns   = diag p `less` (kkrq p .|. slide p)
          !tknights = kkrq p `less` (diag p .|. slide p)
          !tqueens  = slide p .&. kkrq p .&. diag p
          !trooks   = slide p .&. kkrq p `less` diag p
          !tbishops = slide p .&. diag p `less` kkrq p
          !twpawns = tpawns .&. twhite
          !tbpawns = tpawns .&. black p
          !tpassed = whitePassed twpawns tbpawns .|. blackPassed twpawns tbpawns
          !tcheck1 = isCheckSquare toccup tkings tqueens trooks tbishops tknights
                         tpawns tyo        c  $ firstOne $ tme .&. tkings
          !tcheck2 = isCheckSquare toccup tkings tqueens trooks tbishops tknights
                         tpawns tme (other c) $ firstOne $ tyo .&. tkings
          -- Further ideas:
          -- 1. The old method could be faster for afew pawns! Tests!!
          -- 2. This is necessary only after a pawn move, otherwise passed remains the same
          -- 3. Unify updatePos: one function with basic fields as parameter and eventually
          --    the old position, then everything in one go - should avoid copying

updatePosAttacs :: MyPos -> MyPos
updatePosAttacs !p
    | moving p == White = p { myAttacks = whAtt, yoAttacks = blAtt }
    | otherwise         = p { myAttacks = blAtt, yoAttacks = whAtt }
    where whAtt = AttBB {
                      allAttacs = twhAttacs,
                      pbAttacs = twhPAtt, nbAttacs = twhNAtt, bbAttacs = twhBAtt,
                      rbAttacs = twhRAtt, qbAttacs = twhQAtt, kbAttacs = twhKAtt
                  }
          blAtt = AttBB {
                      allAttacs = tblAttacs,
                      pbAttacs = tblPAtt, nbAttacs = tblNAtt, bbAttacs = tblBAtt,
                      rbAttacs = tblRAtt, qbAttacs = tblQAtt, kbAttacs = tblKAtt
                  }
          !twhPAtt = bbToSquaresBB (pAttacs White) $ pawns p .&. white
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

{--
updatePosCheck :: MyPos -> MyPos
updatePosCheck p = p {
                  check = tcheck
               }
    where !mecheck = me p .&. kings p .&. yoAttacs p
          !yocheck = yo p .&. kings p .&. myAttacs p
          !tcheck = mecheck .|. yocheck
--}


-- Small optimisation: .&. instead `less` below
-- This one should be done in Muster.hs and used elsewhere too
notFileA, notFileH :: BBoard
notFileA = 0xFEFEFEFEFEFEFEFE
notFileH = 0x7F7F7F7F7F7F7F7F

-- Passed pawns: only with bitboard operations
whitePassed :: BBoard -> BBoard -> BBoard
whitePassed !wp !bp = wpa
    where !bpL = (bp .&. notFileA) `unsafeShiftR` 1	-- left
          !bpR = (bp .&. notFileH) `unsafeShiftL` 1	-- and right
          !wb0 = bpR .|. bpL .|. bp .|. wp
          !sha = shadowDown wb0	-- erase
          !wpa = wp `less` sha

blackPassed :: BBoard -> BBoard -> BBoard
blackPassed !wp !bp = bpa
    where !wpL = (wp .&. notFileA) `unsafeShiftR` 1	-- left
          !wpR = (wp .&. notFileH) `unsafeShiftL` 1	-- and right
          !wb0 = wpR .|. wpL .|. wp .|. bp
          !sha = shadowUp wb0	-- erase
          !bpa = bp `less` sha

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
          bsq = uBit sq
          !nbsq = complement bsq
