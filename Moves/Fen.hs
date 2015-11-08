{-# LANGUAGE BangPatterns #-}
module Moves.Fen (
    posFromFen, initPos, updatePos, setPiece
    ) where

import Data.Bits
import Data.Char
import Data.List (tails)
import Data.Maybe (fromJust)

import Struct.Struct
import Moves.Moves
import Moves.BitBoard
import Moves.Pattern
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

updatePos :: MyPos -> MyPos
updatePos = updatePosLazy . updatePosOccup

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

updatePosLazy :: MyPos -> MyPos
updatePosLazy !p
    | moving p == White
        = let lzb = LazyBits {
                      _check = tcheck,
                      _myPAttacs = twhPAtt, _myNAttacs = twhNAtt, _myBAttacs = twhBAtt,
                      _myRAttacs = twhRAtt, _myQAttacs = twhQAtt, _myKAttacs = twhKAtt,
                      _yoPAttacs = tblPAtt, _yoNAttacs = tblNAtt, _yoBAttacs = tblBAtt,
                      _yoRAttacs = tblRAtt, _yoQAttacs = tblQAtt, _yoKAttacs = tblKAtt,
                      _myAttacs  = twhAttacs, _yoAttacs = tblAttacs
                  }
        in p { lazyBits = lzb }
    | otherwise
        = let lzb = LazyBits {
                      _check = tcheck,
                      _myPAttacs = tblPAtt, _myNAttacs = tblNAtt, _myBAttacs = tblBAtt,
                      _myRAttacs = tblRAtt, _myQAttacs = tblQAtt, _myKAttacs = tblKAtt,
                      _yoPAttacs = twhPAtt, _yoNAttacs = twhNAtt, _yoBAttacs = twhBAtt,
                      _yoRAttacs = twhRAtt, _yoQAttacs = twhQAtt, _yoKAttacs = twhKAtt,
                      _myAttacs  = tblAttacs, _yoAttacs = twhAttacs
                  }
        in p { lazyBits = lzb }
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
          !white = ocp `less` black p
          !whcheck = white   .&. kings p .&. tblAttacs
          !blcheck = black p .&. kings p .&. twhAttacs
          !tcheck = whcheck .|. blcheck
          ocp = occup p

-- Passed pawns: only with bitboard operations
whitePassed :: BBoard -> BBoard -> BBoard
whitePassed !wp !bp = wpa
    where !bpL = bbLeft bp
          !bpR = bbRight bp
          !wb0 = bpR .|. bpL .|. bp .|. wp
          !sha = shadowDown wb0	-- erase
          !wpa = wp `less` sha

blackPassed :: BBoard -> BBoard -> BBoard
blackPassed !wp !bp = bpa
    where !wpL = bbLeft wp
          !wpR = bbRight wp
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
