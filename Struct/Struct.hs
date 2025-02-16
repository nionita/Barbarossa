{-# LANGUAGE BangPatterns #-}
module Struct.Struct (
    BBoard, Square, ZKey, ShArray, MaArray, DbArray, Move(..),
    Piece(..), Color(..), TabCont(..), MyPos(..), LazyBits(..),
    other, moving, myAccum,
    isCheck, inCheck, checkOk, clearCast, genMoveCast, castKingRookOk, castQueenRookOk,
    lsbBBoard, bbToSquares, less, firstOne, bbToSquaresBB,
    shadowDown, shadowUp, uTestBit, uBit,
    zobMove, zobPiece, zobCastKw, zobCastQw, zobCastKb, zobCastQb, zobEP,
    epMask, mvMask, halfMoves, get50Moves,
    decodeCastWhiteKing, decodeCastBlackKing, decodeCastWhiteQueen, decodeCastBlackQueen,
    tabla, emptyPos, isReversible, remis50Moves, set50Moves, reset50Moves, addHalfMove,
    fromSquare, toSquare, isSlide, isDiag, isKkrq,
    moveIsNormal, moveIsCastle, moveIsPromo, moveIsEnPas, moveColor, movePiece,
    movePromoPiece, moveEnPasDel, makeEnPas, moveAddColor, moveAddPiece,
    moveHisAdr, moveHisOfs,
    makeCastleFor, makePromo, moveFromTo, showWord64,
    activatePromo, fromColRow, checkCastle, checkEnPas, toString,
    myAttacs, yoAttacs, check,
    myPAttacs, myNAttacs, myBAttacs, myRAttacs, myQAttacs, myKAttacs,
    yoPAttacs, yoNAttacs, yoBAttacs, yoRAttacs, yoQAttacs, yoKAttacs
) where

import Data.Array.Unboxed
import Data.Array.Base
import Data.Bits
import Data.Char (ord, chr)
import Data.List (unfoldr)
import Data.Word
import Numeric
import GHC.Arr (unsafeIndex)
import System.Random
-- import Control.Exception (assert)

import Eval.NNUE

-- The very basic data types used in the modules
type BBoard = Word64
type Square = Int
type ZKey = Word64

type ShArray = UArray Square Int
type MaArray = UArray Square BBoard
type DbArray = UArray Int BBoard

data Piece = Pawn | Knight | Bishop | Rook | Queen | King
    deriving (Eq, Ord, Enum, Ix, Show)

data Color = White | Black deriving (Eq, Show)

data TabCont = Empty
             | Busy !Color !Piece
             deriving (Eq, Show)

data MyPos = MyPos {
    black, slide, kkrq, diag, epcas :: !BBoard, -- These fields completely represents of a position
    zobkey :: !ZKey,	-- hash key
    me, yo, occup, kings, pawns :: !BBoard,	-- further heavy used bitboards computed for efficiency
    queens, rooks, bishops, knights, passed :: !BBoard,
    whAccum, blAccum :: Accum,	-- the 2 NNUE accumulators
    staticScore :: Int,	-- lazy, not always needed
    lazyBits :: LazyBits	-- lazy of course
    }

data LazyBits = LazyBits {
    _myAttacs, _yoAttacs, _check :: !BBoard,		-- my & yours attacs, check
    _myPAttacs, _myNAttacs, _myBAttacs, _myRAttacs, _myQAttacs, _myKAttacs :: !BBoard,
    _yoPAttacs, _yoNAttacs, _yoBAttacs, _yoRAttacs, _yoQAttacs, _yoKAttacs :: !BBoard
    }
    deriving Eq

myAttacs, yoAttacs, check :: MyPos -> BBoard
myPAttacs, myNAttacs, myBAttacs, myRAttacs, myQAttacs, myKAttacs :: MyPos -> BBoard
yoPAttacs, yoNAttacs, yoBAttacs, yoRAttacs, yoQAttacs, yoKAttacs :: MyPos -> BBoard

check     = _check     . lazyBits
myAttacs  = _myAttacs  . lazyBits
myPAttacs = _myPAttacs . lazyBits
myNAttacs = _myNAttacs . lazyBits
myBAttacs = _myBAttacs . lazyBits
myRAttacs = _myRAttacs . lazyBits
myQAttacs = _myQAttacs . lazyBits
myKAttacs = _myKAttacs . lazyBits
yoAttacs  = _yoAttacs  . lazyBits
yoPAttacs = _yoPAttacs . lazyBits
yoNAttacs = _yoNAttacs . lazyBits
yoBAttacs = _yoBAttacs . lazyBits
yoRAttacs = _yoRAttacs . lazyBits
yoQAttacs = _yoQAttacs . lazyBits
yoKAttacs = _yoKAttacs . lazyBits

{-# INLINE myAttacs #-}
{-# INLINE yoAttacs #-}
{-# INLINE check #-}
{-# INLINE myPAttacs #-}
{-# INLINE myNAttacs #-}
{-# INLINE myBAttacs #-}
{-# INLINE myRAttacs #-}
{-# INLINE myQAttacs #-}
{-# INLINE myKAttacs #-}
{-# INLINE yoPAttacs #-}
{-# INLINE yoNAttacs #-}
{-# INLINE yoBAttacs #-}
{-# INLINE yoRAttacs #-}
{-# INLINE yoQAttacs #-}
{-# INLINE yoKAttacs #-}

instance Show MyPos where
   show p = "MyPos {" ++ concatMap showField [
            (black, "black"),
            (slide, "slide"),
            (kkrq, "kkrq"),
            (diag, "diag"),
            (epcas, "epcas"),
            (zobkey, "zobkey"),
            (me, "me"),
            (yo, "yo"),
            (occup, "occup"),
            (kings, "kings"),
            (pawns, "pawns"),
            (queens, "queens"),
            (rooks, "rooks"),
            (bishops, "bishops"),
            (knights, "knights"),
            (passed, "passed"),
            (myAttacs, "myAttacs"),
            (yoAttacs, "yoAttacs"),
            (check, "check"),
            (myPAttacs, "myPAttacs"),
            (myNAttacs, "myNAttacs"),
            (myBAttacs, "myBAttacs"),
            (myRAttacs, "myRAttacs"),
            (myQAttacs, "myQAttacs"),
            (myKAttacs, "myKAttacs"),
            (yoPAttacs, "yoPAttacs"),
            (yoNAttacs, "yoNAttacs"),
            (yoBAttacs, "yoBAttacs"),
            (yoRAttacs, "yoRAttacs"),
            (yoQAttacs, "yoQAttacs"),
            (yoKAttacs, "yoKAttacs")
            ]
          ++ "}"
       where showField  (f, sf) = " " ++ sf ++ " = " ++ showWord64 (f p)

{-
Piece coding in MyPos (vertical over slide, kkrq and diag):
Piece    slide kkrq diag
Pawn   = 0     0    1
Knight = 0     1    0
King   = 0     1    1
Bishop = 1     0    1
Rook   = 1     1    0
Queen  = 1     1    1
-}

pieceAt :: MyPos -> BBoard -> Piece
pieceAt !p bsq
    = case bsq .&. diag p of
        0 -> case bsq .&. slide p of
               0 -> Knight
               _ -> Rook
        _ -> case bsq .&. kkrq p of
               0 -> case bsq .&. slide p of
                      0 -> Pawn
                      _ -> Bishop
               _ -> case bsq .&. slide p of
                      0 -> King
                      _ -> Queen

{-# INLINE tabla #-}
tabla :: MyPos -> Square -> TabCont
tabla p sq
    | occup p .&. bsq == 0 = Empty
    | otherwise            = Busy c f
    where c = if black p .&. bsq /= 0 then Black else White
          f = pieceAt p bsq
          bsq = 1 `unsafeShiftL` sq

newtype Move = Move Word16 deriving Eq

instance Show Move where
    show = toString

-- Some constant bitboards for additional conditions like
-- en-passant, castle rights and 50 moves rule
fyBit :: Int
epMask, fyMask, fyIncr, fyZero, fyMaxi, mvMask, caRiMa :: BBoard
caRKiw, caRQuw, caRMKw, caRMQw, caRAKw, caRAQw :: BBoard
caRKib, caRQub, caRMKb, caRMQb, caRAKb, caRAQb :: BBoard
fyBit  = 8                      -- bit number (beginning with 0) which holds the move increment
epMask = 0x0000FF0000FF0000	-- en passant mask
fyMask = 0x000000000000FF00	-- mask for 50 moves rules
-- fyIncr = 0x0000000000000100	-- 50 moves rule increment
fyIncr = uBit fyBit        	-- 50 moves rule increment
fyZero = complement fyMask	-- to reset the 50 moves count
fyMaxi = 0x0000000000006400	-- to compare if we reaches 100 halfmoves
mvMask = 0x0080000000000000	-- Moving mask (1 in that bit means black moves)
caRiMa = 0x9100000000000091	-- Mask for castle rights
caRKiw = 0x0000000000000090	-- white: king & rook position for kingside castle
caRQuw = 0x0000000000000011	-- white: king & rook pisition for queenside castle
caRMKw = 0x0000000000000060	-- white: empty fields for kingside castle
caRMQw = 0x000000000000000E	-- white: empty fields for queenside castle
caRAKw = 0x0000000000000060	-- white: not attacked fields for kingside castle
caRAQw = 0x000000000000000C	-- white: not attacked fields for queenside castle
caRKib = 0x9000000000000000	-- black: king & rook position for kingside castle
caRQub = 0x1100000000000000	-- black: king & rook position for queenside castle
caRMKb = 0x6000000000000000	-- black: empty fields for kingside castle
caRMQb = 0x0E00000000000000	-- black: empty fields for queenside castle
caRAKb = 0x6000000000000000	-- black: not attacked fields for kingside castle
caRAQb = 0x0C00000000000000	-- black: not attacked fields for queenside castle

emptyPos :: MyPos
emptyPos = MyPos {
        black = 0, slide = 0, kkrq = 0, diag = 0, epcas = 0,
        zobkey = 0, whAccum = zeroAccum, blAccum = zeroAccum,
        me = 0, yo = 0, occup = 0, kings = 0, pawns = 0,
        queens = 0, rooks = 0, bishops = 0, knights = 0,
        staticScore = 0, passed = 0, lazyBits = leb
    }
    where leb = LazyBits {
        _myAttacs = 0, _yoAttacs = 0, _check = 0,
        _myPAttacs = 0, _myNAttacs = 0, _myBAttacs = 0, _myRAttacs = 0,
        _myQAttacs = 0, _myKAttacs = 0,
        _yoPAttacs = 0, _yoNAttacs = 0, _yoBAttacs = 0, _yoRAttacs = 0,
        _yoQAttacs = 0, _yoKAttacs = 0
        }

-- Stuff related to 50 moves rule
{-# INLINE isReversible #-}
isReversible :: MyPos -> Bool
isReversible p = fyMask .&. epcas p /= 0

{-# INLINE remis50Moves #-}
remis50Moves :: MyPos -> Bool
remis50Moves p = epcas p .&. fyMask >= fyMaxi

{-# INLINE reset50Moves #-}
reset50Moves :: BBoard -> BBoard
reset50Moves b = b .&. fyZero

{-# INLINE set50Moves #-}
set50Moves :: Int -> BBoard -> BBoard
set50Moves i b = reset50Moves b .|. (fromIntegral i `shift` 8 .&. fyMask)

{-# INLINE get50Moves #-}
get50Moves :: MyPos -> Int
get50Moves p = fromIntegral $ (epcas p .&. fyMask) `unsafeShiftR` 8

{-# INLINE addHalfMove #-}
addHalfMove :: BBoard -> BBoard
addHalfMove b = b + fyIncr

{-# INLINE halfMoves #-}
halfMoves :: MyPos -> BBoard
halfMoves p = (epcas p .&. fyMask) `unsafeShiftR` fyBit

{-# INLINE isSlide #-}
isSlide :: Piece -> Bool
isSlide Bishop = True
isSlide Rook   = True
isSlide Queen  = True
isSlide _      = False

{-# INLINE isKkrq #-}
isKkrq :: Piece -> Bool
isKkrq Pawn   = False
isKkrq Bishop = False
isKkrq _      = True

{-# INLINE isDiag #-}
isDiag :: Piece -> Bool
isDiag Knight = False
isDiag Rook   = False
isDiag _      = True

isKingAt :: Square -> MyPos -> Bool
isKingAt !sq !p = kkrq p `testBit` sq
    && diag p `testBit` sq
    && not (slide p `testBit` sq)

isKingMoving :: Move -> MyPos -> Bool
isKingMoving m !p = isKingAt src p
    where src = fromSquare m

isPawnAt :: Square -> MyPos -> Bool
isPawnAt !sq !p = diag p `testBit` sq
    && not (kkrq p `testBit` sq)
    && not (slide p `testBit` sq)

isPawnMoving :: Move -> MyPos -> Bool
isPawnMoving m !p = isPawnAt src p
    where src = fromSquare m

{-# INLINE moveFromTo #-}
moveFromTo :: Square -> Square -> Move
moveFromTo f t = Move $ encodeFromTo f t

fromColRow :: Int -> Int -> Square
fromColRow c r = r * 8 + c - 9

{-# INLINE other #-}
other :: Color -> Color
other White = Black
other Black = White

{-# INLINE moving #-}
moving :: MyPos -> Color
moving !p = case epcas p .&. mvMask of
               0 -> White
               _ -> Black

showWord64 :: Word64 -> String
showWord64 x = reverse $ take 16 (map f xs)
    where xs = x : map (`unsafeShiftR` 4) xs
          f w = unsafeAt hex . fromIntegral $ w .&. 0xF
          hex :: UArray Int Char
          hex = listArray (0, 15) "0123456789ABCDEF"

-- The move is now coded in 16 bits
-- Normal moves are coded:
-- c<pie> <frsq> <tosq>
-- where:
--   c = 0 for white, 1 for black (1 bit)
--   pie = 0 - pawn, 1 - knight, 2 - bishop, 3 - rook, 4 - queen, 5 - king (3 bits)
--   frsq - from square (6 bits)
--   tosq - to square (6 bits)
{-# INLINE moveIsNormal #-}
moveIsNormal :: Move -> Bool
moveIsNormal (Move m) = m .&. 0x6000 /= 0x6000

-- For which color is the move:
{-# INLINE moveColor #-}
moveColor :: Move -> Color
moveColor (Move m)
    | m .&. 0x8000 == 0 = White
    | otherwise         = Black

-- En passant is coded:
-- c110 <frsq><tosq>
-- where:
--   c = 0 for white, 1 for black (1 bit)
--   frsq - from square (6 bits)
--   tosq - to square (6 bits)
{-# INLINE moveIsEnPas #-}
moveIsEnPas :: Move -> Bool
moveIsEnPas (Move w) = w .&. 0x7000 == 0x6000

-- The location of the adverse pawn to delete:
{-# INLINE moveEnPasDel #-}
moveEnPasDel :: Move -> Square
moveEnPasDel m
    | moveColor m == White = dst - 8
    | otherwise            = dst + 8
    where dst = toSquare m

{-# INLINE makeEnPas #-}
makeEnPas :: Square -> Square -> Move
makeEnPas f t
    | f < t     = Move $ 0x6000 .|. encodeFromTo f t	-- white
    | otherwise = Move $ 0xE000 .|. encodeFromTo f t	-- black

-- Promotions are coded:
-- c111 <pro><frf><tosq>
-- where:
--   c = 0 for white, 1 for black (1 bit)
--   pro = 1 - knight, 2 - bishop, 3 - rook, 4 - queen (3 bits)
--   frf = from files (3 bits)
--   tosq = to square (6 bits)
-- {-#INLINE moveIsPromo #-}
moveIsPromo :: Move -> Bool
moveIsPromo (Move w)
    = w .&. 0x7000 == 0x7000 && (s == tcQueen || s == tcRook || s == tcBishop || s == tcKnight)
    where s = w .&. 0x0E00

{-# INLINE movePromoPiece #-}
movePromoPiece :: Move -> Piece
movePromoPiece (Move w)
    | r >= 1 && r <= 4 = toEnum r
    | otherwise        = error $ "Wrong promo piece in move: " ++ showHex w ""
    where r = fromIntegral $ (w `unsafeShiftR` 9) .&. 0x07

makePromo :: Piece -> Square -> Square -> Move
makePromo p f t
    | f < t     = Move $ 0x7000 .|. w	-- white
    | otherwise = Move $ 0xF000 .|. w	-- black
    where !w = tc p .|. (encodeFromTo f t .&. 0x01FF)
          tc Queen  = tcQueen
          tc Rook   = tcRook
          tc Bishop = tcBishop
          tc Knight = tcKnight
          tc _      = tcQueen	-- to eliminate warning

tcQueen, tcRook, tcBishop, tcKnight :: Word16
tcQueen  = (fromIntegral $ fromEnum Queen ) `shiftL` 9
tcRook   = (fromIntegral $ fromEnum Rook  ) `shiftL` 9
tcBishop = (fromIntegral $ fromEnum Bishop) `shiftL` 9
tcKnight = (fromIntegral $ fromEnum Knight) `shiftL` 9

-- Castles are coded:
-- c111 <frsq> <tosq>
-- where:
--   c = 0 for white, 1 for black (1 bit)
--   frsq - from square, 4 for white, 60 for black (6 bits)
--   tosq - to square, 6 or 2 for white, 62 or 58 for black (6 bits)
{-# INLINE moveIsCastle #-}
moveIsCastle :: Move -> Bool
moveIsCastle (Move w) = s == 0x7E00 || s == 0x7000
    where s = w .&. 0x7E00

{-# INLINE makeCastleFor #-}
makeCastleFor :: Color -> Bool -> Move
makeCastleFor White True  = Move 0x7106	-- white, kingside
makeCastleFor White False = Move 0x7102	-- white, queenside
makeCastleFor Black True  = Move 0xFF3E	-- black, kingside
makeCastleFor Black False = Move 0xFF3A	-- black, queenside

-- General functions for move encoding / decoding
encodeFromTo :: Square -> Square -> Word16
encodeFromTo f t = fromIntegral t .|. (fromIntegral f `unsafeShiftL` 6)

-- {-# INLINE movePiece #-}
movePiece :: Move -> Piece
movePiece m@(Move w)
    | moveIsNormal m
        = if r >= 0 && r <= 5 then toEnum r else error ("Wrong moving piece in move: " ++ showHex w "")
    | moveIsEnPas  m ||
      moveIsPromo  m = Pawn
    | moveIsCastle m = King
    | otherwise      = error $ "Wrong move type: " ++ showHex w ""
    where r = fromIntegral $ (w `unsafeShiftR` 12) .&. 0x7

-- For history purposes: quick 'n' dirty "piece"
{-# INLINE moveHisAdr #-}
moveHisAdr :: Move -> Int
moveHisAdr (Move w) = fromIntegral $ (w `unsafeShiftR` 12) .&. 0x7

-- For history purposes: quick 'n' dirty "color"
{-# INLINE moveHisOfs #-}
moveHisOfs :: Move -> Int
moveHisOfs (Move w) = fromIntegral $ w `unsafeShiftR` 15

-- {-# INLINE fromSquare #-}
fromSquare :: Move -> Square
fromSquare m@(Move w)
    | moveIsPromo m       = let !ffl = (w `unsafeShiftR` 6) .&. 0x7
                            in case moveColor m of
                                   White -> fromIntegral $ 0x30 .|. ffl
                                   Black -> fromIntegral $ 0x08 .|. ffl
    | otherwise           = fromIntegral (w `unsafeShiftR` 6) .&. 0x3F

{-# INLINE toSquare #-}
toSquare :: Move -> Square
toSquare (Move m) = fromIntegral (m .&. 0x3F)

{-# INLINE moveAddColor #-}
moveAddColor :: Color -> Move -> Move
moveAddColor White (Move w) = Move $ w .&. 0x7FFF
moveAddColor Black (Move w) = Move $ w .|. 0x8000

{-# INLINE moveAddPiece #-}
moveAddPiece :: Piece -> Move -> Move
moveAddPiece piece (Move w)
    = Move $ (fromIntegral (fromEnum piece) `unsafeShiftL` 12) .|. (w .&. 0x8FFF)

checkCastle :: Move -> MyPos -> Move
checkCastle m p
    | moveIsNormal m && isKingMoving m p
        = case ds of
            2  -> makeCastleFor c True
            -2 -> makeCastleFor c False
            _  -> m
    | otherwise = m
    where s = fromSquare m
          d = toSquare m
          ds = d - s
          c = moving p

checkEnPas :: Move -> MyPos -> Move
checkEnPas m p
    | moveIsNormal m && isPawnMoving m p
         = if (epcas p .&. epMask) `testBit` t then makeEnPas f t else m
    | otherwise        = m
    where f = fromSquare m
          t = toSquare m

activatePromo :: Char -> Move -> Move
activatePromo b m = makePromo p f t
    where f = fromSquare m
          t = toSquare m
          p = chToPc b
          chToPc 'q' = Queen
          chToPc 'r' = Rook
          chToPc 'b' = Bishop
          chToPc 'n' = Knight
          chToPc _   = King	-- to eliminate warnings

toString :: Move -> String
toString m = col sc : row sr : col dc : row dr : promo
    where s = fromSquare m
          d = toSquare m
          (sr, sc) = s `divMod` 8
          (dr, dc) = d `divMod` 8
          orda = ord 'a'
          ord1 = ord '1'
          col x = chr (orda + x)
          row x = chr (ord1 + x)
          promo = [pcToCh (movePromoPiece m) | moveIsPromo m ]
          pcToCh Queen  = 'q'
          pcToCh Rook   = 'r'
          pcToCh Bishop = 'b'
          pcToCh Knight = 'n'
          pcToCh Pawn   = 'p'	-- is used not only for promotion!
          pcToCh King   = 'k'

-- Is color c in check in position p?
{-# INLINE isCheck #-}
isCheck :: MyPos -> Color -> Bool
isCheck p White | inCheck p = check p .&. black p == 0
                | otherwise = False
isCheck p Black | check p .&. black p == 0 = False
                | otherwise                = True

{-# INLINE inCheck #-}
inCheck :: MyPos -> Bool
inCheck = (/= 0) . check

{-# INLINE clearCast #-}
clearCast :: BBoard -> BBoard -> (BBoard, ZKey)
clearCast cas sd
    | sdposs == 0 || sdcas == 0 = (0, 0)	-- most of the time
    | otherwise = clearingCast sdcas cas	-- complicated cases
    where sdposs = sd .&. caRiMa	-- moving from/to king/rook position?
          sdcas  = sdposs .&. cas	-- first time touched

{-# INLINE clearingCast #-}
clearingCast :: BBoard -> BBoard -> (BBoard, ZKey)
clearingCast sdcas cas = (cascl, zobcl)
    where (casw, zobw) | casrw == 0 = (0, 0)	-- cast rights & changes for white
                       | casrw == wkqb = if sdcas .&. wkqb /= 0
                                            then (wkqb, zobCastQw)
                                            else (0, 0)
                       | casrw == wkkb = if sdcas .&. wkkb /= 0
                                            then (wkkb, zobCastKw)
                                            else (0, 0)
                       | otherwise     = if sdcas .&. wkbb /= 0
                                            then (wkqb .|. wkkb, zobCastQw `xor` zobCastKw)
                                            else if sdcas .&. wqrb /= 0
                                                    then (wqrb, zobCastQw)
                                                    else if sdcas .&. wkrb /= 0
                                                            then (wkrb, zobCastKw)
                                                            else (0, 0)
          (casb, zobb) | casrb == 0 = (0, 0)	-- cast rights & changes for white
                       | casrb == bkqb = if sdcas .&. bkqb /= 0
                                            then (bkqb, zobCastQb)
                                            else (0, 0)
                       | casrb == bkkb = if sdcas .&. bkkb /= 0
                                            then (bkkb, zobCastKb)
                                            else (0, 0)
                       | otherwise     = if sdcas .&. bkbb /= 0
                                            then (bkqb .|. bkkb, zobCastQb `xor` zobCastKb)
                                            else if sdcas .&. bqrb /= 0
                                                    then (bqrb, zobCastQb)
                                                    else if sdcas .&. bkrb /= 0
                                                            then (bkrb, zobCastKb)
                                                            else (0, 0)
          !casr  = cas .&. caRiMa
          !casrw = casr .&. 0xFF
          !casrb = casr .&. 0xFF00000000000000
          !cascl = casw .|. casb
          !zobcl = zobw `xor` zobb
          wkqb = 0x11	-- king & queen rook for white
          wkkb = 0x90	-- king & king rook for white
          wkbb = 0x10	-- white king
          wqrb = 0x01	-- white queen rook
          wkrb = 0x80	-- white king rook
          bkqb = 0x1100000000000000	-- king & queen rook for black
          bkkb = 0x9000000000000000	-- king & king rook for black
          bkbb = 0x1000000000000000	-- black king
          bqrb = 0x0100000000000000	-- black queen rook
          bkrb = 0x8000000000000000	-- black king rook

-- Generate the castle moves
genMoveCast :: MyPos -> [Move]
genMoveCast p
    | inCheck p = []
    | otherwise = kingside ++ queenside
    where (cmidk, cmidq, cattk, cattq)
              | c == White = (caRMKw, caRMQw, caRAKw, caRAQw)
              | otherwise  = (caRMKb, caRMQb, caRAKb, caRAQb)
          kingside  = if castKingRookOk p c && (occup p .&. cmidk == 0) && (yoAttacs p .&. cattk == 0)
                         then [caks] else []
          queenside = if castQueenRookOk p c && (occup p .&. cmidq == 0) && (yoAttacs p .&. cattq == 0)
                         then [caqs] else []
          caks = makeCastleFor c True
          caqs = makeCastleFor c False
          c = moving p

{-# INLINE castKingRookOk #-}
castKingRookOk :: MyPos -> Color -> Bool
castKingRookOk !p White = epcas p .&.  b7 /= 0 where b7 = uBit 7
castKingRookOk !p Black = epcas p .&. b63 /= 0 where b63 = uBit 63

{-# INLINE castQueenRookOk #-}
castQueenRookOk :: MyPos -> Color -> Bool
castQueenRookOk !p White = epcas p .&.  b0 /= 0 where b0 = 1 
castQueenRookOk !p Black = epcas p .&. b56 /= 0 where b56 = uBit 56

{-# INLINE checkOk #-}
checkOk :: MyPos -> Bool
checkOk p = yo p .&. kings p .&. myAttacs p == 0

-- A bitboard with only one bit set from the argument (least significant bit)
{-# INLINE lsbBBoard #-}
lsbBBoard :: BBoard -> BBoard
lsbBBoard = uBit . firstOne

{-# INLINE less #-}
less :: BBoard -> BBoard -> BBoard
less w1 w2 = w1 .&. complement w2

-- First set bit number (lsb order)
{-# INLINE firstOne #-}
firstOne :: BBoard -> Square
firstOne = countTrailingZeros

{-# INLINE bbToSquares #-}
bbToSquares :: BBoard -> [Square]
bbToSquares = unfoldr f
    where f :: BBoard -> Maybe (Square, BBoard)
          f 0 = Nothing
          f b = Just $ extractSquare b

-- Which implementation is better?
{-# INLINE bbToSquaresBB #-}
bbToSquaresBB :: (Square -> BBoard) -> BBoard -> BBoard
bbToSquaresBB f = foldr (\sq w -> f sq .|. w) 0 . bbToSquares
{-
bbToSquaresBB f = go 0
    where go w 0 = w
          go w b = let (sq, b') = extractSquare b
                       !w' = f sq .|. w
                   in go w' b'
-}

{-# INLINE extractSquare #-}
extractSquare :: BBoard -> (Square, BBoard)
extractSquare b = let !sq = firstOne b
                  in (sq, b `xor` uBit sq)

-- Because the normal bits operations are all safe
-- we define here the unsafe versions specialized for BBoard
{-# INLINE uTestBit #-}
uTestBit :: BBoard -> Int -> Bool
uTestBit w b = w .&. uBit b /= 0

{-# INLINE uBit #-}
uBit :: Square -> BBoard
uBit = unsafeShiftL 1

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


genInit, zLen :: Int
genInit = 118863
zLen = 781

zobrist :: UArray Int ZKey
zobrist = listArray (0, zLen-1) $ take zLen $ randoms (mkStdGen genInit)

-- When black is moving: xor with that number
zobMove :: ZKey
zobMove = fromIntegral $ zobrist `unsafeAt` (12*64)

-- For every pice type of every color on every valid
-- field: one index in zobrist (0 to 12*64-1)
{-# INLINE zobPiece #-}
zobPiece :: Color -> Piece -> Square -> ZKey
zobPiece White p sq = zobrist `unsafeAt` idx
    where !idx = (p2intw `unsafeAt` unsafeIndex (Pawn, King) p) + sq
zobPiece Black p sq = zobrist `unsafeAt` idx
    where !idx = (p2intb `unsafeAt` unsafeIndex (Pawn, King) p) + sq

p2intw, p2intb :: UArray Piece Int
p2intw = array (Pawn, King) $ zip [Pawn .. King] [0, 64 .. ]
p2intb = array (Pawn, King) $ zip [Pawn .. King] [b0, b1 .. ]
    where b0 = p2intw!King + 64
          b1 = b0 + 64

zobCastBegin :: Int
zobCastBegin = 12*64+1

zobCastKw, zobCastQw, zobCastKb, zobCastQb :: ZKey
zobCastKw = zobrist `unsafeAt` zobCastBegin
zobCastQw = zobrist `unsafeAt` (zobCastBegin + 1)
zobCastKb = zobrist `unsafeAt` (zobCastBegin + 2)
zobCastQb = zobrist `unsafeAt` (zobCastBegin + 3)

{-# INLINE zobEP #-}
zobEP :: Int -> ZKey
zobEP x = zobrist `unsafeAt` (zobCastBegin + 4 + x)

{-# INLINE decodeCastWhiteKing #-}
decodeCastWhiteKing :: String -> ((BBoard -> BBoard), BBoard)
decodeCastWhiteKing fen3
    | 'K' `elem` fen3 = ((.|. caRKiw), zobCastKw)
    | otherwise       = (id, 0)

{-# INLINE decodeCastBlackKing #-}
decodeCastBlackKing :: String -> ((BBoard -> BBoard), BBoard)
decodeCastBlackKing fen3
    | 'k' `elem` fen3 = ((.|. caRKib), zobCastKb)
    | otherwise       = (id, 0)

{-# INLINE decodeCastWhiteQueen #-}
decodeCastWhiteQueen :: String -> ((BBoard -> BBoard), BBoard)
decodeCastWhiteQueen fen3
    | 'Q' `elem` fen3 = ((.|. caRQuw), zobCastQw)
    | otherwise       = (id, 0)

{-# INLINE decodeCastBlackQueen #-}
decodeCastBlackQueen :: String -> ((BBoard -> BBoard), BBoard)
decodeCastBlackQueen fen3
    | 'q' `elem` fen3 = ((.|. caRQub), zobCastQb)
    | otherwise       = (id, 0)

myAccum :: MyPos -> Accum
myAccum pos | moving pos == White = whAccum pos
            | otherwise           = blAccum pos
