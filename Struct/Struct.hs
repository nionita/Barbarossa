{-# LANGUAGE BangPatterns #-}
module Struct.Struct (
         BBoard, Square, ZKey, ShArray, MaArray, DbArray, Move(..),
         Piece(..), Color(..), TabCont(..), MyPos(..),
         black, slide, kkrq, diag, epcas, other, moving,
         epMask, fyMask, fyIncr, fyZero, mvMask, caRiMa,
         caRKiw, caRQuw, caRMKw, caRMQw, caRKib, caRQub, caRMKb, caRMQb,
         tabla, emptyPos, isReversible, remis50Moves, set50Moves, reset50Moves, addHalfMove,
         fromSquare, toSquare, isSlide, isDiag, isKkrq,
         moveIsNormal, moveIsCastle, moveIsTransf, moveIsEnPas,
         moveColor, moveTransfPiece, moveEnPasDel, makeEnPas,
         makeCastleFor, makeTransf, moveFromTo, showWord64,
         activateTransf, fromColRow, checkCastle, checkEnPas, toString
         -- isPawnMoving, isKingMoving
    ) where

import Data.Array.Unboxed
import Data.Array.Base
import Data.Char (ord, chr)
import Data.List (unfoldr)
import Data.Word
import Data.Bits
import Data.Ix
import qualified Data.Vector.Storable as V
import Foreign

-- The very basic data types used in the modules
type BBoard = Word64
type Square = Int
type ZKey = Word64

type ShArray = UArray Square Int
type MaArray = UArray Square BBoard
type DbArray = UArray Int BBoard

data Piece = Pawn | Knight | Bishop | Rook | Queen | King
    deriving (Eq, Ord, Enum, Ix, Show)

data Color = White | Black deriving (Eq, Show, Ord, Enum, Ix)

data TabCont = Empty
             | Busy !Color !Piece
             deriving (Eq, Show)

data MyPos = MyPos {
    black, slide, kkrq, diag, epcas :: !BBoard, -- These fields completely represents of a position
    zobkey :: !ZKey,	-- hash key
    mater :: !Int,	-- material balance
    me, yo, occup, kings, pawns :: !BBoard,	-- further heavy used bitboards computed for efficiency
    queens, rooks, bishops, knights, passed :: !BBoard,
    myAttacs, yoAttacs, check :: BBoard,		-- my & yours attacs, check
    myPAttacs, myNAttacs, myBAttacs, myRAttacs, myQAttacs, myKAttacs :: BBoard,
    yoPAttacs, yoNAttacs, yoBAttacs, yoRAttacs, yoQAttacs, yoKAttacs :: BBoard,
    staticScore :: Int,
    staticFeats :: [Int]
    }
    deriving Eq

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
       where showField (f, sf) = " " ++ sf ++ " = " ++ showWord64 (f p)

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

{-# INLINE pieceAt #-}
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

newtype Move = Move Word32 deriving Eq

instance Show Move where
    show = toString

-- some constant bitboards for additional conditions like
-- en-passant, castle rights and 50 moves rule
epMask, fyMask, fyIncr, fyZero, mvMask, caRiMa :: BBoard
caRKiw, caRQuw, caRMKw, caRMQw :: BBoard
caRKib, caRQub, caRMKb, caRMQb :: BBoard
epMask = 0x0000FF0000FF0000	-- en passant mask
fyMask = 0x000000000000FF00	-- mask for 50 moves rules
fyIncr = 0x0000000000000100	-- 50 moves rule increment
fyZero = complement fyMask	-- to reset the 50 moves count
fyMaxi = 0x0000000000006400	-- to compare if we reaches 100 halfmoves
mvMask = 0x0080000000000000	-- Moving mask (1 in that bit means black moves)
caRiMa = 0x9100000000000091	-- Mask for castle rights
caRKiw = 0x0000000000000090	-- white: king & rook position for kingside castle
caRQuw = 0x0000000000000011	-- white: king & rook pisition for queenside castle
caRMKw = 0x0000000000000060	-- white: empty fields for kingside castle
caRMQw = 0x000000000000000E	-- white: empty fields for queenside castle
caRKib = 0x9000000000000000	-- black: king & rook position for kingside castle
caRQub = 0x1100000000000000	-- black: king & rook position for queenside castle
caRMKb = 0x6000000000000000	-- black: empty fields for kingside castle
caRMQb = 0x0E00000000000000	-- black: empty fields for queenside castle

emptyPos = MyPos {
        black = 0, slide = 0, kkrq = 0, diag = 0, epcas = 0,
        zobkey = 0, mater = 0,
        me = 0, yo = 0, occup = 0, kings = 0, pawns = 0,
        queens = 0, rooks = 0, bishops = 0, knights = 0,
        myAttacs = 0, yoAttacs = 0, check = 0,
        myPAttacs = 0, myNAttacs = 0, myBAttacs = 0, myRAttacs = 0, myQAttacs = 0, myKAttacs = 0,
        yoPAttacs = 0, yoNAttacs = 0, yoBAttacs = 0, yoRAttacs = 0, yoQAttacs = 0, yoKAttacs = 0,
        staticScore = 0, passed = 0,
        staticFeats = []
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

{-# INLINE addHalfMove #-}
addHalfMove :: BBoard -> BBoard
addHalfMove b = b + fyIncr

{-# INLINE linco #-}
-- Gives all pieces with line/column move (rooks, queens)
linco :: MyPos -> BBoard
linco !p = slide p .&. kkrq p

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

{-# INLINE pieceBB #-}
pieceBB :: Piece -> MyPos -> BBoard
pieceBB Pawn   !p = diag p .&. complement (slide p .|. kkrq p)
pieceBB Knight !p = kkrq p .&. complement (slide p .|. diag p)
pieceBB Bishop !p = slide p .&. diag p .&. complement (kkrq p)
pieceBB Rook   !p = slide p .&. kkrq p .&. complement (diag p)
pieceBB Queen  !p = slide p .&. kkrq p .&. diag p
pieceBB King   !p = kkrq p .&. diag p .&. complement (slide p)

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

-- The move is coded in currently 19 bits (the lower of a Word32)
-- So we need a few functions to handle them
-- With the new coding we actually need only 16 bits, but
-- the "special" attribute does not fit it, so we keep it
-- (on the same place, bit 18)
-- It can be replaced in the future with some function

-- Normal move (from - to)
moveIsNormal :: Move -> Bool
moveIsNormal (Move m) = m .&. 0xE000 == 0

-- For which color is the move:
-- But, as for now, we don't set the move color! (And don't use it too)
moveColor :: Move -> Color
moveColor (Move m) = if testBit m 12 then Black else White
{-
moveColor (Move m) = case testBit m 12 of
                         False -> White
                         _     -> Black
-}

-- Castles
moveIsCastle :: Move -> Bool
moveIsCastle (Move w) = w .&. 0xE000 == 0x8000

makeCastleFor :: Color -> Bool -> Move
makeCastleFor White True  = makeCastle 0
makeCastleFor White False = makeCastle 1
makeCastleFor Black True  = makeCastle 2
makeCastleFor Black False = makeCastle 3

-- Codes are: 0 - kingside, white, 1 - queenside, white,
--            2 - kingside, black, 3 - queenside, black
castleKing :: UArray Int Word32
castleKing  = listArray (0, 3)
                [uncurry encodeFromTo ft `setBit` 15 | ft <- [(4, 6), (4, 2), (60, 62), (60, 58)]]

{-# INLINE makeCastle #-}
makeCastle :: Int -> Move
makeCastle = Move . unsafeAt castleKing

-- En passant:
moveIsEnPas :: Move -> Bool
moveIsEnPas (Move w) = w .&. 0x6000 == 0x4000

-- The location of the adverse pawn to delete:
{-# INLINE moveEnPasDel #-}
moveEnPasDel :: Move -> Square
moveEnPasDel m@(Move w) = if testBit w 15 then dst + 8 else dst - 8	-- set means +
    where dst = toSquare m

{-# INLINE makeEnPas #-}
makeEnPas f t del = Move w2
    where w1 = encodeFromTo f t `setBit` 14
          w2 = if del == t + 8 then w1 `setBit` 15 else w1	-- set when +

-- Promotions:
transfCodes :: Array Int Piece
transfCodes = listArray (0, 3) [Knight, Bishop, Rook, Queen]
-- transfRev :: Array Piece Word32
-- transfRev   = array (Knight, Queen)
--                     [(Knight, 0), (Bishop, 0x4000), (Rook, 0x8000), (Queen, 0xC000)]

moveIsTransf :: Move -> Bool
moveIsTransf (Move w) = testBit w 13

moveTransfPiece (Move w) = transfCodes `unsafeAt` fromIntegral x
    where x = (w `shiftR` 14) .&. 0x03

{-# INLINE makeTransf #-}
makeTransf :: Piece -> Square -> Square -> Move
makeTransf p f t = Move w
    where !w = tc p .|. encodeFromTo f t .|. b13
          b13 = 1 `unsafeShiftL` 13	-- bit 13
          tc Queen  = 0xC000
          tc Rook   = 0x8000
          tc Bishop = 0x4000
          tc _      = 0

-- General functions for move encoding / decoding
encodeFromTo :: Square -> Square -> Word32
encodeFromTo f t = fromIntegral t .|. (fromIntegral f `shiftL` 6)

-- The type have to be only 2 bits (i.e. 0 to 3)
movetype :: Int -> Word32 -> Word32
movetype t w = fromIntegral (t `shiftL` 12) .|. w

-- code :: Word32 -> Word32 -> Word32
-- code c w = (c `shiftL` 14) .|. w

fromSquare :: Move -> Square
fromSquare (Move m) = fromIntegral (m `shiftR` 6) .&. 0x3F

toSquare :: Move -> Square
toSquare (Move m) = fromIntegral (m .&. 0x3F)

checkCastle :: Move -> MyPos -> Move
checkCastle m p
    | moveIsNormal m && isKingMoving m p
        = if ds == 2
             then makeCastleFor c True
             else if ds == -2
                     then makeCastleFor c False
                     else m
    | otherwise        = m
    where s = fromSquare m
          d = toSquare m
          ds = d - s
          c = moving p

checkEnPas :: Move -> MyPos -> Move
checkEnPas m p
    | moveIsNormal m && isPawnMoving m p
         = if (epcas p .&. epMask) `testBit` t then makeEnPas f t del else m
    | otherwise        = m
    where f = fromSquare m
          t = toSquare m
          del = t + if moving p == White then -8 else 8

activateTransf :: Char -> Move -> Move
activateTransf b m = makeTransf p f t
    where f = fromSquare m
          t = toSquare m
          p = chToPc b
          chToPc 'q' = Queen
          chToPc 'r' = Rook
          chToPc 'b' = Bishop
          chToPc 'n' = Knight

toString :: Move -> String
toString m = col sc : row sr : col dc : row dr : transf
    where s = fromSquare m
          d = toSquare m
          (sr, sc) = s `divMod` 8
          (dr, dc) = d `divMod` 8
          orda = ord 'a'
          ord1 = ord '1'
          col x = chr (orda + x)
          row x = chr (ord1 + x)
          transf = [pcToCh (moveTransfPiece m) | moveIsTransf m ]
          pcToCh Queen  = 'q'
          pcToCh Rook   = 'r'
          pcToCh Bishop = 'b'
          pcToCh Knight = 'n'
