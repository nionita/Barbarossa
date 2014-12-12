{-# LANGUAGE BangPatterns #-}
module Struct.Struct (
         BBoard, Square, ZKey, ShArray, MaArray, DbArray, Move(..),
         Piece(..), Color(..), TabCont(..), MyPos(..),
         other, moving, epMask, fyMask, fyIncr, fyZero, mvMask, caRiMa,
         caRKiw, caRQuw, caRMKw, caRMQw, caRKib, caRQub, caRMKb, caRMQb,
         tabla, emptyPos, isReversible, remis50Moves, set50Moves, reset50Moves, addHalfMove,
         fromSquare, toSquare, isSlide, isDiag, isKkrq,
         moveIsNormal, moveIsCastle, moveIsPromo, moveIsEnPas, moveColor, movePiece,
         movePromoPiece, moveEnPasDel, makeEnPas, moveAddColor, moveAddPiece,
         makeCastleFor, makePromo, moveFromTo, showWord64,
         activatePromo, fromColRow, checkCastle, checkEnPas, toString
    ) where

import Data.Array.Unboxed
import Data.Array.Base
import Data.Char (ord, chr, toUpper)
import Data.Word
import Data.Bits
import Numeric

-- The very basic data types used in the modules
type BBoard = Word64
type Square = Int
type ZKey = Word64

type ShArray = UArray Square Int
type MaArray = UArray Square BBoard
type DbArray = UArray Int BBoard

data Piece = Pawn | Knight | Bishop | Rook | Queen | King
    deriving (Eq, Ord, Enum, Ix, Show)

-- data Color = White | Black deriving (Eq, Show, Ord, Enum, Ix)
data Color = White | Black deriving (Eq, Show)

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

-- {-# INLINE tabla #-}
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

-- some constant bitboards for additional conditions like
-- en-passant, castle rights and 50 moves rule
epMask, fyMask, fyIncr, fyZero, fyMaxi, mvMask, caRiMa :: BBoard
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

emptyPos :: MyPos
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

-- {-# INLINE makePromo #-}
makePromo :: Piece -> Square -> Square -> Move
makePromo p f t
    | f < t     = Move $ 0x7000 .|. w	-- white
    | otherwise = Move $ 0xF000 .|. w	-- black
    where !w = tc p .|. (encodeFromTo f t .&. 0x01FF)
          tc Queen  = tcQueen
          tc Rook   = tcRook
          tc Bishop = tcBishop
          tc Knight = tcKnight

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

{--
moveHasFromSquare :: Move -> Bool
moveHasFromSquare (Move w) = w .&. 0x7000 == 0x7000
--}

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

-- {-# INLINE moveAddColor #-}
moveAddColor :: Color -> Move -> Move
moveAddColor White (Move w) = Move $ w .&. 0x7FFF
moveAddColor Black (Move w) = Move $ w .|. 0x8000

-- {-# INLINE moveAddPiece #-}
moveAddPiece :: Piece -> Move -> Move
moveAddPiece piece (Move w)
    = Move $ (fromIntegral (fromEnum piece) `unsafeShiftL` 12) .|. (w .&. 0x8FFF)
-- moveAddPiece piece m@(Move w) = Move $ (fromIntegral (fromEnum piece) `unsafeShiftL` 12) .|. w
    -- | moveIsNormal m = Move $ (fromIntegral (fromEnum piece) `unsafeShiftL` 12) .|. w
    -- | otherwise      = m

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
-- toString m = mvpiece : col sc : row sr : col dc : row dr : transf
toString m = col sc : row sr : col dc : row dr : transf
    where s = fromSquare m
          d = toSquare m
          (sr, sc) = s `divMod` 8
          (dr, dc) = d `divMod` 8
          orda = ord 'a'
          ord1 = ord '1'
          col x = chr (orda + x)
          row x = chr (ord1 + x)
          transf = [pcToCh (movePromoPiece m) | moveIsPromo m ]
          pcToCh Queen  = 'q'
          pcToCh Rook   = 'r'
          pcToCh Bishop = 'b'
          pcToCh Knight = 'n'
          pcToCh Pawn   = 'p'	-- is used not only for promotion!
          pcToCh King   = 'k'
          -- mvpc = pcToCh (movePiece m)
          -- mvpiece | moveColor m == White = toUpper mvpc
          --         | otherwise            = mvpc
