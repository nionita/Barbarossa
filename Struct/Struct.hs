{-# LANGUAGE BangPatterns, PatternGuards #-}

module Struct.Struct (
         BBoard, Square, ZKey, ShArray, MaArray, DbArray, Move(..),
         Piece(..), Color(..), TabCont(..), MyPos(..), LazyBits(..),
         other, moving, epMask, fyMask, fyIncr, fyZero, mvMask, caRiMa,
         caRKiw, caRQuw, caRMKw, caRMQw, caRAKw, caRAQw, caRKib, caRQub, caRMKb, caRMQb, caRAKb, caRAQb,
         tabla, emptyPos, isReversible, remis50Moves, set50Moves, reset50Moves, addHalfMove,
         fromSquare, toSquare, isSlide, isDiag, isKkrq,
         moveIsNormal, moveIsCastle, moveIsPromo, moveIsEnPas, moveColor, movePiece,
         moveIsTTMove, makeTTMove, moveIsCapture, makeCapture, moveIsCheck, makeCheck,
         movePromoPiece, moveEnPasDel, makeEnPas, moveAddColor, moveAddPiece,
         makeCastleFor, makePromo, moveFromTo, showWord64,
         activatePromo, fromColRow, checkCastle, checkEnPas, toString,
         myAttacs, yoAttacs, check,
         myPAttacs, myNAttacs, myBAttacs, myRAttacs, myQAttacs, myKAttacs,
         yoPAttacs, yoNAttacs, yoBAttacs, yoRAttacs, yoQAttacs, yoKAttacs
    ) where

import Data.Array.Unboxed
import Data.Array.Base
import Data.Char (ord, chr)
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
    black, slide, kkrq, diag, epcas :: !BBoard,		-- These completely represents a position
    me, yo,	-- opponents pices: me: the moving one, yo - the other
    mek, yok,	-- help fields for check calculations (pinned, indirect)
    occup, passed,	-- all pieces, all passed pawns
    kings, pawns, queens, rooks, bishops, knights :: !BBoard, -- all pieces by type
    zobkey :: !ZKey,	-- hash key
    mater  :: !Int,	-- material balance
    staticScore :: Int,
    lazyBits :: LazyBits	-- these are not always needed
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
            (mek, "mek"),
            (yok, "yok"),
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

-- {-# INLINE tabla #-}
tabla :: MyPos -> Square -> TabCont
tabla p sq
    | occup p .&. bsq == 0 = Empty
    | otherwise            = Busy c f
    where c = if black p .&. bsq /= 0 then Black else White
          f = pieceAt p bsq
          bsq = 1 `unsafeShiftL` sq

newtype Move = Move Word32

instance Eq Move where
    Move w1 == Move w2 = w1 .&. 0xFFFF == w2 .&. 0xFFFF

instance Show Move where
    show = toString

-- some constant bitboards for additional conditions like
-- en-passant, castle rights and 50 moves rule
epMask, fyMask, fyIncr, fyZero, fyMaxi, mvMask, caRiMa :: BBoard
caRKiw, caRQuw, caRMKw, caRMQw, caRAKw, caRAQw :: BBoard
caRKib, caRQub, caRMKb, caRMQb, caRAKb, caRAQb :: BBoard
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
        zobkey = 0, mater = 0,
        me = 0, yo = 0, mek = 0, yok = 0, occup = 0, kings = 0, pawns = 0,
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

-- The basic move is now coded in the lower 16 bits
-- The coding of that bits informs about color, piece, from square & to square
-- Special moves may differ
-- The upper 16 bits are used to code some properties in the moves, like:
-- move comes from TT
-- move is capture
-- move checks
-- Fot these one bit per property will be used
-- These properties will not be stored in TT, so a move coming from TT
-- will lose all previous properties, but have the TT bit set (which is probably
-- strong enough to cover all other interesting cases)

{-# INLINE moveIsTTMove #-}
moveIsTTMove :: Move -> Bool
moveIsTTMove (Move w) = w .&. 0x80000000 /= 0

{-# INLINE makeTTMove #-}
makeTTMove :: Move -> Move
makeTTMove (Move w) = Move $ w .|. 0x80000000

{-# INLINE moveIsCapture #-}
moveIsCapture :: Move -> Bool
moveIsCapture (Move w) = w .&. 0x40000000 /= 0

{-# INLINE makeCapture #-}
makeCapture :: Move -> Move
makeCapture (Move w) = Move $ w .|. 0x40000000

{-# INLINE moveIsCheck #-}
moveIsCheck :: Move -> Bool
moveIsCheck (Move w) = w .&. 0x20000000 /= 0

{-# INLINE makeCheck #-}
makeCheck :: Move -> Move
makeCheck (Move w) = Move $ w .|. 0x20000000

-- Normal moves are coded:
-- c<pie> <frsq> <tosq>
-- where:
--   c = 0 for white, 1 for black (1 bit)
--   pie = 0 - pawn, 1 - knight, 2 - bishop, 3 - rook, 4 - queen, 5 - king (3 bits)
--   frsq - from square (6 bits)
--   tosq - to square (6 bits)
{-# INLINE moveIsNormal #-}
moveIsNormal :: Move -> Bool
moveIsNormal (Move w) = w .&. 0x6000 /= 0x6000

-- For which color is the move:
{-# INLINE moveColor #-}
moveColor :: Move -> Color
moveColor (Move w)
    | w .&. 0x8000 == 0 = White
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

tcQueen, tcRook, tcBishop, tcKnight :: Word32
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
encodeFromTo :: Square -> Square -> Word32
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

-- This one is used only to check the real moves (from UCI), which come
-- just with from & to square, and have to be translated accordingly
checkCastle :: Move -> MyPos -> Move
checkCastle m p
    | Busy c King <- tabla p s, moveIsNormal m
        = case ds of
            2  -> makeCastleFor c True
            -2 -> makeCastleFor c False
            _  -> m
    | otherwise = m
    where s = fromSquare m
          d = toSquare m
          ds = d - s

-- This one is used only to check the real moves (from UCI), which come
-- just with from & to square, and have to be translated accordingly
checkEnPas :: Move -> MyPos -> Move
checkEnPas m p
    | Busy _ Pawn <- tabla p f, moveIsNormal m
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
