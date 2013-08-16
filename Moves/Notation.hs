module Moves.Notation where

import Data.Array.Unboxed
import Data.Bits
import Data.Char (ord, chr, toUpper, toLower)
import Data.List
import Data.Word
import qualified Text.ParserCombinators.Parsec as P
import Text.ParserCombinators.Parsec ((<|>))

import Struct.Struct
import Moves.BitBoard
import Moves.Muster
import Moves.Moves
import Moves.Board

{--
showc :: UArray Int Char
showc = array (0, 15) $ zip [0..] [
             '.', 'P', 'N', 'K', 'x', 'B', 'R', 'Q',
             '.', 'p', 'n', 'k', 'y', 'b', 'r', 'q'
        ]

showLine :: Word8 -> Word8 -> Word8 -> Word8 -> String
showLine w1 w2 w3 w4 = go w1 w2 w3 w4 8 ""
    where go :: Word8 -> Word8 -> Word8 -> Word8 -> Int -> String -> String
          go _ _ _ _ 0 cs = cs
          go x y z t n cs
                   = go (x `shift` 1) (y `shift` 1) (z `shift` 1) (t `shift` 1) (n-1) (c:' ':cs)
               where c = showc ! cap x y z t

cap x y z t = fromIntegral $ (x' .|. shiftR y' 1 .|. shiftR z' 2 .|. shiftR t' 3) `shiftR` 4
    where x' = x .&. 0x80
          y' = y .&. 0x80
          z' = z .&. 0x80
          t' = t .&. 0x80
--}

-- Given a position and a move, write the move in nice human readable form
toNiceNotation :: MyPos -> Move -> String
toNiceNotation p m
    | moveIsCastle m = if s > d then "0-0-0" else "0-0"
    | otherwise      = piece ++ src ++ capt ++ dst ++ transf ++ check
    where piece = pcToCh False fig
          s = fromSquare m
          d = toSquare m
          (sr, sc) = s `divMod` 8
          (dr, dc) = d `divMod` 8
          (fig, fcol) | Busy c f <- tabla p s = (f, c)
          iscapt | Busy _ _ <- tabla p d = True
                 | otherwise             = False
          capt = if iscapt then "x" else ""
          att = fAttacs d fig (occup p)
          our = if fcol == White then white p else black p
          src | fig == Pawn = if iscapt then col sc : "" else ""
              | fig == King = ""
              | fig == Knight = desamb (knights p)
              | fig == Bishop = desamb (bishops p)
              | fig == Rook   = desamb (rooks p)
              | fig == Queen  = desamb (queens p)
          dst = col dc : row dr : ""
          transf = if moveIsTransf m then pcToCh False (moveTransfPiece m) else ""
          p' = doFromToMove m p
          check = if isCheck p' (other fcol) then "+" else ""
          orda = ord 'a'
          ord1 = ord '1'
          col x = chr (orda + x)
          row x = chr (ord1 + x)
          desamb b
              | popCount1 b0 == 1 = ""
              | popCount1 (b0 .&. colBB sc) == 1 = col sc : ""
              | popCount1 (b0 .&. rowBB sr) == 1 = row sr : ""
              | otherwise         = col sc : row sr : ""
              where b0 = b .&. att .&. our

pcToCh :: Bool -> Piece -> String
pcToCh _ King   = "K"
pcToCh _ Queen  = "Q"
pcToCh _ Rook   = "R"
pcToCh _ Bishop = "B"
pcToCh _ Knight = "N"
pcToCh False _  = ""	-- it remains only pawn, which sometimes
pcToCh True  _  = "P"	-- (in fen) must be printed, sometimes not

data SrcDest = SDCol Int | SDRow Int | SDColRow Int Int

-- What about castle? What about en passant?
fromNiceNotation :: MyPos -> Color -> P.Parser Move
fromNiceNotation p c = do
    piece <- parsePiece
    srcc  <- parseSrcOrCapt
    (msrc, capt, dst) <- case srcc of
        Left src -> do
            capt <- parseCapt
            if capt
               then do
                   dst  <- parseSrcDst
                   return (Just src, capt, dst)
               else do
                   mdst <- fmap Just parseSrcDst <|> return Nothing
                   case mdst of
                       Just dst -> return (Just src, capt, dst)
                       Nothing  -> return (Nothing,  capt, src)
        Right capt -> do
            dst <- parseSrcDst
            return (Nothing, capt, dst)
    mtra <- parseTransf
    chk <- parseCheck
    sqdst <- case dst of
        SDColRow x y -> return $ colRowToSquare x y
        _            -> fail "Wrong destination"
    if piece == Pawn	-- the pawn is difficult
       then case msrc of
                Just (SDCol x) -> if capt
                                     then parsePawnCapt p x sqdst mtra
                                     else parsePawnMove p x sqdst mtra
                _              -> fail "Wrong source for pawn"
       else case mtra of
                Just _  -> fail "Promotion, but no pawn move"
                Nothing -> parseFigureMove p piece msrc sqdst

-- Todo here: promotion and en passant!
parsePawnCapt p x sqdst mtra = do
    let fcol = moving p
        our = if fcol == White then white p else black p
        target = our .&. pawns p
        att  = pAttacs (other fcol) sqdst
    sqsrc <- bbToSingleSquare (target .&. att .&. colBB x)
    return $ moveFromTo sqsrc sqdst

-- Todo here: promotion and en passant!
parsePawnMove p x sqdst mtra = do
    let fcol = moving p
        our = if fcol == White then white p else black p
        target = our .&. pawns p
        att  = pAttacs (other fcol) sqdst
    sqsrc <- bbToSingleSquare (target .&. att .&. colBB x)
    return $ moveFromTo sqsrc sqdst

parseFigureMove p piece msrc sqdst = do
    let fcol = moving p
        our = if fcol == White then white p else black p
        target | piece == King = our .&. kings p
               | piece == Queen = our .&. queens p
               | piece == Rook = our .&. rooks p
               | piece == Bishop = our .&. bishops p
               | piece == Knight = our .&. knights p
        att = fAttacs sqdst piece (occup p)
    sqsrc <- case msrc of
                 Just (SDCol x) -> bbToSingleSquare (target .&. att .&. colBB x) 
                 Just (SDRow y) -> bbToSingleSquare (target .&. att .&. rowBB y)
                 Just (SDColRow x y) -> return $ colRowToSquare x y
                 Nothing        -> bbToSingleSquare (target .&. att)
    -- we don't check if it's really check
    return $ moveFromTo sqsrc sqdst

parsePiece = parseFigure <|> return Pawn

parseFigure = fmap chToPc $ P.oneOf "KQRBN"

chToPc 'K' = King
chToPc 'Q' = Queen
chToPc 'R' = Rook
chToPc 'B' = Bishop
chToPc 'N' = Knight

parseSrcOrCapt = (P.char 'x' >> return (Right True))
             <|> fmap Left parseSrcDst

parseCapt = (P.char 'x' >> return True) <|> return False

parseSrcDst = parseRow <|> parseColRow

parseCol = fmap (SDCol . (\c -> ord c - ord 'a')) $ P.oneOf "abcdefgh"

parseRow = fmap (SDRow . (\c -> ord c - ord '1')) $ P.oneOf "12345678"

parseColRow = do
    col@(SDCol c) <- parseCol
    parseRow >>= \(SDRow r) -> return (SDColRow c r) <|> return col

parseTransf = fmap (Just . chToPc . toUpper) (P.oneOf "QRBNqrbn")
              <|> return Nothing

parseCheck = (P.char '+' >> return True) <|> return False

colRowToSquare x y = y*8 + x

bbToSingleSquare b
    | b == 0     = fail "No piece found"
    | exactOne b = return $ firstOne b
    | otherwise  = fail "Ambiguous piece"

posToFen :: MyPos -> String
posToFen pos = unwords [lines, tmv, cast, rest]
    where lines :: String
          lines = concat $ map (extline . foldl tra ("", 0))
                         $ map (\s -> map (tabla pos) [s..s+7]) $ reverse [0, 8 .. 56]
          tra :: (String, Int) -> TabCont -> (String, Int)
          tra (s, n) Empty      = (s, n+1)
          tra (s, n) (Busy c f) = let zw = if n > 0 then show n else "" in (s ++ zw ++ trasq c f, 0)
          trasq White f = pcToCh True f
          trasq Black f = map toLower $ pcToCh True f
          extline (s, 0) = s ++ "/"
          extline (s, k) = s ++ show k ++ "/"
          tmv = if moving pos == White then "w" else "b"
          cast = [ckw, cqw, ckb, cqb]
          ckw = if not (kingMoved pos White) && castKingRookOk  pos White then 'K' else '-'
          cqw = if not (kingMoved pos White) && castQueenRookOk pos White then 'Q' else '-'
          ckb = if not (kingMoved pos Black) && castKingRookOk  pos Black then 'k' else '-'
          cqb = if not (kingMoved pos Black) && castQueenRookOk pos Black then 'q' else '-'
          rest = "- 0 1"	-- rest not yet implemented
