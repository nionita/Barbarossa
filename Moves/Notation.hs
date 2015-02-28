{-# LANGUAGE PatternGuards #-}

module Moves.Notation where

import Data.Bits
import Data.Char (ord, chr, toUpper, toLower)
import qualified Text.ParserCombinators.Parsec as P
import Text.ParserCombinators.Parsec ((<|>))

import Struct.Struct
import Moves.BitBoard
import Moves.Muster
import Moves.Moves
import Moves.Board

-- Given a position and a move, write the move in nice human readable form
toNiceNotation :: MyPos -> Move -> String
toNiceNotation p m
    | moveIsCastle m = if s > d then "0-0-0" else "0-0"
    | otherwise      = piece ++ src ++ capt ++ dst ++ transf ++ chk
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
          src | fig == Pawn = if iscapt then col sc : "" else ""
              | fig == Knight = desamb (knights p)
              | fig == Bishop = desamb (bishops p)
              | fig == Rook   = desamb (rooks p)
              | fig == Queen  = desamb (queens p)
              | otherwise     = ""	-- king
          dst = col dc : row dr : ""
          transf = if moveIsPromo m then pcToCh False (movePromoPiece m) else ""
          p' = doFromToMove m p
          chk = if isCheck p' (other fcol) then "+" else ""
          orda = ord 'a'
          ord1 = ord '1'
          col x = chr (orda + x)
          row x = chr (ord1 + x)
          desamb b
              | popCount b0 == 1 = ""
              | popCount (b0 .&. colBB sc) == 1 = col sc : ""
              | popCount (b0 .&. rowBB sr) == 1 = row sr : ""
              | otherwise         = col sc : row sr : ""
              where b0 = b .&. att .&. me p

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
fromNiceNotation p _ = do
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
    _ <- parseCheck
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
parsePawnCapt :: MyPos -> Int -> Int -> Maybe Piece -> P.Parser Move
parsePawnCapt p x sqdst _ = do
    let fcol = moving p
        target = me p .&. pawns p
        att  = pAttacs (other fcol) sqdst
    sqsrc <- bbToSingleSquare (target .&. att .&. colBB x)
    return $ moveAddColor (moving p) $ moveAddPiece Pawn $ moveFromTo sqsrc sqdst

-- Todo here: promotion and en passant!
parsePawnMove :: MyPos -> Int -> Int -> Maybe Piece -> P.Parser Move
parsePawnMove p x sqdst _ = do
    let fcol = moving p
        target = me p .&. pawns p
        att  = pAttacs (other fcol) sqdst
    sqsrc <- bbToSingleSquare (target .&. att .&. colBB x)
    return $ moveAddColor (moving p) $ moveAddPiece Pawn $ moveFromTo sqsrc sqdst

parseFigureMove :: MyPos -> Piece -> Maybe SrcDest -> Int -> P.Parser Move
parseFigureMove p piece msrc sqdst = do
    let target | piece == King   = me p .&. kings p
               | piece == Queen  = me p .&. queens p
               | piece == Rook   = me p .&. rooks p
               | piece == Bishop = me p .&. bishops p
               | piece == Knight = me p .&. knights p
               | otherwise       = 0	-- this is dummy, for warnings
        att = fAttacs sqdst piece (occup p)
    sqsrc <- case msrc of
                 Just (SDCol x) -> bbToSingleSquare (target .&. att .&. colBB x) 
                 Just (SDRow y) -> bbToSingleSquare (target .&. att .&. rowBB y)
                 Just (SDColRow x y) -> return $ colRowToSquare x y
                 Nothing        -> bbToSingleSquare (target .&. att)
    -- we don't check if it's really check
    return $ moveAddColor (moving p) $ moveAddPiece piece $ moveFromTo sqsrc sqdst

parsePiece :: P.Parser Piece
parsePiece = parseFigure <|> return Pawn

parseFigure :: P.Parser Piece
parseFigure = fmap chToPc $ P.oneOf "KQRBN"

chToPc :: Char -> Piece
chToPc 'K' = King
chToPc 'Q' = Queen
chToPc 'R' = Rook
chToPc 'B' = Bishop
chToPc 'N' = Knight
chToPc _   = Pawn	-- this is dummy, just to eliminate warnings

parseSrcOrCapt :: P.Parser (Either SrcDest Bool)
parseSrcOrCapt = (P.char 'x' >> return (Right True))
             <|> fmap Left parseSrcDst

parseCapt :: P.Parser Bool
parseCapt = (P.char 'x' >> return True) <|> return False

parseSrcDst :: P.Parser SrcDest
parseSrcDst = parseRow <|> parseColRow

parseCol :: P.Parser SrcDest
parseCol = fmap (SDCol . (\c -> ord c - ord 'a')) $ P.oneOf "abcdefgh"

parseRow :: P.Parser SrcDest
parseRow = fmap (SDRow . (\c -> ord c - ord '1')) $ P.oneOf "12345678"

parseColRow :: P.Parser SrcDest
parseColRow = do
    col@(SDCol c) <- parseCol
    parseRow >>= \(SDRow r) -> return (SDColRow c r) <|> return col

parseTransf :: P.Parser (Maybe Piece)
parseTransf = fmap (Just . chToPc . toUpper) (P.oneOf "QRBNqrbn")
              <|> return Nothing

parseCheck :: P.Parser Bool
parseCheck = (P.char '+' >> return True) <|> return False

colRowToSquare :: Int -> Int -> Int
colRowToSquare x y = y*8 + x

bbToSingleSquare :: Monad m => BBoard -> m Square
bbToSingleSquare b
    | b == 0     = fail "No piece found"
    | exactOne b = return $ firstOne b
    | otherwise  = fail "Ambiguous piece"

posToFen :: MyPos -> String
posToFen pos = unwords [lns, tmv, correct cast, ep, halb, rest]
    where lns :: String
          lns = concat $ map (extline . foldl tra ("", 0))
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
          ckw = if castKingRookOk  pos White then 'K' else '-'
          cqw = if castQueenRookOk pos White then 'Q' else '-'
          ckb = if castKingRookOk  pos Black then 'k' else '-'
          cqb = if castQueenRookOk pos Black then 'q' else '-'
          epbb = epcas pos .&. epMask
          ep | epbb == 0 = "-"
             | otherwise = let sq = firstOne epbb
                               (r, c) = sq `divMod` 8
                           in chr (ord 'a' + c) : chr (ord '1' + r) : []
          halb = show $ (epcas pos .&. fyMask) `div` fyIncr
          rest = "1"	-- rest not yet implemented
          correct "----" = "-"
          correct x = filter ((/=) '-') x
