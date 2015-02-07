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
              | popCount1 b0 == 1 = ""
              | popCount1 (b0 .&. colBB sc) == 1 = col sc : ""
              | popCount1 (b0 .&. rowBB sr) == 1 = row sr : ""
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

chToPc :: Char -> Piece
chToPc 'K' = King
chToPc 'Q' = Queen
chToPc 'R' = Rook
chToPc 'B' = Bishop
chToPc 'N' = Knight
chToPc _   = Pawn	-- this is dummy, just to eliminate warnings

type MoveParser = Either String Move

files, ranks :: String
files = "abcdefgh"
ranks = "12345678"

fileToInt :: Char -> Int
fileToInt c = ord c - ord 'a'

rankToInt :: Char -> Int
rankToInt c = ord c - ord '1'

fromNiceNotation :: MyPos -> String -> MoveParser
fromNiceNotation p "O-O"   = Right $ makeCastleFor (moving p) True
fromNiceNotation p "O-O-O" = Right $ makeCastleFor (moving p) False
fromNiceNotation p (c:cs)
    | c `elem` "KQBNR" = isPiece p (chToPc c) cs
    | c `elem` files   = isPawn p (fileToInt c) cs
fromNiceNotation _ cs  = Left $ cs ++ ": expect O, or piece, or file"

isPiece :: MyPos -> Piece -> String -> MoveParser
isPiece p pc (c:cs)
    | c == 'x'       = pieceX p pc cs
    | c `elem` files = pieceA p pc (fileToInt c) cs
    | c `elem` ranks = piece1 p pc (rankToInt c) cs
isPiece _ _ cs       = Left $ cs ++ ": expect x, or file or rank"

pieceX :: MyPos -> Piece -> String -> MoveParser
pieceX p pc (c1:c2:cs)
    | c1 `elem` files && c2 `elem` ranks
        = pieceXA1 p pc (fileToInt c1) (rankToInt c2) cs
pieceX _ _ cs = Left $ cs ++ ": expect file and rank"

pieceA :: MyPos -> Piece -> Int -> String -> MoveParser
pieceA p pc f (c:cs)
    | c == 'x' = pieceAX p pc f cs
    | c `elem` files = pieceAB p pc f (fileToInt c) cs
    | c `elem` ranks = pieceA1 p pc f (rankToInt c) cs
pieceA _ _ _ cs = Left $ cs ++ ": expect x, or file, or rank"

piece1 :: MyPos -> Piece -> Int -> String -> MoveParser
piece1 p pc r (c:cs)
    | c == 'x'       = piece1X p pc r cs
    | c `elem` files = piece1A p pc r (fileToInt c) cs
piece1 _ _ _ cs = Left $ cs ++ ": expect x or file"

pieceAX :: MyPos -> Piece -> Int -> String -> MoveParser
pieceAX p pc f (c1:c2:cs)
    | c1 `elem` files && c2 `elem` ranks
        = pieceAXB1 p pc f (fileToInt c1) (rankToInt c2) cs
pieceAX _ _ _ cs = Left $ cs ++ ": expect file and rank"

piece1X :: MyPos -> Piece -> Int -> String -> MoveParser
piece1X p pc r (c1:c2:cs)
    | c1 `elem` files && c2 `elem` ranks
        = piece1XA2 p pc r (fileToInt c1) (rankToInt c2) cs
piece1X _ _ _ cs = Left $ cs ++ ": expect file and rank"

pieceAB :: MyPos -> Piece -> Int -> Int -> String -> MoveParser
pieceAB p pc f0 f (c:cs)
    | c `elem` ranks = pieceAB1 p pc f0 f (rankToInt c) cs
pieceAB _ _ _ _ cs = Left $ cs ++ ": expect rank"

piece1A :: MyPos -> Piece -> Int -> Int -> String -> MoveParser
piece1A p pc r f (c:cs)
    | c `elem` ranks = piece1A2 p pc r f (rankToInt c) cs
piece1A _ _ _ _ cs = Left $ cs ++ ": expect rank"

pieceXA1 :: MyPos -> Piece -> Int -> Int -> String -> MoveParser
pieceXA1 p pc f r cs
    | null cs || cs == "+" || cs == "#" =
        -- we will not check if it's mate or check
        case popCount1 pbb of
            0 -> Left $ "Semantic: no source piece"
            1 -> case tabla p sqdst of
                     Busy oco opc -> if opc == King
                                        then Left $ "Semantic: king capture"
                                        else if oco == moving p
                                                then Left $ "Semantic: capture own piece"
                                                else Right m
                     _            -> Left $ "Semantic: capture on empty square"
            _ -> Left $ "Semantic: too many pieces (pieceXA1)"
    | otherwise = Left $ cs ++ ": expect +, # or end of string"
    where target | pc == King   = me p .&. kings p
                 | pc == Queen  = me p .&. queens p
                 | pc == Rook   = me p .&. rooks p
                 | pc == Bishop = me p .&. bishops p
                 | pc == Knight = me p .&. knights p
                 | otherwise       = 0	-- this is dummy, for warnings
          sqdst = colRowToSquare f r
          att = fAttacs sqdst pc (occup p)
          pbb = target .&. att
          sqsrc = firstOne pbb
          m = moveAddColor (moving p) $ moveAddPiece pc $ moveFromTo sqsrc sqdst

pieceAB1 :: MyPos -> Piece -> Int -> Int -> Int -> String -> MoveParser
pieceAB1 p pc f0 f r cs
    | null cs || cs == "+" || cs == "#" =
        -- we will not check if it's mate or check
        case popCount1 pbb of
            0 -> Left $ "Semantic: no source piece"
            1 -> case tabla p sqdst of
                     Empty -> Right m
                     _     -> Left $ "Semantic: move is capture"
            _ -> Left $ "Semantic: too many pieces (pieceAB1)"
    | otherwise = Left $ cs ++ ": expect +, # or end of string"
    where target | pc == King   = me p .&. kings p
                 | pc == Queen  = me p .&. queens p
                 | pc == Rook   = me p .&. rooks p
                 | pc == Bishop = me p .&. bishops p
                 | pc == Knight = me p .&. knights p
                 | otherwise       = 0	-- this is dummy, for warnings
          sqdst = colRowToSquare f r
          pbb = target .&. colBB f0
          sqsrc = firstOne pbb
          m = moveAddColor (moving p) $ moveAddPiece pc $ moveFromTo sqsrc sqdst

piece1A2 :: MyPos -> Piece -> Int -> Int -> Int -> String -> MoveParser
piece1A2 p pc r0 f r cs
    | null cs || cs == "+" || cs == "#" =
        -- we will not check if it's mate or check
        case popCount1 pbb of
            0 -> Left $ "Semantic: no source piece"
            1 -> case tabla p sqdst of
                     Empty -> Right m
                     _     -> Left $ "Semantic: move is capture"
            _ -> Left $ "Semantic: too many pieces (piece1A2)"
    | otherwise = Left $ cs ++ ": expect +, # or end of string"
    where target | pc == King   = me p .&. kings p
                 | pc == Queen  = me p .&. queens p
                 | pc == Rook   = me p .&. rooks p
                 | pc == Bishop = me p .&. bishops p
                 | pc == Knight = me p .&. knights p
                 | otherwise       = 0	-- this is dummy, for warnings
          sqdst = colRowToSquare f r
          -- att = fAttacs sqdst pc (occup p)
          -- pbb = target .&. att .&. rowBB r0
          pbb = target .&. rowBB r0
          sqsrc = firstOne pbb
          m = moveAddColor (moving p) $ moveAddPiece pc $ moveFromTo sqsrc sqdst

pieceAXB1 :: MyPos -> Piece -> Int -> Int -> Int -> String -> MoveParser
pieceAXB1 p pc f0 f r cs
    | null cs || cs == "+" || cs == "#" =
        -- we will not check if it's mate or check
        case popCount1 pbb of
            0 -> Left $ "Semantic: no source piece"
            1 -> case tabla p sqdst of
                     Busy oco opc -> if opc == King
                                        then Left $ "Semantic: king capture"
                                        else if oco == moving p
                                                then Left $ "Semantic: capture own piece"
                                                else Right m
                     _            -> Left $ "Semantic: capture on empty square"
            _ -> Left $ "Semantic: too many pieces (pieceAXB1)"
    | otherwise = Left $ cs ++ ": expect +, # or end of string"
    where target | pc == King   = me p .&. kings p
                 | pc == Queen  = me p .&. queens p
                 | pc == Rook   = me p .&. rooks p
                 | pc == Bishop = me p .&. bishops p
                 | pc == Knight = me p .&. knights p
                 | otherwise       = 0	-- this is dummy, for warnings
          sqdst = colRowToSquare f r
          -- att = fAttacs sqdst pc (occup p)
          -- pbb = target .&. att .&. colBB f0
          pbb = target .&. colBB f0
          sqsrc = firstOne pbb
          m = moveAddColor (moving p) $ moveAddPiece pc $ moveFromTo sqsrc sqdst

piece1XA2 :: MyPos -> Piece -> Int -> Int -> Int -> String -> MoveParser
piece1XA2 p pc r0 f r cs
    | null cs || cs == "+" || cs == "#" =
        -- we will not check if it's mate or check
        case popCount1 pbb of
            0 -> Left $ "Semantic: no source piece"
            1 -> case tabla p sqdst of
                     Busy oco opc -> if opc == King
                                        then Left $ "Semantic: king capture"
                                        else if oco == moving p
                                                then Left $ "Semantic: capture own piece"
                                                else Right m
                     _            -> Left $ "Semantic: capture on empty square"
            _ -> Left $ "Semantic: too many pieces (piece1XA2)"
    | otherwise = Left $ cs ++ ": expect +, # or end of string"
    where target | pc == King   = me p .&. kings p
                 | pc == Queen  = me p .&. queens p
                 | pc == Rook   = me p .&. rooks p
                 | pc == Bishop = me p .&. bishops p
                 | pc == Knight = me p .&. knights p
                 | otherwise       = 0	-- this is dummy, for warnings
          sqdst = colRowToSquare f r
          -- att = fAttacs sqdst pc (occup p)
          -- pbb = target .&. att .&. rowBB r0
          pbb = target .&. rowBB r0
          sqsrc = firstOne pbb
          m = moveAddColor (moving p) $ moveAddPiece pc $ moveFromTo sqsrc sqdst

pieceA1 :: MyPos -> Piece -> Int -> Int -> String -> MoveParser
pieceA1 p pc f r cs
    | null cs || cs == "+" || cs == "#" =
        -- we will not check if it's mate or check
        case popCount1 pbb of
            0 -> Left $ "Semantic: no source piece"
            1 -> case tabla p sqdst of
                     Empty -> Right m
                     _     -> Left $ "Semantic: move is capture"
            _ -> Left $ "Semantic: too many pieces (pieceA1)"
    | otherwise = Left $ cs ++ ": expect +, # or end of string"
    where target | pc == King   = me p .&. kings p
                 | pc == Queen  = me p .&. queens p
                 | pc == Rook   = me p .&. rooks p
                 | pc == Bishop = me p .&. bishops p
                 | pc == Knight = me p .&. knights p
                 | otherwise       = 0	-- this is dummy, for warnings
          sqdst = colRowToSquare f r
          att = fAttacs sqdst pc (occup p)
          pbb = target .&. att
          sqsrc = firstOne pbb
          m = moveAddColor (moving p) $ moveAddPiece pc $ moveFromTo sqsrc sqdst

isPawn :: MyPos -> Int -> String -> MoveParser
isPawn p f (c:cs)
    | c == 'x'       = pawnX p f cs
    | c `elem` ranks = pawn1 p f (rankToInt c) cs
isPawn _ _ cs        = Left $ cs ++ ": expect x or rank"

pawnX :: MyPos -> Int -> String -> MoveParser
pawnX p f (c1:c2:cs)
    | c1 `elem` files && c2 `elem` ranks = pawnXA1 p f (fileToInt c1) (rankToInt c2) cs
pawnX _ _ cs = Left $ cs ++ ": expect file and rank"

pawn1 :: MyPos -> Int -> Int -> String -> MoveParser
pawn1 p f r (c:cs)
    | c == '=' = pawn1Q p f r cs
    | c == '+' || c == '#' = pawn1 p f r []	-- simplified a bit...
pawn1 p f r []
    | Just sqsrc <- msqsrc =
          if (fcol == White && r == 7) || (fcol == Black && r == 0)
             then Right $ makePromo Queen sqsrc sqdst	-- assume queen promotion
             else return $ moveAddColor fcol $ moveAddPiece Pawn $ moveFromTo sqsrc sqdst
    | otherwise = Left $ "Semantic: no pawn can move there"
    where fcol = moving p
          target = me p .&. pawns p .&. colBB f
          sqdst = colRowToSquare f r
          src1 | fcol == White = sqdst - 8
               | otherwise     = sqdst + 8
          src2 | fcol == White = src1  - 8
               | otherwise     = src1  + 8
          msqsrc = if target `uTestBit` src1
                      then Just src1
                      else if occup p `uTestBit` src1
                              then Nothing
                              else if src2 < 0 || src2 > 63
                                      then Nothing
                                      else if target `uTestBit` src2
                                              then Just src2
                                              else Nothing
pawn1 _ _ _ cs = Left $ cs ++ ": expect +, #, = or end of string"

pawn1Q :: MyPos -> Int -> Int -> String -> MoveParser
pawn1Q p f r (c:cs)
    | c `elem` "QRBN" && (null cs || cs == "+" || cs == "#")
        = case msqsrc of
              Just sqsrc -> if (fcol == White && r /= 7) || (fcol == Black && r /= 0)
                               then Left $ "Semantic: this is not a promoting pawn"
                               else Right $ makePromo pc sqsrc sqdst
              Nothing    -> Left $ "Semantic: no pawn can promote here"
    where fcol = moving p
          target = me p .&. pawns p .&. colBB f
          pc = chToPc c
          sqdst = colRowToSquare f r
          src1 | fcol == White = sqdst - 8
               | otherwise     = sqdst + 8
          src2 | fcol == White = src1  - 8
               | otherwise     = src1  + 8
          msqsrc = if target `uTestBit` src1
                      then Just src1
                      else if occup p `uTestBit` src1
                              then Nothing
                              else if src2 < 0 || src2 > 63
                                      then Nothing
                                      else if target `uTestBit` src2
                                              then Just src2
                                              else Nothing
pawn1Q _ _ _ cs = Left $ cs ++ ": expect promotion piece, then +, # or end of string"

pawnXA1 :: MyPos -> Int -> Int -> Int -> String -> MoveParser
pawnXA1 p f0 f r (c:cs)
    | c == '=' = pawnXA1Q p f0 f r cs
    | c == '+' || c == '#' = pawnXA1 p f0 f r []	-- simplified a bit...
pawnXA1 p f0 f r []
    | Just sqsrc <- msqsrc =
          if (fcol == White && r == 7) || (fcol == Black && r == 0)
             then Right $ makePromo Queen sqsrc sqdst	-- assume queen promotion
             else let m = moveAddColor fcol $ moveAddPiece Pawn $ moveFromTo sqsrc sqdst
                  in case tabla p sqdst of
                         Busy cc cp | cc == fcol -> Left $ "Semantic: pawn captures own piece"
                                    | cp == King -> Left $ "Semantic: pawn captures king"
                                    | otherwise  -> Right m
                         Empty -> if (fcol == White && r == 5) || (fcol == Black && r == 2)
                                     then let m' = checkEnPas m p
                                          in if moveIsEnPas m'
                                                then Right m'
                                                else Left $ "Semantic: pawn capture on empty square"
                                     else Left $ "Semantic: pawn capture on empty square"
    | otherwise = Left $ "Semantic: no pawn can capture there"
    where fcol = moving p
          srcr | fcol == White = r - 1
               | otherwise     = r + 1
          target = me p .&. pawns p .&. colBB f0 .&. rowBB srcr
          sqdst = colRowToSquare f r
          msqsrc = if popCount1 target == 1
                      then Just $ firstOne target
                      else Nothing
pawnXA1 _ _ _ _ cs = Left $ cs ++ ": expect +, #, = or end of string"

pawnXA1Q :: MyPos -> Int -> Int -> Int -> String -> MoveParser
pawnXA1Q p f0 f r (c:cs)
    | c `elem` "QRBN" && (null cs || cs == "+" || cs == "#") =
         case msqsrc of
              Just sqsrc -> if (fcol == White && r == 7) || (fcol == Black && r == 0)
                               then Right $ makePromo pc sqsrc sqdst
                               else Left $ "Semantic: pawn capture is not a promotion"
              Nothing    -> Left $ "Semantic: no pawn can capture there"
    where fcol = moving p
          pc = chToPc c
          srcr | fcol == White = r - 1
               | otherwise     = r + 1
          target = me p .&. pawns p .&. colBB f0 .&. rowBB srcr
          sqdst = colRowToSquare f r
          msqsrc = if popCount1 target == 1
                      then Just $ firstOne target
                      else Nothing
pawnXA1Q _ _ _ _ cs = Left $ cs ++ ": expect promotion piece, then +, # or end of string"

{--
                 Just (SDCol x) -> bbToSingleSquare (target .&. att .&. colBB x) 
                 Just (SDRow y) -> bbToSingleSquare (target .&. att .&. rowBB y)
                 Just (SDColRow x y) -> return $ colRowToSquare x y
                 Nothing        -> bbToSingleSquare (target .&. att)
    -- we don't check if it's really check
    --}

data SrcDest = SDCol Int | SDRow Int | SDColRow Int Int

fromNiceNotation' :: MyPos -> String -> Either P.ParseError Move
fromNiceNotation' p = P.parse (parseNiceNotation p) ""

parseNiceNotation :: MyPos -> P.Parser Move
parseNiceNotation p = parseNiceCastleOO p <|> parseNiceCastleOOO p <|> parseNiceMove p

parseNiceCastleOO :: MyPos -> P.Parser Move
parseNiceCastleOO p = do
    P.string "0-0"
    return $ makeCastleFor (moving p) True

parseNiceCastleOOO :: MyPos -> P.Parser Move
parseNiceCastleOOO p = do
    P.string "0-0-0"
    return $ makeCastleFor (moving p) False

parseNiceMove :: MyPos -> P.Parser Move
parseNiceMove p = do
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

-- Pawn capture
parsePawnCapt :: MyPos -> Int -> Int -> Maybe Piece -> P.Parser Move
parsePawnCapt p x sqdst mpro = do
    let fcol = moving p
        target = me p .&. pawns p
        att  = pAttacs (other fcol) sqdst
    sqsrc <- bbToSingleSquare (target .&. att .&. colBB x)
    if (fcol == White && sqdst >= 58) || (fcol == Black && sqdst < 8)
       then case mpro of	-- promotion
                Just ppc -> return $ makePromo ppc   sqsrc sqdst
                Nothing  -> return $ makePromo Queen sqsrc sqdst	-- assume queen promotion
       else do
           let m = moveAddColor fcol $ moveAddPiece Pawn $ moveFromTo sqsrc sqdst
           case tabla p sqdst of
               Busy cc cp | cc == fcol -> fail "Pawn captures own piece"
                          | cp == King -> fail "Pawn captures king"
                          | otherwise  -> return m
               Empty -> do
                   let m' = checkEnPas m p
                   if moveIsEnPas m' then return m' else fail "Pawn capture on empty square"

-- Pawn move
parsePawnMove :: MyPos -> Int -> Int -> Maybe Piece -> P.Parser Move
parsePawnMove p x sqdst mpro = do
    let fcol = moving p
        target = me p .&. pawns p .&. colBB x
        src1 | fcol == White = sqdst - 8
             | otherwise     = sqdst + 8
        src2 | fcol == White = src1  - 8
             | otherwise     = src1  + 8
        msqsrc = if target `uTestBit` src1
                    then Just src1
                    else if occup p `uTestBit` src1
                            then Nothing
                            else if src2 < 0 || src2 > 63
                                    then Nothing
                                    else if target `uTestBit` src2
                                            then Just src2
                                            else Nothing
    case msqsrc of
        Nothing    -> fail "Wrong pawn move"
        Just sqsrc ->
            if (fcol == White && sqdst >= 58) || (fcol == Black && sqdst < 8)
               then case mpro of	-- promotion
                        Just ppc -> return $ makePromo ppc   sqsrc sqdst
                        Nothing  -> return $ makePromo Queen sqsrc sqdst	-- assume queen promotion
               else return $ moveAddColor fcol $ moveAddPiece Pawn $ moveFromTo sqsrc sqdst

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
