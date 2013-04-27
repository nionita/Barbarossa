module Uci.UCI (
         UCIMess(..), Pos(..), GoCmds(..), ExpCommand(..),
         parseUciStr, parseMoveStr, parseExploreStr,
         findDepth, findTInc, findTime, findMovesToGo
    ) where

import Data.Char
import Data.Array.Unboxed
import qualified Text.ParserCombinators.Parsec as P
import Text.ParserCombinators.Parsec ((<|>))

import Struct.Struct
import Moves.Base

data UCIMess
    = Uci
    | Debug Bool
    | IsReady
    | SetOption Option
    | UciNewGame
    | Position Pos [Move]
    | Go [GoCmds]
    | Stop
    | Ponderhit
    | Quit
    deriving Show

data Option
    = Name String
    | NameValue String String
    deriving Show

data Pos
    = StartPos
    | Pos String
    deriving Show

data GoCmds
    = SearchMoves [Move]
    | Ponder
    | Time Color Int
    | TInc Color Int
    | MovesToGo Int
    | Depth Int
    | Nodes Int
    | Mate Int
    | MoveTime Int
    | Infinite
    deriving (Eq, Show)

data ExpCommand = Fen String	-- new game, position from fen
             | Init		-- new game, initial position
             | Moves		-- print all moves
             | QMoves		-- print quiescent moves
             | Down Move	-- one level deep with the move
             | Up 		-- one level up with the Int as score
             | Eval 		-- evaluate the position
             | QEval 		-- evaluate the position after quiescent search
             | Help		-- print some help
             | Exit		-- exit program

parseUciStr = P.parse parseUCIMess ""

parseMoveStr = P.parse parseMove ""

parseExploreStr = P.parse parseExplore ""

literal s = P.spaces >> P.string s

untilP s = go s ""
    where go s acc = (P.string s >> return (reverse acc))
             `orElse` do
                  c <- P.anyChar
                  go s (c:acc)

orElse a b = P.try a <|> b

parseUCIMess = P.choice $ map P.try [
        parseUciNewGame,
        parseUci,
        parseDebug,
        parseIsReady,
        parseStop,
        parseSetOption,
        parsePosition,
        -- parsePonderhit,
        parseGo,
        parseQuit
    ]

parseUci = literal "uci" >> return Uci

parseUciNewGame = literal "ucinewgame" >> return UciNewGame

parseIsReady = literal "isready" >> return IsReady

parseStop = literal "stop" >> return Stop

-- parsePonderhit = literal "ponderhit" >> return Ponderhit

parseQuit = literal "quit" >> return Quit

parseDebug = do
    literal "debug"
    P.spaces
    P.char 'o'
    t <- P.try (P.char 'n' >> return True) <|> (P.string "ff" >> return False)
    return (Debug t)

parseSetOption = do
    literal "setoption"
    literal "name"
    P.spaces
    o <- (do
        nm <- untilP "value"
        vl <- P.many P.alphaNum
        return (NameValue nm vl)
     ) `orElse` (do
        nm <- P.many P.alphaNum
        return (Name nm)
     )
    return $ SetOption o

parsePosition = do
    literal "position"
    P.spaces
    parseStartPos `orElse` parseFen

parseFenPosition = do
    s <- P.many1 $ P.oneOf "/12345678rnbqkpRNBQKP"
    P.space
    c <- P.oneOf "wb"
    P.space
    cr <- P.many1 $ P.oneOf "-QKqk"
    P.space
    ep <- P.many1 (P.oneOf "-abcdefgh36")
    P.space
    h <- P.many1 P.digit
    P.space
    P.anyChar
    return $ s ++ " " ++ [c] ++ " " ++ cr ++ " " ++ ep ++ " " ++ h

parseStartPos = do
    literal "startpos"
    P.spaces
    ms <- (literal "moves" >> P.spaces >> parseMoves)
        `orElse` return []
    return $ Position StartPos ms

parseFen = do
    literal "fen"
    P.spaces
    fenp <- parseFenPosition
    P.spaces
    ms <- (literal "moves" >> P.spaces >> parseMoves)
        `orElse` return []
    return $ Position (Pos fenp) ms

parseMoves = P.sepBy parseMove P.spaces

parseMove = do
    sf <- parseFeld
    ef <- parseFeld
    pr <- parsePromo `orElse` return Nothing
    let m = moveFromTo sf ef
    case pr of
        Just b  -> return $ activateTransf b m
        Nothing -> return m

parseFeld = do
    lit <- P.oneOf ['a'..'h']
    cif <- P.oneOf ['1'..'8']
    return $ fromColRow (ord lit - ord 'a' + 1) (ord cif - ord '0')

parsePromo = do
    pro <- P.oneOf "qrbn"
    return $ Just pro

parseGo = do
    literal "go"
    P.spaces
    -- gcs <- P.sepBy parseGoCmd P.spaces
    gcs <- P.many parseGoCmd
    return $ Go gcs

parseGoCmd = P.choice $ map P.try [
        parsePonder,
        parseTime,
        parseTInc,
        parseMovesToGo,
        parseDepth,
        parseNodes,
        parseMate,
        parseMoveTime,
        parseInfinite,
        parseSearchMoves
    ]

parseSearchMoves = do
    literal "searchmoves"
    P.spaces
    mvs <- parseMoves
    return $ SearchMoves mvs

parsePonder = literal "ponder" >> return Ponder

parseWithInt :: String -> (Int -> a) -> P.Parser a
parseWithInt s con = do
    literal s
    P.spaces
    num <- P.many P.digit
    return $ con (read num)

parseTime = parseWithInt "wtime" (Time White)
            `orElse` parseWithInt "btime" (Time Black)
parseTInc  = parseWithInt "winc" (TInc White)
             `orElse` parseWithInt "binc" (TInc Black)
parseMovesToGo = parseWithInt "movestogo" MovesToGo
parseDepth = parseWithInt "depth" Depth
parseNodes = parseWithInt "nodes" Nodes
parseMate  = parseWithInt "mate" Mate
parseMoveTime = parseWithInt "movetime" MoveTime

parseInfinite = literal "infinite" >> return Infinite

-- Parsing the explore commands:
parseExplore = parseExpFen <|> parseExpInit <|> parseExpMoves <|> parseExpQMoves
            <|> parseExpDown <|> parseExpUp <|> parseExpHelp <|> parseExpExit
            <|> parseExpEval <|> parseExpQEval

parseExpFen  = P.char 'f' >> P.spaces >> parseFenPosition >>= return . Fen
parseExpInit = P.char 'i' >> return Init
parseExpMoves = P.char 'm' >> return Moves
parseExpQMoves = P.char 'q' >> return QMoves
parseExpDown = P.char 'd' >> parseMove >>= return . Down
parseExpUp   = P.char 'u' >> return Up
parseExpEval = P.char 'e' >> return Eval
parseExpQEval = P.char 'v' >> return QEval
parseExpHelp = P.char 'h' >> return Help
parseExpExit = P.char 'x' >> return Exit

-- Some utilities to find information in the uci go commands:

findDepth :: [GoCmds] -> Maybe Int
findDepth [] = Nothing
findDepth (Depth d : _) = Just d
findDepth (_ : cms) = findDepth cms

findTime :: Color -> [GoCmds] -> Maybe Int
findTime _ [] = Nothing
findTime c (Time c1 ms : cms)
    | c == c1   = Just ms
    | otherwise = findTime c cms
findTime c (_ : cms) = findTime c cms

findTInc :: Color -> [GoCmds] -> Maybe Int
findTInc _ [] = Nothing
findTInc c (TInc c1 ms : cms)
    | c == c1   = Just ms
    | otherwise = findTInc c cms
findTInc c (_ : cms) = findTInc c cms

findMovesToGo :: [GoCmds] -> Maybe Int
findMovesToGo [] = Nothing
findMovesToGo (MovesToGo m : _) = Just m
findMovesToGo (_ : cms) = findMovesToGo cms
