{-# LANGUAGE RankNTypes #-}
module Uci.UCI (
         UCIMess(..), Option(..), Pos(..), GoCmds(..), ExpCommand(..),
         parseUciStr, parseMoveStr, parseExploreStr,
         findDepth, findTInc, findTime, findMovesToGo
    ) where

import Data.Char
import qualified Text.ParserCombinators.Parsec as P
import Text.ParserCombinators.Parsec (Parser, (<|>))

import Struct.Struct

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

parseUciStr :: String -> Either P.ParseError UCIMess
parseUciStr = P.parse parseUCIMess ""

parseMoveStr :: String -> Either P.ParseError Move
parseMoveStr = P.parse parseMove ""

parseExploreStr :: String -> Either P.ParseError ExpCommand
parseExploreStr = P.parse parseExplore ""

literal :: String -> Parser ()
literal s = P.spaces >> P.string s >> return ()

orElse :: Parser a -> Parser a -> Parser a
orElse a b = P.try a <|> b

parseUCIMess :: Parser UCIMess
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

parseUci :: Parser UCIMess
parseUci = literal "uci" >> return Uci

parseUciNewGame :: Parser UCIMess
parseUciNewGame = literal "ucinewgame" >> return UciNewGame

parseIsReady :: Parser UCIMess
parseIsReady = literal "isready" >> return IsReady

parseStop :: Parser UCIMess
parseStop = literal "stop" >> return Stop

-- parsePonderhit = literal "ponderhit" >> return Ponderhit

parseQuit :: Parser UCIMess
parseQuit = literal "quit" >> return Quit

parseDebug :: Parser UCIMess
parseDebug = do
    literal "debug"
    _ <- P.spaces
    _ <- P.char 'o'
    t <- P.try (P.char 'n' >> return True) <|> (P.string "ff" >> return False)
    return (Debug t)

parseSetOption :: Parser UCIMess
parseSetOption = do
    literal "setoption"
    literal "name"
    P.spaces
    o <- (do
            nm <- P.many P.alphaNum
            P.spaces
            literal "value"
            P.spaces
            vl <- P.many P.anyToken
            return (NameValue nm vl)
         ) `orElse` (do
            nm <- P.many P.alphaNum
            return (Name nm)
         )
    return $ SetOption o

parsePosition :: Parser UCIMess
parsePosition = do
    literal "position"
    P.spaces
    parseStartPos `orElse` parseFen

parseFenPosition :: Parser String
parseFenPosition = do
    s <- P.many1 $ P.oneOf "/12345678rnbqkpRNBQKP"
    P.spaces
    c <- P.oneOf "wb"
    P.spaces
    cr <- P.many1 $ P.oneOf "-QKqk"
    P.spaces
    ep <- P.many1 (P.oneOf "-abcdefgh36")
    P.spaces
    h <- P.many1 P.digit
    P.spaces
    _ <- P.anyChar
    return $ s ++ " " ++ [c] ++ " " ++ cr ++ " " ++ ep ++ " " ++ h

parseStartPos :: Parser UCIMess
parseStartPos = do
    literal "startpos"
    P.spaces
    ms <- (literal "moves" >> P.spaces >> parseMoves)
        `orElse` return []
    return $ Position StartPos ms

parseFen :: Parser UCIMess
parseFen = do
    literal "fen"
    P.spaces
    fenp <- parseFenPosition
    P.spaces
    ms <- (literal "moves" >> P.spaces >> parseMoves)
        `orElse` return []
    return $ Position (Pos fenp) ms

parseMoves :: Parser [Move]
parseMoves = P.sepBy parseMove P.spaces

parseMove :: Parser Move
parseMove = do
    sf <- parseFeld
    ef <- parseFeld
    pr <- parsePromo `orElse` return Nothing
    let m = moveFromTo sf ef
    case pr of
        Just b  -> return $ activatePromo b m
        Nothing -> return m

parseFeld :: Parser Square
parseFeld = do
    lit <- P.oneOf ['a'..'h']
    cif <- P.oneOf ['1'..'8']
    return $ fromColRow (ord lit - ord 'a' + 1) (ord cif - ord '0')

parsePromo :: Parser (Maybe Char)
parsePromo = do
    pro <- P.oneOf "qrbn"
    return $ Just pro

parseGo :: Parser UCIMess
parseGo = do
    literal "go"
    P.spaces
    -- gcs <- P.sepBy parseGoCmd P.spaces
    gcs <- P.many parseGoCmd
    return $ Go gcs

parseGoCmd :: Parser GoCmds
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

parseSearchMoves :: Parser GoCmds
parseSearchMoves = do
    literal "searchmoves"
    P.spaces
    mvs <- parseMoves
    return $ SearchMoves mvs

parsePonder :: Parser GoCmds
parsePonder = literal "ponder" >> return Ponder

parseWithInt :: String -> (Int -> a) -> Parser a
parseWithInt s con = do
    literal s
    P.spaces
    num <- P.many P.digit
    return $ con (read num)

parseTime :: Parser GoCmds
parseTime = parseWithInt "wtime" (Time White)
            `orElse` parseWithInt "btime" (Time Black)

parseTInc :: Parser GoCmds
parseTInc  = parseWithInt "winc" (TInc White)
             `orElse` parseWithInt "binc" (TInc Black)

parseMovesToGo :: Parser GoCmds
parseMovesToGo = parseWithInt "movestogo" MovesToGo

parseDepth :: Parser GoCmds
parseDepth = parseWithInt "depth" Depth

parseNodes :: Parser GoCmds
parseNodes = parseWithInt "nodes" Nodes

parseMate :: Parser GoCmds
parseMate  = parseWithInt "mate" Mate

parseMoveTime :: Parser GoCmds
parseMoveTime = parseWithInt "movetime" MoveTime

parseInfinite :: Parser GoCmds
parseInfinite = literal "infinite" >> return Infinite

-- Parsing the explore commands:
parseExplore :: Parser ExpCommand
parseExplore = parseExpFen <|> parseExpInit <|> parseExpMoves <|> parseExpQMoves
            <|> parseExpDown <|> parseExpUp <|> parseExpHelp <|> parseExpExit
            <|> parseExpEval <|> parseExpQEval

parseExpFen :: Parser ExpCommand
parseExpFen  = fmap Fen $ P.char 'f' >> P.spaces >> parseFenPosition

parseExpInit :: Parser ExpCommand
parseExpInit = P.char 'i' >> return Init

parseExpMoves :: Parser ExpCommand
parseExpMoves = P.char 'm' >> return Moves

parseExpQMoves :: Parser ExpCommand
parseExpQMoves = P.char 'q' >> return QMoves

parseExpDown :: Parser ExpCommand
parseExpDown = fmap Down $ P.char 'd' >> parseMove

parseExpUp :: Parser ExpCommand
parseExpUp   = P.char 'u' >> return Up

parseExpEval :: Parser ExpCommand
parseExpEval = P.char 'e' >> return Eval

parseExpQEval :: Parser ExpCommand
parseExpQEval = P.char 'v' >> return QEval

parseExpHelp :: Parser ExpCommand
parseExpHelp = P.char 'h' >> return Help

parseExpExit :: Parser ExpCommand
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
