-- Chess Engine Log Processor
-- Extracts training data from chess engine log files
-- Author: Generated for chess training data preparation

module Main (main) where

import Data.List (isPrefixOf)
import System.Console.GetOpt
import System.Environment (getArgs)

-- processLogFile inputFile outputFile trackCount lowLimit highLimit = do
data Options = Options {
        optInPath     :: FilePath,	-- input file
        optOutPath    :: FilePath,	-- output file (CSV)
        optTrackCount :: Int,    	-- number of scores to track for game result
        optLowLimit   :: Int,    	-- lower margin for unclear games
        optHighLimit  :: Int    	-- higher margin for unclear games
    }

defaultOptions :: Options
defaultOptions = Options {
        optInPath     = "",
        optOutPath    = "",
        optTrackCount = 5,
        optLowLimit   = 100,
        optHighLimit  = 450
    }

addInPath :: FilePath -> Options -> Options
addInPath fi opt = opt { optInPath = fi }

addOutPath :: FilePath -> Options -> Options
addOutPath fi opt = opt { optOutPath = fi }

setTrackCount :: String -> Options -> Options
setTrackCount ba opt = opt { optTrackCount = read ba }

setLowLimit :: String -> Options -> Options
setLowLimit ba opt = opt { optLowLimit = read ba }

setHighLimit :: String -> Options -> Options
setHighLimit ba opt = opt { optHighLimit = read ba }

options :: [OptDescr (Options -> Options)]
options = [
        Option "i" ["input"]  (ReqArg addInPath   "STRING") "Input file",
        Option "o" ["output"] (ReqArg addOutPath  "STRING") "Output file",
        Option "t" ["track"]  (ReqArg setTrackCount  "INT") "Track count for game result",
        Option "H" ["high"]   (ReqArg setHighLimit   "INT") "High limit for unclear games",
        Option "L" ["low"]    (ReqArg setLowLimit    "INT") "Low limit for unclear games"
    ]

theOptions :: IO (Options, [String])
theOptions = do
    args <- getArgs
    case getOpt Permute options args of
        (o, n, []) -> return (foldr ($) defaultOptions o, n)
        (_, _, es) -> ioError (userError (concat es ++ usageInfo header options))
    where header = "Usage: LogScan -i PATH -o PATH -t TRACK -L LOW -H HIGH"

-- DATA TYPES --

-- | Represents a chess position with its evaluation score and FEN notation
data Position = Position 
  { posScore :: Int    -- ^ Evaluation score from the engine
  , posFen :: String   -- ^ FEN (Forsyth-Edwards Notation) string
  } deriving (Show, Eq)

-- | Types of lines in the log file
data LineType 
  = OtherLine           -- ^ Lines we don't care about (Type 0)
  | PositionLine Position  -- ^ Lines containing position information (Type 1)
  | NewGameLine         -- ^ Lines marking the start of a new game (Type 2)
  deriving (Show, Eq)

-- | Possible game outcomes
data GameWinner 
  = WhiteWon      -- ^ White won the game
  | BlackWon      -- ^ Black won the game
  | DrawGame      -- ^ Game was a draw
  | UnclearGame   -- ^ Cannot determine the outcome
  deriving (Show, Eq)


-- PARSING FUNCTIONS --

-- | Parse a line from the log file to determine its type
-- Returns PositionLine with score and FEN if it's a Type 1 line,
-- NewGameLine if it's a Type 2 line, or OtherLine otherwise
parseLineType :: String -> LineType
parseLineType line
  | "[Info]: New game" `isPrefixOf` dropWhile (/= '[') line = NewGameLine
  | "[Info]: Origin" `isPrefixOf` dropWhile (/= '[') line = 
      case extractScoreAndFen line of
        Just (score, fen) -> PositionLine (Position score fen)
        Nothing -> OtherLine
  | otherwise = OtherLine

-- | Extract score and FEN from a Type 1 line
-- The line format is: <ms> [Info]: Origin |<fen1>|<draft>|<score>|<fen2>
-- Returns Just (score, fen2) if parsing succeeds, Nothing otherwise
extractScoreAndFen :: String -> Maybe (Int, String)
extractScoreAndFen line = 
  let parts = splitOnPipe line
  in if length parts >= 5
     then case reads (parts !! 3) of
            [(score, "")] -> Just (score, parts !! 4)
            _ -> Nothing
     else Nothing

-- | Split a string on pipe character '|'
-- Returns a list of substrings
splitOnPipe :: String -> [String]
splitOnPipe = go ""
  where
    go acc [] = [reverse acc]
    go acc ('|':rest) = reverse acc : go "" rest
    go acc (c:rest) = go (c:acc) rest

-- | Extract the side to move from a FEN string
-- The side to move is the second field in FEN notation
-- Returns 'w' for white, 'b' for black, or '?' if parsing fails
getSideToMove :: String -> Char
getSideToMove fen = 
  let fields = words fen
  in if length fields >= 2
     then head (fields !! 1)
     else '?'


-- GAME GROUPING FUNCTIONS --

-- | Split a list into chunks separated by elements matching a predicate
-- Arguments:
--   p: predicate to test elements
--   list: input list
-- Returns a list of sublists, split at elements matching the predicate
splitWhen :: (a -> Bool) -> [a] -> [[a]]
splitWhen p = go
  where
    go [] = []
    go xs = case break p xs of
              (chunk, []) -> [chunk]
              (chunk, _:rest) -> chunk : go rest

-- | Group positions by games
-- Games are separated by NewGameLine markers
-- Arguments:
--   lineTypes: list of parsed line types
-- Returns a list of games, where each game is a list of positions
groupByGames :: [LineType] -> [[Position]]
groupByGames lineTypes =
  let chunks = splitWhen (== NewGameLine) lineTypes
      games = map extractPositions chunks
  in filter (not . null) games
  where
    extractPositions :: [LineType] -> [Position]
    extractPositions = foldr go []
      where
        go (PositionLine pos) acc = pos : acc
        go _ acc = acc


-- GAME RESULT DETERMINATION --

-- | Determine the game result based on the last N positions
-- Uses a heuristic based on score magnitudes:
--   - All scores in [-lowLimit, lowLimit]: Draw
--   - All scores outside [-highLimit, highLimit]: Decided game
--   - Otherwise: Unclear
-- Arguments:
--   trackCount: how many positions to examine (typically 4-5)
--   lowLimit: maximum score magnitude for a draw (typically 100)
--   highLimit: minimum score magnitude for a decided game (typically 400)
--   positions: list of all positions in the game
-- Returns the game result (WhiteWon, BlackWon, DrawGame, or UnclearGame)
determineGameResult :: Int -> Int -> Int -> [Position] -> GameWinner
determineGameResult trackCount lowLimit highLimit positions =
  let lastN = take trackCount (reverse positions)
      scores = map posScore lastN
  in if null lastN
     then UnclearGame
     else if all (\s -> abs s <= lowLimit) scores
          then DrawGame
          else if all (\s -> abs s >= highLimit) scores
               then determineWinner (last lastN)
               else UnclearGame
  where
    -- Determine which side won based on the last position's score and side to move
    -- High positive score means the side to move is winning
    -- High negative score means the side to move is losing
    determineWinner :: Position -> GameWinner
    determineWinner pos =
      let score = posScore pos
          side = getSideToMove (posFen pos)
      in if score >= highLimit
         then if side == 'w' then WhiteWon else BlackWon
         else if score <= -highLimit
              then if side == 'w' then BlackWon else WhiteWon
              else UnclearGame


-- OUTPUT GENERATION --

-- | Convert a position to a CSV output line
-- Arguments:
--   pos: the position to convert
--   gameResult: the overall game result
-- Returns a CSV line with format: <fen>,<score>,<result>
-- where result is 0 (loss), 1 (draw), or 2 (win) from the side to move's perspective
positionToOutput :: Position -> GameWinner -> String
positionToOutput pos gameResult =
  posFen pos ++ "," ++ show (posScore pos) ++ "," ++ show resultCode
  where
    side = getSideToMove (posFen pos)
    resultCode :: Int
    resultCode = case (gameResult, side) of
      (DrawGame, _) -> 1      -- Draw is always 1
      (WhiteWon, 'w') -> 2    -- White to move, white won = win
      (WhiteWon, 'b') -> 0    -- Black to move, white won = loss
      (BlackWon, 'w') -> 0    -- White to move, black won = loss
      (BlackWon, 'b') -> 2    -- Black to move, black won = win
      _ -> 1                  -- Fallback for unclear or invalid cases

-- | Process a single game and generate output lines
-- Arguments:
--   trackCount: how many positions to track for the heuristic
--   lowLimit: low threshold for draw determination
--   highLimit: high threshold for decided game determination
--   positions: all positions in the game
-- Returns a list of CSV output lines (empty if game result is unclear)
processGame :: Int -> Int -> Int -> [Position] -> [String]
processGame trackCount lowLimit highLimit positions =
  let result = determineGameResult trackCount lowLimit highLimit positions
  in case result of
       UnclearGame -> []
       _ -> map (`positionToOutput` result) positions


-- MAIN PROCESSING FUNCTION --

-- | Main function to process a log file and generate training data
-- Arguments:
--   inputFile: path to the input log file
--   outputFile: path to the output CSV file
--   trackCount: how many last positions to examine (typically 4-5)
--   lowLimit: max score magnitude for draw (typically 100)
--   highLimit: min score magnitude for decided game (typically 400)
-- 
-- The function reads the input file, processes each line, groups positions
-- into games, determines game results, and writes CSV output
processLogFile :: FilePath -> FilePath -> Int -> Int -> Int -> IO ()
processLogFile inputFile outputFile trackCount lowLimit highLimit = do
  -- Read input file
  content <- readFile inputFile
  let linesOfFile = lines content
      
      -- Parse each line to determine its type
      lineTypes = map parseLineType linesOfFile
      
      -- Group positions by games
      games = groupByGames lineTypes
      
      -- Process each game and collect output lines
      outputLines = concatMap (processGame trackCount lowLimit highLimit) games
  
  -- Write output file
  writeFile outputFile (unlines outputLines)

main :: IO ()
main = do
    (opts, _) <- theOptions
    processLogFile (optInPath opts) (optOutPath opts)
                   (optTrackCount opts) (optLowLimit opts) (optHighLimit opts)

{-
This will:
- Read from "input.log"
- Track the last 5 positions for game result determination
- Consider scores in [-100, 100] as draws
- Consider scores outside [-400, 400] as decided games
- Write training data to "output.csv"
-}
