module LogScan.Core
    ( GameWinner(..)
    , LineType(..)
    , Position(..)
    , determineGameResult
    , parseLineType
    , positionToOutput
    , processGame
    , processLogContent
    ) where

import Data.List (isPrefixOf)

data Position = Position
    { posDepth :: Int
    , posScore :: Int
    , posFen   :: String
    } deriving (Eq, Show)

data LineType
    = OtherLine
    | PositionLine Position
    | NewGameLine
    deriving (Eq, Show)

data GameWinner
    = WhiteWon
    | BlackWon
    | DrawGame
    | UnclearGame
    deriving (Eq, Show)

parseLineType :: String -> LineType
parseLineType line
    | "[Info]: New game" `isPrefixOf` dropWhile (/= '[') line = NewGameLine
    | "[Info]: Origin" `isPrefixOf` dropWhile (/= '[') line =
        case extractPosition line of
            Just pos -> PositionLine pos
            Nothing  -> OtherLine
    | otherwise = OtherLine

extractPosition :: String -> Maybe Position
extractPosition line =
    let parts = splitOnPipe line
    in if length parts >= 5
       then case (reads (parts !! 2), reads (parts !! 3)) of
            ([(depth, "")], [(score, "")]) ->
                Just Position
                    { posDepth = depth
                    , posScore = score
                    , posFen = parts !! 4
                    }
            _ -> Nothing
       else Nothing

splitOnPipe :: String -> [String]
splitOnPipe = go ""
  where
    go acc [] = [reverse acc]
    go acc ('|':rest) = reverse acc : go "" rest
    go acc (c:rest) = go (c:acc) rest

getSideToMove :: String -> Char
getSideToMove fen =
    case words fen of
        (_:side:_) ->
            case side of
                (sideToMove:_) -> sideToMove
                []             -> '?'
        _ -> '?'

splitWhen :: (a -> Bool) -> [a] -> [[a]]
splitWhen p = go
  where
    go [] = []
    go xs =
        case break p xs of
            (chunk, [])       -> [chunk]
            (chunk, _:rest) -> chunk : go rest

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
        go _ acc                  = acc

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
    determineWinner :: Position -> GameWinner
    determineWinner pos =
        let score = posScore pos
            side = getSideToMove (posFen pos)
        in if score >= highLimit
           then if side == 'w' then WhiteWon else BlackWon
           else if score <= -highLimit
                then if side == 'w' then BlackWon else WhiteWon
                else UnclearGame

positionToOutput :: Position -> GameWinner -> String
positionToOutput pos gameResult =
    posFen pos ++ "," ++ show (posScore pos) ++ "," ++ show resultCode
  where
    side = getSideToMove (posFen pos)
    resultCode :: Int
    resultCode =
        case (gameResult, side) of
            (DrawGame, _)     -> 1
            (WhiteWon, 'w')   -> 2
            (WhiteWon, 'b')   -> 0
            (BlackWon, 'w')   -> 0
            (BlackWon, 'b')   -> 2
            _                 -> 1

processGame :: Int -> Int -> Int -> Int -> [Position] -> [String]
processGame minDepth trackCount lowLimit highLimit positions =
    let result = determineGameResult trackCount lowLimit highLimit positions
    in case result of
        UnclearGame -> []
        _ ->
            map (`positionToOutput` result) $
                filter (\pos -> posDepth pos >= minDepth) positions

processLogContent :: Int -> Int -> Int -> Int -> String -> String
processLogContent minDepth trackCount lowLimit highLimit content =
    let lineTypes = map parseLineType (lines content)
        games = groupByGames lineTypes
        outputLines = concatMap (processGame minDepth trackCount lowLimit highLimit) games
    in unlines outputLines
