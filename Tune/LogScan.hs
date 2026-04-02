{-# LANGUAGE PatternGuards #-}

module Tune.LogScan
    ( FindKind(..)
    , GameWinner(..)
    , LineType(..)
    , LogFormat(..)
    , LogEvent(..)
    , NoEvalInfo(..)
    , OriginInfo(..)
    , Position(..)
    , ReplayGame(..)
    , ReplayMove(..)
    , ReplayMoveSource(..)
    , ReplayResult(..)
    , ReplayTurn(..)
    , SearchSummary(..)
    , determineGameResult
    , findReplayGame
    , parseLineType
    , parseLogEvent
    , positionToOutput
    , processGame
    , processLogContent
    , processLogContentWithFormat
    , processReplayLogContent
    , renderReplayPgn
    ) where

import Data.Char (isDigit)
import Data.List (find, groupBy, intercalate, isPrefixOf, stripPrefix)
import Data.Maybe (fromMaybe, listToMaybe)
import Control.Applicative ((<|>))
import Text.Read (readMaybe)

import Moves.Core
    ( doFromToMove
    , genMoveCaptWL
    , genMoveCast
    , genMoveFCheck
    , genMoveNCapt
    , genMovePromo
    , inCheck
    , posFromFen
    , startFen
    )
import Moves.Notation (posToFen, toNiceNotation)
import Struct.Struct (Color(..), Move, MyPos, toString)

data Position = Position
    { posDepth :: Int
    , posScore :: Int
    , posFen   :: String
    , posOrigFen :: String
    } deriving (Eq, Show)

data LineType
    = OtherLine
    | PositionLine Position
    | NoEvalLine NoEvalInfo
    | NewGameLine
    deriving (Eq, Show)

data GameWinner
    = WhiteWon
    | BlackWon
    | DrawGame
    | UnclearGame
    deriving (Eq, Show)

data FindKind
    = MatchOrig
    | MatchEval
    | MatchBoth
    deriving (Eq, Show)

data LogFormat
    = NewLogFormat
    | OldLogFormat
    deriving (Eq, Show)

data OriginInfo = OriginInfo
    { originOrigFen :: String
    , originDepth   :: Int
    , originScore   :: Int
    , originEvalFen :: String
    } deriving (Eq, Show)

data NoEvalInfo = NoEvalInfo
    { noEvalFen   :: String
    , noEvalDepth :: Int
    } deriving (Eq, Show)

data SearchSummary = SearchSummary
    { summaryDraft :: Int
    , summaryScore :: Int
    , summaryPv    :: [String]
    } deriving (Eq, Show)

data ReplayMoveSource
    = ReplayExplicit
    | ReplayInferred
    deriving (Eq, Show)

data ReplayMove = ReplayMove
    { replayMoveUci    :: String
    , replayMoveSan    :: String
    , replayMoveSource :: ReplayMoveSource
    } deriving (Eq, Show)

data ReplayTurn = ReplayTurn
    { turnCurrentFen    :: String
    , turnMove          :: Maybe ReplayMove
    , turnMoveWarning   :: Maybe String
    , turnReplyMove     :: Maybe ReplayMove
    , turnReplyWarning  :: Maybe String
    , turnSearch        :: Maybe SearchSummary
    , turnNoEval        :: Maybe NoEvalInfo
    , turnOrigin        :: Maybe OriginInfo
    , turnMatchedNoEval :: Maybe NoEvalInfo
    , turnMatchedOrigin :: Maybe OriginInfo
    } deriving (Eq, Show)

data ReplayResult
    = ReplayWhiteWins String
    | ReplayBlackWins String
    | ReplayDraw String
    | ReplayAborted String
    | ReplayUnknown
    deriving (Eq, Show)

data ReplayGame = ReplayGame
    { replayWhite     :: String
    , replayBlack     :: String
    , replayStartFen  :: String
    , replayTurns     :: [ReplayTurn]
    , replayResult    :: ReplayResult
    , replayTargetFen :: String
    } deriving (Eq, Show)

data LogEvent
    = OtherEvent
    | NewGameEvent
    | SearchStartEvent
    | SetupPlayersEvent String String
    | ColorEvent String Color
    | StartingFenEvent String
    | CurrentFenEvent String
    | RealMoveEvent Int String String
    | BestMoveEvent String
    | NoEvalEvent NoEvalInfo
    | OriginEvent OriginInfo
    | DraftEvent SearchSummary
    | WinnerEvent String String
    | DrawEvent String
    | AbortedEvent String
    deriving (Eq, Show)

data TurnBuilder = TurnBuilder
    { tbCurrentFen    :: Maybe String
    , tbPlayedMoveUci :: Maybe String
    , tbBestNoEval    :: Maybe NoEvalInfo
    , tbBestOrigin    :: Maybe OriginInfo
    , tbBestSearch    :: Maybe SearchSummary
    , tbMatchedNoEval :: Maybe NoEvalInfo
    , tbMatchedOrigin :: Maybe OriginInfo
    } deriving (Eq, Show)

data GameBuilder = GameBuilder
    { gbPlayer1    :: Maybe String
    , gbPlayer2    :: Maybe String
    , gbWhiteName  :: Maybe String
    , gbBlackName  :: Maybe String
    , gbStartFen   :: Maybe String
    , gbTurnsRev   :: [TurnBuilder]
    , gbPending    :: Maybe TurnBuilder
    , gbWinnerName :: Maybe (String, String)
    , gbDrawReason :: Maybe String
    , gbAbortReason :: Maybe String
    , gbMatched    :: Bool
    } deriving (Eq, Show)

emptyGameBuilder :: GameBuilder
emptyGameBuilder = GameBuilder
    { gbPlayer1 = Nothing
    , gbPlayer2 = Nothing
    , gbWhiteName = Nothing
    , gbBlackName = Nothing
    , gbStartFen = Nothing
    , gbTurnsRev = []
    , gbPending = Nothing
    , gbWinnerName = Nothing
    , gbDrawReason = Nothing
    , gbAbortReason = Nothing
    , gbMatched = False
    }

parseLineType :: String -> LineType
parseLineType line =
    case parseLogEvent line of
        NewGameEvent -> NewGameLine
        NoEvalEvent noEval -> NoEvalLine noEval
        OriginEvent origin ->
            PositionLine Position
                { posDepth = originDepth origin
                , posScore = originScore origin
                , posFen = originEvalFen origin
                , posOrigFen = originOrigFen origin
                }
        _ -> OtherLine

parseLogEvent :: String -> LogEvent
parseLogEvent line =
    case dropWhile (/= '[') (normalizeLogLine line) of
        msg
            | "[Info]: New game" `isPrefixOf` msg ->
                NewGameEvent
            | Just draftNo <- stripPrefix "[Info]: searchTheTree starts draft " msg
            , Just draftNoInt <- (readMaybe draftNo :: Maybe Int)
            , draftNoInt == 1 ->
                SearchStartEvent
            | Just rest <- stripPrefix "[Warning]: Setup new game between " msg ->
                parseSetupPlayers rest
            | Just rest <- stripPrefix "[Warning]: Color for " msg ->
                parseColorLine rest
            | Just rest <- stripPrefix "[Warning]: Starting position: " msg ->
                StartingFenEvent rest
            | Just rest <- stripPrefix "[Info]: Current fen: " msg ->
                CurrentFenEvent rest
            | Just rest <- stripPrefix "[Info]: Real move " msg ->
                parseRealMove rest
            | Just moveUci <- stripPrefix "[Output]: bestmove " msg ->
                BestMoveEvent moveUci
            | Just noEval <- parseNoEvalLine msg ->
                NoEvalEvent noEval
            | Just origin <- parseOriginLine msg ->
                OriginEvent origin
            | Just draft <- stripPrefix "[Info]: Draft " msg >>= parseDraftLine ->
                DraftEvent draft
            | Just draft <- stripPrefix "[Info]: seldepth " msg >>= parseSeldepthLine ->
                DraftEvent draft
            | Just rest <- stripPrefix "[Warning]: Mated (" msg ->
                parseWinner rest "Mate"
            | Just rest <- stripPrefix "[Warning]: Mate (" msg ->
                parseWinner rest "Mate"
            | Just rest <- stripPrefix "[Warning]: Resign (" msg ->
                parseWinner rest "Resignation"
            | Just rest <- stripPrefix "[Warning]: Remis: " msg ->
                DrawEvent rest
            | Just rest <- stripPrefix "[Error]: Aborted: " msg ->
                AbortedEvent rest
            | otherwise ->
                OtherEvent

normalizeLogLine :: String -> String
normalizeLogLine = reverse . dropWhile isIgnoredSuffix . reverse
  where
    isIgnoredSuffix c = c == '\r' || c == ' ' || c == '\t'

parseSetupPlayers :: String -> LogEvent
parseSetupPlayers rest =
    case splitOnString " and " rest of
        Just (p1, p2) -> SetupPlayersEvent p1 p2
        Nothing       -> OtherEvent

parseColorLine :: String -> LogEvent
parseColorLine rest =
    case splitOnString ": " rest of
        Just (name, colorStr) ->
            case colorStr of
                "White" -> ColorEvent name White
                "Black" -> ColorEvent name Black
                _       -> OtherEvent
        Nothing -> OtherEvent

parseRealMove :: String -> LogEvent
parseRealMove rest =
    let (digits, rest1) = span isDigit rest
    in case (readMaybe digits, stripPrefix " from " rest1) of
        (Just plyNo, Just rest2) ->
            case splitOnString ": " rest2 of
                Just (playerName, moveUci) -> RealMoveEvent plyNo playerName moveUci
                Nothing                    -> OtherEvent
        _ -> OtherEvent

parseOriginLine :: String -> Maybe OriginInfo
parseOriginLine msg = do
    rest <- stripPrefix "[Info]: Origin " msg
    let parts = splitOnPipe rest
    if length parts < 5
       then Nothing
       else do
           depth <- readMaybe (parts !! 2)
           score <- readMaybe (parts !! 3)
           pure OriginInfo
               { originOrigFen = parts !! 1
               , originDepth = depth
               , originScore = score
               , originEvalFen = parts !! 4
               }

parseNoEvalLine :: String -> Maybe NoEvalInfo
parseNoEvalLine msg = do
    rest <- stripPrefix "[Info]: NoEval " msg
    let parts = splitOnPipe rest
    if length parts < 3
       then Nothing
       else do
           depth <- readMaybe (parts !! 2)
           pure NoEvalInfo
               { noEvalFen = parts !! 1
               , noEvalDepth = depth
               }

parseDraftLine :: String -> Maybe SearchSummary
parseDraftLine rest =
    case words rest of
        draftStr : "Score" : scoreStr : "path" : pvToken : _
            | Just draft <- readMaybe draftStr
            , Just score <- readMaybe scoreStr ->
                Just SearchSummary
                    { summaryDraft = draft
                    , summaryScore = score
                    , summaryPv = parsePathToken pvToken
                    }
        _ -> Nothing

parseSeldepthLine :: String -> Maybe SearchSummary
parseSeldepthLine rest =
    case words rest of
        selDepthStr : "score" : scoreStr : "path" : pvToken : _
            | Just depth <- readMaybe selDepthStr
            , Just score <- readMaybe scoreStr ->
                Just SearchSummary
                    { summaryDraft = depth
                    , summaryScore = score
                    , summaryPv = parsePathToken pvToken
                    }
        _ -> Nothing

parseWinner :: String -> String -> LogEvent
parseWinner rest reason =
    case stripSuffix " wins)" rest of
        Just winnerName -> WinnerEvent winnerName reason
        Nothing         -> OtherEvent

splitOnPipe :: String -> [String]
splitOnPipe = go ""
  where
    go acc [] = [reverse acc]
    go acc ('|':rest) = reverse acc : go "" rest
    go acc (c:rest) = go (c:acc) rest

splitOnString :: String -> String -> Maybe (String, String)
splitOnString needle haystack = go "" haystack
  where
    go _ [] = Nothing
    go acc (c:rest)
        | needle `isPrefixOf` (c:rest) = Just (reverse acc, drop (length needle) (c:rest))
        | otherwise = go (c:acc) rest

stripSuffix :: String -> String -> Maybe String
stripSuffix suf xs
    | suf `isSuffixOf` xs = Just (take (length xs - length suf) xs)
    | otherwise = Nothing
  where
    isSuffixOf ys zs = reverse ys `isPrefixOf` reverse zs

parsePathToken :: String -> [String]
parsePathToken token =
    case stripPrefix "[" token >>= stripSuffix "]" of
        Just ""   -> []
        Just body -> splitOnComma body
        Nothing   -> []

splitOnComma :: String -> [String]
splitOnComma = go ""
  where
    go acc [] = [reverse acc]
    go acc (',':rest) = reverse acc : go "" rest
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
            (chunk, [])     -> [chunk]
            (chunk, _:rest) -> chunk : go rest

groupByGames :: [LineType] -> [[LineType]]
groupByGames lineTypes =
    filter hasPositionChunk (splitWhen (== NewGameLine) lineTypes)
  where
    hasPositionChunk = any isPositionLine
    isPositionLine (PositionLine _) = True
    isPositionLine _                = False

determineGameResult :: Int -> Int -> Int -> [LineType] -> GameWinner
determineGameResult = determineGameResultWithFormat NewLogFormat

determineGameResultWithFormat :: LogFormat -> Int -> Int -> Int -> [LineType] -> GameWinner
determineGameResultWithFormat logFormat trackCount lowLimit highLimit lineTypes =
    let indexedPositions =
            [ (idx, pos)
            | (idx, PositionLine pos) <- zip [0 :: Int ..] lineTypes
            ]
        lastN = take trackCount (reverse indexedPositions)
        scores = map (posScore . snd) lastN
        tailPositions = map snd (reverse lastN)
        tailStart = minimumMaybe (map fst lastN)
        hasNoEvalInTail =
            case tailStart of
                Nothing -> False
                Just idx0 -> any isNoEvalLine (drop idx0 lineTypes)
        hasContinuousTail =
            case logFormat of
                NewLogFormat -> True
                OldLogFormat -> visibleTailIsContinuous tailPositions
    in if null lastN
       then UnclearGame
       else if hasNoEvalInTail
            then UnclearGame
       else if all (\s -> abs s <= lowLimit) scores
            then DrawGame
       else if all (\s -> abs s >= highLimit) scores
                 then if hasContinuousTail
                         then determineWinner (snd (last lastN))
                         else UnclearGame
                 else UnclearGame
  where
    determineWinner :: Position -> GameWinner
    determineWinner pos =
        let score = posScore pos
            side = getSideToMove (posFen pos)
        in if score >= highLimit
           then if side == 'w' then WhiteWon else BlackWon
           else if score <= negate highLimit
                then if side == 'w' then BlackWon else WhiteWon
                else UnclearGame

    isNoEvalLine :: LineType -> Bool
    isNoEvalLine (NoEvalLine _) = True
    isNoEvalLine _              = False

minimumMaybe :: [Int] -> Maybe Int
minimumMaybe [] = Nothing
minimumMaybe xs = Just (minimum xs)

visibleTailIsContinuous :: [Position] -> Bool
visibleTailIsContinuous positions =
    all positionsContinuous (zip rootTail (drop 1 rootTail))
  where
    rootTail = map last (groupBy sameRoot positions)
    sameRoot pos1 pos2 = sameReplayFen (posOrigFen pos1) (posOrigFen pos2)

positionsContinuous :: (Position, Position) -> Bool
positionsContinuous (pos1, pos2) =
    let rootFen1 = posOrigFen pos1
        rootFen2 = posOrigFen pos2
    in sameReplayFen rootFen1 rootFen2
        || rootsReachableInOneOrTwoPlies rootFen1 rootFen2

rootsReachableInOneOrTwoPlies :: String -> String -> Bool
rootsReachableInOneOrTwoPlies rootFen1 rootFen2 =
    not (null (matchingMoves rootFen1 rootFen2))
        || any replyReachesNextRoot firstPlyPositions
  where
    firstPlyPositions =
        [ posToFen (doFromToMove move (posFromFen rootFen1))
        | move <- legalMovesForPosition (posFromFen rootFen1)
        ]
    replyReachesNextRoot currentFen =
        not (null (matchingMoves currentFen rootFen2))

positionToOutput :: Position -> GameWinner -> String
positionToOutput pos gameResult =
    posFen pos ++ "," ++ show (posScore pos) ++ "," ++ show resultCode
  where
    side = getSideToMove (posFen pos)
    resultCode :: Int
    resultCode =
        case (gameResult, side) of
            (DrawGame, _)   -> 1
            (WhiteWon, 'w') -> 2
            (WhiteWon, 'b') -> 0
            (BlackWon, 'w') -> 0
            (BlackWon, 'b') -> 2
            _               -> 1

processGame :: Int -> Int -> Int -> Int -> [LineType] -> [String]
processGame = processGameWithFormat NewLogFormat

processGameWithFormat :: LogFormat -> Int -> Int -> Int -> Int -> [LineType] -> [String]
processGameWithFormat logFormat minDepth trackCount lowLimit highLimit lineTypes =
    let positions =
            [ pos
            | PositionLine pos <- lineTypes
            ]
        result = determineGameResultWithFormat logFormat trackCount lowLimit highLimit lineTypes
    in case result of
        UnclearGame -> []
        _ ->
            map (`positionToOutput` result) $
                filter (\pos -> posDepth pos >= minDepth) positions

processLogContent :: Int -> Int -> Int -> Int -> String -> String
processLogContent = processLogContentWithFormat NewLogFormat

processLogContentWithFormat :: LogFormat -> Int -> Int -> Int -> Int -> String -> String
processLogContentWithFormat logFormat minDepth trackCount lowLimit highLimit content =
    let lineTypes = map parseLineType (lines content)
        games = groupByGames lineTypes
        outputLines = concatMap (processGameWithFormat logFormat minDepth trackCount lowLimit highLimit) games
    in unlines outputLines

processReplayLogContent :: FindKind -> Int -> String -> String -> Either String String
processReplayLogContent findKind minDepth targetFen content =
    case findReplayGame findKind minDepth targetFen content of
        Nothing   -> Left "Target FEN not found in any game"
        Just game -> Right (renderReplayPgn game)

findReplayGame :: FindKind -> Int -> String -> String -> Maybe ReplayGame
findReplayGame findKind minDepth targetFen content =
    go (map parseLogEvent (lines content)) emptyGameBuilder
  where
    go [] builder
        | gbMatched builder = Just (finalizeReplayGame targetFen builder)
        | otherwise = Nothing
    go (event:rest) builder =
        case event of
            NewGameEvent
                | gbMatched builder ->
                    Just (finalizeReplayGame targetFen builder)
                | otherwise ->
                    go rest emptyGameBuilder
            _ ->
                go rest (updateGameBuilder findKind minDepth targetFen event builder)

updateGameBuilder :: FindKind -> Int -> String -> LogEvent -> GameBuilder -> GameBuilder
updateGameBuilder findKind minDepth targetFen event builder =
    case event of
        OtherEvent ->
            builder
        SearchStartEvent ->
            startReplayTurn builder
        SetupPlayersEvent p1 p2 ->
            builder { gbPlayer1 = Just p1, gbPlayer2 = Just p2 }
        ColorEvent name White ->
            builder { gbWhiteName = Just name }
        ColorEvent name Black ->
            builder { gbBlackName = Just name }
        StartingFenEvent fen ->
            builder { gbStartFen = Just fen }
        CurrentFenEvent fen ->
            startTurnWithFen builder fen
        RealMoveEvent _ _ moveUci ->
            let builder1 = ensurePending builder
            in builder1 { gbPending = fmap (\tb -> tb { tbPlayedMoveUci = Just moveUci }) (gbPending builder1) }
        BestMoveEvent moveUci ->
            let builder1 = ensurePending builder
            in builder1 { gbPending = fmap (\tb -> tb { tbPlayedMoveUci = Just moveUci }) (gbPending builder1) }
        NoEvalEvent noEval ->
            updateNoEval findKind minDepth targetFen noEval builder
        OriginEvent origin ->
            updateOrigin findKind minDepth targetFen origin builder
        DraftEvent draft ->
            let builder1 = ensurePending builder
            in builder1 { gbPending = fmap (attachDraft draft) (gbPending builder1) }
        WinnerEvent name reason ->
            builder { gbWinnerName = Just (name, reason) }
        DrawEvent reason ->
            builder { gbDrawReason = Just reason }
        AbortedEvent reason ->
            builder { gbAbortReason = Just reason }
        NewGameEvent ->
            builder

newTurnBuilder :: Maybe String -> TurnBuilder
newTurnBuilder fen = TurnBuilder
    { tbCurrentFen = fen
    , tbPlayedMoveUci = Nothing
    , tbBestNoEval = Nothing
    , tbBestOrigin = Nothing
    , tbBestSearch = Nothing
    , tbMatchedNoEval = Nothing
    , tbMatchedOrigin = Nothing
    }

pushPending :: GameBuilder -> GameBuilder
pushPending builder =
    case gbPending builder of
        Just pending
            | turnBuilderEmpty pending ->
                builder { gbPending = Nothing }
            | otherwise ->
                builder { gbTurnsRev = pending : gbTurnsRev builder, gbPending = Nothing }
        Nothing      -> builder

startReplayTurn :: GameBuilder -> GameBuilder
startReplayTurn builder =
    let builder1 = pushPending builder
    in builder1 { gbPending = Just (newTurnBuilder Nothing) }

ensurePending :: GameBuilder -> GameBuilder
ensurePending builder =
    case gbPending builder of
        Just _ -> builder
        Nothing -> builder { gbPending = Just (newTurnBuilder Nothing) }

startTurnWithFen :: GameBuilder -> String -> GameBuilder
startTurnWithFen builder fen =
    case gbPending builder of
        Just pending
            | turnBuilderEmpty pending ->
                builder { gbPending = Just (pending { tbCurrentFen = Just fen }) }
        _ ->
            let builder1 = pushPending builder
            in builder1 { gbPending = Just (newTurnBuilder (Just fen)) }

turnBuilderEmpty :: TurnBuilder -> Bool
turnBuilderEmpty turnBuilder =
    tbCurrentFen turnBuilder == Nothing
        && tbPlayedMoveUci turnBuilder == Nothing
        && tbBestNoEval turnBuilder == Nothing
        && tbBestOrigin turnBuilder == Nothing
        && tbBestSearch turnBuilder == Nothing
        && tbMatchedNoEval turnBuilder == Nothing
        && tbMatchedOrigin turnBuilder == Nothing

updateNoEval :: FindKind -> Int -> String -> NoEvalInfo -> GameBuilder -> GameBuilder
updateNoEval findKind minDepth targetFen noEval builder =
    let builder1 = ensurePendingFromNoEval builder noEval
        matchedHere = noEvalMatchesTarget findKind minDepth targetFen noEval
        builder2
            | matchedHere && not (gbMatched builder1) =
                builder1
                    { gbTurnsRev = []
                    , gbPending = Just (newTurnBuilder (Just (noEvalFen noEval)))
                    }
            | otherwise =
                builder1
        pending1 = fmap (attachNoEval noEval) (gbPending builder2)
        pending2 =
            if matchedHere
               then fmap (\tb -> tb { tbMatchedNoEval = tbMatchedNoEval tb <|> Just noEval }) pending1
               else pending1
    in builder2
        { gbPending = pending2
        , gbMatched = gbMatched builder2 || matchedHere
        }

ensurePendingFromNoEval :: GameBuilder -> NoEvalInfo -> GameBuilder
ensurePendingFromNoEval builder noEval =
    case gbPending builder of
        Nothing ->
            builder { gbPending = Just (attachNoEval noEval (newTurnBuilder (Just (noEvalFen noEval)))) }
        Just pending
            | tbCurrentFen pending == Nothing || tbCurrentFen pending == Just (noEvalFen noEval) ->
                builder
            | otherwise ->
                let builder' = pushPending builder
                in builder' { gbPending = Just (attachNoEval noEval (newTurnBuilder (Just (noEvalFen noEval)))) }

attachNoEval :: NoEvalInfo -> TurnBuilder -> TurnBuilder
attachNoEval noEval turnBuilder =
    turnBuilder
        { tbCurrentFen = tbCurrentFen turnBuilder <|> Just (noEvalFen noEval)
        , tbBestNoEval = betterNoEval (tbBestNoEval turnBuilder) noEval
        }

updateOrigin :: FindKind -> Int -> String -> OriginInfo -> GameBuilder -> GameBuilder
updateOrigin findKind minDepth targetFen origin builder =
    let builder1 = ensurePendingFromOrigin builder origin
        matchedHere = originMatchesTarget findKind minDepth targetFen origin
        builder2
            | matchedHere && not (gbMatched builder1) =
                builder1
                    { gbTurnsRev = []
                    , gbPending = Just (newTurnBuilder (Just (originOrigFen origin)))
                    }
            | otherwise =
                builder1
        pending1 = fmap (attachOrigin origin) (gbPending builder2)
        pending2 =
            if matchedHere
               then fmap (\tb -> tb { tbMatchedOrigin = tbMatchedOrigin tb <|> Just origin }) pending1
               else pending1
    in builder2
        { gbPending = pending2
        , gbMatched = gbMatched builder2 || matchedHere
        }

ensurePendingFromOrigin :: GameBuilder -> OriginInfo -> GameBuilder
ensurePendingFromOrigin builder origin =
    case gbPending builder of
        Nothing ->
            builder { gbPending = Just (attachOrigin origin (newTurnBuilder (Just (originOrigFen origin)))) }
        Just pending
            | tbCurrentFen pending == Nothing || tbCurrentFen pending == Just (originOrigFen origin) ->
                builder
            | otherwise ->
                let builder' = pushPending builder
                in builder' { gbPending = Just (attachOrigin origin (newTurnBuilder (Just (originOrigFen origin)))) }

attachOrigin :: OriginInfo -> TurnBuilder -> TurnBuilder
attachOrigin origin turnBuilder =
    turnBuilder
        { tbCurrentFen = tbCurrentFen turnBuilder <|> Just (originOrigFen origin)
        , tbBestOrigin = betterOrigin (tbBestOrigin turnBuilder) origin
        }

attachDraft :: SearchSummary -> TurnBuilder -> TurnBuilder
attachDraft summary turnBuilder =
    turnBuilder { tbBestSearch = betterSummary (tbBestSearch turnBuilder) summary }

betterNoEval :: Maybe NoEvalInfo -> NoEvalInfo -> Maybe NoEvalInfo
betterNoEval Nothing noEval = Just noEval
betterNoEval (Just oldNoEval) noEval
    | noEvalDepth noEval >= noEvalDepth oldNoEval = Just noEval
    | otherwise = Just oldNoEval

betterOrigin :: Maybe OriginInfo -> OriginInfo -> Maybe OriginInfo
betterOrigin Nothing origin = Just origin
betterOrigin (Just oldOrigin) origin
    | originDepth origin >= originDepth oldOrigin = Just origin
    | otherwise = Just oldOrigin

betterSummary :: Maybe SearchSummary -> SearchSummary -> Maybe SearchSummary
betterSummary Nothing summary = Just summary
betterSummary (Just oldSummary) summary
    | summaryDraft summary >= summaryDraft oldSummary = Just summary
    | otherwise = Just oldSummary

originMatchesTarget :: FindKind -> Int -> String -> OriginInfo -> Bool
originMatchesTarget findKind minDepth targetFen origin
    | originDepth origin < minDepth = False
    | otherwise =
        case findKind of
            MatchOrig -> originOrigFen origin == targetFen
            MatchEval -> originEvalFen origin == targetFen
            MatchBoth -> originOrigFen origin == targetFen || originEvalFen origin == targetFen

noEvalMatchesTarget :: FindKind -> Int -> String -> NoEvalInfo -> Bool
noEvalMatchesTarget findKind minDepth targetFen noEval
    | noEvalDepth noEval < minDepth = False
    | otherwise =
        case findKind of
            MatchOrig -> noEvalFen noEval == targetFen
            MatchEval -> False
            MatchBoth -> noEvalFen noEval == targetFen

finalizeReplayGame :: String -> GameBuilder -> ReplayGame
finalizeReplayGame targetFen builder =
    let turns0 = reverse (gbTurnsRev finalBuilder)
        turns = dropBeforeMatched turns0
        finalBuilder = pushPending builder
        startFen0 = fromMaybe (fromMaybe startFen (joinMaybe (tbCurrentFen <$> listToMaybe turns))) (gbStartFen finalBuilder)
        whiteName = fromMaybe (fromMaybe "White" (gbPlayer1 finalBuilder)) (gbWhiteName finalBuilder)
        blackName = fromMaybe (fromMaybe "Black" (gbPlayer2 finalBuilder)) (gbBlackName finalBuilder)
    in ReplayGame
        { replayWhite = whiteName
        , replayBlack = blackName
        , replayStartFen = startFen0
        , replayTurns = resolveTurns turns
        , replayResult = finalizeReplayResult finalBuilder
        , replayTargetFen = targetFen
        }

dropBeforeMatched :: [TurnBuilder] -> [TurnBuilder]
dropBeforeMatched turns =
    case dropWhile (\turn -> tbMatchedOrigin turn == Nothing && tbMatchedNoEval turn == Nothing) turns of
        [] -> turns
        matchedTurns -> matchedTurns

joinMaybe :: Maybe (Maybe a) -> Maybe a
joinMaybe maybeMaybe =
    case maybeMaybe of
        Just value -> value
        Nothing    -> Nothing

finalizeReplayResult :: GameBuilder -> ReplayResult
finalizeReplayResult builder =
    case gbAbortReason builder of
        Just reason -> ReplayAborted reason
        Nothing ->
            case gbDrawReason builder of
                Just reason -> ReplayDraw reason
                Nothing ->
                    case gbWinnerName builder of
                        Just (winnerName, reason)
                            | Just winnerName == gbWhiteName builder ->
                                ReplayWhiteWins reason
                            | Just winnerName == gbBlackName builder ->
                                ReplayBlackWins reason
                            | otherwise ->
                                ReplayUnknown
                        Nothing -> ReplayUnknown

resolveTurns :: [TurnBuilder] -> [ReplayTurn]
resolveTurns turns = zipWith3 resolveTurn turns nextFens isLastFlags
  where
    nextFens = map tbCurrentFen (drop 1 turns) ++ [Nothing]
    isLastFlags =
        case turns of
            [] -> []
            _  -> replicate (length turns - 1) False ++ [True]

resolveTurn :: TurnBuilder -> Maybe String -> Bool -> ReplayTurn
resolveTurn turnBuilder nextFen isLastTurn =
    let currentFen = fromMaybe startFen (tbCurrentFen turnBuilder)
        visibleResolution = resolveVisibleMove currentFen (tbPlayedMoveUci turnBuilder) nextFen
        postVisibleFen = replayMoveUci <$> visibleResolution >>= \moveUci -> applyMoveUci currentFen moveUci
        (replyResolution, replyWarning) = resolveReplyMove postVisibleFen nextFen isLastTurn
        moveWarning = resolveVisibleWarning (tbCurrentFen turnBuilder) (tbPlayedMoveUci turnBuilder) visibleResolution isLastTurn
    in ReplayTurn
        { turnCurrentFen = currentFen
        , turnMove = visibleResolution
        , turnMoveWarning = moveWarning
        , turnReplyMove = replyResolution
        , turnReplyWarning = replyWarning
        , turnSearch = tbBestSearch turnBuilder
        , turnNoEval = tbBestNoEval turnBuilder
        , turnOrigin = tbBestOrigin turnBuilder
        , turnMatchedNoEval = tbMatchedNoEval turnBuilder
        , turnMatchedOrigin = tbMatchedOrigin turnBuilder
        }

resolveVisibleMove :: String -> Maybe String -> Maybe String -> Maybe ReplayMove
resolveVisibleMove currentFen (Just moveUci) _ =
    resolveExplicitMove currentFen moveUci
resolveVisibleMove currentFen Nothing (Just nextFen)
    | sameReplayFen currentFen nextFen = Nothing
    | otherwise =
        case matchingMoves currentFen nextFen of
            [move] ->
                Just ReplayMove
                    { replayMoveUci = toString move
                    , replayMoveSan = toNiceNotation (posFromFen currentFen) move
                    , replayMoveSource = ReplayInferred
                    }
            _ -> Nothing
resolveVisibleMove _ Nothing Nothing = Nothing

resolveVisibleWarning
    :: Maybe String
    -> Maybe String
    -> Maybe ReplayMove
    -> Bool
    -> Maybe String
resolveVisibleWarning maybeCurrentFen maybeMoveUci resolvedMove isLastTurn =
    case (maybeCurrentFen, maybeMoveUci, resolvedMove, isLastTurn) of
        (_, _, Just _, _) -> Nothing
        (_, Nothing, _, True) -> Nothing
        (_, Nothing, _, False) -> Just "missing logged move"
        (Nothing, Just moveUci, _, _) -> Just ("missing current position for logged move " ++ moveUci)
        (Just currentFen, Just moveUci, Nothing, _) ->
            Just ("logged move " ++ moveUci ++ " is not legal in " ++ currentFen)

resolveReplyMove
    :: Maybe String
    -> Maybe String
    -> Bool
    -> (Maybe ReplayMove, Maybe String)
resolveReplyMove Nothing _ isLastTurn
    | isLastTurn = (Nothing, Nothing)
    | otherwise = (Nothing, Just "could not compute position after logged move")
resolveReplyMove _ Nothing _ = (Nothing, Nothing)
resolveReplyMove (Just postVisibleFen) (Just nextCurrentFen) _
    | sameReplayFen postVisibleFen nextCurrentFen = (Nothing, Nothing)
resolveReplyMove (Just postVisibleFen) (Just nextCurrentFen) _ =
    case matchingMoves postVisibleFen nextCurrentFen of
        [move] ->
            ( Just ReplayMove
                { replayMoveUci = toString move
                , replayMoveSan = toNiceNotation (posFromFen postVisibleFen) move
                , replayMoveSource = ReplayInferred
                }
            , Nothing
            )
        [] ->
            (Nothing, Just ("could not infer hidden reply from " ++ postVisibleFen ++ " to " ++ nextCurrentFen))
        moves ->
            (Nothing, Just ("ambiguous hidden reply from " ++ postVisibleFen ++ " to " ++ nextCurrentFen ++ " (" ++ show (length moves) ++ " legal moves)"))

resolveExplicitMove :: String -> String -> Maybe ReplayMove
resolveExplicitMove currentFen moveUci = do
    move <- findMoveByUci currentFen moveUci
    pure ReplayMove
        { replayMoveUci = moveUci
        , replayMoveSan = toNiceNotation (posFromFen currentFen) move
                , replayMoveSource = ReplayExplicit
                }

matchingMoves :: String -> String -> [Move]
matchingMoves currentFen nextFen =
    let pos = posFromFen currentFen
    in filter (\move -> sameReplayFen (posToFen (doFromToMove move pos)) nextFen) (legalMovesForPosition pos)

findMoveByUci :: String -> String -> Maybe Move
findMoveByUci currentFen moveUci =
    let pos = posFromFen currentFen
    in find (\move -> toString move == moveUci) (legalMovesForPosition pos)

applyMoveUci :: String -> String -> Maybe String
applyMoveUci currentFen moveUci = do
    move <- findMoveByUci currentFen moveUci
    pure (posToFen (doFromToMove move (posFromFen currentFen)))

sameReplayFen :: String -> String -> Bool
sameReplayFen fen1 fen2 = take 3 (words fen1) == take 3 (words fen2)

legalMovesForPosition :: MyPos -> [Move]
legalMovesForPosition pos
    | inCheck pos = genMoveFCheck pos
    | otherwise =
        genMoveCast pos
        ++ promoQueens
        ++ promoRest
        ++ capWins
        ++ capLosses
        ++ genMoveNCapt pos
  where
    (promoQueens, promoRest) = genMovePromo pos
    (capWins, capLosses) = genMoveCaptWL pos

renderReplayPgn :: ReplayGame -> String
renderReplayPgn game =
    unlines (headerLines ++ [""] ++ movetextLines)
  where
    replayOutcome = effectiveReplayResult game
    (movetextTokens, finalResult) = renderMovetext game replayOutcome
    headerLines =
        [ pgnTag "Event" "LogScan Debug"
        , pgnTag "Annotator" "LogScan"
        , pgnTag "White" (replayWhite game)
        , pgnTag "Black" (replayBlack game)
        , pgnTag "Result" finalResult
        , pgnTag "TargetFen" (replayTargetFen game)
        ]
        ++ setupTags
    setupTags
        | replayStartFen game == startFen = []
        | otherwise =
            [ pgnTag "SetUp" "1"
            , pgnTag "FEN" (replayStartFen game)
            ]
    movetextLines = wrapMovetextUnits 120 (movetextTokens ++ [finalResult])

effectiveReplayResult :: ReplayGame -> ReplayResult
effectiveReplayResult game =
    case replayResult game of
        ReplayUnknown ->
            case inferReplayResult game of
                ReplayUnknown -> inferReplayScoreResult game
                replayOutcome -> replayOutcome
        replayOutcome -> replayOutcome

inferReplayResult :: ReplayGame -> ReplayResult
inferReplayResult game =
    case finalReplayFen game of
        Nothing -> ReplayUnknown
        Just fen ->
            let pos = posFromFen fen
                side = getSideToMove fen
            in case legalMovesForPosition pos of
                [] | inCheck pos ->
                    if side == 'w'
                       then ReplayBlackWins "Inferred mate"
                       else ReplayWhiteWins "Inferred mate"
                [] ->
                    ReplayDraw "Inferred stalemate"
                _ ->
                    ReplayUnknown

inferReplayScoreResult :: ReplayGame -> ReplayResult
inferReplayScoreResult game =
    case listToMaybe (reverse decisiveTurns) of
        Just turn -> replayResultFromScore (turnCurrentFen turn) (turnScore turn)
        Nothing -> ReplayUnknown
  where
    decisiveTurns =
        filter (\turn -> abs (turnScore turn) >= replayScoreResultThreshold) (replayTurns game)

replayScoreResultThreshold :: Int
replayScoreResultThreshold = 450

turnScore :: ReplayTurn -> Int
turnScore turn =
    case turnSearch turn of
        Just summary -> summaryScore summary
        Nothing ->
            case turnOrigin turn of
                Just origin -> originScore origin
                Nothing ->
                    case turnMatchedOrigin turn of
                        Just origin -> originScore origin
                        Nothing -> 0

replayResultFromScore :: String -> Int -> ReplayResult
replayResultFromScore currentFen score =
    let side = getSideToMove currentFen
    in if score >= replayScoreResultThreshold
          then if side == 'w'
                  then ReplayWhiteWins "Inferred from score"
                  else ReplayBlackWins "Inferred from score"
       else if score <= negate replayScoreResultThreshold
               then if side == 'w'
                       then ReplayBlackWins "Inferred from score"
                       else ReplayWhiteWins "Inferred from score"
            else ReplayUnknown

finalReplayFen :: ReplayGame -> Maybe String
finalReplayFen game =
    case reverse (replayTurns game) of
        [] -> Just (replayStartFen game)
        turn:_ -> resolvedTurnFen turn

resolvedTurnFen :: ReplayTurn -> Maybe String
resolvedTurnFen turn =
    resolvedReplyFen turn <|> resolvedVisibleFen turn <|> Just (turnCurrentFen turn)

resolvedVisibleFen :: ReplayTurn -> Maybe String
resolvedVisibleFen turn = do
    replayMove <- turnMove turn
    applyMoveUci (turnCurrentFen turn) (replayMoveUci replayMove)

resolvedReplyFen :: ReplayTurn -> Maybe String
resolvedReplyFen turn = do
    visibleFen <- resolvedVisibleFen turn
    replyMove <- turnReplyMove turn
    applyMoveUci visibleFen (replayMoveUci replyMove)

renderMovetext :: ReplayGame -> ReplayResult -> ([String], String)
renderMovetext game replayOutcome = go 1 [] (replayTurns game)
  where
    go _ acc [] = (reverse acc, replayResultToken replayOutcome)
    go moveNumber acc (turn:rest) =
        case turnMove turn of
            Just replayMove ->
                let visibleSide = getSideToMove (turnCurrentFen turn)
                    visibleToken = renderMoveToken moveNumber visibleSide replayMove ++ renderTurnComment turn
                    nextMoveNumber = advanceMoveNumber moveNumber visibleSide
                    acc1 = visibleToken : acc
                in case turnReplyMove turn of
                    Just replyMove ->
                        let replySide = oppositeSide visibleSide
                            replyToken = renderMoveToken nextMoveNumber replySide replyMove ++ renderReplyComment turn
                            moveNumberAfterReply = advanceMoveNumber nextMoveNumber replySide
                        in go moveNumberAfterReply (replyToken : acc1) rest
                    Nothing
                        | Just warning <- turnReplyWarning turn ->
                            let stopComment = "{stopped: " ++ sanitizeComment warning ++ "}"
                            in (reverse (stopComment : acc1), "*")
                        | otherwise ->
                            go nextMoveNumber acc1 rest
            Nothing
                | null rest ->
                    let endComment =
                            case turnMoveWarning turn of
                                Just warning -> Just ("{stopped: " ++ sanitizeComment warning ++ "}")
                                Nothing -> renderTerminalComment turn
                    in (reverse (maybe acc (:acc) endComment), replayResultToken replayOutcome)
                | otherwise ->
                    let stopComment = "{stopped: " ++ sanitizeComment (fromMaybe "missing move" (turnMoveWarning turn)) ++ "}"
                    in (reverse (stopComment : acc), "*")

renderMoveToken :: Int -> Char -> ReplayMove -> String
renderMoveToken moveNumber side replayMove =
    show moveNumber ++ suffixForSide side ++ replayMoveSan replayMove
  where
    suffixForSide 'w' = ". "
    suffixForSide _ = "... "

advanceMoveNumber :: Int -> Char -> Int
advanceMoveNumber moveNumber side
    | side == 'b' = moveNumber + 1
    | otherwise = moveNumber

oppositeSide :: Char -> Char
oppositeSide 'w' = 'b'
oppositeSide 'b' = 'w'
oppositeSide side = side

renderTurnComment :: ReplayTurn -> String
renderTurnComment turn =
    case filter (not . null)
        [ renderSearchComment (turnSearch turn)
        , renderNoEvalComment (turnMatchedNoEval turn)
        , renderOriginComment (turnMatchedOrigin turn)
        , renderMoveWarning (turnMove turn) (turnMoveWarning turn)
        ] of
        [] -> ""
        parts -> " {" ++ sanitizeComment (intercalate "; " parts) ++ "}"

renderReplyComment :: ReplayTurn -> String
renderReplyComment turn =
    case filter (not . null)
        [ renderReplyWarning (turnReplyMove turn) (turnReplyWarning turn) ] of
        [] -> ""
        parts -> " {" ++ sanitizeComment (intercalate "; " parts) ++ "}"

renderSearchComment :: Maybe SearchSummary -> String
renderSearchComment Nothing = ""
renderSearchComment (Just summary) =
    "score " ++ show (summaryScore summary)
        ++ " depth " ++ show (summaryDraft summary)
        ++ pvPart
  where
    pvPart
        | null (summaryPv summary) = ""
        | otherwise = " pv " ++ unwords (summaryPv summary)

renderOriginComment :: Maybe OriginInfo -> String
renderOriginComment Nothing = ""
renderOriginComment (Just origin) =
    "matched depth " ++ show (originDepth origin)
        ++ " eval " ++ originEvalFen origin

renderNoEvalComment :: Maybe NoEvalInfo -> String
renderNoEvalComment Nothing = ""
renderNoEvalComment (Just noEval) =
    "matched no-eval depth " ++ show (noEvalDepth noEval)

renderMoveWarning :: Maybe ReplayMove -> Maybe String -> String
renderMoveWarning Nothing Nothing = ""
renderMoveWarning Nothing (Just warning) = warning
renderMoveWarning (Just replayMove) Nothing =
    case replayMoveSource replayMove of
        ReplayExplicit -> ""
        ReplayInferred -> "move inferred"
renderMoveWarning (Just replayMove) (Just warning) =
    let srcNote =
            case replayMoveSource replayMove of
                ReplayExplicit -> ""
                ReplayInferred -> "move inferred; "
    in srcNote ++ warning

renderReplyWarning :: Maybe ReplayMove -> Maybe String -> String
renderReplyWarning = renderMoveWarning

renderTerminalComment :: ReplayTurn -> Maybe String
renderTerminalComment turn =
    case filter (not . null)
        [ renderSearchComment (turnSearch turn)
        , renderNoEvalComment (turnMatchedNoEval turn)
        , renderOriginComment (turnMatchedOrigin turn)
        ] of
        [] -> Nothing
        parts -> Just ("{" ++ sanitizeComment (intercalate "; " parts) ++ "}")

replayResultToken :: ReplayResult -> String
replayResultToken replayOutcome =
    case replayOutcome of
        ReplayWhiteWins _ -> "1-0"
        ReplayBlackWins _ -> "0-1"
        ReplayDraw _      -> "1/2-1/2"
        ReplayAborted _   -> "*"
        ReplayUnknown     -> "*"

wrapMovetextUnits :: Int -> [String] -> [String]
wrapMovetextUnits maxWidth units = finalize currentLine completedLines
  where
    (currentLine, completedLines) = foldl packUnit ("", []) units

    packUnit :: (String, [String]) -> String -> (String, [String])
    packUnit ("", linesAcc) unit = (unit, linesAcc)
    packUnit (current, linesAcc) unit
        | length current + 1 + length unit <= maxWidth =
            (current ++ " " ++ unit, linesAcc)
        | otherwise =
            (unit, linesAcc ++ [current])

    finalize :: String -> [String] -> [String]
    finalize "" linesAcc = linesAcc
    finalize current linesAcc = linesAcc ++ [current]

pgnTag :: String -> String -> String
pgnTag key value = "[" ++ key ++ " \"" ++ escapeTagValue value ++ "\"]"

escapeTagValue :: String -> String
escapeTagValue = concatMap escapeChar
  where
    escapeChar '"' = "\\\""
    escapeChar '\\' = "\\\\"
    escapeChar c = [c]

sanitizeComment :: String -> String
sanitizeComment = map replaceBrace
  where
    replaceBrace '{' = '('
    replaceBrace '}' = ')'
    replaceBrace c = c
