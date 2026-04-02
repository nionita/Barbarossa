{-# LANGUAGE PatternGuards #-}

module Tune.LogScan
    ( FindKind(..)
    , GameWinner(..)
    , LineType(..)
    , LogEvent(..)
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
    , processReplayLogContent
    , renderReplayPgn
    ) where

import Data.Char (isDigit)
import Data.List (find, intercalate, isPrefixOf, stripPrefix)
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

data FindKind
    = MatchOrig
    | MatchEval
    | MatchBoth
    deriving (Eq, Show)

data OriginInfo = OriginInfo
    { originOrigFen :: String
    , originDepth   :: Int
    , originScore   :: Int
    , originEvalFen :: String
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
    , turnOrigin        :: Maybe OriginInfo
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
    | OriginEvent OriginInfo
    | DraftEvent SearchSummary
    | WinnerEvent String String
    | DrawEvent String
    | AbortedEvent String
    deriving (Eq, Show)

data TurnBuilder = TurnBuilder
    { tbCurrentFen    :: Maybe String
    , tbPlayedMoveUci :: Maybe String
    , tbBestOrigin    :: Maybe OriginInfo
    , tbBestSearch    :: Maybe SearchSummary
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
        OriginEvent origin ->
            PositionLine Position
                { posDepth = originDepth origin
                , posScore = originScore origin
                , posFen = originEvalFen origin
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
           else if score <= negate highLimit
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
            (DrawGame, _)   -> 1
            (WhiteWon, 'w') -> 2
            (WhiteWon, 'b') -> 0
            (BlackWon, 'w') -> 0
            (BlackWon, 'b') -> 2
            _               -> 1

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
    , tbBestOrigin = Nothing
    , tbBestSearch = Nothing
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
        && tbBestOrigin turnBuilder == Nothing
        && tbBestSearch turnBuilder == Nothing
        && tbMatchedOrigin turnBuilder == Nothing

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
    case dropWhile ((== Nothing) . tbMatchedOrigin) turns of
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
        , turnOrigin = tbBestOrigin turnBuilder
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
    unlines (headerLines ++ ["", movetext])
  where
    (movetextTokens, finalResult) = renderMovetext game
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
    movetext = unwords movetextTokens ++ " " ++ finalResult

renderMovetext :: ReplayGame -> ([String], String)
renderMovetext game = go [] (replayTurns game)
  where
    go acc [] = (reverse acc, replayResultToken (replayResult game))
    go acc (turn:rest) =
        case turnMove turn of
            Just replayMove ->
                let acc1 = (renderMoveToken turn replayMove ++ renderTurnComment turn) : acc
                in case turnReplyMove turn of
                    Just replyMove ->
                        go ((renderReplyMoveToken turn replyMove ++ renderReplyComment turn) : acc1) rest
                    Nothing
                        | Just warning <- turnReplyWarning turn ->
                            let stopComment = "{stopped: " ++ sanitizeComment warning ++ "}"
                            in (reverse (stopComment : acc1), "*")
                        | otherwise ->
                            go acc1 rest
            Nothing
                | null rest ->
                    let endComment =
                            case turnMoveWarning turn of
                                Just warning -> Just ("{stopped: " ++ sanitizeComment warning ++ "}")
                                Nothing -> renderTerminalComment turn
                    in (reverse (maybe acc (:acc) endComment), replayResultToken (replayResult game))
                | otherwise ->
                    let stopComment = "{stopped: " ++ sanitizeComment (fromMaybe "missing move" (turnMoveWarning turn)) ++ "}"
                    in (reverse (stopComment : acc), "*")

renderMoveToken :: ReplayTurn -> ReplayMove -> String
renderMoveToken turn replayMove =
    show moveNumber ++ suffix ++ replayMoveSan replayMove
  where
    moveNumber = fenMoveNumber (turnCurrentFen turn)
    suffix
        | getSideToMove (turnCurrentFen turn) == 'w' = ". "
        | otherwise = "... "

renderReplyMoveToken :: ReplayTurn -> ReplayMove -> String
renderReplyMoveToken turn replayMove =
    show moveNumber ++ suffix ++ replayMoveSan replayMove
  where
    moveNumber = fenMoveNumber (turnCurrentFen turn)
    suffix
        | getSideToMove (turnCurrentFen turn) == 'w' = "... "
        | otherwise = ". "

renderTurnComment :: ReplayTurn -> String
renderTurnComment turn =
    case filter (not . null)
        [ renderSearchComment (turnSearch turn)
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

fenMoveNumber :: String -> Int
fenMoveNumber fen =
    case words fen of
        (_:_:_:_:_:fullmove:_) ->
            fromMaybe 1 (readMaybe fullmove)
        _ -> 1
