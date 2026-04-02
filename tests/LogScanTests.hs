module Main (main) where

import Data.List (isInfixOf, isSuffixOf)
import Test.HUnit
import Tune.LogScan
    ( FindKind(..)
    , LogEvent(..)
    , LogFormat(..)
    , LineType(..)
    , NoEvalInfo(..)
    , parseLineType
    , parseLogEvent
    , processLogContent
    , processLogContentWithFormat
    , processReplayLogContent
    )

main :: IO ()
main = do
    testCounts <- runTestTT tests
    if failures testCounts > 0 || errors testCounts > 0
        then error "LogScan tests failed"
        else pure ()

tests :: Test
tests = TestList
    [ TestLabel "default min depth emits depth 6" testDefaultMinDepth
    , TestLabel "custom min depth filters rows" testCustomMinDepth
    , TestLabel "malformed depth is ignored" testMalformedDepth
    , TestLabel "game result still uses all positions" testResultUsesAllPositions
    , TestLabel "current fen line parses" testParseCurrentFen
    , TestLabel "no eval line parses" testParseNoEval
    , TestLabel "no eval tail suppresses attribution" testNoEvalTailIsUnclear
    , TestLabel "new mode ignores old-log continuity heuristic" testNewModeIgnoresOldContinuityHeuristic
    , TestLabel "same root tail still attributes winner in old mode" testSameRootTailStillAttributes
    , TestLabel "continuous old-log tail still attributes winner in old mode" testContinuousTailStillAttributes
    , TestLabel "discontinuous old-log tail is dropped in old mode" testDiscontinuousTailIsDropped
    , TestLabel "replay mode renders PGN from explicit moves" testReplayModePgn
    , TestLabel "replay mode infers terminal result without winner line" testReplayModeInfersTerminalResult
    , TestLabel "replay mode infers result from decisive final score" testReplayModeInfersScoreResult
    , TestLabel "replay mode keeps unknown result when final state is non-terminal" testReplayModeKeepsUnknownResult
    , TestLabel "replay mode wraps movetext by whole move units" testReplayModeWrapsMovetext
    , TestLabel "replay mode infers missing move from next position" testReplayModeInference
    , TestLabel "replay mode handles side local bestmove logs" testReplayModeBestmoveInference
    ]

testDefaultMinDepth :: Test
testDefaultMinDepth =
    TestCase $
        assertEqual "default min depth should keep depth 6 and drop depth 5"
            [fenW ++ ",450,2", fenW ++ ",460,2"]
            (lines (processLogContent 6 2 100 400 sampleWinningLog))

testCustomMinDepth :: Test
testCustomMinDepth =
    TestCase $
        assertEqual "min depth 8 should keep only depth 8+ rows"
            [fenW ++ ",460,2"]
            (lines (processLogContent 8 3 100 400 sampleWinningLog))

testMalformedDepth :: Test
testMalformedDepth =
    TestCase $
        assertEqual "invalid depth should be treated as OtherLine"
            OtherLine
            (parseLineType malformedDepthLine)

testResultUsesAllPositions :: Test
testResultUsesAllPositions =
    TestCase $
        assertEqual "winner should still be determined from all positions in the game"
            [fenW ++ ",-500,2"]
            (lines (processLogContent 6 2 100 400 mixedDepthWinnerLog))

testParseCurrentFen :: Test
testParseCurrentFen =
    TestCase $
        assertEqual "current fen event should be parsed"
            (CurrentFenEvent startFen)
            (parseLogEvent ("0 [Info]: Current fen: " ++ startFen))

testParseNoEval :: Test
testParseNoEval =
    TestCase $
        assertEqual "no eval event should be parsed"
            (NoEvalEvent (NoEvalInfo fenAfterE4E5 1))
            (parseLogEvent ("0 [Info]: NoEval |" ++ fenAfterE4E5 ++ "|1"))

testNoEvalTailIsUnclear :: Test
testNoEvalTailIsUnclear =
    TestCase $
        assertEqual "games with no-eval gaps near the end should be dropped"
            []
            (lines (processLogContent 6 2 100 400 noEvalTailLog))

testNewModeIgnoresOldContinuityHeuristic :: Test
testNewModeIgnoresOldContinuityHeuristic =
    TestCase $
        assertEqual "new mode should not reject discontinuous old-log tails"
            [fenAfterE4 ++ ",500,2", fenAfterE4E5Nf3 ++ ",520,2"]
            (lines (processLogContent 6 2 100 400 discontinuousTailLog))

testSameRootTailStillAttributes :: Test
testSameRootTailStillAttributes =
    TestCase $
        assertEqual "multiple depths for the same root should not be treated as discontinuous in old mode"
            [fenAfterE4 ++ ",500,2", fenAfterE4E5Nf3 ++ ",520,2"]
            (lines (processLogContentWithFormat OldLogFormat 6 2 100 400 sameRootTailLog))

testContinuousTailStillAttributes :: Test
testContinuousTailStillAttributes =
    TestCase $
        assertEqual "continuous visible tail should still allow attribution for old logs"
            [fenAfterE4 ++ ",500,2", fenAfterE4E5Nf3 ++ ",520,2"]
            (lines (processLogContentWithFormat OldLogFormat 6 2 100 400 continuousTailLog))

testDiscontinuousTailIsDropped :: Test
testDiscontinuousTailIsDropped =
    TestCase $
        assertEqual "discontinuous visible tail should be rejected for old logs"
            []
            (lines (processLogContentWithFormat OldLogFormat 6 2 100 400 discontinuousTailLog))

testReplayModePgn :: Test
testReplayModePgn =
    TestCase $
        case processReplayLogContent MatchBoth 6 fenAfterE4E5 replayLog of
            Left err ->
                assertFailure err
            Right pgn -> do
                assertContains "PGN should contain first white move" "1. e4 {score 20 depth 6 pv e2e4 e7e5" pgn
                assertContains "PGN should contain black move" "1... e5 {score -15 depth 6 pv e7e5 g1f3" pgn
                assertContains "PGN should contain final white move" "2. Nf3 {score 30 depth 6 pv g1f3 b8c6" pgn
                assertContains "PGN header should contain explicit result" "[Result \"1-0\"]" pgn
                assertContains "PGN should contain result" "1-0" pgn

testReplayModeInfersTerminalResult :: Test
testReplayModeInfersTerminalResult =
    TestCase $
        case processReplayLogContent MatchBoth 6 mateEndFen replayMateLogNoWinner of
            Left err ->
                assertFailure err
            Right pgn -> do
                assertContains "PGN header should contain inferred mate result" "[Result \"1-0\"]" pgn
                assertContains "PGN should end with inferred mate result" "1-0" pgn

testReplayModeInfersScoreResult :: Test
testReplayModeInfersScoreResult =
    TestCase $
        case processReplayLogContent MatchBoth 6 fenAfterE4E5 replayLogNoWinnerDecisive of
            Left err ->
                assertFailure err
            Right pgn -> do
                assertContains "PGN header should contain inferred score result" "[Result \"0-1\"]" pgn
                assertBool "PGN should end with inferred score result"
                    (case reverse (filter (not . null) (lines pgn)) of
                        lastLine:_ -> "0-1" `isSuffixOf` lastLine
                        [] -> False)

testReplayModeKeepsUnknownResult :: Test
testReplayModeKeepsUnknownResult =
    TestCase $
        case processReplayLogContent MatchBoth 6 fenAfterE4E5 replayLogNoWinnerUndecisive of
            Left err ->
                assertFailure err
            Right pgn -> do
                assertContains "PGN header should keep unknown result" "[Result \"*\"]" pgn
                assertBool "PGN should still end with unknown result"
                    (case reverse (filter (not . null) (lines pgn)) of
                        lastLine:_ -> "*" `isSuffixOf` lastLine
                        [] -> False)

testReplayModeWrapsMovetext :: Test
testReplayModeWrapsMovetext =
    TestCase $
        case processReplayLogContent MatchBoth 6 fenAfterE4E5 replayLog of
            Left err ->
                assertFailure err
            Right pgn -> do
                let movetextLines = getMovetextLines pgn
                assertBool "movetext should be wrapped to multiple lines" (length movetextLines > 1)
                assertBool "wrapped lines should keep comments balanced"
                    (all hasBalancedBraces movetextLines)
                assertContains "wrapped output should continue at the next move boundary" "}\n1... e5" pgn

testReplayModeInference :: Test
testReplayModeInference =
    TestCase $
        case processReplayLogContent MatchEval 6 fenAfterE4E5Nf3 replayLogWithMissingBlackMove of
            Left err ->
                assertFailure err
            Right pgn -> do
                assertContains "replay should infer the missing black move" "1... e5 {score -15 depth 6 pv e7e5 g1f3" pgn
                assertContains "replay should mark inferred moves" "move inferred" pgn
                assertContains "replay should still finish with the known result" "1-0" pgn

testReplayModeBestmoveInference :: Test
testReplayModeBestmoveInference =
    TestCase $
        case processReplayLogContent MatchEval 6 fenAfterE4E5 sparseReplayLog of
            Left err ->
                assertFailure err
            Right pgn -> do
                assertContains "replay should render logged bestmove" "1. e4 {score 20 depth 6 pv e2e4 e7e5" pgn
                assertContains "replay should infer the hidden reply" "1... e5 {move inferred}" pgn
                assertContains "replay should render the next logged move" "2. Nf3 {score 30 depth 15 pv g1f3 b8c6" pgn
                assertContains "replay should keep the result" "1-0" pgn

assertContains :: String -> String -> String -> Assertion
assertContains message needle haystack =
    assertBool message (needle `isInfixOf` haystack)

sampleWinningLog :: String
sampleWinningLog =
    unlines
        [ "0 [Info]: New game"
        , originLine 5 430 fenW
        , originLine 6 450 fenW
        , originLine 8 460 fenW
        ]

mixedDepthWinnerLog :: String
mixedDepthWinnerLog =
    unlines
        [ "0 [Info]: New game"
        , originLine 5 500 fenW
        , originLine 6 (-500) fenW
        ]

noEvalTailLog :: String
noEvalTailLog =
    unlines
        [ "0 [Info]: New game"
        , originLine 6 500 fenW
        , "0 [Info]: NoEval |" ++ startFen ++ "|1"
        , originLine 8 520 fenW
        ]

sameRootTailLog :: String
sameRootTailLog =
    unlines
        [ "0 [Info]: New game"
        , originLine 6 500 fenAfterE4
        , originLine 7 520 fenAfterE4E5Nf3
        ]

continuousTailLog :: String
continuousTailLog =
    unlines
        [ "0 [Info]: New game"
        , originLineFrom startFen 6 500 fenAfterE4
        , originLineFrom fenAfterE4E5 7 520 fenAfterE4E5Nf3
        ]

discontinuousTailLog :: String
discontinuousTailLog =
    unlines
        [ "0 [Info]: New game"
        , originLineFrom startFen 6 500 fenAfterE4
        , originLineFrom fenW 7 520 fenAfterE4E5Nf3
        ]

malformedDepthLine :: String
malformedDepthLine =
    "0 [Info]: Origin |" ++ fenW ++ "|bad-depth|200|" ++ fenW

originLine :: Int -> Int -> String -> String
originLine depth score finalFen =
    originLineFrom startFen depth score finalFen

originLineFrom :: String -> Int -> Int -> String -> String
originLineFrom origFen depth score finalFen =
    "0 [Info]: Origin |" ++ origFen ++ "|" ++ show depth ++ "|" ++ show score ++ "|" ++ finalFen

replayLog :: String
replayLog =
    unlines
        [ "0 [Info]: New game"
        , "1 [Warning]: Setup new game between Alpha and Beta"
        , "2 [Warning]: Color for Alpha: White"
        , "3 [Warning]: Color for Beta: Black"
        , "4 [Warning]: Starting position: " ++ startFen
        , "5 [Info]: Current fen: " ++ startFen
        , "6 [Info]: Origin |" ++ startFen ++ "|6|20|" ++ fenAfterE4E5
        , "7 [Info]: Draft 6 Score 20 path [e2e4,e7e5] ms 0 used 10"
        , "8 [Info]: Real move 1 from Alpha: e2e4"
        , "9 [Info]: Current fen: " ++ fenAfterE4
        , "10 [Info]: Origin |" ++ fenAfterE4 ++ "|6|-15|" ++ fenAfterE4E5Nf3
        , "11 [Info]: Draft 6 Score -15 path [e7e5,g1f3] ms 0 used 10"
        , "12 [Info]: Real move 2 from Beta: e7e5"
        , "13 [Info]: Current fen: " ++ fenAfterE4E5
        , "14 [Info]: Origin |" ++ fenAfterE4E5 ++ "|6|30|" ++ fenAfterE4E5Nf3
        , "15 [Info]: Draft 6 Score 30 path [g1f3,b8c6] ms 0 used 10"
        , "16 [Info]: Real move 3 from Alpha: g1f3"
        , "17 [Warning]: Mate (Alpha wins)"
        ]

replayLogWithMissingBlackMove :: String
replayLogWithMissingBlackMove =
    unlines
        [ "0 [Info]: New game"
        , "1 [Warning]: Setup new game between Alpha and Beta"
        , "2 [Warning]: Color for Alpha: White"
        , "3 [Warning]: Color for Beta: Black"
        , "4 [Warning]: Starting position: " ++ startFen
        , "5 [Info]: Current fen: " ++ startFen
        , "6 [Info]: Origin |" ++ startFen ++ "|6|20|" ++ fenAfterE4E5
        , "7 [Info]: Draft 6 Score 20 path [e2e4,e7e5] ms 0 used 10"
        , "8 [Info]: Real move 1 from Alpha: e2e4"
        , "9 [Info]: Current fen: " ++ fenAfterE4
        , "10 [Info]: Origin |" ++ fenAfterE4 ++ "|6|-15|" ++ fenAfterE4E5Nf3
        , "11 [Info]: Draft 6 Score -15 path [e7e5,g1f3] ms 0 used 10"
        , "12 [Info]: Current fen: " ++ fenAfterE4E5
        , "13 [Info]: Origin |" ++ fenAfterE4E5 ++ "|6|30|" ++ fenAfterE4E5Nf3
        , "14 [Info]: Draft 6 Score 30 path [g1f3,b8c6] ms 0 used 10"
        , "15 [Info]: Real move 3 from Alpha: g1f3"
        , "16 [Warning]: Mate (Alpha wins)"
        ]

replayLogNoWinnerDecisive :: String
replayLogNoWinnerDecisive =
    unlines
        [ "0 [Info]: New game"
        , "1 [Warning]: Setup new game between Alpha and Beta"
        , "2 [Warning]: Color for Alpha: White"
        , "3 [Warning]: Color for Beta: Black"
        , "4 [Warning]: Starting position: " ++ startFen
        , "5 [Info]: Current fen: " ++ startFen
        , "6 [Info]: Origin |" ++ startFen ++ "|6|20|" ++ fenAfterE4E5
        , "7 [Info]: Draft 6 Score 20 path [e2e4,e7e5] ms 0 used 10"
        , "8 [Info]: Real move 1 from Alpha: e2e4"
        , "9 [Info]: Current fen: " ++ fenAfterE4
        , "10 [Info]: Origin |" ++ fenAfterE4 ++ "|6|-15|" ++ fenAfterE4E5Nf3
        , "11 [Info]: Draft 6 Score -15 path [e7e5,g1f3] ms 0 used 10"
        , "12 [Info]: Real move 2 from Beta: e7e5"
        , "13 [Info]: Current fen: " ++ fenAfterE4E5
        , "14 [Info]: Origin |" ++ fenAfterE4E5 ++ "|6|-600|" ++ fenAfterE4E5Nf3
        , "15 [Info]: Draft 6 Score -600 path [g1f3,b8c6] ms 0 used 10"
        , "16 [Info]: Real move 3 from Alpha: g1f3"
        ]

replayLogNoWinnerUndecisive :: String
replayLogNoWinnerUndecisive =
    unlines
        [ "0 [Info]: New game"
        , "1 [Warning]: Setup new game between Alpha and Beta"
        , "2 [Warning]: Color for Alpha: White"
        , "3 [Warning]: Color for Beta: Black"
        , "4 [Warning]: Starting position: " ++ startFen
        , "5 [Info]: Current fen: " ++ startFen
        , "6 [Info]: Origin |" ++ startFen ++ "|6|20|" ++ fenAfterE4E5
        , "7 [Info]: Draft 6 Score 20 path [e2e4,e7e5] ms 0 used 10"
        , "8 [Info]: Real move 1 from Alpha: e2e4"
        , "9 [Info]: Current fen: " ++ fenAfterE4
        , "10 [Info]: Origin |" ++ fenAfterE4 ++ "|6|-15|" ++ fenAfterE4E5Nf3
        , "11 [Info]: Draft 6 Score -15 path [e7e5,g1f3] ms 0 used 10"
        , "12 [Info]: Real move 2 from Beta: e7e5"
        , "13 [Info]: Current fen: " ++ fenAfterE4E5
        , "14 [Info]: Origin |" ++ fenAfterE4E5 ++ "|6|30|" ++ fenAfterE4E5Nf3
        , "15 [Info]: Draft 6 Score 30 path [g1f3,b8c6] ms 0 used 10"
        , "16 [Info]: Real move 3 from Alpha: g1f3"
        ]

sparseReplayLog :: String
sparseReplayLog =
    unlines
        [ "0 [Info]: New game"
        , "1 [Warning]: Setup new game between Alpha and Beta"
        , "2 [Warning]: Color for Alpha: White"
        , "3 [Warning]: Color for Beta: Black"
        , "4 [Info]: searchTheTree starts draft 1"
        , "5 [Info]: Origin |" ++ startFen ++ "|6|20|" ++ fenAfterE4E5
        , "6 [Info]: Draft 6 Score 20 path [e2e4,e7e5] ms 0 used 10"
        , "7 [Output]: bestmove e2e4"
        , "8 [Info]: searchTheTree starts draft 1"
        , "9 [Info]: NoEval |" ++ fenAfterE4E5 ++ "|1"
        , "10 [Info]: Draft 15 Score 30 path [g1f3,b8c6] ms 0 used 10"
        , "11 [Output]: bestmove g1f3"
        , "12 [Warning]: Mate (Alpha wins)"
        ]

startFen :: String
startFen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR/ w KQkq - 0 1"

fenAfterE4 :: String
fenAfterE4 = "rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR/ b KQkq e3 0 1"

fenAfterE4E5 :: String
fenAfterE4E5 = "rnbqkbnr/pppp1ppp/8/4p3/4P3/8/PPPP1PPP/RNBQKBNR/ w KQkq e6 0 2"

fenAfterE4E5Nf3 :: String
fenAfterE4E5Nf3 = "rnbqkbnr/pppp1ppp/8/4p3/4P3/5N2/PPPP1PPP/RNBQKB1R/ b KQkq - 1 2"

mateStartFen :: String
mateStartFen = "7k/5Q2/6K1/8/8/8/8/8 w - - 0 1"

mateEndFen :: String
mateEndFen = "7k/6Q1/6K1/8/8/8/8/8 b - - 1 1"

replayMateLogNoWinner :: String
replayMateLogNoWinner =
    unlines
        [ "0 [Info]: New game"
        , "1 [Warning]: Setup new game between Alpha and Beta"
        , "2 [Warning]: Color for Alpha: White"
        , "3 [Warning]: Color for Beta: Black"
        , "4 [Warning]: Starting position: " ++ mateStartFen
        , "5 [Info]: Current fen: " ++ mateStartFen
        , "6 [Info]: Origin |" ++ mateStartFen ++ "|6|999|" ++ mateEndFen
        , "7 [Info]: Draft 6 Score 999 path [f7g7] ms 0 used 10"
        , "8 [Info]: Real move 1 from Alpha: f7g7"
        ]

fenW :: String
fenW = "8/8/8/8/8/8/8/8 w - - 0 1"

getMovetextLines :: String -> [String]
getMovetextLines pgn =
    case dropWhile (not . null) (lines pgn) of
        [] -> []
        (_:movetextLines) -> filter (not . null) movetextLines

hasBalancedBraces :: String -> Bool
hasBalancedBraces line =
    length (filter (== '{') line) == length (filter (== '}') line)
