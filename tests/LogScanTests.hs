module Main (main) where

import Data.List (isInfixOf)
import Test.HUnit
import Tune.LogScan
    ( FindKind(..)
    , LogEvent(..)
    , LineType(..)
    , parseLineType
    , parseLogEvent
    , processLogContent
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
    , TestLabel "replay mode renders PGN from explicit moves" testReplayModePgn
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
                assertContains "PGN should contain result" "1-0" pgn

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

malformedDepthLine :: String
malformedDepthLine =
    "0 [Info]: Origin |" ++ fenW ++ "|bad-depth|200|" ++ fenW

originLine :: Int -> Int -> String -> String
originLine depth score finalFen =
    "0 [Info]: Origin |" ++ startFen ++ "|" ++ show depth ++ "|" ++ show score ++ "|" ++ finalFen

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
        , "9 [Info]: Origin |" ++ fenAfterE4E5 ++ "|15|30|" ++ fenAfterE4E5Nf3
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

fenW :: String
fenW = "8/8/8/8/8/8/8/8 w - - 0 1"
