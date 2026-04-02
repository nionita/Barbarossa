module Main (main) where

import LogScan.Core (LineType(..), parseLineType, processLogContent)
import Test.HUnit

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

startFen :: String
startFen = "8/8/8/8/8/8/8/8 w - - 0 1"

fenW :: String
fenW = "8/8/8/8/8/8/8/8 w - - 0 1"
