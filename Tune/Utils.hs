module Tune.Utils (
    justifyLeft, justifyRight
) where

justifyLeft, justifyRight :: Int -> Char -> String -> String
justifyLeft  n c s = s ++ replicate (n - length s) c
justifyRight n c s = replicate (n - length s) c ++ s
