module Lib (someFunc) where

import Lexical.Mod (lexicalAnalyzer)

someFunc :: IO ()
someFunc = do
    program <- readFile "./commentsOnly.mla"
    print $ lexicalAnalyzer "./commentsOnly.mla" (program ++ "\n")
