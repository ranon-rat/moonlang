module Lib
    ( someFunc
    ) where
import Tokens
someFunc :: IO ()
someFunc = do
     program<-readFile "./commentsOnly.mla"
     print $  tokenize program "" 0 []