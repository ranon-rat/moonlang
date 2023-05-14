{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Lexical.Solvers.SlashAndAsterisk (slashAndAsteriskHandler) where

import Lexical.Definitions (Context(..))

slashAndAsteriskHandler :: Context -> Context
slashAndAsteriskHandler context = do
    find [(
            context,
            moduleContent context !! moduleCursor context
        )]

    where
        find :: [(Context, Char)] -> Context
        find [(ctx, char)]
            | moduleCursor ctx >= length (moduleContent ctx) = ctx
            | char == '*' && moduleContent ctx !! (moduleCursor ctx + 1) == '/' = ctx {
                moduleCursor = moduleCursor ctx + 1
            }
            | otherwise = find [(
                ctx { moduleCursor = moduleCursor ctx + 1 },
                moduleContent ctx !! (moduleCursor ctx + 1)
            )]
