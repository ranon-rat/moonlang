{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Lexical.Solvers.DoubleSlash (doubleSlashHandler) where

import Lexical.Definitions (Context(..))

doubleSlashHandler :: Context -> Context
doubleSlashHandler context = do
    let ctx = find [(
                context,
                moduleContent context !! moduleCursor context
            )]

    ctx {
        moduleCursor = moduleCursor ctx - 1
    }

    where
        find :: [(Context, Char)] -> Context
        find [(ctx, char)]
            | moduleCursor ctx >= length (moduleContent ctx) = ctx
            | char == '\n' = ctx
            | otherwise = find [(
                ctx { moduleCursor = moduleCursor ctx + 1 },
                moduleContent ctx !! (moduleCursor ctx + 1)
            )]
