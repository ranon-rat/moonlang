{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Lexical.Solvers.DoubleQuotes (doubleQuotesHandler) where

import Lexical.Definitions (Context(..), Token(..), TokenInfo (..))

doubleQuotesHandler :: Context -> Context
doubleQuotesHandler context = do
    let cursor = moduleCursor context
    let ctx = context { moduleCursor = moduleCursor context + 1 }

    let [(str, ctx')] = find [(
                ctx,
                moduleContent ctx !! moduleCursor ctx,
                "",
                False
            )]

    ctx' {
        entries = entries ctx' ++ [TokenInfo {
            typeOf = DoubleQuotes,
            value = Just str,
            absoluteFilePath = modulePath ctx',
            absoluteCursor = cursor
        }]
    }

    where
        find :: [(Context, Char, String, Bool)] -> [(String, Context)]
        find [(ctx, char, str, escape)]
            | moduleCursor ctx >= length (moduleContent ctx) = [(str, ctx)]
            | char == '\\' && not escape = find [(
                ctx { moduleCursor = moduleCursor ctx + 1},
                moduleContent ctx !! (moduleCursor ctx + 1),
                str,
                True
            )]
            | char == '"' && not escape = [(str, ctx)]
            | otherwise = find [(
                ctx { moduleCursor = moduleCursor ctx + 1},
                moduleContent ctx !! (moduleCursor ctx + 1),
                str ++ [char],
                False
            )]
