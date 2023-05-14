{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Lexical.Solvers.Unknown (unknownHandler) where

import Lexical.Definitions (Context(..), Token(..), TokenInfo (..))

unknownHandler :: Context -> Context
unknownHandler context = do
    let cursor = moduleCursor context

    let [(str, ctx)] = find [(
                context,
                moduleContent context !! moduleCursor context,
                ""
            )]

    ctx {
        moduleCursor = moduleCursor ctx - 1,
        entries = entries ctx ++ [TokenInfo {
            typeOf = Unknown,
            value = Just str,
            absoluteFilePath = modulePath ctx,
            absoluteCursor = cursor
        }]
    }

    where
        find :: [(Context, Char, String)] -> [(String, Context)]
        find [(ctx, char, str)]
            | moduleCursor ctx >= length (moduleContent ctx) = [(str, ctx)]
            | char `elem` [' ', '\t', '\n'] = [(str, ctx)]
            | otherwise = find [(
                ctx { moduleCursor = moduleCursor ctx + 1 },
                moduleContent ctx !! (moduleCursor ctx + 1),
                str ++ [char]
            )]
