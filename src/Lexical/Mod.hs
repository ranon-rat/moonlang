{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Lexical.Mod (lexicalAnalyzer) where

import Lexical.Definitions (Context(..), Token(..), TokenInfo (..), toToken)

import Lexical.Solvers.DoubleQuotes as Solver
import Lexical.Solvers.DoubleSlash as Solver
import Lexical.Solvers.Identifier as Solver
import Lexical.Solvers.SlashAndAsterisk as Solver
import Lexical.Solvers.Unknown as Solver

lexicalAnalyzer'' :: Context -> Char -> Context
lexicalAnalyzer'' ctx char
    | char `elem` [' ', '\t'] = ctx { moduleCursor = moduleCursor ctx + 1 }
    | char `elem` ['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9'] = Solver.identifierHandler ctx
    | char == '/' && moduleContent ctx !! (moduleCursor ctx + 1) == '*' = Solver.slashAndAsteriskHandler ctx
    | char == '/' && moduleContent ctx !! (moduleCursor ctx + 1) == '/' = Solver.doubleSlashHandler ctx
    | char == '"' = Solver.doubleQuotesHandler ctx
    | toToken [char] == Unknown = Solver.unknownHandler ctx
    | otherwise = ctx {
        moduleCursor = moduleCursor ctx,
        entries = entries ctx ++ [TokenInfo {
            typeOf = toToken [char],
            value = Nothing,
            absoluteFilePath = modulePath ctx,
            absoluteCursor = moduleCursor ctx
        }]
    }

lexicalAnalyzer' :: Context -> Char -> Context
lexicalAnalyzer' ctx char
    | moduleCursor ctx >= length (moduleContent ctx) = ctx
    | otherwise = do
        let ctx' = lexicalAnalyzer'' ctx char

        let ctx = ctx' { moduleCursor = moduleCursor ctx' + 1 }

        lexicalAnalyzer' ctx (moduleContent ctx !! moduleCursor ctx)

lexicalAnalyzer :: String -> String -> [TokenInfo]
lexicalAnalyzer modulePath moduleContent = do
    let ctx = Context
            {
                modulePath = modulePath,
                moduleContent = moduleContent,
                moduleCursor = 0,
                entries = []
            }

    entries (lexicalAnalyzer' ctx (moduleContent !! moduleCursor ctx))
