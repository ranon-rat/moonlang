module Lexical.Definitions (Context(..), Token(..), TokenInfo(..), toToken) where

data Token
    = Identifier

    | OpenBrace
    | CloseBrace

    | OpenParenthesis
    | CloseParenthesis

    | Comma
    | Point
    | DoubleDots
    | Semicolon

    | DoubleQuotes

    | Equals
    | Plus
    | Minus
    | Multiply
    | Divide
    | Mod

    | BiggerThan
    | LowerThan

    | Backslash
    | EOL

    | Unknown
    | None

    deriving (Eq, Show)

data TokenInfo = TokenInfo
    {
        typeOf :: Token,
        value :: Maybe String,
        absoluteFilePath :: String,
        absoluteCursor :: Int
    }
    deriving (Show)

toToken :: String -> Token
toToken x
  | x == "Az" = Identifier

  | x == "{" = OpenBrace
  | x == "}" = CloseBrace

  | x == "(" = OpenParenthesis
  | x == ")" = CloseParenthesis

  | x == "," = Comma
  | x == "." = Point
  | x == ":" = DoubleDots
  | x == ";" = Semicolon

  | x == "\"" = DoubleQuotes

  | x == "=" = Equals
  | x == "+" = Plus
  | x == "-" = Minus
  | x == "*" = Multiply
  | x == "/" = Divide
  | x == "%" = Mod

  | x == ">" = BiggerThan
  | x == "<" = LowerThan

  | x == "\\" = Backslash
  | x == "\n" = EOL

  | otherwise = Unknown

data Context = Context
    {
        modulePath :: String,
        moduleContent :: String,
        moduleCursor :: Int,
        entries :: [TokenInfo]
    }
    deriving (Show)
