module Lexer where

import Data.List
import Data.Maybe
import Data.Ord
import Data.Char
import Data.Maybe.HT

data Token = Token { name :: String, lexeme :: String }

type TokenPattern = String -> Maybe Token

type LexemePattern = String -> Maybe String

matchAgainst :: [TokenPattern] -> String -> Maybe (Token, String)
matchAgainst [] _ = Nothing
matchAgainst _ [] = Nothing
matchAgainst acceptors string = case mapMaybe ($ string) acceptors of
    [] -> error "Lexing failed."
    tokens -> let token = maximumBy (comparing (length . lexeme)) tokens
              in Just (token, string \\ lexeme token)

patterns :: [TokenPattern]
patterns = map (\(n, p) -> fmap (Token n) . p) $
    map (\keyword -> (keyword, makeLiteral keyword)) (words "class public static void main String extends return int boolean if else while true false this new System.out.println { } ( ) [ ] ; = && || < <= == != > >= + - * / % ! . ,")
    ++
    [ ("identifier", identifier)
    , ("integer", integer)
    , ("whitespace", whitespace)
    , ("comment line", commentLine)
    , ("comment block", commentBlock)
    ]

makeLiteral :: String -> LexemePattern
makeLiteral l = \string -> toMaybe (l `isPrefixOf` string) l

identifier :: LexemePattern
identifier string = let i = takeWhile (\c -> isAlphaNum c || c == '_') string in
    toMaybe (not $ null i || isDigit (head i)) i

whitespace :: LexemePattern
whitespace string = let w = takeWhile (`elem` " \n\t\f") string in
    toMaybe (not $ null w) w

integer :: LexemePattern
integer string = let i = takeWhile isDigit string in
    toMaybe (not $ null i) i

commentLine :: LexemePattern
commentLine string = toMaybe (isPrefixOf "//" string) $ takeWhile (/= '\n') string

commentBlock :: LexemePattern
commentBlock string = do
    rest <- stripPrefix "/*" string
    (c, _) <- find (isPrefixOf "*/" . snd) $ zip (inits rest) (tails rest)
    return ("/*" ++ c ++ "*/")
