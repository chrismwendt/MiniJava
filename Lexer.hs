module Lexer where

import Data.List
import Data.Maybe
import Data.Ord
import Data.Char
import Data.Maybe.HT
import Control.Monad

data Token = Token { name :: String, lexeme :: String }

type TokenMatcher = String -> Maybe Token

type LexemeMatcher = String -> Maybe String

doLex :: String -> [Token]
doLex input = [token |
    token <- unfoldr (matchAgainst patterns) input,
    name token `notElem` ["whitespace", "comment line", "comment block"]]

matchAgainst :: [TokenMatcher] -> String -> Maybe (Token, String)
matchAgainst [] _ = Nothing
matchAgainst _ [] = Nothing
matchAgainst acceptors string = case mapMaybe ($ string) acceptors of
    [] -> error "Lexing failed."
    tokens -> let token = maximumBy (comparing (length . lexeme)) tokens
              in Just (token, string \\ lexeme token)

patterns :: [TokenMatcher]
patterns = map (\(name, matcher) -> matcher >=> return . Token name) $
    map (\keyword -> (keyword, makeLiteral keyword)) (words "class public static void main String extends return int boolean if else while true false this new System.out.println { } ( ) [ ] ; = && || < <= == != > >= + - * / % ! . ,")
    ++
    [ ("identifier", identifier)
    , ("integer", matchWhile isDigit)
    , ("whitespace", matchWhile isSpace)
    , ("comment line", commentLine)
    , ("comment block", commentBlock)
    ]

makeLiteral :: String -> LexemeMatcher
makeLiteral l = \string -> toMaybe (l `isPrefixOf` string) l

matchWhile :: (Char -> Bool) -> LexemeMatcher
matchWhile f = \string -> let s = takeWhile f string in toMaybe (not $ null s) s

identifier :: LexemeMatcher
identifier string = let i = takeWhile (\c -> isAlphaNum c || c == '_') string in
    toMaybe (not $ null i || isDigit (head i)) i

commentLine :: LexemeMatcher
commentLine string = toMaybe (isPrefixOf "//" string) $ takeWhile (/= '\n') string

commentBlock :: LexemeMatcher
commentBlock string = do
    rest <- stripPrefix "/*" string
    (content, _) <- find (isPrefixOf "*/" . snd) $ zip (inits rest) (tails rest)
    return ("/*" ++ content ++ "*/")
