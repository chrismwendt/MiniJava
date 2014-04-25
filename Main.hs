import System.Environment
import Data.List
import Lexer

main :: IO ()
main = do
    args <- getArgs
    case args of
        [arg] -> do
            string <- readFile arg
            putStr $ unlines [lexeme token |
                token <- unfoldr (matchAgainst patterns) string,
                name token `notElem` ["whitespace", "comment line", "comment block"]]
        _ -> putStrLn "Expected a filename argument."
