import Text.Read
import Text.ParserCombinators.ReadP as P
import Data.List
import Text.Printf

main :: IO ()
main = interact (pretty 80 . read)

data SExp = Leaf String | Branch [SExp]

instance Show SExp where
    show (Leaf s) = s
    show (Branch sExps) = printf "(%s)" (intercalate " " $ map show sExps)

instance Read SExp where
    readPrec = readP_to_Prec $ const sExp

sExp :: ReadP SExp
sExp =
    (fmap Branch $ between (char '(') (char ')') $ sepBy sExp (char ' '))
    P.<++
    (fmap Leaf $ munch (not . (`elem` " ()")))

pretty :: Int -> SExp -> String
pretty maxWidth = p
    where
    p (Leaf l) = l
    p branch@(Branch sExps) = if (length (show branch) < maxWidth)
        then show branch
        else intercalate "\n" $
            zipWith (++) ("" : repeat " ") $ lines $
            printf "(%s)" $ intercalate "\n" (map p sExps)
