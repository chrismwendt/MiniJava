import System.Environment
import Data.List
import Options.Applicative
import Parser
import AST
import SSACompiler
import qualified TypeChecker as TC
import qualified RegisterAllocator as Reg
import Control.Monad.State
import Data.Maybe
import qualified Data.Map as M
import Data.Bifunctor

data Options = Options { stopAt :: String, file :: String }

options :: Parser Options
options = Options
    <$> strOption (long "stopAt" <> metavar "[parse|SSA|type|reg|code]" <> value "code")
    <*> argument str (metavar "file")

compile :: Options -> String -> String
compile (Options "parse" _) source = sExpProgram $ parseString source
compile (Options "SSA" _) source = (++ "\n") $ show $ fst $ freeze $ ssaCompile $ parseString source
compile (Options "type" _) source = (++ "\n") $ show $ fst $ freeze $ uncurry3 TC.typeCheck $ ssaCompile $ parseString source
-- compile (Options "reg" _) source = (++ "\n") $ show $ fst $ freeze $ uncurry3 (Reg.allocate 22) $ uncurry3 TC.typeCheck $ ssaCompile $ parseString source
compile (Options "code" _) source = error "unimplemented target: code"
compile (Options target _) _ = error $ "unknown target: " ++ target

freeze (program, l, m) = let f ref = fromJust $ M.lookup ref m in (bimap id f program, map f l)

uncurry3 f (a, b, c) = f a b c

main :: IO ()
main = execParser opts >>= \os -> do
    input <- readFile $ file os
    putStr $ compile os input
    where
    opts = info (helper <*> options) (fullDesc <> header "A MiniJava compiler")
