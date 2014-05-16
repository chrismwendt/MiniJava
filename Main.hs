import System.Environment
import Options.Applicative
import Parser
import qualified TypeChecker as TC
import qualified SSACompiler as SSA
import qualified RegisterAllocator as Reg
import qualified CodeGenerator as Code
import Control.Monad.State
import Data.Maybe
import qualified Data.Map as M
import Data.Bifunctor

data Options = Options
    { oStopAt :: String
    , oFile :: String
    }

options :: Parser Options
options = Options
    <$> strOption (long "stopAt" <> metavar "[parse|SSA|type|reg|code]" <> value "code")
    <*> argument str (metavar "file")

registerLimit :: Int
registerLimit = 22

main :: IO ()
main = do
    let opts = info (helper <*> options) (fullDesc <> header "A MiniJava compiler")
    Options { oStopAt = stop, oFile = file } <- execParser opts

    input <- readFile file

    let atParse = parseString input
        atType  = TC.typeCheck atParse
        atSSA   = SSA.compile atType
        atReg   = Reg.allocate registerLimit atSSA
        atCode  = Code.generate atType atReg

    putStrLn $ case stop of
        "parse" -> show atParse
        "type"  -> show atType
        "SSA"   -> show atSSA
        "reg"   -> show atReg
        _       -> atCode
