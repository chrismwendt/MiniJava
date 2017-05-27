import System.Environment
import Options.Applicative
import Interface
import Control.Monad.Identity
import Control.Monad.Except

data Options = Options
    { oStopAt :: String
    , oFile :: String
    }

options :: Parser Options
options = Options
    <$> strOption (long "stopAt" <> metavar "[parse|SSA|type|reg|code]" <> value "code")
    <*> argument str (metavar "file")

main :: IO ()
main = do
    let opts = info (helper <*> options) (fullDesc <> header "A MiniJava compiler")
    Options { oStopAt = stop, oFile = file } <- execParser opts

    input <- readFile file

    putStrLn $ case stop of
        "parse" -> show $ atParse input
        "type"  -> show $ atType input
        "SSA"   -> show $ atSSA input
        "reg"   -> show $ atReg input
        _       -> either id id $ runExcept $ atCode input
