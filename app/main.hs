import Prelude     (IO, (>>=), print)
import Application (appMain)
import System.Directory

main :: IO ()
main = do
    getCurrentDirectory >>= print
    appMain
