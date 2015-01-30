import Prelude     (IO)
import Application (appMain)
import System.Directory
import System.Environment

main :: IO ()
main = do
    print "starting"
    getCurrentDirectory >>= print
    getArgs >>= print
    appMain
