import Prelude     (IO)
import Application (appMain)
import System.Directory

main :: IO ()
main = do
    print "starting"
    getCurrentDirectory >>= print
    appMain
