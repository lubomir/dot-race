import Prelude     (IO)
import Application (appMain)
import System.Environment

main :: IO ()
main = do
    print "starting"
    getArgs >>= print
    appMain
