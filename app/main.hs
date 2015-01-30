import Prelude     (IO, (>>=), print)
import Application (appMain)
import System.Environment

main :: IO ()
main = do
    print "starting"
    getArgs >>= print
    appMain
