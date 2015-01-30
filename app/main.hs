import Prelude     (IO, (>>=), print, (++))
import Application (appMain)
import System.Directory
import System.Environment

main :: IO ()
main = do
    baseDir <- getEnv "OPENSHIFT_DEPLOYMENTS_DIR"
    setCurrentDirectory (baseDir ++ "current/repo/")
    appMain
