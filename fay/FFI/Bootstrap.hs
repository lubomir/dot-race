module FFI.Bootstrap where

import Data.Text (Text)
import FFI

showCrashDialog :: Fay ()
showCrashDialog = ffi "$('#crash-dialog')['modal']()"

getPlayerName :: Fay Text
getPlayerName = ffi "$('#inputName')['val']()"
