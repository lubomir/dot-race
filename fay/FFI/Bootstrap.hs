module FFI.Bootstrap where

import FFI

showCrashDialog :: Fay ()
showCrashDialog = ffi "$('#crash-dialog').modal()"
