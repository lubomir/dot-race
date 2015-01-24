module FFI.Bootstrap where

import Data.Text (Text)
import FFI

showCrashDialog :: Fay ()
showCrashDialog = ffi "$('#crash-dialog')['modal']()"

getPlayerName :: Fay Text
getPlayerName = ffi "$('#inputName')['val']()"

getNumPlayers :: Fay Int
getNumPlayers = ffi "parseInt($('#numPlayers')['val'](), 10)"

displayPlayerJoin :: Int -> Text -> Fay ()
displayPlayerJoin =
    ffi "$('#chat div').append('<p><span class=player-'+%1+'>'+%2+'</span> has joined.</p>')"
