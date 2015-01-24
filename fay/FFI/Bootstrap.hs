module FFI.Bootstrap where

import Data.Text (Text, fromString)
import FFI

showDialog :: Text -> Fay ()
showDialog = ffi "$('#'+%1+'')['modal']()"

showCrashDialog :: Fay ()
showCrashDialog = showDialog (fromString "crashDialog")

getPlayerName :: Fay Text
getPlayerName = ffi "$('#inputName')['val']()"

getNumPlayers :: Fay Int
getNumPlayers = ffi "parseInt($('#numPlayers')['val'](), 10)"

displayPlayerJoin :: Int -> Text -> Fay ()
displayPlayerJoin =
    ffi "$('#chat div').append('<p><span class=player-'+%1+'>'+%2+'</span> has joined.</p>')"

showWinDialog :: Fay ()
showWinDialog = showDialog (fromString "win-dialog")

showLoseDialog :: Text -> Fay ()
showLoseDialog winner = setWinner winner >> showDialog (fromString "lose-dialog")

setWinner :: Text -> Fay ()
setWinner = ffi "$('#winner')['text'](%1)"

displayThisPlayerName :: Text -> Fay ()
displayThisPlayerName = ffi "$('#thisPlayerName').text(%1)"

displayThisPlayerNum :: Int -> Fay ()
displayThisPlayerNum = ffi "$('#thisPlayerName').addClass('player-'+%1)"
