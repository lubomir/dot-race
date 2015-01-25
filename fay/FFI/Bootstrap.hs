module FFI.Bootstrap where

import Data.Text (Text, fromString, (<>), showInt)
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
displayPlayerJoin i name =
    displaySystemMsg (fromString "<span class='player-" <> showInt i
                   <> fromString"'>" <> name <> fromString "</span> has joined.")

displayPlayerQuit :: Int -> Text -> Fay ()
displayPlayerQuit i name =
    displaySystemMsg (fromString "<span class='player-" <> showInt i
                   <> fromString"'>" <> name <> fromString "</span> has left.")

displaySystemMsg :: Text -> Fay ()
displaySystemMsg = displayChatMsg 0 (fromString "System")

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

displayGameStatus :: Text -> Fay ()
displayGameStatus = ffi "$('#gameStatus').html(%1)"

displayWaitingFor :: Text -> Int -> Fay ()
displayWaitingFor n i =
    displayGameStatus $ fromString "Waiting for <span class='player-"
                     <> showInt i <> fromString "'>" <> n <> fromString "</span>…"

displayChatMsg :: Int -> Text -> Text -> Fay ()
displayChatMsg i name msg = do
    displayChatMsgRaw $ fromString "<p><span class='player-" <> showInt i
                      <> fromString "'>" <> name <> fromString "</span>: "
                      <> msg <> fromString "</p>"
    scrollChat

displayChatMsgRaw :: Text -> Fay ()
displayChatMsgRaw = ffi "$('#chat div').append(%1)"

scrollChat :: Fay ()
scrollChat = ffi "$('#chat div').stop().animate({scrollTop: $('#chat div')[0].scrollHeight}, 800)"
