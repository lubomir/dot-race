module FFI.Custom where

import FFI
import Data.Text (Text, showInt, fromString, (<>))

import FFI.Types
import SharedTypes (Track, readI)

readTrackData :: Fay Text
readTrackData = ffi "$('#track')['val']()"

parseTrackData :: Text -> Fay Track
parseTrackData = ffi "JSON['parse'](%1)"

addWindowEvent :: Text -> (Event -> Fay ()) -> Fay ()
addWindowEvent = ffi "window['addEventListener'](%1, %2)"

selectId :: Text -> Fay Element
selectId = ffi "document['getElementById'](%1)"

selectClass :: Text -> Fay [Element]
selectClass = ffi "document['getElementsByClassName'](%1)"

addEvent :: Element -> Text -> (Event -> Fay ()) -> Fay ()
addEvent = ffi "$(%1)['on'](%2, %3)"

eventPageX, eventPageY :: Event -> Fay Double
eventPageX = ffi "%1['pageX']"
eventPageY = ffi "%1['pageY']"

getBoundingClientRect :: Element -> Fay Element
getBoundingClientRect = ffi "%1['getBoundingClientRect']()"

rectLeft, rectTop, getScrollTop, getScrollLeft :: Element -> Fay Double
rectLeft = ffi "%1['left']"
rectTop = ffi "%1['top']"
getScrollTop = ffi "%1['scrollTop']"
getScrollLeft = ffi "%1['scrollLeft']"

eventLocation :: Element -> Event -> Fay (Double, Double)
eventLocation element ev = do
    r <- getBoundingClientRect element
    st <- getScrollTop element
    sl <- getScrollLeft element
    t <- rectTop r
    l <- rectLeft r
    x <- eventPageX ev
    y <- eventPageY ev
    return (x - l + sl, y - t + st)

hide :: Element -> Fay ()
hide = ffi "%1['remove']()"

getURL :: Fay Text
getURL = ffi "document['URL']"

getWSConnection :: Fay Connection
getWSConnection = ffi "new WebSocket(document['URL']['replace']('http', 'ws'))"

onMessage :: Connection -> (Event -> Fay ()) -> Fay ()
onMessage = ffi "%1['onmessage'] = %2"

getText :: Event -> Fay Text
getText = ffi "%1['data']"

sendText :: Connection -> Text -> Fay ()
sendText = ffi "%1['send'](%2)"

addChatMessage :: Text -> Fay ()
addChatMessage = ffi "$('#chat div').append('<p>'+%1+'</p>')"

preventDefault :: Event -> Fay ()
preventDefault = ffi "%1['preventDefault']()"

eventKey :: Event -> Int
eventKey = ffi "%1['keyCode']"

getChatMessage :: Fay Text
getChatMessage = ffi "$('#chatInput')['val']()"

clearChatMessage :: Fay ()
clearChatMessage = ffi "$('#chatInput')['val']('')"

getSelectedValue :: Element -> Fay Text
getSelectedValue = ffi "%1['options'][%1['selectedIndex']].value"

setMaxLimit :: Element -> Int -> Fay ()
setMaxLimit el m = do
    setMax el m
    cur <- getValue el
    when (readI cur > m) (setValue el m)

setMax :: Element -> Int -> Fay ()
setMax = ffi "%1['setAttribute']('max', %2)"

getValue :: Element -> Fay Text
getValue = ffi "%1['value']"

setValue :: Element -> Int -> Fay ()
setValue = ffi "%1['value'] = %2"

setTextValue :: Element -> Text -> Fay ()
setTextValue = ffi "%1['value'] = %2"

focusElement :: Element -> Fay ()
focusElement = ffi "%1['focus']()"

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
