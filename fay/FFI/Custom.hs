module FFI.Custom where

import FFI
import JQuery (Event, Element)
import Data.Text (Text)

import SharedTypes (Track)

readTrackData :: Fay String
readTrackData = ffi "$('#track')['val']()"

parseTrackData :: String -> Fay Track
parseTrackData = ffi "JSON['parse'](%1)"

addWindowEvent :: String -> (Event -> Fay ()) -> Fay ()
addWindowEvent = ffi "window['addEventListener'](%1, %2)"

selectId :: String -> Fay Element
selectId = ffi "document['getElementById'](%1)"

selectClass :: String -> Fay [Element]
selectClass = ffi "document['getElementsByClassName'](%1)"

addEvent :: Element -> String -> (Event -> Fay ()) -> Fay ()
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

