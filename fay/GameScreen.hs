-- {-# LANGUAGE EmptyDataDecls, OverloadedStrings, RebindableSyntax #-}
{-# LANGUAGE RecordWildCards #-}
module GameScreen where

import Prelude
import FFI
import JQuery hiding (not)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Var

import SharedTypes
import Geometry
import Constants
import FFI.SVG

readTrackData :: Fay String
readTrackData = ffi "$('#track').val()"

parseTrackData :: String -> Fay Track
parseTrackData = ffi "JSON.parse(%1)"

drawingId :: Text
drawingId = T.pack "drawing"

addWindowEvent :: String -> (Event -> Fay ()) -> Fay ()
addWindowEvent = ffi "window.addEventListener(%1, %2)"

selectId :: String -> Fay Element
selectId = ffi "document.getElementById(%1)"

drawGrid :: Track -> Element -> Fay ()
drawGrid t@(Track inner outer _ _) drawing = do
    grid <- svgGroup drawing
    let (_, _, w', h') = extents outer
        w = round $ w' + 1
        h = round $ h' + 1
    forM_ [0..w+1] $ \x' ->
        let x = fromIntegral x'
        in svgLine drawing x 0 x h' >>= setClass "grid" >>= groupAdd grid
    forM_ [0..h+1] $ \y' ->
        let y = fromIntegral y'
        in svgLine drawing 0 y w' y >>= setClass "grid" >>= groupAdd grid
    innerOutline <- svgPolygon drawing inner >>= setClass "inner_grid_cover"
    outerOutline <- svgPolygon drawing outer
    grid `clipWith` outerOutline

drawStartLine :: Element -> (Point, Point) -> Fay Element
drawStartLine drawing (P x1 y1, P x2 y2) =
    svgLine drawing x1 y1 x2 y2 >>= setClass "start_line"

addEvent :: Element -> String -> (Event -> Fay ()) -> Fay ()
addEvent = ffi "$(%1).on(%2, %3)"

eventPageX, eventPageY :: Event -> Fay Double
eventPageX = ffi "%1['clientX']"
eventPageY = ffi "%1['clientY']"

getBoundingClientRect :: Element -> Fay Element
getBoundingClientRect = ffi "%1.getBoundingClientRect()"

rectLeft, rectTop :: Element -> Fay Double
rectLeft = ffi "%1.left"
rectTop = ffi "%1.top"

eventLocation :: Element -> Event -> Fay (Double, Double)
eventLocation element ev = do
    r <- getBoundingClientRect element
    t <- rectTop r
    l <- rectLeft r
    x <- eventPageX ev
    y <- eventPageY ev
    return (x - l, y - t)

setXY :: Double -> Double -> Element -> Fay ()
setXY x y el = setX x el >> setY y el

onTrack :: Track -> Point -> Bool
onTrack (Track inner outer _ _) p =
    (p `isInside` outer) && not (p `isInside` inner)

draw :: Track -> Element -> Fay ()
draw track@Track{..} drawing = do
    drawGrid track drawing
    drawStartLine drawing trackStartLine
    _ <- svgPolygon drawing trackInner >>= setClass "outline"
    _ <- svgPolygon drawing trackOuter >>= setClass "outline"
    return ()

drawMove :: Element -> [Point] -> Fay ()
drawMove drawing pts = do
    svgLine drawing x1 y1 x2 y2 >>= setClass "trace"
    svgCircle drawing tickRadius
        >>= setClass "tick"
        >>= setXY (x2 - tickRadius / 2) (y2 - tickRadius / 2)
    return ()
  where
    (x1, y1, x2, y2) = case pts of
        [P x y]             -> (x, y, x, y)
        (P x1 y1:P x2 y2:_) -> (x2, y2, x1, y1)

getPosition :: Double -> Element -> Event -> Fay (Double, Double)
getPosition z element event = do
    (x, y) <- eventLocation element event
    return (toNaturalCoord x, toNaturalCoord y)
  where toNaturalCoord = fromIntegral . round . (/ z)

initGame :: Event -> Fay ()
initGame _ = do
    track@(Track inner outer startLine startPos) <- readTrackData >>= parseTrackData
    let (_, _, xmax, ymax) = extents outer
    zoom <- newVar initialZoom
    playerTrace <- newVar [startPos !! 1]
    drawing <- initSVG drawingId
    canvas <- selectId "drawing"
    pointer <- svgCircle drawing pointerRadius >>= setClass "pointer"
    setXY (-10) (-10) pointer

    get zoom >>= \z -> do
        svgSize (z * (xmax + 0.5)) (z * (ymax + 0.5)) drawing
        draw track drawing
        get playerTrace >>= drawMove drawing
        svgScale z z drawing

    addEvent canvas "mousemove" $ \event -> do
        z <- get zoom
        (x, y) <- getPosition z canvas event
        svgMoveFront pointer
        if onTrack track (P x y)
            then setXY (x - pointerRadius / 2) (y - pointerRadius / 2) pointer
            else setXY (-10) (-10) pointer

    addEvent canvas "click" $ \event -> do
        z <- get zoom
        (x, y) <- getPosition z canvas event
        modify playerTrace (P x y:)
        get playerTrace >>= drawMove drawing
        return ()

    zoomInBtn <- selectId "zoom-in"
    addEvent zoomInBtn "click" $ \_ -> modify zoom zoomIn

    zoomOutBtn <- selectId "zoom-out"
    addEvent zoomOutBtn "click" $ \_ -> modify zoom zoomOut

    _ <- subscribe zoom $ \z -> do
        svgScale z z drawing
        svgSize (z * (xmax + 0.5)) (z * (ymax + 0.5)) drawing
    return ()

zoomIn :: Double -> Double
zoomIn now = if now < maxZoom then now + zoomIncrement else now

zoomOut :: Double -> Double
zoomOut now = if now > minZoom then now - zoomIncrement else now

main :: Fay ()
main = do
    addWindowEvent "load" initGame
