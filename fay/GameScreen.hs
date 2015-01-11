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

readTrackData :: Fay String
readTrackData = ffi "$('#track').val()"

parseTrackData :: String -> Fay Track
parseTrackData = ffi "JSON.parse(%1)"

drawingId :: Text
drawingId = T.pack "drawing"

addWindowEvent :: String -> (Event -> Fay ()) -> Fay ()
addWindowEvent = ffi "window.addEventListener(%1, %2)"

initSVG :: Text -> Fay Element
initSVG = ffi "SVG(%1)"

size :: Double -> Double -> Element -> Fay Element
size = ffi "%3.size(%1, %2)"

svgGroup :: Element -> Fay Element
svgGroup = ffi "%1.group()"

groupAdd :: Element -> Element -> Fay ()
groupAdd = ffi "%1.add(%2)"

svgPolygon' :: Element -> [(Double, Double)] -> Fay Element
svgPolygon' = ffi "%1.polygon(%2)"

svgPolygon :: Element -> Double -> [Point] -> Fay Element
svgPolygon drawing scale = svgPolygon' drawing . scalePoints scale

svgCircle :: Element -> Double -> Fay Element
svgCircle = ffi "%1.circle(%2)"

svgLine' :: Element -> Double -> Double -> Double -> Double -> Fay Element
svgLine' = ffi "%1.line(%2, %3, %4, %5)"

svgLine :: Element -> Double -> Double -> Double -> Double -> Double -> Fay Element
svgLine drawing scale x1 y1 x2 y2 = svgLine' drawing (scale * x1) (scale * y1)
                                                     (scale * x2) (scale * y2)

setClass :: String -> Element -> Fay Element
setClass = ffi "%2.attr('class', %1)"

scalePoints :: Double -> [Point] -> [(Double, Double)]
scalePoints s = map (\(P x y) -> (x * s, y * s))

selectId :: String -> Fay Element
selectId = ffi "document.getElementById(%1)"

clipWith :: Element -> Element -> Fay ()
clipWith = ffi "%1.clipWith(%2)"

drawGrid :: Track -> Double -> Element -> Fay ()
drawGrid t@(Track inner outer _ _) scale drawing = do
    grid <- svgGroup drawing
    let (_, _, w', h') = extents outer
        w = round $ w' + 1
        h = round $ h' + 1
    forM_ [0..w+1] $ \x' ->
        let x = fromIntegral x'
        in svgLine drawing scale x 0 x h' >>= setClass "grid" >>= groupAdd grid
    forM_ [0..h+1] $ \y' ->
        let y = fromIntegral y'
        in svgLine drawing scale 0 y w' y >>= setClass "grid" >>= groupAdd grid
    innerOutline <- svgPolygon drawing scale inner >>= setClass "inner_grid_cover"
    outerOutline <- svgPolygon drawing scale outer
    grid `clipWith` outerOutline

drawStartLine :: Element -> Double -> (Point, Point) -> Fay Element
drawStartLine drawing scale (P x1 y1, P x2 y2) =
    svgLine drawing scale x1 y1 x2 y2 >>= setClass "start_line"

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

setX, setY :: Double -> Element -> Fay ()
setX = ffi "%2.x(%1)"
setY = ffi "%2.y(%1)"

setXY :: Double -> Double -> Element -> Fay ()
setXY x y el = setX x el >> setY y el

onTrack :: Track -> Point -> Bool
onTrack (Track inner outer _ _) p =
    (p `isInside` outer) && not (p `isInside` inner)

draw :: Track -> Element -> Double -> Fay ()
draw track@Track{..} drawing scale = do
    drawGrid track scale drawing
    drawStartLine drawing scale trackStartLine
    _ <- svgPolygon drawing scale trackInner >>= setClass "outline"
    _ <- svgPolygon drawing scale trackOuter >>= setClass "outline"
    return ()

tickRadius :: Double
tickRadius = 8

drawMove :: Element -> Double -> [Point] -> Fay ()
drawMove drawing scale pts = do
    svgLine drawing scale x1 y1 x2 y2 >>= setClass "trace"
    svgCircle drawing tickRadius
        >>= setClass "tick"
        >>= setXY (x2 * scale - tickRadius / 2) (y2 * scale - tickRadius / 2)
    return ()
  where
    (x1, y1, x2, y2) = case pts of
        [P x y]             -> (x, y, x, y)
        (P x1 y1:P x2 y2:_) -> (x2, y2, x1, y1)

getPosition :: Double -> Double -> Element -> Event -> Fay (Double, Double)
getPosition z scale element event = do
    (x', y') <- eventLocation element event
    let x = fromIntegral $ round $ x' / (scale * z)
    let y = fromIntegral $ round $ y' / (scale * z)
    return (x, y)

pointerRadius :: Double
pointerRadius = 6

svgMoveFront :: Element -> Fay Element
svgMoveFront = ffi "%1.front()"

initGame :: Event -> Fay ()
initGame _ = do
    track@(Track inner outer startLine startPos) <- readTrackData >>= parseTrackData
    let (_, _, xmax, ymax) = extents outer
    let scale = 20
    zoom <- newVar 1
    playerTrace <- newVar [startPos !! 1]
    drawing <- initSVG drawingId
    size (scale * (xmax + 0.5)) (scale * (ymax + 0.5)) drawing
    draw track drawing scale
    get playerTrace >>= drawMove drawing scale

    canvas <- selectId "drawing"
    pointer <- svgCircle drawing pointerRadius >>= setClass "pointer"
    setXY (-10) (-10) pointer
    addEvent canvas "mousemove" $ \event -> do
        z <- get zoom
        (x, y) <- getPosition z scale canvas event
        svgMoveFront pointer
        if onTrack track (P x y)
            then setXY (scale * x - pointerRadius / 2) (scale * y - pointerRadius / 2) pointer
            else setXY (-10) (-10) pointer

    addEvent canvas "click" $ \event -> do
        z <- get zoom
        (x, y) <- getPosition z scale canvas event
        modify playerTrace (P x y:)
        get playerTrace >>= drawMove drawing (scale * z)
        return ()

    zoomInBtn <- selectId "zoom-in"
    addEvent zoomInBtn "click" $ \_ -> modify zoom zoomIn

    zoomOutBtn <- selectId "zoom-out"
    addEvent zoomOutBtn "click" $ \_ -> modify zoom zoomOut

    _ <- subscribe zoom $ \z -> do
        svgScale z z drawing
        size (z * scale * (xmax + 0.5)) (z * scale * (ymax + 0.5)) drawing
    return ()

zoomIncrement :: Double
zoomIncrement = 0.25

minZoom :: Double
minZoom = 0.5

maxZoom :: Double
maxZoom = 2

zoomIn :: Double -> Double
zoomIn now = if now < maxZoom then now + zoomIncrement else now

zoomOut :: Double -> Double
zoomOut now = if now > minZoom then now - zoomIncrement else now

svgScale :: Double -> Double -> Element -> Fay ()
svgScale = ffi "%3.scale(%1, %2)"

main :: Fay ()
main = do
    addWindowEvent "load" initGame
