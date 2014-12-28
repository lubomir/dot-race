-- {-# LANGUAGE EmptyDataDecls, OverloadedStrings, RebindableSyntax #-}
{-# LANGUAGE RecordWildCards #-}
import Prelude
import FFI
import JQuery hiding (not)
import Fay.Text (Text)
import qualified Fay.Text as T

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

svgLine :: Element -> Double -> Double -> Double -> Double -> Fay Element
svgLine = ffi "%1.line(%2, %3, %4, %5)"

svgWidth, svgHeight :: Element -> Fay Int
svgWidth = ffi "%1.width()"
svgHeight = ffi "%1.height()"

setClass :: String -> Element -> Fay Element
setClass = ffi "%2.attr('class', %1)"

strokeWidth :: String -> Element -> Fay Element
strokeWidth = ffi "%2.stroke({width: %1})"

scalePoints :: Double -> [Point] -> [(Double, Double)]
scalePoints s = map (\(P x y) -> (x * s, y * s))

selectId :: Text -> Fay JQuery
selectId = ffi "jQuery('#'+%1)"

(<$>) :: (a -> b) -> Fay a -> Fay b
f <$> x = x >>= return . f

clipWith :: Element -> Element -> Fay ()
clipWith = ffi "%1.clipWith(%2)"

drawGrid :: Track -> Double -> Element -> Fay ()
drawGrid t@(Track inner outer _ _) scale drawing = do
    grid <- svgGroup drawing
    el <- selectId drawingId
    w <- getInnerWidth el
    h <- getInnerHeight el
    forM_ (takeWhile (< w) $ map (*fromIntegral scale) [1..]) $ \x ->
        svgLine drawing x 0 x h >>= setClass "grid" >>= groupAdd grid
    forM_ (takeWhile (< h) $ map (*fromIntegral scale) [1..]) $ \y ->
        svgLine drawing 0 y w y >>= setClass "grid" >>= groupAdd grid
    innerOutline <- svgPolygon drawing scale inner >>= setClass "inner_grid_cover"
    outerOutline <- svgPolygon drawing scale outer
    grid `clipWith` outerOutline

drawStartLine :: Element -> Double -> (Point, Point) -> Fay Element
drawStartLine drawing scale (P x1 y1, P x2 y2) =
    svgLine drawing (scale * x1) (scale * y1) (scale * x2) (scale * y2)
        >>= setClass "start_line"

addEvent :: Element -> String -> (Event -> Fay ()) -> Fay ()
addEvent = ffi "%1.on(%2, %3)"

eventPageX, eventPageY :: Event -> Fay Double
eventPageX = ffi "%1['clientX']"
eventPageY = ffi "%1['clientY']"

getBoundingClientRect :: Element -> Fay Element
getBoundingClientRect = ffi "%1.getBoundingClientRect()"

rectLeft, rectTop :: Element -> Fay Double
rectLeft = ffi "%1.left"
rectTop = ffi "%1.top"

getNode :: Element -> Fay Element
getNode = ffi "%1.node"

eventLocation :: Element -> Event -> Fay (Double, Double)
eventLocation element ev = do
    r <- getNode element >>= getBoundingClientRect
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

initGame :: Event -> Fay ()
initGame _ = do
    track@(Track inner outer startLine _) <- readTrackData >>= parseTrackData
    let (_, _, xmax, ymax) = extents outer
    let scale = 20
    drawing <- initSVG drawingId
    size (scale * (xmax + 0.5)) (scale * (ymax + 0.5)) drawing
    draw track drawing scale

    pointer <- svgCircle drawing 5 >>= setClass "pointer"
    setXY (-10) (-10) pointer
    addEvent drawing "mousemove" $ \event -> do
        (x', y') <- eventLocation drawing event
        let x = fromIntegral $ round $ x' / scale
        let y = fromIntegral $ round $ y' / scale
        if onTrack track (P x y)
            then setXY (scale * x - 2.5) (scale * y - 2.5) pointer
            else setXY (-10) (-10) pointer

main :: Fay ()
main = do
    addWindowEvent "load" initGame
