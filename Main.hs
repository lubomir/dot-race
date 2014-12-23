-- {-# LANGUAGE EmptyDataDecls, OverloadedStrings, RebindableSyntax #-}
module Main where

import Prelude
import FFI
import JQuery hiding (not)
import Fay.Text (Text)
import qualified Fay.Text as T

import Geometry

drawingId :: Text
drawingId = T.pack "drawing"

addWindowEvent :: String -> (Event -> Fay ()) -> Fay ()
addWindowEvent = ffi "window.addEventListener(%1, %2)"

initSVG :: Text -> Fay Element
initSVG = ffi "SVG(%1)"

svgGroup :: Element -> Fay Element
svgGroup = ffi "%1.group()"

svgRect :: Element -> Fay Element
svgRect = ffi "%1.rect(100, 100, 100, 100)"

groupAdd :: Element -> Element -> Fay ()
groupAdd = ffi "%1.add(%2)"

svgPolygon :: Element -> [(Double, Double)] -> Fay Element
svgPolygon = ffi "%1.polygon(%2)"

svgCircle :: Element -> Double -> Fay Element
svgCircle = ffi "%1.circle(%2)"

svgLine :: Element -> Int -> Int -> Int -> Int -> Fay Element
svgLine = ffi "%1.line(%2, %3, %4, %5)"

fill :: String -> Element -> Fay Element
fill = ffi "%2.fill(%1)"

svgWidth, svgHeight :: Element -> Fay Int
svgWidth = ffi "%1.width()"
svgHeight = ffi "%1.height()"

attr :: String -> String -> Element -> Fay Element
attr = ffi "%3.attr(%1, %2)"

strokeWidth :: String -> Element -> Fay Element
strokeWidth = ffi "%2.stroke({width: %1})"

inner, outer :: [(Double, Double)]
inner = [ (5.5, 5.5), (13.5, 5.5), (13.5, 12.4), (5.5, 12.5) ];
outer = [ (3, 0.5), (12.5, 0.5), (12.5, 2.5), (16, 0.5), (18.5, 3), (18.5, 15),
          (16, 17.5), (3, 17.5), (0.5, 15), (0.5, 3) ]

(***) :: (a -> b) -> (c -> d) -> (a, c) -> (b, d)
(***) f g (x, y) = (f x, g y)

scalePoints :: Double -> [(Double, Double)] -> [(Double, Double)]
scalePoints s = map ((*s) *** (*s))

selectId :: Text -> Fay JQuery
selectId = ffi "jQuery('#'+%1)"

(<$>) :: (a -> b) -> Fay a -> Fay b
f <$> x = x >>= return . f

clipWith :: Element -> Element -> Fay ()
clipWith = ffi "%1.clipWith(%2)"

drawGrid :: Double -> Element -> Fay ()
drawGrid scale drawing = do
    grid <- svgGroup drawing
    el <- selectId drawingId
    w <- round <$> getInnerWidth el
    h <- round <$> getInnerHeight el
    forM_ (takeWhile (< w) $ map (*fromIntegral scale) [1..]) $ \x -> do
        svgLine drawing x 0 x h >>= attr "class" "grid" >>= groupAdd grid
    forM_ (takeWhile (< h) $ map (*fromIntegral scale) [1..]) $ \y -> do
        svgLine drawing 0 y w y >>= attr "class" "grid" >>= groupAdd grid
    innerOutline <- svgPolygon drawing (scalePoints scale inner)
        >>= fill "#fff"
    outerOutline <- svgPolygon drawing (scalePoints scale outer)
    grid `clipWith` outerOutline

addEvent :: Element -> String -> (Event -> Fay ()) -> Fay ()
addEvent = ffi "%1.on(%2, %3)"

eventClientX, eventClientY :: Event -> Fay Double
eventClientX = ffi "%1['clientX']"
eventClientY = ffi "%1['clientY']"

eventLocation :: Event -> Fay (Double, Double)
eventLocation ev = do
    x <- eventClientX ev
    y <- eventClientY ev
    return (x, y)

setX, setY :: Double -> Element -> Fay ()
setX = ffi "%2.x(%1)"
setY = ffi "%2.y(%1)"

onTrack :: Double -> Double -> Bool
onTrack x y = let p = P x y
                  out = map (uncurry P) outer
                  inn = map (uncurry P) inner
              in (p `isInside` out) && not (p `isInside` inn)

draw :: Event -> Fay ()
draw _ = do
    let scale = 20
    drawing <- initSVG drawingId
    drawGrid scale drawing
    _ <- svgPolygon drawing (scalePoints scale inner) >>= attr "class" "outline"
    _ <- svgPolygon drawing (scalePoints scale outer) >>= attr "class" "outline"
    pointer <- svgCircle drawing 5 >>= attr "class" "pointer"
    addEvent drawing "mousemove" $ \event -> do
        (x', y') <- eventLocation event
        let x = fromIntegral $ round $ x' / scale
        let y = fromIntegral $ round $ y' / scale
        if onTrack x y
            then do setX (scale * x - 2.5) pointer
                    setY (scale * y - 2.5) pointer
            else setX (-10) pointer >> setY (-10) pointer

main :: Fay ()
main = do
    addWindowEvent "load" draw
