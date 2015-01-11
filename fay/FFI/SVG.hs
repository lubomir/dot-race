module FFI.SVG where

import JQuery hiding (not)
import Data.Text (Text)
import FFI

import SharedTypes

initSVG :: Text -> Fay Element
initSVG = ffi "SVG(%1)"

svgSize :: Double -> Double -> Element -> Fay Element
svgSize = ffi "%3.size(%1, %2)"

svgGroup :: Element -> Fay Element
svgGroup = ffi "%1.group()"

groupAdd :: Element -> Element -> Fay ()
groupAdd = ffi "%1.add(%2)"

svgPolygon' :: Element -> [(Double, Double)] -> Fay Element
svgPolygon' = ffi "%1.polygon(%2)"

svgPolygon :: Element -> [Point] -> Fay Element
svgPolygon drawing = svgPolygon' drawing . map (\(P x y) -> (x, y))

svgCircle :: Element -> Double -> Fay Element
svgCircle = ffi "%1.circle(%2)"

svgLine :: Element -> Double -> Double -> Double -> Double -> Fay Element
svgLine = ffi "%1.line(%2, %3, %4, %5)"

setClass :: String -> Element -> Fay Element
setClass = ffi "%2.attr('class', %1)"

clipWith :: Element -> Element -> Fay ()
clipWith = ffi "%1.clipWith(%2)"

setX, setY :: Double -> Element -> Fay ()
setX = ffi "%2.x(%1)"
setY = ffi "%2.y(%1)"

svgMoveFront :: Element -> Fay Element
svgMoveFront = ffi "%1.front()"

svgScale :: Double -> Double -> Element -> Fay ()
svgScale = ffi "%3.scale(%1, %2)"
