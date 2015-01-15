-- {-# LANGUAGE EmptyDataDecls, OverloadedStrings, RebindableSyntax #-}
{-# LANGUAGE RecordWildCards #-}
module GameScreen where

import Prelude
import FFI
import JQuery (Event, Element)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Var
import Data.Function (fmap)

import SharedTypes
import Geometry
import Constants
import FFI.SVG

data TrackData = TrackData { track     :: Track
                           , xmin      :: Double
                           , xmax      :: Double
                           , ymin      :: Double
                           , ymax      :: Double
                           , inner     :: [Point]
                           , outer     :: [Point]
                           , startLine :: (Point, Point)
                           , startPos  :: [Point]
                           }

makeTrackData :: Track -> TrackData
makeTrackData track = TrackData {..}
  where
    inner = trackInner track
    outer = trackOuter track
    startLine = trackStartLine track
    startPos = trackStartPos track
    (xmin, ymin, xmax, ymax) = extents outer

readTrackData :: Fay String
readTrackData = ffi "$('#track')['val']()"

parseTrackData :: String -> Fay Track
parseTrackData = ffi "JSON['parse'](%1)"

drawingId :: Text
drawingId = T.pack "drawing"

addWindowEvent :: String -> (Event -> Fay ()) -> Fay ()
addWindowEvent = ffi "window['addEventListener'](%1, %2)"

selectId :: String -> Fay Element
selectId = ffi "document['getElementById'](%1)"

selectClass :: String -> Fay [Element]
selectClass = ffi "document['getElementsByClassName'](%1)"

drawGrid :: TrackData -> Element -> Fay ()
drawGrid TrackData{..} drawing = do
    grid <- svgGroup drawing
    let w = round $ xmax + 1
        h = round $ ymax + 1
    forM_ [0..w+1] $ \x' ->
        let x = fromIntegral x'
        in svgLine drawing x 0 x ymax >>= setClass "grid" >>= groupAdd grid
    forM_ [0..h+1] $ \y' ->
        let y = fromIntegral y'
        in svgLine drawing 0 y xmax y >>= setClass "grid" >>= groupAdd grid
    innerOutline <- svgPolygon drawing inner >>= setClass "inner_grid_cover"
    outerOutline <- svgPolygon drawing outer
    grid `clipWith` outerOutline

drawStartLine :: Element -> (Point, Point) -> Fay Element
drawStartLine drawing (P x1 y1, P x2 y2) =
    svgLine drawing x1 y1 x2 y2 >>= setClass "start_line"

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

setXY :: Double -> Double -> Element -> Fay ()
setXY x y el = setX x el >> setY y el

draw :: TrackData -> Element -> Fay ()
draw td@(TrackData{..}) drawing = do
    drawGrid td drawing
    drawStartLine drawing startLine
    _ <- svgPolygon drawing inner >>= setClass "outline"
    _ <- svgPolygon drawing outer >>= setClass "outline"
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

getNextPoint :: [Point] -> Point
getNextPoint [p] = p
getNextPoint (P x2 y2: P x1 y1:_) = P (2 * x2 - x1) (2 * y2 - y1)

getNeighbors :: Point -> [Point]
getNeighbors (P x y) = [ P (x + xd) (y + yd)
                       | xd <- [-1, 0, 1]
                       , yd <- [-1, 0, 1]
                       ]

drawCrash :: Element -> Point -> Fay Element
drawCrash drawing (P x y) = svgPolygon drawing pts >>= setClass "crashPoint"
  where
    s = 0.5
    q1 = s * 0.2
    q2 = s * 0.4
    q3 = s * 0.7
    pts = [ P (x + q1) (y - q2)
          , P (x + q3) (y - q3)
          , P (x + q2) (y - q1)
          , P (x + s )  y

          , P (x + q2) (y + q1)
          , P (x + q3) (y + q3)
          , P (x + q1) (y + q2)
          , P  x       (y + s )

          , P (x - q1) (y + q2)
          , P (x - q3) (y + q3)
          , P (x - q2) (y + q1)
          , P (x - s )  y

          , P (x - q2) (y - q1)
          , P (x - q3) (y - q3)
          , P (x - q1) (y - q2)
          , P  x       (y - s )
          ]


drawOpt :: Element -> Point -> Fay Element
drawOpt drawing (P x y) = do
    opt <- svgCircle drawing optionRadius >>= setClass "option"
    setXY (x - offset) (y - offset) opt
    return opt
  where
    offset = optionRadius / 2

-- |Compute and redraw options. Returns the list of options or empty list on
-- crash.
--
refreshOptions :: Element -> Track -> [Point] -> Fay [Point]
refreshOptions drawing track trace@(tp:_) = do
    opts <- selectClass "option"
    mapM_ svgRemove opts
    let opts' = filter isValid $ getNeighbors $ getNextPoint trace
    case opts' of
        [] -> drawCrash drawing tp >> return ()
        opts -> mapM_ (drawOpt drawing) opts
    return opts'
  where
    isValid p = (p /= tp)&& not90Deg p && notThruWall p

    not90Deg p = case trace of
        (_:tp':_) -> distance p tp' >= 2
        _ -> True

    notThruWall p = let ln = Line p tp
                    in not (ln `intersectsWith` trackInner track) &&
                       not (ln `intersectsWith` trackOuter track)

initGame :: Event -> Fay ()
initGame _ = do
    td@TrackData{..} <- makeTrackData `fmap` (readTrackData >>= parseTrackData)

    zoom <- newVar initialZoom
    playerTrace <- newVar [startPos !! 1]
    drawing <- initSVG drawingId
    canvas <- selectId "drawing"
    options <- newVar []
    pointer <- svgCircle drawing pointerRadius >>= setClass "pointer"
    setXY (-10) (-10) pointer

    get zoom >>= \z -> do
        svgSize (z * (xmax + 0.5)) (z * (ymax + 0.5)) drawing
        draw td drawing
        trace <- get playerTrace
        drawMove drawing trace
        refreshOptions drawing track trace >>= set options
        svgScale z z drawing

    addEvent canvas "mousemove" $ \event -> do
        z <- get zoom
        (x, y) <- getPosition z canvas event
        opts <- get options
        if P x y `elem` opts
            then do
                svgMoveFront pointer
                setXY (x - pointerRadius / 2) (y - pointerRadius / 2) pointer
            else setXY (-10) (-10) pointer

    addEvent canvas "click" $ \event -> do
        z <- get zoom
        (x, y) <- getPosition z canvas event
        opts <- get options
        when (P x y `elem` opts) $ do
            modify playerTrace (P x y:)
            trace <- get playerTrace
            drawMove drawing trace
            refreshOptions drawing track trace >>= set options
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
