{-# LANGUAGE RecordWildCards, OverloadedStrings, RebindableSyntax #-}
module GameScreen where

import Prelude
import FFI
import Data.Text (Text, fromString)
import qualified Data.Text as T
import Data.Var
import Data.Function (fmap)

import SharedTypes
import Geometry
import Constants
import FFI.SVG
import FFI.Custom
import FFI.Types
import FFI.Bootstrap

data TrackData = TrackData { track     :: Track
                           , outerExtents :: Extremes
                           , innerExtents :: Extremes
                           , inner     :: [Point]
                           , outer     :: [Point]
                           , startLine :: Line
                           , startPos  :: [Point]
                           , innerSegments :: [BoundedLine]
                           , outerSegments :: [BoundedLine]
                           }

makeTrackData :: Track -> TrackData
makeTrackData track = TrackData {..}
  where
    inner = trackInner track
    outer = trackOuter track
    startLine = uncurry Line $ trackStartLine track
    startPos = trackStartPos track
    innerSegments = getSegments inner
    outerSegments = getSegments outer
    outerExtents = extents outer
    innerExtents = extents inner

drawingId :: Text
drawingId = "drawing"

drawGrid :: TrackData -> Element -> Fay ()
drawGrid TrackData{..} drawing = do
    grid <- svgGroup drawing
    let xmax = eXMax outerExtents
        ymax = eYMax outerExtents
        w = round $ xmax + 1
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

drawStartLine :: Element -> Line -> Fay Element
drawStartLine drawing (Line (P x1 y1) (P x2 y2)) =
    svgLine drawing x1 y1 x2 y2 >>= setClass "start_line"

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
drawCrash drawing p = svgPolygon drawing (crashMark p) >>= setClass "crashPoint"

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
refreshOptions :: Element -> TrackData -> [Point] -> Fay [Point]
refreshOptions drawing TrackData{..} trace@(tp:_) = do
    selectClass "option" >>= mapM_ svgRemove
    let opts = filter isValid $ getNeighbors $ getNextPoint trace
    if null opts
        then drawCrash drawing tp >> showCrashDialog
        else mapM_ (drawOpt drawing) opts
    return opts
  where
    isValid p = (p /= tp) && not90Deg p && notThruWall p

    not90Deg p = case trace of
        (_:tp':_) -> distance p tp' >= 2
        _ -> True

    notThruWall p = let ln = boundedLine p tp
                    in not (ln `intersectsWithAnyBounded` innerSegments) &&
                       not (ln `intersectsWithAnyBounded` outerSegments)

initGame :: Event -> Fay ()
initGame _ = do
    td@TrackData{..} <- makeTrackData `fmap` (readTrackData >>= parseTrackData)

    zoom <- newVar initialZoom
    playerTrace <- newVar [startPos !! 1]
    playerExtents <- newVar (Extremes (eXMax outerExtents) 0 (eYMax outerExtents) 0)
    drawing <- initSVG drawingId
    canvas <- selectId drawingId
    options <- newVar []
    pointer <- svgCircle drawing pointerRadius >>= setClass "pointer"
    setXY (-10) (-10) pointer
    conn <- getWSConnection

    get zoom >>= \z -> do
        svgSize (z * (eXMax outerExtents + canvasPadding))
                (z * (eYMax outerExtents + canvasPadding))
                drawing
        draw td drawing
        trace <- get playerTrace
        drawMove drawing trace
        refreshOptions drawing td trace >>= set options
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
        sendText conn "click"
        print "click"
        z <- get zoom
        (x, y) <- getPosition z canvas event
        opts <- get options
        when (P x y `elem` opts) $ do
            modify playerTrace (P x y:)
            modify playerExtents (updateExtremes (P x y))
            reached <- get playerExtents
            trace <- get playerTrace
            when (encapsulates innerExtents reached && hasWinningMove trace startLine) $ do
                print "You won"
            drawMove drawing trace
            refreshOptions drawing td trace >>= set options
            return ()

    zoomInBtn <- selectId "zoom-in"
    addEvent zoomInBtn "click" $ \_ -> modify zoom zoomIn

    zoomOutBtn <- selectId "zoom-out"
    addEvent zoomOutBtn "click" $ \_ -> modify zoom zoomOut

    _ <- subscribe zoom $ \z -> do
        svgScale z z drawing
        svgSize (z * (eXMax outerExtents + canvasPadding))
                (z * (eYMax outerExtents + canvasPadding))
                drawing

    conn `onMessage` \e -> do
        print "onMessage"
        t <- getText e
        print t
        addChatMessage t

    joinButton <- selectId "joinButton"
    addEvent joinButton "click" $ \_ -> do
        selectId "join-game-dialog" >>= hide
        sendText conn "Hello"

    return ()

updateExtremes :: Point -> Extremes -> Extremes
updateExtremes (P x y) ex = Extremes { eXMin = min x (eXMin ex)
                                     , eXMax = max x (eXMax ex)
                                     , eYMin = min y (eYMin ex)
                                     , eYMax = max y (eYMax ex)
                                     }

hasWinningMove :: [Point] -> Line -> Bool
hasWinningMove (p2:p1:_) ln = Line p1 p2 `hasIntersection` ln
hasWinningMove _ _ = False

encapsulates :: Extremes -> Extremes -> Bool
encapsulates inner outer = eXMin outer < eXMin inner
                        && eYMin outer < eYMin inner
                        && eXMax outer > eXMax inner
                        && eYMax outer > eYMax inner

zoomIn :: Double -> Double
zoomIn now = if now < maxZoom then now + zoomIncrement else now

zoomOut :: Double -> Double
zoomOut now = if now > minZoom then now - zoomIncrement else now

main :: Fay ()
main = do
    addWindowEvent "load" initGame
