{-# LANGUAGE RecordWildCards, OverloadedStrings, RebindableSyntax #-}
module GameScreen where

import Prelude
import FFI
import Data.Text (Text, fromString, (<>), showInt)
import qualified Data.Text as T
import Data.Var
import Data.Function (fmap)

import SharedTypes
import Geometry
import Constants
import FFI.SVG
import DotRace.FFI
import FFI.Types

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

drawMove :: Element -> Int -> [Point] -> Fay ()
drawMove drawing i pts = do
    let cls = "player-" <> showInt i
    svgLine drawing x1 y1 x2 y2 >>= setClass ("trace " <> cls)
    svgCircle drawing tickRadius
        >>= setClass ("tick " <> cls)
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

removeOptions :: Fay ()
removeOptions = selectClass "option" >>= mapM_ svgRemove

-- |Compute and redraw options. Returns the list of options or empty list on
-- crash.
--
refreshOptions :: GameState -> Element -> TrackData -> [Point] -> Fay [Point]
refreshOptions GameState{..} drawing TrackData{..} trace@(tp:_) = do
    let opts = filter isValid $ getNeighbors $ getNextPoint trace
    if null opts
        then drawCrash drawing tp >> showCrashDialog
        else mapM_ (drawOpt drawing) opts
    return opts
  where
    isValid p = isEmpty p && not90Deg p && notThruWall p
    isEmpty p = all ((/= p) . head . ptPath) gsTraces

    not90Deg p = case trace of
        (_:tp':_) -> distance p tp' >= 2
        _ -> True

    notThruWall p = let ln = boundedLine p tp
                    in not (ln `intersectsWithAnyBounded` innerSegments) &&
                       not (ln `intersectsWithAnyBounded` outerSegments)

data PlayerTrace = PlayerTrace { ptPath :: [Point]
                               , ptExtents :: Extremes
                               }

initPlayerTrace :: Point -> PlayerTrace
initPlayerTrace p@(P x y) = PlayerTrace [p] (Extremes x x y y)

data GameState = GameState { gsTraces :: [PlayerTrace]
                           , gsPlayerNames :: [Text]
                           , gsNumPlayers :: Int
                           , gsCurrentPlayer :: Int
                           , gsThisPlayer :: Int
                           }

addPlayer :: Text -> [Point] -> GameState -> GameState
addPlayer n starts s@(GameState ts ps _ _ _) =
    let num = length ts
    in s { gsTraces = ts ++ [initPlayerTrace (starts !! num)]
         , gsPlayerNames = ps ++ [n]
         }

setThisPlayer :: Int -> GameState -> GameState
setThisPlayer n s = s { gsThisPlayer = n }

isReady :: GameState -> Bool
isReady GameState{..} = length gsTraces == gsNumPlayers

thisIsCurrentPlayer :: GameState -> Bool
thisIsCurrentPlayer s = gsThisPlayer s == gsCurrentPlayer s

getCurrentTrace :: GameState -> PlayerTrace
getCurrentTrace s = gsTraces s !! (gsCurrentPlayer s - 1)

getNthPlayerName :: GameState -> Int -> Text
getNthPlayerName s i = gsPlayerNames s !! (i - 1)

getCurrentPlayerName :: GameState -> Text
getCurrentPlayerName s = getNthPlayerName s (gsCurrentPlayer s)

getNthTrace :: GameState -> Int -> PlayerTrace
getNthTrace s n = gsTraces s !! (n - 1)

getCurrentPlayerTrace :: GameState -> PlayerTrace
getCurrentPlayerTrace s = getNthTrace s (gsCurrentPlayer s)

setIx :: Int -> a -> [a] -> [a]
setIx 1 x (_:ys) = x:ys
setIx n x (y:ys) = y : setIx (n-1) x ys

moveCurrentPlayer :: Point -> GameState -> GameState
moveCurrentPlayer p s =
    let trace = getCurrentTrace s
    in s { gsTraces = setIx (gsCurrentPlayer s) (addPoint p trace) (gsTraces s) }

nextPlayer :: GameState -> GameState
nextPlayer s = s { gsCurrentPlayer = gsCurrentPlayer s `mod` gsNumPlayers s + 1 }

initGame :: Event -> Fay ()
initGame _ = do
    td@TrackData{..} <- makeTrackData `fmap` (readTrackData >>= parseTrackData)
    numPlayers <- getNumPlayers
    state <- newVar (GameState [] [] numPlayers 1 0)

    zoom <- newVar initialZoom
    drawing <- initSVG drawingId
    canvas <- selectId drawingId
    options <- newVar []
    pointer <- svgCircle drawing pointerRadius >>= setClass "pointer"
    setXY (-10) (-10) pointer
    conn <- getWSConnection

    svgViewBox drawing 0 0 (eXMax outerExtents + canvasPadding)
                           (eYMax outerExtents + canvasPadding)

    _ <- subscribe zoom $ \z ->
        svgSize (z * (eXMax outerExtents + canvasPadding))
                (z * (eYMax outerExtents + canvasPadding))
                drawing

    draw td drawing
    set zoom initialZoom

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
            (sendText conn . serializeCommand . Move) (P x y)
            removeOptions
            set options []

    zoomInBtn <- selectId "zoom-in"
    addEvent zoomInBtn "click" $ \_ -> modify zoom zoomIn

    zoomOutBtn <- selectId "zoom-out"
    addEvent zoomOutBtn "click" $ \_ -> modify zoom zoomOut

    conn `onMessage` \e -> do
        t <- getText e
        case deserializeCommand t of
            Nothing -> print $ "Failed decoding message: " T.<> t
            Just (Join name) -> do
                modify state (addPlayer name startPos)
                s <- get state
                let n = length (gsPlayerNames s)
                displayPlayerJoin n name
                let curTrace = getNthTrace s n

                drawMove drawing n (ptPath curTrace)
                when (isReady s) $ if gsThisPlayer s == 1
                        then do
                            displayGameStatus "Choosing next move…"
                            refreshOptions s drawing td (ptPath (getCurrentTrace s)) >>= set options
                        else displayWaitingFor (head (gsPlayerNames s)) 1

            Just (Quit name) -> do
                s <- get state
                let n = length (gsPlayerNames s)
                displayPlayerQuit n name

            Just (Welcome n) -> do
                modify state (setThisPlayer n)
                displayThisPlayerNum n
                displayGameStatus "Waiting for players to join…"

            Just (Move p) -> do
                modify state (moveCurrentPlayer p)
                s <- get state
                let trace = getCurrentTrace s
                drawMove drawing (gsCurrentPlayer s) (ptPath trace)
                if hasWon innerExtents startLine trace
                    then do
                        if thisIsCurrentPlayer s
                            then showWinDialog
                            else showLoseDialog (getCurrentPlayerName s)
                        displayGameStatus "Game over"
                    else do
                        modify state nextPlayer
                        s <- get state
                        when (thisIsCurrentPlayer s) $ do
                            let curTrace = getCurrentTrace s
                            opts <- refreshOptions s drawing td (ptPath curTrace)
                            when (null opts) (sendText conn $ serializeCommand Crashed)
                            set options opts
                        if gsCurrentPlayer s == gsThisPlayer s
                            then displayGameStatus "Choosing next move…"
                            else displayWaitingFor (getCurrentPlayerName s) (gsCurrentPlayer s)

            Just (Chat pId msg) -> do
                s <- get state
                displayChatMsg pId (getNthPlayerName s pId) msg

            Just Crashed -> do
                s <- get state
                drawCrash drawing (head $ ptPath $ getCurrentPlayerTrace s)
                showCrashDialog

            Just (System msg) -> displaySystemMsg msg

            x -> print "ERROR" >> print t >> print x

    joinForm <- selectId "joinForm"
    addEvent joinForm "submit" $ \e -> do
        preventDefault e
        name <- getPlayerName
        when (T.length name > 0) $ do
            displayThisPlayerName name
            (sendText conn . serializeCommand . Join) name
            selectId "join-game-dialog" >>= hide

    selectId "inputName" >>= focusElement

    chatInput <- selectId "chatInput"
    addEvent chatInput "keydown" $ \e ->
        when (eventKey e == 13) $ do
            preventDefault e
            s <- get state
            msg <- getChatMessage
            (sendText conn . serializeCommand) (Chat (gsThisPlayer s) msg)
            clearChatMessage

    urlField <- selectId "urlField"
    url <- getURL
    setTextValue urlField url

    return ()

hasWon :: Extremes -> Line -> PlayerTrace -> Bool
hasWon insides startLine PlayerTrace{..} =
    encapsulates insides ptExtents && hasWinningMove ptPath startLine

addPoint :: Point -> PlayerTrace -> PlayerTrace
addPoint p (PlayerTrace ps ex) = PlayerTrace (p:ps) (updateExtremes p ex)

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
main = addWindowEvent "load" initGame
