module Handler.Game where

import Import hiding (readChan, writeChan)

import Data.Aeson (encode)
import Yesod.WebSockets

getGameR :: GameId -> Handler Html
getGameR gid = do
    games <- appGames <$> getYesod >>= liftIO . readIORef
    case lookup gid games of
        Nothing -> notFound
        Just game -> do
            webSockets $ gameApp gid game
            let trackData = decodeUtf8 $ toStrict $ encode (gameTrack game)
            defaultLayout $ do
                setTitleI MsgDotRace
                addScript (StaticR js_svg_js)
                addScript (StaticR js_jquery_2_1_3_min_js)
                addScript (StaticR js_bootstrap_min_js)
                $(widgetFile "game-screen")
                $(fayFile "GameScreen")


gameApp :: GameId -> Game -> WebSocketsT Handler ()
gameApp _gid game = do
    -- TODO: handle creating new user
    let writeChan = gameChannel game
    readChan <- atomically $ do
        -- TODO: send join message to write chan
        dupTChan writeChan
    race_
        (forever $ atomically (readTChan readChan) >>= sendTextData)
        (sourceWS $$ mapM_C (atomically . writeTChan writeChan))
