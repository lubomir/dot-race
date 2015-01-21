{-# LANGUAGE RecordWildCards #-}
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
gameApp _gid Game{..} = do
    cmd <- receiveData
    liftIO $ print cmd
    liftIO $ print $ deserializeCommand cmd
    (readChan, players) <- case deserializeCommand cmd of
        Just (Join name) -> atomically $ do
            -- TODO handle full game
            modifyTMVar gamePlayers (flip (++) [name])
            players <- readTMVar gamePlayers
            writeTChan gameChannel (serializeCommand (Joined name))
            c <- dupTChan gameChannel
            return (c, players)
        _ -> invalidArgs ["Expected Join command"]
    liftIO $ print players
    mapM_ (sendTextData . serializeCommand . Joined) players
    race_
        (forever $ atomically (readTChan readChan) >>= sendTextData)
        (sourceWS $$ mapM_C (atomically . writeTChan gameChannel))

-- This is wrong!
modifyTMVar :: TMVar a -> (a -> a) -> STM ()
modifyTMVar var f = readTMVar var >>= putTMVar var . f
