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
            currentPlayers <- atomically $ readTMVar (gamePlayers game)
            if length currentPlayers >= gameNumPlayers game
                then do
                    setMessage "Game is full already."
                    redirect NewGameR
                else do
                    webSockets $ gameApp gid game
                    let trackData = decodeUtf8 $ toStrict $ encode (gameTrack game)
                    defaultLayout $ do
                        setTitleI MsgDotRace
                        $(widgetFile "game-screen")
                        $(fayFile "GameScreen")

data JoinResult = GameFull
                | JoinOk (TChan Text) [Player]
                | InvalidCommand
                deriving (Eq)

gameApp :: GameId -> Game -> WebSocketsT Handler ()
gameApp _gid Game{..} = do
    cmd <- receiveData
    res <- case deserializeCommand cmd of
        Just (Join name) -> atomically $ do
            mplayers <- readMayAdd gameNumPlayers name gamePlayers
            case mplayers of
                Nothing -> return GameFull
                Just players -> do
                    writeTChan gameChannel (serializeCommand (Join name))
                    c <- dupTChan gameChannel
                    return (JoinOk c players)
        _ -> return InvalidCommand
    case res of
        GameFull -> sendTextData ("Game is full already." :: Text)
        InvalidCommand -> sendTextData ("Expected JOIN command." :: Text)
        JoinOk readChan players -> do
            (sendTextData . serializeCommand . Welcome) (length players)
            mapM_ (sendTextData . serializeCommand . Join) players
            race_
                (forever $ atomically (readTChan readChan) >>= sendTextData)
                (sourceWS $$ mapM_C (atomically . writeTChan gameChannel))

readMayAdd :: Int -> a -> TMVar [a] -> STM (Maybe [a])
readMayAdd maxLen x var = do
    xs <- readTMVar var
    if length xs >= maxLen
        then return Nothing
        else do let xs' = xs ++ [x]
                _ <- takeTMVar var
                putTMVar var xs'
                return (Just xs')
