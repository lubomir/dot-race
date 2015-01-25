{-# LANGUAGE RecordWildCards #-}
module Handler.Game where

import Import hiding (readChan, writeChan, delete)

import Data.List (delete)
import Data.Aeson (encode)
import Yesod.WebSockets
import Network.WebSockets (ConnectionException)
import qualified Data.Map.Strict as M

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
                | JoinOk (TChan Text) Player [Player]
                | InvalidCommand
                deriving (Eq)

gameApp :: GameId -> Game -> WebSocketsT Handler ()
gameApp gid Game{..} = do
    cmd <- receiveData
    res <- case deserializeCommand cmd of
        Just (Join name) -> atomically $ do
            mplayers <- readMayAdd gameNumPlayers name gamePlayers
            case mplayers of
                Nothing -> return GameFull
                Just players -> do
                    writeTChan gameChannel (serializeCommand (Join name))
                    c <- dupTChan gameChannel
                    return (JoinOk c name players)
        _ -> return InvalidCommand
    case res of
        GameFull -> send (System "Game is full already.")
        InvalidCommand -> send (System "Expected JOIN command.")
        JoinOk readChan name players -> do
            (sendTextData . serializeCommand . Welcome) (length players)
            mapM_ (sendTextData . serializeCommand . Join) players
            handle (connectionClosed gameChannel gamePlayers name) $ do
                race_
                    (forever $ atomically (readTChan readChan) >>= sendTextData)
                    (sourceWS $$ mapM_C (atomically . writeTChan gameChannel))
            remaining <- atomically $ readTMVar gamePlayers
            when (null remaining) $ do
                games <- appGames <$> getYesod
                liftIO $ atomicModifyIORef' games (\m -> (M.delete gid m, ()))
  where
    send = sendTextData . serializeCommand
    connectionClosed :: TChan Text
                     -> TMVar [Player]
                     -> Player
                     -> ConnectionException
                     -> WebSocketsT Handler ()
    connectionClosed chan players name _ = atomically $ do
        writeTChan chan (serializeCommand (Quit name))
        remove players name

-- |Read a TMVar containing a list. If the list is shorter than given limit,
-- append a value to it, update the TMVar with it and return new list.
-- Otherwise return Nothing.
--
readMayAdd :: Int -> a -> TMVar [a] -> STM (Maybe [a])
readMayAdd maxLen x var = do
    xs <- readTMVar var
    if length xs >= maxLen
        then return Nothing
        else do let xs' = xs ++ [x]
                _ <- takeTMVar var
                putTMVar var xs'
                return (Just xs')

remove :: Eq a => TMVar [a] -> a -> STM ()
remove var x = do
    xs <- takeTMVar var
    putTMVar var (delete x xs)
