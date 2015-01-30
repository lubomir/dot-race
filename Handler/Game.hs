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
                        addScript (StaticR js_fay_runtime_js)
                        addScript (StaticR js_game_screen_js)

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
                    started <- readTMVar gameStarted
                    if started
                        then return GameFull
                        else do
                            when (length players == gameNumPlayers)
                                (void $ swapTMVar gameStarted True)
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
            handle connectionClosed $ race_
                (forever $ atomically (readTChan readChan) >>= sendTextData)
                (sourceWS $$ mapM_C (atomically . writeTChan gameChannel))
            remaining <- atomically $ do
                writeTChan gameChannel (serializeCommand (Quit name))
                removeFromTMVar gamePlayers name
            when (null remaining) $ do
                games <- appGames <$> getYesod
                liftIO $ atomicModifyIORef' games (\m -> (M.delete gid m, ()))
  where
    send = sendTextData . serializeCommand
    connectionClosed :: ConnectionException -> WebSocketsT Handler ()
    connectionClosed _ = return ()

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

removeFromTMVar :: Eq a => TMVar [a] -> a -> STM [a]
removeFromTMVar var x = do
    xs <- takeTMVar var
    let res = delete x xs
    putTMVar var res
    return res
