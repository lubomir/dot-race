module Handler.NewGame where

import Import

import Yesod.Form.Bootstrap3
import Data.Map (keys, insert)

data NewGame = NewGame { track :: Text
                       , numPlayers :: Int
                       }


getTrackOptions :: Handler (OptionList Text)
getTrackOptions = mkOptionList . map mkOption . keys . appTracks <$> getYesod
  where
    mkOption t = Option t t t

newGameForm :: Form NewGame
newGameForm = renderBootstrap3 BootstrapBasicForm $ NewGame
    <$> areq (selectField getTrackOptions) (bfs MsgTrack) Nothing
    <*> areq intField  (addMin $ bfs MsgNumPlayers) Nothing
  where
    addMin fs = fs { fsAttrs = ("min", "1") : fsAttrs fs }

getNewGameR :: Handler Html
getNewGameR = do
    (widget, enctype) <- generateFormPost newGameForm
    defaultLayout $ do
        setTitleI MsgNewGame
        $(widgetFile "new-game")

postNewGameR :: Handler Html
postNewGameR = do
    ((res, widget), enctype) <- runFormPost newGameForm
    case res of
        FormSuccess g -> do
            mtrack <- lookup (track g) . appTracks <$> getYesod
            case mtrack of
                Nothing -> return ()
                Just track -> do
                    chan <- atomically newBroadcastTChan
                    players <- atomically $ newTMVar []
                    let game = Game { gameTrack = track
                                    , gameNumPlayers = numPlayers g
                                    , gamePlayers = players
                                    , gameChannel = chan
                                    }
                    games <- appGames <$> getYesod
                    gameId <- liftIO mkGameId
                    atomicModifyIORef' games (\m -> (insert gameId game m, ()))
                    redirect (GameR gameId)
        _ -> return ()
    defaultLayout $ do
            setTitleI MsgNewGame
            $(widgetFile "new-game")
