module Handler.NewGame where

import Import

import Yesod.Form.Bootstrap3
import Data.Map (assocs, insert)

data NewGame = NewGame { track :: Track
                       , numPlayers :: Int
                       } deriving (Show)


getTrackOptions :: Handler (OptionList Track)
getTrackOptions = mkOptionList . map mkOption . assocs . appTracks <$> getYesod
  where
    mkOption (n, t) = Option n t (n <> ":" <> tshow (length $ trackStartPos t))

newGameForm :: Form NewGame
newGameForm = renderBootstrap3 BootstrapBasicForm $ formToAForm $ do
    (tr, tw) <- mreq (selectField getTrackOptions) (bfs MsgTrack) Nothing
    (nr, nw) <- mreq (checkM (hasCorrectValFor tr) intField)
                     (addMin $ bfs MsgNumPlayers) Nothing
    return (NewGame <$> tr <*> nr, [tw, nw])
  where
    addMin fs = fs { fsAttrs = ("min", "1") : fsAttrs fs }

getNewGameR :: Handler Html
getNewGameR = do
    (widget, enctype) <- generateFormPost newGameForm
    defaultLayout $ do
        setTitleI MsgNewGame
        $(fayFile "NewGame")
        $(widgetFile "new-game")

hasCorrectValFor :: FormResult Track -> Int -> Handler (Either Text Int)
hasCorrectValFor (FormSuccess track) n
  | n < 1      = return $ Left "Need at least one player."
  | n > maxNum = return $ Left ("This track can only support "
                             <> tshow maxNum <> " players.")
  | otherwise  = return $ Right n
  where maxNum = length (trackStartPos track)
hasCorrectValFor _ n
  | n < 1      = return $ Left "Need at least one player."
  | otherwise  = return $ Right n

postNewGameR :: Handler Html
postNewGameR = do
    ((res, widget), enctype) <- runFormPost newGameForm
    case res of
        FormSuccess g -> do
            (chan, players, started) <- atomically ((,,) <$> newBroadcastTChan
                                                         <*> newTMVar []
                                                         <*> newTMVar False)
            let game = Game { gameTrack = track g
                            , gameNumPlayers = numPlayers g
                            , gamePlayers = players
                            , gameChannel = chan
                            , gameStarted = started
                            }
            games <- appGames <$> getYesod
            gameId <- liftIO mkGameId
            atomicModifyIORef' games (\m -> (insert gameId game m, ()))
            redirect (GameR gameId)
        _ -> return ()
    defaultLayout $ do
        setTitleI MsgNewGame
        $(widgetFile "new-game")
        $(fayFile "NewGame")
