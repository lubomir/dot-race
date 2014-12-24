module Handler.Game where

import Import

getGameR :: GameId -> Handler Html
getGameR gid = do
    games <- appGames <$> getYesod >>= liftIO . readIORef
    let game = lookup gid games
    when (isNothing game) notFound
    defaultLayout $ do
        setTitleI MsgDotRace
        $(widgetFile "game-screen")
