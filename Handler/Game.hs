module Handler.Game where

import Import

import Data.Aeson (encode)

getGameR :: GameId -> Handler Html
getGameR gid = do
    games <- appGames <$> getYesod >>= liftIO . readIORef
    case lookup gid games of
        Nothing -> notFound
        Just game -> do
            let trackData = decodeUtf8 $ toStrict $ encode (gameTrack game)
            defaultLayout $ do
                setTitleI MsgDotRace
                addScript (StaticR js_svg_js)
                addScript (StaticR js_jquery_2_1_3_min_js)
                $(widgetFile "game-screen")
                $(fayFile "GameScreen")
