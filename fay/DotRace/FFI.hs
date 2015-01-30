{-# LANGUAGE OverloadedStrings, RebindableSyntax #-}
module DotRace.FFI ( module DotRace.FFI.Raw
                   , eventLocation
                   , displayPlayerJoin
                   , displayPlayerQuit
                   , displayWaitingFor
                   , showWinDialog
                   , showCrashDialog
                   , showLoseDialog
                   , displayChatMsg
                   , displaySystemMsg
                   , setMaxLimit
                   ) where

import Prelude
import Data.Text (Text, showInt, fromString, (<>))

import FFI.Types
import SharedTypes (Track, readI)
import DotRace.FFI.Raw

eventLocation :: Element -> Event -> Fay (Double, Double)
eventLocation element ev = do
    r <- getBoundingClientRect element
    st <- getScrollTop element
    sl <- getScrollLeft element
    t <- rectTop r
    l <- rectLeft r
    x <- eventPageX ev
    y <- eventPageY ev
    return (x - l + sl, y - t + st)

setMaxLimit :: Element -> Int -> Fay ()
setMaxLimit el m = do
    setMax el m
    cur <- getValue el
    when (readI cur > m) (setValue el m)

showCrashDialog :: Fay ()
showCrashDialog = showDialog "crash-dialog"

displayPlayerJoin :: Int -> Text -> Fay ()
displayPlayerJoin = displayPlayerAction "joined"

displayPlayerQuit :: Int -> Text -> Fay ()
displayPlayerQuit = displayPlayerAction "left"

displayPlayerAction :: Text -> Int -> Text -> Fay ()
displayPlayerAction act i name =
    displaySystemMsg ("<span class='player-" <> showInt i
                   <> "'>" <> name <> "</span> has " <> act
                   <> ".")

displaySystemMsg :: Text -> Fay ()
displaySystemMsg = displayChatMsg 0 "System"

showWinDialog :: Fay ()
showWinDialog = showDialog "win-dialog"

showLoseDialog :: Text -> Fay ()
showLoseDialog winner = setWinner winner >> showDialog "lose-dialog"

displayWaitingFor :: Text -> Int -> Fay ()
displayWaitingFor n i =
    displayGameStatus $ "Waiting for <span class='player-"
                     <> showInt i <> "'>" <> n <> "</span>…"

displayChatMsg :: Int -> Text -> Text -> Fay ()
displayChatMsg i name msg = do
    displayChatMsgRaw $ "<p><span class='player-" <> showInt i <> "'>" <> name
                      <> "</span>: " <> msg <> "</p>"
    scrollChat
