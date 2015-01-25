{-# LANGUAGE OverloadedStrings, RebindableSyntax #-}
module NewGame where

import Prelude
import Data.Text (Text, fromString)

import FFI.Custom
import FFI.Types
import SharedTypes

setMaxVal :: Element -> Element -> Fay ()
setMaxVal trackSel numPSel = do
	opt <- getSelectedValue trackSel
	case splitOn ":" opt of
		[_, max] -> setMaxLimit numPSel (readI max)
		_ -> return ()

initForm :: Event -> Fay ()
initForm _ = do
	trackSelector <- selectId "hident2"
	numPlayerSelector <- selectId "hident3"
	addEvent trackSelector "change" $ \_ -> setMaxVal trackSelector numPlayerSelector
	setMaxVal trackSelector numPlayerSelector

main = addWindowEvent "load" initForm
