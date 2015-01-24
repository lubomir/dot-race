{-# LANGUAGE RecordWildCards, OverloadedStrings, RebindableSyntax #-}
module NewGame where

import Prelude
import FFI
import Data.Text (Text, fromString)

import FFI.Custom
import FFI.Types
import SharedTypes

initForm :: Event -> Fay ()
initForm _ = do
	trackSelector <- selectId "hident2"
	numPlayerSelector <- selectId "hident3"
	addEvent trackSelector "change" $ \e -> do
		opt <- getSelectedValue trackSelector
		case splitOn ":" opt of
			[_, max] -> setMaxLimit numPlayerSelector (readI max)
			_ -> return ()

main = addWindowEvent "load" initForm
