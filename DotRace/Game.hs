module DotRace.Game where

import qualified Network.WebSockets as WS

import ClassyPrelude
import DotRace.Track

type GameId = Text

type Player = (Text, WS.Connection)

data Game = Game { gameTrack :: Track
                 , gameNumPlayers :: Int
                 , gamePlayers :: [Player]
                 }
