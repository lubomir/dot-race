module DotRace.Game where

import qualified Network.WebSockets as WS

import ClassyPrelude
import System.Random
import Text.Printf

import DotRace.Track

type GameId = Text

type Player = (Text, WS.Connection)

data Game = Game { gameTrack :: Track
                 , gameNumPlayers :: Int
                 , gamePlayers :: [Player]
                 }

mkGameId :: IO GameId
mkGameId = fmt . randoms <$> newStdGen
  where
    fmt :: [Word8] -> GameId
    fmt = fromString . concatMap (printf "%02x") . take 10
