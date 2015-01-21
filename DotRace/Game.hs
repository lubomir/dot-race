module DotRace.Game where

import ClassyPrelude
import System.Random
import Text.Printf

import SharedTypes

type GameId = Text

type Player = Text

data Game = Game { gameTrack :: Track
                 , gameNumPlayers :: Int
                 , gamePlayers :: TMVar [Player]
                 , gameChannel :: TChan Text
                 }

mkGameId :: IO GameId
mkGameId = fmt . randoms <$> newStdGen
  where
    fmt :: [Word8] -> GameId
    fmt = fromString . concatMap (printf "%02x") . take 10
