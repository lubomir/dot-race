{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
module SharedTypes where

import Prelude

#ifndef FAY
import Data.Aeson
import Control.Applicative
import Data.Monoid
#endif


data Point = P { _x :: Double
               , _y :: Double
               } deriving (Show)

data Line = Line Point Point

type Polygon = Path

type Path = [Point]

data Track = Track { trackInner     :: [Point]
                   , trackOuter     :: [Point]
                   , trackStartLine :: (Point, Point)
                   , trackStartPos  :: [Point]
                   }

#ifndef FAY
instance FromJSON Point where
    parseJSON (Object o) = P <$> o .: "x" <*> o .: "y"
    parseJSON arr@(Array _) = uncurry P <$> parseJSON arr
    parseJSON _ = mempty

instance ToJSON Point where
    toJSON (P x y) = object [ "instance" .= ("P" :: String)
                            , "_x" .= x
                            , "_y" .= y
                            ]

deriving instance Show Track

instance FromJSON Track where
    parseJSON = withObject "Track" $ \o -> do
        trackInner      <- o .: ":inner"
        trackOuter      <- o .: ":outer"
        trackStartLine  <- o .: ":start_line"
        trackStartPos   <- o .: ":start_pos"
        return Track {..}

instance ToJSON Track where
    toJSON (Track {..}) = object [ "trackInner" .= trackInner
                                 , "trackOuter" .= trackOuter
                                 , "trackStartLine" .= trackStartLine
                                 , "trackStartPos" .= trackStartPos
                                 , "instance" .= ("Track" :: String)
                                 ]
#endif
