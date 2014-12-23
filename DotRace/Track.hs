module DotRace.Track where

import ClassyPrelude.Yesod
import Data.Aeson                  (withObject)

type Coords = (Double, Double)

data Track = Track { trackInner     :: [Coords]
                   , trackOuter     :: [Coords]
                   , trackStartLine :: (Coords, Coords)
                   , trackStartPos  :: [Coords]
                   } deriving (Show)

instance FromJSON Track where
    parseJSON = withObject "Track" $ \o -> do
        trackInner      <- o .: ":inner"
        trackOuter      <- o .: ":outer"
        trackStartLine  <- o .: ":start_line"
        trackStartPos   <- o .: ":start_pos"
        return Track {..}
