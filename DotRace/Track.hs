module DotRace.Track where

import           ClassyPrelude.Yesod       hiding (replace)
import           Control.Exception         (throw)
import           Data.Aeson                (withObject)
import           Data.Aeson                (Result (..), fromJSON)
import qualified Data.Map.Strict           as M
import           Data.Text                 (replace, toTitle)
import           Data.Yaml                 (decodeEither')
import           Filesystem.Path.CurrentOS
import           System.Directory

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


loadTracks :: FilePath -> IO (Map Text Track)
loadTracks dir = do
    files <- map fromString <$> getDirectoryContents (fpToString dir)
    M.fromList <$> mapM loadTrack (filter isYaml files)
  where
    loadTrack :: FilePath -> IO (Text, Track)
    loadTrack p = do
        let name = getName p
        val <- (either throw id . decodeEither') <$> readFile (dir </> p)
        case fromJSON val of
            Error e -> error e
            Success track -> return (name, track)

    getName :: FilePath -> Text
    getName = toTitle . replace "-" " " . either id id . toText . basename

    isYaml p = p `hasExtension` "yaml"
