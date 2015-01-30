module DotRace.Track where

import           ClassyPrelude.Yesod       hiding (replace)
import           Control.Exception         (throw)
import           Data.Aeson                (Result (..), fromJSON)
import qualified Data.Map.Strict           as M
import           Data.Text                 (replace, toTitle)
import           Data.Yaml                 (decodeEither')
import           Filesystem.Path.CurrentOS
import           System.Directory

import SharedTypes
import Geometry

loadTracks :: FilePath -> IO (Map Text Track)
loadTracks dir = return M.empty {-do
    files <- map fromString <$> getDirectoryContents (fpToString dir)
    M.fromList <$> mapM loadTrack (filter isYaml files)
  where
    loadTrack :: FilePath -> IO (Text, Track)
    loadTrack p = do
        let name = getName p
        val <- (either throw id . decodeEither') <$> readFile (dir </> p)
        case fromJSON val of
            Error e -> error e
            Success track -> return (name, normalizeTrack track)

    getName :: FilePath -> Text
    getName = toTitle . replace "-" " " . either id id . toText . basename

    isYaml p = p `hasExtension` "yaml"-}


-- | Move track so that there is no empty space on the left or above it.
--
normalizeTrack :: Track -> Track
normalizeTrack Track{..} =
    let (Extremes xmin _ ymin _) = extents trackOuter
        (start, end) = trackStartLine
        x' = negate $ max (xmin - 0.5) 0
        y' = negate $ max (ymin - 0.5) 0
        shift = translate x' y'
        shiftP = clamp . translatePoint x' y'
   in Track { trackInner = shift trackInner
            , trackOuter = shift trackOuter
            , trackStartLine = (shiftP start, shiftP end)
            , trackStartPos = map clamp $ shift trackStartPos
            }
