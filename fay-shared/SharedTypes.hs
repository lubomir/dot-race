{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
module SharedTypes where


#ifndef FAY
import ClassyPrelude
import Data.Aeson
import Data.Text
#else
import Prelude
import FFI
import Data.Maybe
import Data.Text (Text, fromString, (<>))
import qualified Data.Text as T
#endif


data Point = P { _x :: Double
               , _y :: Double
               }

data Line = Line Point Point

type Polygon = Path

type Path = [Point]

data Track = Track { trackInner     :: [Point]
                   , trackOuter     :: [Point]
                   , trackStartLine :: (Point, Point)
                   , trackStartPos  :: [Point]
                   }

data Extremes = Extremes { eXMin :: Double
                         , eXMax :: Double
                         , eYMin :: Double
                         , eYMax :: Double
                         }

data Command = Join Text    -- ^Player name
             | Move Point
             | Welcome Int  -- ^Player number

#ifndef FAY
deriving instance Show Point

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

deriving instance Show Command
#endif

serializeCommand :: Command -> Text
serializeCommand (Join name)   = fromString "join\t" <> name
serializeCommand (Move p)      = fromString "move\t" <> tshow (_x p)
                                                     <> fromString "\t"
                                                     <> tshow (_y p)
serializeCommand (Welcome i)   = fromString "welcome\t" <> tshowI i

deserializeCommand :: Text -> Maybe Command
deserializeCommand t = go $ splitOn (fromString "\t") t
  where go [cmd, arg]
          | cmd == fromString "join"    = Just (Join arg)
          | cmd == fromString "welcome" = Just (Welcome (readI arg))
          | otherwise =  Nothing
        go [cmd, arg1, arg2]
          | cmd == fromString "move" = Just (Move (P (read arg1) (read arg2)))
          | otherwise = Nothing
        go _ = Nothing

#ifdef FAY
tshow :: Double -> Text
tshow = ffi "'' + %1"

tshowI :: Int -> Text
tshowI = ffi "'' + %1"

splitOn :: Text -> Text -> [Text]
splitOn p t = T.splitOn (T.head p) t

read :: Text -> Double
read = ffi "parseInt(%1, 10)"

readI :: Text -> Int
readI = ffi "parseInt(%1, 10)"
#else
read, readI :: Read a => Text -> a
read val = case readMay val of
    Nothing -> error "Expected something else"
    Just x -> x

readI = read

tshowI :: Int -> Text
tshowI = tshow
#endif
