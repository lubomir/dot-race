module Constants where

import SharedTypes (Point(..))

zoomIncrement :: Double
zoomIncrement = 5

initialZoom :: Double
initialZoom = 20

minZoom :: Double
minZoom = 5

maxZoom :: Double
maxZoom = 40

tickRadius :: Double
tickRadius = 0.4

pointerRadius :: Double
pointerRadius = 0.3

optionRadius :: Double
optionRadius = 0.3

canvasPadding :: Double
canvasPadding = 0.5

crashMark :: Point -> [Point]
crashMark (P x y) = pts
  where
    s = 0.5
    q1 = s * 0.2
    q2 = s * 0.4
    q3 = s * 0.7
    pts = [ P (x + q1) (y - q2)
          , P (x + q3) (y - q3)
          , P (x + q2) (y - q1)
          , P (x + s )  y

          , P (x + q2) (y + q1)
          , P (x + q3) (y + q3)
          , P (x + q1) (y + q2)
          , P  x       (y + s )

          , P (x - q1) (y + q2)
          , P (x - q3) (y + q3)
          , P (x - q2) (y + q1)
          , P (x - s )  y

          , P (x - q2) (y - q1)
          , P (x - q3) (y - q3)
          , P (x - q1) (y - q2)
          , P  x       (y - s )
          ]
