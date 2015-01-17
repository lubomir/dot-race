{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS -fno-warn-orphans #-}
module Geometry ( extents
                , translate
                , clamp
                , translatePoint
                , distance
                , getSegments
                , intersectsWithAnyBounded
                , BoundedLine
                , boundedLine
                ) where

import Prelude

import SharedTypes

data BoundedLine = BoundedLine { p1             :: Point
                               , p2             :: Point
                               , leftMost       :: Double
                               , rightMost      :: Double
                               , topMost        :: Double
                               , bottomMost     :: Double
                               }

boundedLine :: Point -> Point -> BoundedLine
boundedLine p1@(P x1 y1) p2@(P x2 y2) = BoundedLine {..}
  where leftMost = min x1 x2
        rightMost = max x1 x2
        topMost = min y1 y2
        bottomMost = max y1 y2

-- $setup
-- >>> :{
--  let path = zipWith P [0, 4, 7,  6,  1, -1, -3, -6, -7, -3]
--                       [4, 5, 2, -2, -4,  1, -2, -1,  3,  5]
--      prettyLine bl = concat [ show (_x $ p1 bl) , "x" , show (_y $ p1 bl)
--                             , "--"
--                             , show (_x $ p2 bl) , "x" , show (_y $ p2 bl)]
-- :}


-- |Check if two numbers are almost equal with precision of 99.99 %.
--
-- This function is specialized to Double, but would work with any type
-- satisfying `(Ord a, Fractional a)`.
--
-- prop> \x -> almostEqual x (1.000001 * x)
-- prop> \x -> x /= 0.0 ==> not (almostEqual x (1.001 * x))
--
almostEqual :: Double -> Double -> Bool
almostEqual a b
  | a == b    = True
  | otherwise = let relativeError = abs ((a - b) / if abs b > abs a then b else a)
                in relativeError <= 0.0001

instance Eq Point where
    (P x1 y1) == (P x2 y2) = almostEqual x1 x2 && almostEqual y1 y2


-- |Compute length of the hypothenuse of a right-angled triangle with sides of
-- length `x` and `y`.
--
-- >>> hypot 3 4
-- 5.0
-- >>> hypot 1 1
-- 1.4142135623730951
--
hypot :: Double -> Double -> Double
hypot x y = sqrt (x * x + y * y)

-- |Compute distance of two points.
--
-- >>> distance (P 0 0) (P 0 1)
-- 1.0
-- >>> distance (P 1 1) (P 2 2)
-- 1.4142135623730951
--
-- prop> \x1 y1 x2 y2 -> x1 /= x2 && y1 /= y2 ==> distance (P x1 y1) (P x2 y2) > 0
--
distance :: Point -> Point -> Double
distance a b = hypot (_x a - _x b) (_y a - _y b)

-- |Round point to nearest integer coordinates. This takes a round trip through
-- Int, so it will not work for arbitrarily larger numbers.
--
-- >>> clamp (P 1 1)
-- P {_x = 1.0, _y = 1.0}
-- >>> clamp (P 1.2 1.8)
-- P {_x = 1.0, _y = 2.0}
--
clamp :: Point -> Point
clamp (P x y) = P (roundI x) (roundI y)
  where roundI = (fromIntegral :: Int -> Double) . round


--

-- |Check if a point lies on a line.
--
-- >>> let line = Line (P 0 0) (P 1 0)
-- >>> hasPoint line (P 1 0)
-- True
-- >>> hasPoint line (P 0.5 0)
-- True
-- >>> hasPoint line (P 0.5 1)
-- False
--
hasPoint :: Line -> Point -> Bool
hasPoint (Line p1 p2) p
  | almostEqual (_x p1) (_x p2) = almostEqual (_x p) (_x p1)    -- Vertical line
  | otherwise = let a = (_y p1 - _y p2) / (_x p1 - _x p2)
                    b = _y p1 - a * _x p1
                in almostEqual (_y p) (a * _x p + b)

-- |Check if point lies on a line segment.
--
-- >>> let line = Line (P 0 0) (P 1 0)
-- >>> segmentHasPoint line (P 1 0)
-- True
-- >>> segmentHasPoint line (P 2 0)
-- False
--
segmentHasPoint :: Line -> Point -> Bool
segmentHasPoint l@(Line p1 p2) p
  | not (hasPoint l p) = False
  | otherwise = let len = distance p1 p2
                in distance p1 p <= len && distance p2 p <= len

-- |Check if two line segments have an intersection.
--
-- Done by finding the intersection and then checking it lies between the
-- points specifying the lines.
--
-- Algorithm from Wikipedia [2012-08-09]:
-- [[https://en.wikipedia.org/wiki/Line-line_intersection]]
--
-- >>> let l1 = Line (P 0 0)    (P 3 0)
-- >>> let l2 = Line (P 2 (-1)) (P (-1) 2)
-- >>> let l3 = Line (P 3 0)    (P 0 3)
-- >>> let l4 = Line (P 1 (-1)) (P 1 4)
-- >>> let l5 = Line (P 0 0)    (P 0 3)
-- >>> let l6 = Line (P 10 0)   (P 11 0)
--
-- >>> hasIntersection l1 l2
-- True
-- >>> hasIntersection l1 l3
-- True
-- >>> hasIntersection l1 l4
-- True
-- >>> hasIntersection l1 l5
-- True
--
-- >>> hasIntersection l2 l4
-- True
-- >>> hasIntersection l2 l5
-- True
--
-- >>> hasIntersection l3 l4
-- True
-- >>> hasIntersection l3 l5
-- True
--
-- >>> hasIntersection l2 l3
-- False
-- >>> hasIntersection l4 l5
-- False
--
-- >>> hasIntersection l6 l1
-- False
-- >>> hasIntersection l6 l2
-- False
-- >>> hasIntersection l6 l3
-- False
-- >>> hasIntersection l6 l4
-- False
-- >>> hasIntersection l6 l5
-- False
--
hasIntersection :: Line -> Line -> Bool
hasIntersection l1@(Line (P x1 y1) (P x2 y2)) l2@(Line (P x3 y3) (P x4 y4)) =
    let det = (x1 - x2) * (y3 - y4) - (y1 - y2) * (x3 - x4)
    in det /= 0 &&
        (let px = (x1 * y2 - y1 * x2) * (x3 - x4) - (x1 - x2) * (x3 * y4 - y3 * x4)
             py = (x1 * y2 - y1 * x2) * (y3 - y4) - (y1 - y2) * (x3 * y4 - y3 * x4)
             p = P (px / det) (py / det)
         in segmentHasPoint l1 p && segmentHasPoint l2 p)

hasIntersectionBounded :: BoundedLine -> BoundedLine -> Bool
hasIntersectionBounded bl1 bl2 =
    bottomMost bl1 > topMost bl2 &&
    rightMost bl2 > leftMost bl2 &&
    topMost bl1 < bottomMost bl2 &&
    leftMost bl1 < rightMost bl2 &&
    hasIntersection (Line (p1 bl1) (p2 bl1)) (Line (p1 bl2) (p2 bl2))

-- |Find extremes of a path.
--
-- >>> extents path
-- (-7.0,-4.0,7.0,5.0)
--
extents :: Path -> (Double, Double, Double, Double)
extents = go (1e8, 1e8, 0, 0)
  where
    go acc [] = acc
    go (xmin, ymin, xmax, ymax) (P x y:xs) =
        let xmin' = min x xmin
            ymin' = min y ymin
            xmax' = max x xmax
            ymax' = max y ymax
        in xmin' `seq` ymin' `seq` xmax' `seq` ymax' `seq` go (xmin', ymin', xmax', ymax') xs


-- |Get a list of line segments for given polygon.
--
-- >>> map prettyLine $ getSegments [P 0 0, P 0 1, P 1 1, P 1 0]
-- ["0.0x0.0--0.0x1.0","0.0x1.0--1.0x1.0","1.0x1.0--1.0x0.0","1.0x0.0--0.0x0.0"]
--
getSegments :: Polygon -> [BoundedLine]
getSegments [] = []
getSegments pg@(p0:_) = go pg
  where
    go [] = error "Empty list handled in `getSegments`, this can never happen"
    go [p2] = [boundedLine p2 p0]
    go (p1:p2:pss) = boundedLine p1 p2 : go (p2:pss)

-- |Check if line intersects any line in a list.
--
intersectsWithAnyBounded :: BoundedLine -> [BoundedLine] -> Bool
intersectsWithAnyBounded l = any (hasIntersectionBounded l)

-- |Translate the path by moving every single point.
--
translate :: Double -> Double -> Path -> Path
translate x y = map (translatePoint x y)

-- |Move the point.
--
-- >>> translatePoint 1 1 (P 0 0)
-- P {_x = 1.0, _y = 1.0}
-- >>> translatePoint (-1) 0 (P 2 2)
-- P {_x = 1.0, _y = 2.0}
--
-- prop> \x y -> translatePoint 0 0 (P x y) == P x y
-- prop> \dx dy x y -> translatePoint 0 dy (translatePoint dx 0 (P x y)) == translatePoint dx dy (P x y)
--
translatePoint :: Double -> Double -> Point -> Point
translatePoint x' y' (P x y) = P (x + x') (y + y')
