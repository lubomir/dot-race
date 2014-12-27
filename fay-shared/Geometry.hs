module Geometry where

import SharedTypes

-- $setup
-- >>> :{
--  let path = zipWith P [0, 4, 7,  6,  1, -1, -3, -6, -7, -3]
--                       [4, 5, 2, -2, -4,  1, -2, -1,  3,  5]
--  :}


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

-- |Check if point has nearly integral coordinates.
--
-- >>> isAligned (P 1 1)
-- True
-- >>> isAligned (P 1.5 1.5)
-- False
-- >>> isAligned (P 1.1 1)
-- False
--
-- prop> \x y -> isAligned (P (fromIntegral x) (fromIntegral y))
--
isAligned :: Point -> Bool
isAligned p = p == clamp p

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


-- |Format a line into human readable string
--
prettyLine :: Line -> String
prettyLine (Line p1 p2) = concat [ show (_x p1) , "x" , show (_y p1)
                                 , "--"
                                 , show (_x p2) , "x" , show (_y p2)]

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

-- |Find extremes of a path.
--
-- >>> extents path
-- (-7.0,-4.0,7.0,5.0)
--
extents :: Path -> (Double, Double, Double, Double)
extents p = (go minimum _x p, go minimum _y p, go maximum _x p, go maximum _y p)
  where
    go f g = f . map g

-- |Get a list of line segments for given polygon.
--
-- >>> map prettyLine $ getSegments [P 0 0, P 0 1, P 1 1, P 1 0]
-- ["0.0x0.0--0.0x1.0","0.0x1.0--1.0x1.0","1.0x1.0--1.0x0.0","1.0x0.0--0.0x0.0"]
--
getSegments :: Polygon -> [Line]
getSegments [] = []
getSegments pg@(p:ps) = zipWith Line pg (ps ++ [p])

-- |Check if a point lies inside a polygon.
--
-- >>> P 0 0 `isInside` path
-- True
-- >>> P 2 (-3) `isInside` path
-- True
-- >>> P 6 2 `isInside` path
-- True
-- >>> P 4 4 `isInside` path
-- True
-- >>> P 3 (-1) `isInside` path
-- True
-- >>> P (-6) 3 `isInside` path
-- True
-- >>> P 10 0 `isInside` path
-- False
-- >>> P (-10) 0 `isInside` path
-- False
-- >>> P 0 10 `isInside` path
-- False
-- >>> P 0 (-10) `isInside` path
-- False
--
isInside :: Point -> Polygon -> Bool
isInside p pg
  | length pg < 3 = False
  | otherwise = (odd . length . filter inters . getSegments) pg
  where inters (Line p1 p2) =  _y p >  min (_y p1) (_y p2)
                            && _y p <= max (_y p1) (_y p2)
                            && _x p <= max (_x p1) (_x p2)
                            && not (almostEqual (_y p1) (_y p2))
                            && correctInters p1 p2
        correctInters p1 p2 =
            let xinters = (_y p - _y p2) * (_x p1 - _x p2) / (_y p1 - _y p2) + _x p2
            in _x p1 == _x p2 || _x p <= xinters

-- |Check if line intersects a polygon.
--
-- >>> intersectsWith (Line (P 1 3) (P 1 6)) path
-- True
-- >>> intersectsWith (Line (P (-4) (-3)) (P 1 (-2))) path
-- True
-- >>> intersectsWith (Line (P (-4) (-3)) (P (-5) 6)) path
-- True
-- >>> intersectsWith (Line (P 5 3) (P 1 6)) path
-- True
-- >>> intersectsWith (Line (P 1 3) (P 5 3)) path
-- False
-- >>> intersectsWith (Line (P 1 6) (P (-5) (-6))) path
-- True
-- >>> intersectsWith (Line (P 1 6) (P (-5) 6)) path
-- False
--
intersectsWith :: Line -> Polygon -> Bool
intersectsWith l pg
  | length pg < 3 = False
  | otherwise = any (hasIntersection l) (getSegments pg)
