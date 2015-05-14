-- Type Classes are similar to "interfaces" in Java
-- and "protocol" in Objective-C

module Main where

import Prelude hiding ((==), (/=))

-- An example definition of Type Class `Eq`
class Eq a where
    (==) :: a -> a -> Bool
    (/=) :: a -> a -> Bool
    -- default operators
    x /= y = not (x == y)
    x == y = not (x /= y)

main ::  IO ()
main = print "hello"


data Point2 = Point2 Double Double
data Point3 = Point3 Double Double Double

-- Too much repetitions just to handle these 2 data types

distance2 :: Point2 -> Point2 -> Double
distance2 (Point2 x1 y1)  (Point2 x2 y2) =
    sqrt (dx * dx + dy * dy)
    where dx = x1 - x2
          dy = y1 - y2

distance3 :: Point3 -> Point3 -> Double
distance3 (Point3 x1 y1 z1)  (Point3 x2 y2 z2) =
    sqrt (dx * dx + dy * dy + dz * dz)
    where dx = x1 - x2
          dy = y1 - y2
          dz = z1 - z2

pathLength2 :: [Point2] -> Double
pathLength2 [] = 0
pathLength2 (_ : []) = 0
pathLength2 (p0 : p1 : p2) =
    distance2 p0 p1 + pathLength2 (p1 : p2)

pathLength3 :: [Point3] -> Double
pathLength3 [] = 0
pathLength3 (_ : []) = 0
pathLength3 (p0 : p1 : ps) =
    distance3 p0 p1 + pathLength3 (p1 : ps)

-- Let's simplify with a type class
-- we reuse "distance" function which can be applied to
-- different data types

class Measurable a where
    distance :: a -> a -> Double

instance Measurable Point2 where
    distance = distance2

instance Measurable Point3 where
    distance (Point3 x1 y1 z1)  (Point3 x2 y2 z2) =
        sqrt (dx * dx + dy * dy + dz * dz)
        where dx = x1 - x2
              dy = y1 - y2
              dz = z1 - z2

instance Measurable Double where
    distance x y = abs (x - y)

-- Polymorphic function that can handle any type `a`
-- using the appropriate distance function above
pathLength :: Measurable a => [a] -> Double
pathLength [] = 0
pathLength (_ : []) = 0
pathLength (p0 : p1 : ps) =
    distance p0 p1 + pathLength (p1 : ps)
