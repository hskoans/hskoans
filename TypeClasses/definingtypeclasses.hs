-- Type Classes are similar to "interfaces" in Java
-- and "protocol" in Objective-C

module Main where

import Prelude hiding (/=)

-- An example definition of Type Class `Eq`
class Eq a where
    (==) :: a -> a -> Bool
    (/=) :: a -> a -> Bool
    -- default operators
    x /= y = not (x == y)
    x == y = not (x /= y)
    

main = print "hello"
