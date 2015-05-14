-- Ord is a subclass of Eq
--

module Main where

class (Eq a) => Ord a where
    (<) :: a -> a -> Bool
    (>) :: a -> a -> Bool
    (<=) :: a -> a -> Bool
    (>=) :: a -> a -> Bool
    compare :: a -> a -> Ordering'
    max :: a -> a -> a
    min :: a -> a -> a

data Ordering' = LT | EQ | GT

main ::  IO ()
main = print "hello"
