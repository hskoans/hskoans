-- we can simplify all the boiler plate
-- by using deriving, which uses the defaults
-- that we have manually typed in in `instances.hs`

module Main where

data RGB = RGB Int Int Int
    deriving (Show, Eq)

green ::  RGB
green = RGB 0 255 0

main ::  IO ()
main = print green
