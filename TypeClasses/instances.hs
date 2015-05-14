elem' :: Eq a => a -> [a] -> Bool
elem' _ [] = False
elem' x (y : ys)
    | x == y = True
    | otherwise = elem' x ys

data RGB = RGB Int Int Int

-- tell our compiler that RGB type has an `Eq` behavior
-- pattern matching that says that
-- r components are equal
-- b components are equal
-- g components are equal
instance Eq RGB where
    (RGB r1 g1 b1) == (RGB r2 g2 b2) =
        (r1 == r2) && (g1 == g2) && (b1 == b2)

-- without the instance declaration to specify that RGB data type instance has the `Eq` behavior,
-- the following will fail to compile.
colors ::  [RGB]
colors = [RGB 255 0 0, RGB 0 255 0, RGB 0 0 255]
green ::  RGB
green = RGB 0 255 0
greenInColors ::  Bool
greenInColors = elem' green colors

-- this tells our compiler that our RGB type has a `Show` behavior
-- i.e. we can show it :-)
instance Show RGB where
    show (RGB r g b) =
        "RGB " ++ show r ++ " " ++ show g ++ " " ++ show b

main :: IO ()
main = do
    -- notice the difference; using show and not using show
    print green
    print (show green)

    print greenInColors
    print (show greenInColors)

-- Type Class Instances for Parameterized Types
