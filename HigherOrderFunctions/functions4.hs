foo :: Num a => a -> a -> a -> a
foo x y z = x + y + z

fooPartial :: Integer -> Integer
fooPartial = foo 1 2

main :: IO ()
main = do
    let ans = fooPartial 3 :: Integer
    print ans
