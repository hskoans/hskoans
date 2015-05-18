compose :: (t1 -> t) -> (t2 -> t1) -> t2 -> t
compose f g x = f (g x)

add1 :: Num a => a -> a
add1 x = x + 1

mult2 :: Num a => a -> a
mult2 x = 2 * x

ans :: Integer
ans = compose add1 mult2 4

main :: IO ()
main = print ans
