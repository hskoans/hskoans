pass3 :: Num a => (a -> t) -> t
pass3 f = f 3

add1 :: Num a => a -> a
add1 x = x + 1

main :: IO ()
main = do
    print "hello"
    let a = pass3 add1
    let b = show (a :: Integer) -- using explicit type signature to suppress warning
    putStrLn b
