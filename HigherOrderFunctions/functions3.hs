-- these are equivalent

always7 :: Num a => b -> a
always7 _ = 7

always7' :: Num a => b -> a
always7' = const 7

main :: IO ()
main = do
    print (always7 (5 :: Integer) :: Integer)
    print (always7' (5 :: Integer) :: Integer)
