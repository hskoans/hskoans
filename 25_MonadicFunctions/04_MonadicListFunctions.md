# Monadic List Functions: mapM

```
mapM :: Monad m => (a -> m b) -> [a] -> m [b]

forM :: Monad m => [a] -> (a -> m b) -> m [b[]]
```

We notice that mapM is the same as forM except with arguments reversed.

forM is more convenient as a forEach loop.
mapM is more familiar as a map function.

Usage:

```
> mapM print [1, 2, 3]
1
2
3
```

## filterM

```
filterM :: Monad M => (a -> m Bool) -> [a] -> m [a]
```

Example:

```
askToKeep :: Int -> IO Bool
askToKeep x = do
  putStrLn ("keep " ++ (show x) ++ "?")
  (c : _) <- getLine
  return (c == 'y')

askWhichToKeep :: [Int] -> IO [Int]
askWhichToKeep xs =
  filterM askToKeep xs
```

```
> askWhichToKeep [1, 2, 3, 4, 5]
keep 1?
y
keep 2?
n
keep 3?
n
keep 4?
y
keep 5?
y
[1, 4, 5]
```

## foldM

```
foldM :: Monad m => (a -> b -> m a) -> a -> [b] -> m a
```

Example:

```
sayAddition :: Int -> Int -> IO Int
sayAddition x y = do
  let z = x + y
  putStrLn ((show x) ++ " + " ++ (show y) ++ " = " ++ (show z))
  return z

talkingSum :: [Int] -> IO Int
talkingSum xs = foldM sayAddition 0 xs
```

```
> talkingSum [1, 2, 3]
0 + 1 = 1
1 + 2 = 3
3 + 3 = 6
6
```
