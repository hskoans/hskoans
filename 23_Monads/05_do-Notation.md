# do-Notation

```
addM :: Monad m => m Int -> m Int -> m Int
addM mx my =
  mx >>= (\x -> my >>= (\y -> return (x + y)))
```

do-Notation is to make the above code readable:

```
addM' :: Monad m => m Int -> m Int -> m Int
addM' mx my = do
  x <- mx  -- give us x in mx
  y <- my  -- give us y in my
  return (x + y)  -- compute and package them, returning a monad
```

```
do
  x <- mx
  ...
```

equivalent to:

``
mx >>= (\x -> ...)
```

It is essentially syntactic sugar for our bind operator. Do not confuse it with imperative code.

Example usage:

```
people = ["Alice", "Bob", "Eve"]
items = ["car", "puppy"]
missing = do
  person <- people
  item <- items
  return (person ++ " lost a " ++ item)
```

If we execute `missing` function in ghci, we will see that it is nothing like what we know in imperative languages:

```
> missing
[ "Alice lost a car"
, "Alice lost a puppy"
, "Bob lost a car"
, "Bob lost a puppy"
, "Eve lost a car"
, "Eve lost a puppy" ]
```

