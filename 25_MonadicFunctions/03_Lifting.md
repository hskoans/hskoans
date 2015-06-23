# Lifting: liftM

```
liftM :: Monad m => (a -> b) -> (m a -> m b)
```

Example:

```
> liftM (1+) (Just 3)
Just 4
```

More variants of liftM:

```
liftM2 :: Monad m => (a1 -> a2 -> b) -> m a1 -> m a2 -> m b

liftM3 :: Monad m => (a1 -> a2 -> a3 -> b) -> m a1 -> m a2 -> m a3 -> m b

liftM4 :: Monad m => (a1 -> a2 -> a3 -> a4 -> b) -> m a1 -> m a2 -> m a3 -> m a4 -> m b

...

liftM5 ...
```


```
> liftM2 (+) (Just 3) (Just 5)
Just 8
```

When we use do-Notation to unpack some values, pass them to a function and pack the results back up again with return, we can usually refactor our code to use liftM and one of its variants.
