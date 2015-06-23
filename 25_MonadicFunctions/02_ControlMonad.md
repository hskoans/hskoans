# Control.Monad

```
import Control.Monad
```

## Monadic Flow Control

* Analogous with imperative flow control constructs
* Actually just ordinary functions

## forM

Analogous to forEach.

```
forM :: Monad m => [a] -> (a -> m b) -> m [b]

-- alternative version
forM_ :: Monad m => [a] -> (a -> m b) -> m ()
```

Usage:

```
forM list $ do
  x <- first_action
  second_action
  ...
```

```
> forM [1, 2, 3] print
1
2
3
```

## replicateM

```
replicateM :: Monad m => Int -> m a -> m [a]

replicateM_ :: Monad m => Int -> m a -> m ()
```

Example:

```
> replicateM 3 (putStrLn "hello")
hello
hello
hello
```

## when

```
when :: Monad m => Bool -> m () -> m ()
```

```
when debug (putStrLn "Debugging")
```

## Summary

Useful for writing "imperative-style" code (which are actually functional):

```
forM :: Monad m => [a] -> (a -> m b) -> m [b]

replicateM :: Monad m => Int -> m a -> m [a]

when :: Monad m => Bool -> m () -> m ()
```
