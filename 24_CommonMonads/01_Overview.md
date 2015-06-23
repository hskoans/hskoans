# Common Monads

* Reader
* State
* ST

Mentioned in the previous chapter, common applications of monads include:

* representing failure using the `Maybe` monad
* non-determinism using `List` monad to represent carrying multiple values
* State using `State` monad
* Read-only environment using `Reader` monad
* I/O using `IO` monad

Also mentioned in the previous chapter, we go into more details. Monads can be viewed as a standard programming interface to various data or control structures, which is captured by the `Monad` type class.

```
class Monad m where
  (>>=) :: m a -> (a -> m b) -> m n
  (>>) :: m a -> m b -> m b
  return :: a -> m a
  fail :: String -> m a
```

In addition, all instances of Monad should follow *Monad Laws*:

```
return a >>= k = k a
m >>= return = m
m >>= (\x -> k x >>= h) = (m >>= k) >>= h
```
