# Monad Type Class

```
class Monad m where
  return :: a -> m a
  (>>=) :: m a -> (a -> m b) -> m b
```

* This type class captures common pattern in IO, list and Maybe

Now, we can generalize the `join` behavior for all monads.

```
join :: Monad m => m (m a) -> m a
join mmx = mmx >>= id
```

* Type class of parameterized types
* Monad laws
