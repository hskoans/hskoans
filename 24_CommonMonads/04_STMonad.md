# ST Monad

* Implement imperative algorithms
* Modifiable values
* Pure from the outside

```
import Control.Monad.ST

data ST s a

instance Monad (ST s)

runST :: ST s a -> a
```

Updatable variables

```
import Data.STRef

data STRef s a -- an updatable value!

newSTRef :: a -> ST s (STRef s a)

readSTRef :: STRef s a -> ST s a

writeSTRef :: STRef s a -> a -> ST s ()
```

Usage:

```
sumST :: [Int] -> STRef s Int -> ST s ()
sumST [] accumRef = return ()
sumST (x : xs) accumRef = do
  accum <- readSTRef accumRef
  writeSTRef accumRef (x + accum)
  sumST xs accumRef
```

```
sum' :: [Int] -> [Int]
sum' xs = runST $ do
  accumRef <- newSTRef 0
  sumST xs accumRef
  readSTRef accumRef
```

## ST Monad Uses

* High performance
* Translating imperative code
* Complicated, multi-part state
