# Reader Monad

```
import Control.Monad.Reader
```

```
data Reader r a
```

```
instance Monad (Reader r)
```

```
ask :: Reader r r
```

```
runReader :: Reader r a -> r -> a
```

Usage:

```
getFirst :: Reader String String
getFirst = do
  name <- ask
  return (name ++ " woke up")

getSecond :: Reader String String
getSecond = do
  name <- ask
  return (name ++ " wrote some Haskell")

getStory :: Reader String String
getStory = do
  first <- getFirst
  second <- getSecond
  return ("First, " ++ first ++ ". Second, " ++ second ++ ".")

story = runReader getStory "Benson"
```
