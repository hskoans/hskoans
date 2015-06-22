# Functional Reactive Programming

* Declarative event handling
  * UI
  * Animation
  * Robotics

* Implementations
  * Haskell: 20+ implementations on hackage
  * Sodium (Java, Haskell, C++)
  * Flapjax (Javascript)
  * Threepenny-Gui (Haskell library)

## Let's get started

```
cabal install threepenny-gui
```

## Key ideas

* Time variation: representation for time-varying values. rather than having a variable and setting it to a value and later on changing it to another value, we have a declarative description of how that value changes over time. This is captured by the `Behavior` type.

Examples:

```
type Behavior a = Time -> a
```

```
time :: Behavior Time
```

```
afterMidnight :: Behavior Bool
```

* Second type of time variation called Event

```
type Event a = [(Time, a)]

mouseclicks :: Event MouseButton
```

## Arithmetic Example

```
bInput1 :: Behavior String
bInput1 = bValue input1
```

```
bInput1Num :: Behavior (Maybe Int)
bInput1Num = (liftA readMaybe) bInput1
```

```
bInput2 :: Behavior String
bInput2 = bValue input2
```

```
bInput2Num :: Behavior (Maybe Int)
bInput2Num = (liftA readMaybe) bInput2
```

```
bSum :: Behavior (Maybe Int)
bSum = (liftA2 (liftM2 (+))) bInput1Num bInput2Num
```

```
bResult :: Behavior String
bResult = (liftA showMaybe) bSum
```

```
element result # sink text bResult
```

## Counter Example

```
eUp :: Event ()
eUp = UI.click buttonUp

eIncrement :: Event (Int -> Int)
eIncrement = fmap (\() -> (+1)) eUp

eDown :: Event ()
eDown = UI.click buttonDown

eDecrement :: Event (Int -> Int)
eDecrement = fmap (\() -> (subtract 1)) eDown

eChange :: Event (Int -> Int)
eChange = unionWith (.) eIncrement eDecrement

bCounter :: Behavior Int
bCounter = accumB 0 eChange

bResult :: Behavior String
bResult = liftA show bCounter

element result # sink text bResult
```

## Non-GUI events

```
data Message = Message
  { msgSender :: String
  , msgText :: String }

eIncoming :: Event Message

bHistory :: Behavior [Message]
bHistory = accumB [] (fmap (:) eIncoming)
```

## Common ideas implemented by FRP libraries

* Declarative reactions to changes over time
* Composable: create complex behaviors by composing from simple ones
