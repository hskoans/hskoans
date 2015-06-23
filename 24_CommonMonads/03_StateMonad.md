# State Monad

```
import Control.Monad.State
```

```
data State s a
```

```
instance Monad (State s)
```

```
get :: State s s
```

```
put :: s -> State s ()
```

```
evalState :: State s a -> s -> a
```

Usage:

```
harmonicStep :: State (Double, Double) Double
harmonicStep = do
  (position, velocity) <- get
  let acceleration = (-0.01 * position)
      velocity' = velocity + acceleration
      position' = position + velocity'
  put (position', velocity')
  return position
```

```
harmonic :: State (Double, Double) [Double]
harmonic = do
  position <- harmonicStep
  laterPositions <- harmonic
  return (position : laterPositions)
```

```
> let positions = evalState harmonic (1, 0)
> take 5 positions
[1.0,
0.99,
0.9701,
0.940499,
0.9014930099999999]
```

```
newtype State s a = State (s -> (a, s))
```
