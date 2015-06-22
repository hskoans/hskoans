# Map

* Key-value pair collection

```
import qualified Data.Map
```

We like to use the `qualified` keyword because `Map` often conflicts with other function names.

```
empty :: Map k a

insert :: Ord k => k -> a -> Map k a -> Map k a

delete :: Ord k => k -> Map k a -> Map k a

union :: Ord k => Map k a -> Map k a -> Map k a  -- the left key takes precedence

lookup :: Ord k => k -> Map k a -> Maybe a
```
