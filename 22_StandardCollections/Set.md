# Set

* Unordered collection

```
import Data.Set
```

## Functionalities in Data.Set:

A set which is empty:

```
empty :: Set a
```

Inserting a new value into the set? It does so by creating a new set without modifying the given set.

```
insert :: a -> Set a -> Set a
```

Delete an element of a set?

```
delete :: a -> Set a -> Set a
```

Combining two sets:

```
union :: Set a -> Set a -> Set a
```

Is a particular element in the Set?

```
member :: a -> Set a -> Bool
```

## Set restrictions?

```
triple :: Int -> Int
triple x = x + x + x

triple' :: Int -> Int
triple' x = 3 * x

funSet :: Set (Int -> Int)
funSet = insert triple empty

problem :: Bool
problem = member triple' funSet
```

triple and triple' are the same but functions do not have the notion of equality and do not have the Eq type class.

There's no `class Eq a` but `class Ord a` is available in Set functions.  (Note that this applies to Set functions and not to Set itself)

In fact, we should express our Set functions like this:

```
empty :: Set a

insert :: Ord a => a -> Set a -> Set a

delete :: Ord a => a -> Set a -> Set a

union :: Ord a => Set a -> Set a -> Set a

member :: Ord a => a -> Set a -> Bool
```
