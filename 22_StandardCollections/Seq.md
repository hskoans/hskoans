# Seq

* Ordered collection

```
import Data.Sequence
```

Module is named "Sequence". The data type is "Seq".

```
(<|) :: a -> Seq a -> Seq a
```

* Same as cons (:) for lists

```
(|>) :: Seq a -> a -> Seq a
```

* Cocatenate two sequences together

```
(><) :: Seq a -> Seq a -> Seq a
```

## Seq Pattern Matching

Use the View Patterns technique.

Turn on the ViewPatterns language extension.

In source file:

```
{-# LANGUAGE ViewPatterns #-}
```

In ghci:

```
:set -XViewPatterns
```

```
length :: Seq a -> Int
length (view1 -> EmptyL) = 0
length (view1 -> x :< xs) = 1 + length xs
```

```
view1 :: Seq a -> ViewL a
```

```
data ViewL a
  = EmptyL
  | a :< (Seq a)
```

```
length' :: Seq a -> Int
length' (viewr -> EmptyR) = 0
length' (viewr -> xs :> x) = 1 + length xs
```

## Seq Performance

* Seq usually faster than list
