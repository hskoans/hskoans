# Monad Examples

## IO

```
return :: a -> IO a
```

There is no such thing as `unreturn :: IO a -> a` as that would violate the purity of Haskell.

IO is like a box that can hold a value; easy to put something into the box but impossible to take it back out.

But there's a concept called `bindIO` that helps us use the value.

```
bindIO :: IO a -> (a -> IO b) -> IO b
```

`bindIO` temporarily unbinds the value, passes it to a function which packs it up immediately in a new IO and returns the new IO.

## List

A list is a monad.

```
singleton :: a -> [a]
```

There is no `unsingleton :: [a] -> a` to reverse the singleton.

What we do instead is to use a `flatMap`:

```
flatMap :: [a] -> (a -> [b]) -> [b]
```

Usage in ghci:

```
flatMap [1, 7, 11] (\x -> [x, x + 1])
[1, 2, 7, 8, 11, 12]
```

flatMap behaves like bindIO.

## Maybe

data Maybe a = Nothing | Just a

```
Just :: a -> Maybe a
```

Again, there's no `Unjust :: Maybe a -> a`

```
bindMaybe :: Maybe a -> (a -> Maybe b) -> (Maybe b)
```

Usage:

```
bindMaybe (Just 0) (\x ->
  if (x == 0)
  then Nothing
  else Just (2  * x))

Nothing
```

```
bindMaybe (Just 1) (\x ->
  if (x == 0)
  then Nothing
  else Just (2  * x))

Just 2
```

These functions package up our values:

```
return :: a -> IO a
singleton :: a -> [a]
just :: a -> Maybe a
```

These functions allow us to operate on the value in the package and return the changed value in the package again.

```
bindIO :: IO a -> (a -> IO b) -> IO b
flatMap :: [a] -> (a -> [b]) -> [b]
bindMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b
```

Any type with these two operations - packaging something up and modifying the value inside the package - is a monad.
