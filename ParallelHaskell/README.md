# Parallel Haskell

## Basic Parallelism: Eval Monad

Observe lazy evaluation in `ghci`:

```
> let x = 1 + 2
> let y = x + 1
> :sprint x
x = _
> :sprint y
y = _
```
