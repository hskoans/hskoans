# Functional Collections

```
data List a = Empty | Cons a (List a)
```

* All collections are immutable
* For example, this makes a copy of the list instead of modifying the list to update the first element in it

```
updateFirst :: List a -> a -> List a
updateFirst Empty y = Empty
updateFirst (Cons x xs) y = Cons y xs
```

* Modification by copying seems slow
* But the advantage is that Immutable data can be shared
* So, the trick is to copy by referencing to the original, which is fast compared to a pure copy
* Queries are not a problem, copy by referencing is only needed for handling mutation in a pure way
