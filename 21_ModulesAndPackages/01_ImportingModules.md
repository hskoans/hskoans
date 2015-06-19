# Importing Modules

```
import Data.Set
```

Using the qualified keyword means that we have to specify the full qualified name if referring to any functions in the module. This dis-ambiguates any

```
import qualified Data.Set
import qualified Data.Sequence
```

It's troublesome to type out the fully qualified module names just to use a function. So we can cheat by using a shorthand name.

```
import Data.Sequence as Seq
-- both
-- Seq.empty
-- empty
-- will be valid

import qualified Data.Sequence as Seq
-- empty is not defined
-- Seq.empty is defined
```

For fine grain control over exactly which functions you want to import, use import lists:

```
import Data.Set (empty, size)
```

Using import lists to explicitly declare which functions we want to use is considered a good coding practice.
