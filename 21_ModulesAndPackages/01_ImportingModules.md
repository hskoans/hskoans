# Importing Modules

1. import

```
import Data.Set
```

Using the qualified keyword means that we have to specify the full qualified name if referring to any functions in the module. This dis-ambiguates any namespace conflict.

2. import requiring fully qualified name

```
import qualified Data.Set
import qualified Data.Sequence
```

3. import with shorthand name (renaming)

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

4. explicit import lists (i.e. import lists)

For fine grain control over exactly which functions you want to import, use import lists:

```
import Data.Set (empty, size)
```

Using import lists to explicitly declare which functions we want to use is considered a good coding practice.

5. Data Types in explicit import lists

```
import Data.Maybe (Maybe)
import Data.Maybe (Maybe (Just, Nothing))
```

6. Type Classes in explicit import lists

```
import Control.Monad (Monad)
import Control.Monad (Monad, return)
```

* Type class instances ignore any explicit import list
* Automatically imported with the module

We can use

```
import Data.Set ()
```

if we are only interested to import the type class but none of its functions.

7. import hiding

We can specifically specify functions that we want to avoid importing

```
import Data.Set hiding (empty, size)
import Prelude hiding (map)
```

so as to avoid namespace conflicts.
