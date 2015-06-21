# Defining Modules

```
module MyModule where
```

Filename: MyModule.hs

```
module Foo.Bar.Baz where
```

Filename: Foo/Bar/Baz.hs

## Export lists

* By default, everything is exported
* We can specify an explicit export list so only those specified can be exported

```
module Foo.Bar.Baz
  ( myFunction,
  , MyType
  , MyType2 (Construct1)) where
```

* All type class instances (defined or imported) always exported

## Exporting parts of another module

* By default, imported functions and types aren't exported

```
module Foo.Bar.Baz
  (fromMyModule) where

import MyModule (fromMyModule)
```

## Exporting an entire Module

* MyModule here is being re-exported
* This is useful for making functionalities from multiple modules available from one single module

```
module Foor.Bar.Baz
  (module MyModule) where

import MyModule
```
