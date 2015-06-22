# Cabal Sandbox

* Cabal sandbox is the solution for cabal hell
* Cabal hell manifests when one version of a package depending indirectly on two different versions of another package
* Cabal sandbox: independent collection of installed packages

## Project Isolation

In the project root directory, run:

```
cabal sandbox init
```

* Separate sandboxes for each project
* Sandboxes for bleeding edge Hackage packages
* An older version of cabal-install is cabal-dev: simialr to cabal-install but compatible with older package versions
