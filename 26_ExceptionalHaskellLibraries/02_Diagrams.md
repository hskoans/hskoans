# Diagrams

* Declarative
  * What to draw, rather than how to draw it
* Compositional
* Multiple Backends (to render the drawing)
  * File formats: SVG, Bitmap, PDF, Postscript
  * Display: GTK, OpenGL

## Install

```
cabal install diagrams
```

## Usage

Simple triangle with edge length 1.

```
{-# LANGUAGE NoMonomorphismRestriction #-}

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine

-- B type specifies the backend, SVG in this case
-- R2 specifies that this is a 2-dimensional diagram
main = mainWith (triangle 1 :: Diagram B R2)
```

Another triangle:

```
{-# LANGUAGE NoMonomorphismRestriction #-}
import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine

example :: Diagram B R2
example = triangle 1 # fc black # lc blue # lw 0.1

main = mainWith example
```

More diagrams:

```
-- red triangle overlaid on blue circle
example :: Diagram B R2
example = triangle 1 # fc red <>  circle 1 # fc blue
```

```
-- red triangle above blue circle
example :: Diagram B R2
example = triangle 1 # fc red ===  circle 1 # fc blue

```

```
-- red triangle lined up horizontally next to blue circle
example :: Diagram B R2
example = triangle 1 # fc red |||  circle 1 # fc blue
```

```
example :: Diagram B R2
example = triangle 1 # fc red
                     # scaleY 2
                     # rotateBy (1/9)
                     # translateX 1
           <> circle 1 # fc blue
```

```
sierpenski :: Int -> Diagram B R2
sierpenski n
  | n == 0 == empty
  | otherwise ==
      piece red === (centerX (piece green ||| piece blue))
      where
        piece color = triangle 1 # lc color # alignT <> sierpenski (n-1) # scale 0.5 # alignT

drawing :: Diagram B R2
drawing = sierpenski 7 # center # pad 1.2
                                # bg black # lw 0.01
```
