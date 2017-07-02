# Reasons to lose the backend token from the `Diagram` type

This is a writeup of the reasons I believe the backend token in the
`Diagram` type does more harm than good. The main reason I want it to go
is simply that it makes it more difficult for users of diagram. However
there are more technical reasons, like difficulty implements certain
features and performance concerns. I've written my arguments for these
issues below.

## Building on top of diagrams

For the basis uses of diagrams the backend token isn't usually too much
trouble. Just annotate everything with `Diagram B` and it usually works.

The problems start when you don't want to be tied to single backend, 
This usually happens when building a library on top of diagrams, there
are [quite a few](http://packdeps.haskellers.com/reverse/diagrams-lib)
library that depend on `diagrams-lib` now. Some of them keep around the
`Renderable` constraints in all the function. Some of them write an
alias for the backend they want, something like `Backend' b = (V b ~ V2,
N b ~ Double, Renderable (Path V2 Double) b, Renderable (Text Double)
b)`, to reduce the clutter. Unfortunately many just stick to their own
favorite backend so the library can't be used with other backends.
It's hard to blame the library writers here because keeping the backend
polymorphic can be a pain, especially for those not very familiar with
diagrams.

I've had first hand experience with the pains of the backend token when
writing plots. The token infects *everything* so the whole code is
littered with the it even though I don't really want to think about it.
Any time I'd like to make something user customisable (e.g. letting
users use a arbitrary diagram for the plot marker of a scatter plot) I
need to go round and add the token to *all* the types to do with that
thing. This was a significant time drain when developing the library.
For me, this was when I was sure that I wanted the token to go. Not only
was it tedious to write but the resulting types are noisy and
potentially off-putting.

## Difficultly implementing features

There are a number of features that, while usually possible to
implement, are more difficult with the current token setup. The
issue is that all the renderable instances for the primitives need to be
known before hand. And once you've forced it to one backend it's almost
impossible to convert it to another backend.

For instance here's a crude implementation of a multi-backend main with
the current setup:

```
{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE RankNTypes       #-}
import Diagrams.Prelude
import Diagrams.TwoD.Text
import Diagrams.Backend.CmdLine

import System.Environment
import Data.List

import Diagrams.Backend.PGF.CmdLine
import Diagrams.Backend.SVG.CmdLine
import Diagrams.Backend.SVG

type R2Backend b n = (Renderable (Text n) b, Renderable (Path V2 n) b)

type R2Diagram n = (forall b. R2Backend b n => QDiagram b V2 n Any)

data SomeR2Backend n where
  R2Backend :: (Mainable (QDiagram b V2 n Any), Parseable (MainOpts (QDiagram b V2 n Any)), R2Backend b n)
            => String -> b -> SomeR2Backend n

diaWith :: b -> QDiagram b V2 n Any -> QDiagram b V2 n Any
diaWith b = id

mainsWith :: [SomeR2Backend n] -> R2Diagram n -> IO ()
mainsWith backends dia = do
  backendName:args <- getArgs
  case find (\(R2Backend name _) -> name == backendName) backends of
    Nothing -> putStrLn $ "can't find the backend " ++ backendName
    Just (R2Backend _ b) -> withArgs args $ mainWith (diaWith b dia)

mydia :: R2Diagram Double
mydia = circle 2 # fc orange <> square 5 # fc red

main :: IO ()
main = mainsWith [R2Backend "svg" SVG, R2Backend "pgf" PGF] mydia
```

While this works it's now limited to only the `Text` and `Path`
primitives. What if we want a diagram with images in? The `Image`
primitive is supported by most, but not all backend so we can't really
add it to `R2Backend`. It's not really a satisfactory solution we could
add to `digrams-lib`.

It would take too long for me to go in detail for all of the issues but
here's some of other features I'd like to have but are made more
difficult (or close to imposable) with the backend token:

  - use `diagrams-builder` to build a diagram that works with multiple
    backends
  - having a `Diagram V3 -> Diagram V2` projection (and the other way)
  - having a mainable instance that uses another backend (e.g. having an
    SDL token for the mainable instance but the diagram is rendered with
    an OpenGL backend)

## Performance

Whenever you see a constraint in Haskell, it's internally represented as
a function. So

```
myDia :: Renderable (Path V2 Double) b => Diagram b
myDia = square 3 ||| triangle 2
```

is represented as

```
myDia :: (Path V2 Double -> Render b) -> Diagram b
myDia renderDict = square renderDict 3 ||| triangle renderDict 2
```

So until it's given a concrete backend token nothing actually gets
computed. This can be bad if you build up a complicated diagram that's
still polymorphic, many of the optimisations ghc could usually make are
being stops. It can also lead to the same diagram being computed
multiple times if you reuse it.

I don't actually have any performance numbers for keeping the backend
polymorphic but I *do* know that keeping the number type polymorphic can
be lead to over an order of magnitude slowdown. The backend token won't
cause such a slowdown but expect it's non-trivial is many cases.

## My alternative

In my rewrite of diagrams, the backend token is no longer part
of a diagram. The Backend class is still there, it just no longer uses
the `Renderable` class. It handles the `Prim`s itself, optionally taking
a `(Prim -> Maybe (Render b))` for custom primitives. The `Mainable`
class is multi-parameter, taking the token as the first argument. This
actually works out quite nice because you can use tokens for things that
aren't formally backends (or for multiple backends). The `mainWith`
function's first argument is the token. The rest of the nice `Mainable`
functionality (parsing arguments etc.) is still there. I haven't
finalised all the details yet but it works.

## Final remarks

There's usually a balance between type safety and ease of use. In this
case I strongly feel that the costs of having to deal with the backend
token outweighs type safety for rendering primitives.

