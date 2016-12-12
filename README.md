## diagrams

### Current status

Proposed rewrite for the diagrams library. Currently at a proof of
concept stage. So far I've implemented most of old functionality
(missing arrows and offsets) and the pgf backend. I've also started a
very basic 3d [gl] and [sdl] backend that does basic shapes and lets you
move the camera around.

### Changes

#### Reshuffling

`diagrams-core` and `diagrams-lib` would no longer be used. `diagrams`
and [`geometry`] would be used instead. [`geometry`] is a geometry
library based on `diagrams-lib`. The advantage is that [`geometry`]
could be used by other libraries that don't want to depend on diagrams
(whereas `diagrams-core` can only be used by `diagrams-lib`). Many of
the old core modules have moved to `Diagrams.Types.X`. The functionality
of `dual-tree` has been moved to `Diagrams.Types.Tree`.

#### New features

Some of the major proposed changes include:

  - The backend token is gone. The `Diagram` alias now only takes the
    vector space used (so `Diagram V2` or `Diagram V3`). This will ease
    a lot of pain I've had with `plots` and allows lots of new features
    that hard or close to impossible with the current tokened diagram.

  - Named subdiagram traversals! You can traversal over diagrams
    that matching or include a name. The way diagrams are named has also
    changed significantly. It's also possible to traverse over styles
    and primitives (like paths etc.).

There are also some smaller changes:

  - `mainWith` now takes a backend token to decide which backend to use.
    There is also functionally to use `mainWith` with multiple backends
    in the same executable.

  - Rewrite of `Style` internals for better type safety. The interface
    is now entirely lens based. Backends no longer deal with styles and
    instead receive an `Attributes` which is just a map from `TypeRep` to
    `Dynamic` (no transformable or measured attributes).

  - Static annotations (group opacity, hrefs etc.) are now extensible,
    just like styles. They are also transformable so `Clip` has been
    moved to a static annotation.

  - Down annotations are stricter. The `Transformation` type is in
    matrix form, giving us proper O(1) transforms.

  - Up annotations are no longer cached at the top. Among other things
    this means we could traverse parts of a diagram depending of the
    result of querying an up annotation (not yet implemented).

  - The `Alignable` class has been removed. Alignment functions now use
    enveloped. (The previous `Alignable` instances either used enveloped
    anyway, or had bogus definitions (like for functions)). This also
    lets us make more performant definitions for things like `hcat`.

Other possible changes:

  - Limit diagrams back to double again? The geometry library would
    still allow any number type for better precision etc. but I can't
    think of any reason to use anything but double for a diagram. I've
    already limited a bunch of things to double (`ToTexture`, `Backend`)
    and it makes things much easier.

[`dual-tree`]: (https://github.com/diagrams/dual-tree)
[`geometry`]: (https://github.com/cchalmers/geometry)
[gl]: (https://github.com/cchalmers/diagrams-gl)
[sdl]: (https://github.com/cchalmers/diagrams-sdl)

