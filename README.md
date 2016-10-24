## diagrams

Proposed rewrite for the diagrams library. Currently at a proof of
concept stage. The main focus of the rewrite is trying to make things
simpler and faster while adding some extra features.

### Reshuffling

`diagrams-core` and `diagrams-lib` would no longer be used. `diagrams`
and [`geometry`] would be used instead. [`geometry`] is a geomerty
library based on `diagrams-lib`. The advantage is that [`geometry`]
could be used by other libraries that don't want to depend on diagrams
(whereas `diagrams-core` can only be used by `diagrams-lib`). Many of
the old core modules have moved to `Diagrams.Types.X`. The functionality
of `dual-tree` has been moved to `Diagrams.Types.Tree`.

### Changes

Some of the major proposed changes include:

  - The backend token is gone. The `Diagram` alias now only takes the
    vector space used (so `Diagram V2` or `Diagram V3`). The `Backend`
    class has also changed significantly. This should hopefully make
    working with multiple backends at the same time much easier.

  - Named subdiagram traversals! You can traversal over diagrams
    that matching or include a name. The way diagrams are named has also
    changed significantly. It's also possible to traverse over styles
    and primitives (like paths etc.).

There are also some smaller changes:

  - CmdLine generation now supports multiple backends with the same
    executable.

  - Rewrite of style internals for better type safety. The interface is
    now entirely lens based. Backends no longer deal with styles and
    instead receive an `Attributes` that contains no 

  - Static annotations (group opacity, hrefs etc.) are now extensible,
    just like styles. They are also transformable so `Clip` has been
    moved to a static annotation.

  - Downs annotations are stricter. The `Transformation` type is in
    matrix form, giving us proper O(1) transforms.

  - Up annotations are no longer cached at the top. (They where never
    really 'cached' anyway since all up annotations are just functions
    (I need to actually benchmark this but I'd be surprised if the old
    caching method is actually faster)) This also makes implementing
    subdiagram traversals a little easier.

  - The `Alignable` class has been removed. Alignment functions now use
    enveloped. (The previous Alignable instances either used enveloped
    anyway, or had bogus definitions (like for functions)). This also
    lets us make more performant definitions for things like `hcat`.

Other possible changes:

  - Limit diagrams back to double again? The geometry library would
    still allow any number type for better precision etc. but I can't
    think of any reason to anything but double for a diagram. I've
    already limited a bunch of things to double (`ToTexture`, `Backend`)
    and it makes things much easier.

[`dual-tree`]: (https://github.com/diagrams/dual-tree)
[`geometry`]: (https://github.com/cchalmers/geometry)
