# Revision history for diagrams

## 0.1.0.0  -- YYYY-mm-dd

### Style

The style rewrite ended up having more type wizardy than I'd have liked,
but it's not too bad.  The motivation for this change was to have a
'backupAttr' lens that works on any attribute. `backupAttr` takes a
attribute iso (like `_LineWidth` or `_FillTexture`) and stores it as a
backup in the style.  When it comes to compiling, `getAttr` will look
for a backup if the main attributive isn't found. This replaces the old
`Recommend` system which I didn't like because it didn't play nice with
lenses. It also works automatically for any attribute. (There is slight
issue for certain attributes that use lens's `non` utility. If you set
the main one to the default value, the backup version with be used. I'm
not sure the best way to deal with this)

law issues for attributes with default values but I don't think
it's much of a problem)

On the previous system we needed three versions of every function, now
we just need one (albeit with a more complicated type). Even though the
internals are more complicated, it now has a more consistent api and the
'Attribute' wrapper itself never has to be used directly (I don't even
export the constructor anymore).

The Attribute class has a AttrType type familiy to chose which kind of
attribute it will be. This prevents accidentally wrapping an attribute
incorrectly.

In order to offer a single version of attribute functions there are
three new type families (`AttributeSpace'`, `Requirements` and `Rep'`)
to get the constrains and types agreeing between static, measured and
transformable. There's also a `attrReflect` function added which returns
the dictionaries necessary to rebuild an attribute. Annoying when
writing the class you have to give the dictionary for that attribute
type. 

Initially I tried using Typeable on the AttrType, which allowed matching
without the providing the `attrReflect` function but it gave uglier code
and constaints and didn't produce nice core (staticly known attributes
would still do the type check every time they where used, whereas now
they get reduced fully).

Another big change is how backends deal with attributes. Backends never
see the `Style`, they now have a new `Attributes` type that only
contains calculated attributes. Extracting attributes is done with
`getAttr` which takes an `Iso`. So `getAttr _LineWidth :: Attributes ->
Maybe Double` etc.

There's also a 

Now there a new
type, `Attributes`, that contains all the attributes wrapped in a
`Dynamic`. Measured attributes are calcuated so there's no confusion
over weaThis way there's no confusion over weather 

* First version. Released on an unsuspecting world.
