# Brick and Mortar

`mortar` is an alternative architecture for [`brick`][brick] terminal
applications, inspired by [The Elm Architecture][the elm architecture].

See [src/Mortar.hs](/src/Mortar.hs) for documentation and check the
[examples](/examples) directory for usage examples.


## Motivation

* We want to be able to nest components, such that a parent only has to know
  about its children, but not its children's children.

* We want any (nested) component to be able to perform IO.

In [`brick`][brick], the event handlers run in the `EventM (Next s)` monad.
However, the `Next` data constructors are hidden, which means that components
cannot be arbitrarily nested, as the parent component would have to deconstruct
the `Next` value in order to compose the responses of its children.

In `mortar`, we take a different approach. Each component is represented by a
module exporting the following definitions:

```haskell
data Model
data Action
data Request
initModelRequests :: (Model, [Request])
update :: Model -> Action -> Maybe (Model, [Request])
handleRequest :: Request -> m Action
render :: Model -> Brick.Widget
```

The `Model` represents the entire state of the component.

`Action`s are the only way to evolve the `Model`, via the `update` function.

In processing an `Action`, the `update` function may generate `Request`s. These
will be passed to `handleRequest`, which can perform IO and generate a new
`Action`.

`initModelRequests` defines how the component should be initialised.

Finally, `render` renders the current component state to a `brick` `Widget`.

[brick]: https://github.com/jtdaugherty/brick
[the elm architecture]: https://github.com/evancz/elm-architecture-tutorial
