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
However, the `Next` data constructors are hidden. This means that components
cannot be arbitrarily nested -- the parent component would have to deconstruct
the `Next` value in order to compose the responses of its children.


## Overview

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
will be passed to `handleRequest`, which runs in a separate thread and can
perform IO. `Request` handlers return an `Action`, which in turn is passed back
to `update`.

`initModelRequests` defines how the component should be initialised.

Finally, `render` renders the current component state to a `brick` `Widget`.


## Nesting components

A parent component is responsible for threading `Actions` and `Requests` through
to its children's `update` and `handleRequest` functions.

The parent's `Model` will embed the child's:

```haskell
module Parent where
data Model = Model
  { child :: Child.Model }
```

The parent defines `Action`s and `Request`s to wrap the child's:

```haskell
data Action = ChildAction Child.Action | ParentAction1 | ...
data Request = ChildRequest Child.Request | ParentRequest1 | ...
```

The parent's `initModelRequests` function will initialise the child:

```haskell
initModelRequests =
  let (childModel, childRequests) =
        Child.initModelRequests
  in  ( Model { child = childModel }
      , ChildRequest <$> childRequests
      )
```

The parent's `update` function will dispatch `Actions` to the child:

```haskell
update model action =
  case action of
    ChildAction childAction -> do
      (childModel, childRequests) <-
        Child.update (child model) childAction
      Just ( model { child = childModel }
           , ChildRequest <$> childRequests
           )
    ParentAction1 ->
      ...
```

And the `handleRequest` function dispatches `Requests` to the child:

```haskell
handleRequest request =
  case request of
    ChildRequest childRequest -> do
      ChildAction <$> Child.handleRequest childRequest
    ParentRequest1 -> do
      ...
```

Notice that the parent itself can be nested in a containing component, and the
containing component does not need to know anything about the internal workings
of the child.


## Handling `Vty` `Event`s

Typically, components that handle events from `Vty` (the terminal library
underlying `brick`) will define an action to carry `Vty` `Event`s and export it:

```haskell
module MyComponenet (Action(VtyEvent), ...) where

data Action
  = VtyEvent Vty.Event
  | MyAction
```

When it receives a `VtyEvent`, a child's `update` may handle the event, or it
may return `Nothing` to signal that the event was not handled. In that case, the
parent component may decide to handle the event itself.

See the [TwoForms](/examples/TwoForms) example for an implementation of this
pattern.

[brick]: https://github.com/jtdaugherty/brick
[the elm architecture]: https://github.com/evancz/elm-architecture-tutorial
