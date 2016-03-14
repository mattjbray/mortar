# Two Forms Example

To build this example:

```
stack build --flag mortar:examples
stack exec example-readme
```

## Overview of modules

[Main.hs](Main.hs) wires the application together using `mortar`'s `Config`. It
also sets up the logger, which can be used by any component's request handlers.

[TwoForms.hs](TwoForms.hs) sets up two `Form`s, handles toggling between the
forms and quitting, and threads `Actions` and `Requests` through to the active
`Form`.

[Form.hs](Form.hs) represents a simple form with a single text box and a submit
button. It handles form submission, and threads `Actions` and `Requests` through
to its `TextBox`. When a `Form` is submitted, it logs the content of the
`TextBox` by emitting a `LogSubmitted` `Request`.
