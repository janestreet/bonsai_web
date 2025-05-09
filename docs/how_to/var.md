A `Bonsai.Expert.Var.t` is a mutable variable that can be an input to
incremental computations.

`Bonsai.Expert.Var.t` should not be used for local component state. This
is because `Bonsai.Expert.Var.t`s are not part of Bonsai's computation
graph: note that `Var.create` does not take a `graph` parameter. As a
result:

-   `Var`s inside of
    [Bonsai.assoc](../guide/05-control_flow.md#creating-a-dynamic-number-of-bonsaits)s
    will not have separate state for each key in the input map.
-   [Model resetters](./resetting_state.md) will not work on `Var`s
-   [scope_model](./state_per_key.md)s will not work with `Var`s

## Test Input Driver

The most common use of `Bonsai.Expert.Var` is to create a mutable handle
around a `Bonsai.t`, which can be used as an input to some Bonsai code
in [expect tests](./testing.md#testing-dynamic-inputs).

## External Global Data

`Var.t`s can be set / updated synchronously without an `Effect.t`, so
they are convenient for storing data coming from external sources. There
aren't many cases where this is necessary. Some that we've implemented
for you are:

-   [Url_var](./url_var.md)
-   [Persistent_var](https://github.com/janestreet/bonsai/blob/master/web/persistent_var.mli),
    for storing data in [local
    storage](https://developer.mozilla.org/en-US/docs/Web/API/Window/localStorage)
    or [session
    storage](https://developer.mozilla.org/en-US/docs/Web/API/Window/sessionStorage).

You should always instantiate these at the toplevel.

Note that [one-shot RPC responses](./rpcs.md#one-shot-rpcs) can be
stored in a regular [Bonsai.state](../guide/04-state.md).
