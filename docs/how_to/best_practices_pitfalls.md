# Best Practices and Pitfalls

In this article, we'll cover some common Bonsai pitfalls, as well as
some useful design patterns.

## Design Patterns

### A Common Bonsai Function

Many functions in your Bonsai codebase will look something like:

```{=html}
<!-- $MDX file=../../examples/bonsai_guide_code/best_practices_examples.ml,part=typical_f -->
```
``` ocaml
let a_typical_function (input : int Bonsai.t) (local_ graph) =
  (* Declare your state *)
  let num_input_changes, incr_num_input_changes =
    Bonsai.state_machine
      ~default_model:0
      ~apply_action:(fun _ model () -> model + 1)
      graph
  in
  let (logs : string Bonsai.t), (write_log_line : (string -> unit Effect.t) Bonsai.t) =
    Bonsai.state_machine
      ~default_model:""
      ~apply_action:(fun _ logs new_log_line ->
        match logs with
        | "" -> new_log_line
        | _ -> logs ^ ", " ^ new_log_line)
      graph
  in
  (* Build intermediate computations *)
  let magic_number =
    let%arr input and num_input_changes in
    compute_magic_number input num_input_changes
  in
  let log_current_magic_number =
    let%arr magic_number
    and write_log_line
    and now = Bonsai.Clock.get_current_time graph in
    let%bind.Effect now in
    write_log_line [%string "%{Time_ns.to_string_utc now} : %{magic_number#Int}"]
  in
  let on_change : (int -> unit Effect.t) Bonsai.t =
    let%arr incr_num_input_changes in
    fun _new_val -> incr_num_input_changes ()
  in
  (* Declare lifecycle and edge-triggered effects. Most code won't need these, but they're
     not uncommon. *)
  Bonsai.Edge.on_change ~equal:[%equal: int] input ~callback:on_change graph;
  (* Compute main output of your function. This could also be a [match%sub]. *)
  let%arr logs and magic_number and log_current_magic_number in
  View.vbox
    [ Vdom.Node.h1
        [ Vdom.Node.text [%string "Today's Magic Number: %{magic_number#Int}"] ]
    ; Vdom.Node.button
        ~attrs:[ Vdom.Attr.on_click (fun _ -> log_current_magic_number) ]
        [ Vdom.Node.text "Save it!" ]
    ; Vdom.Node.div [ Vdom.Node.text [%string "Saved Magic Numbers: %{logs}"] ]
    ]
;;
```

```{=html}
<iframe data-external="1" src="https://bonsai:8535#typical_f">
```
```{=html}
</iframe>
```
Keep `let%arr` blocks simple. This often involves factoring out "pure"
logic, like we did here with the presumably complicated
`val compute_magic_number : int -> int -> int`.

With more complicated code, you might also want to split out pure logic
for computing `Vdom.Node.t`s, so you can isolate your UI design from
state, business logic, and incremental computation.

### Use cool types for actions and models

A common pattern is declaring a module with
`type t = A | Variant | Type` for the `'action` and/or `'model` types in
a `Bonsai.state_machine` or `Bonsai.state_machine_with_dep`. But
`'action` and `'model` can be anything, including:

-   Polymorphic variants, which are often more lightweight and ergonomic
    than full variant types.
-   `unit`, if your state machine doesn't actually store state, or only
    has one action. The former is another pattern, since you can use a
    `Bonsai.state_machine_with_dep` as a way to centralize logic for
    dispatching effects.
-   A function type, although this is generally an anti-pattern, because
    decentralizing your state machine transition logic out of
    `apply_action` will probably make your code more complicated and
    less maintainable.

### Prefer `('a Bonsai.t * 'b Bonsai.t)` to `('a * 'b) Bonsai.t`

If your functions compute multiple things, consider returning tuples /
records of `Bonsai.t`s instead of a single `Bonsai.t` containing tuples
/ records. This will generally result in a better-structured incremental
graph, since we don't need to split apart things that we want to use
separately.

The caveat to this is that [higher-order
functions](./higher_order_functions.md) expect their `~f` parameter to
return a single `Bonsai.t`. You can combine `Bonsai.t`s with a `let%arr`
block or a `Bonsai.both`, and split them later with `let%sub`:

```{=html}
<!-- $MDX file=../../examples/bonsai_guide_code/best_practices_examples.ml,part=state_with_resetter -->
```
``` ocaml
let state_with_resetter ~default_value (local_ graph)
  : int Bonsai.t * (int -> unit Effect.t) Bonsai.t * unit Effect.t Bonsai.t
  =
  let state_and_setter, reset =
    Bonsai.with_model_resetter
      ~f:(fun (local_ graph) ->
        let state, set_state = Bonsai.state default_value graph in
        Bonsai.both state set_state)
      graph
  in
  let%sub (state : int Bonsai.t), (set_state : (int -> unit Effect.t) Bonsai.t) =
    (state_and_setter : (int * (int -> unit Effect.t)) Bonsai.t)
  in
  state, set_state, reset
;;
```

You might want to keep your output as a single `Bonsai.t` if:

-   You *only* expect your function to be used as an input to
    higher-order functions.
-   The type you are returning is abstract, and so can't be split /
    recombined easily.

### No Mutable Models

The mental model you should bring to writing incremental computations is
that of optimizing a pure computation over pure data. At its core,
Incremental operates by allowing you to compare new versions of your
computed values to old ones: by default, `Bonsai.t`s are recomputed if
their dependencies are no longer `phys_equal`. Using mutable data
undermines this in fairly fundamental ways.

When you do need to use state, reach for [Bonsai's
primitives](../guide/04-state.md). Likewise, any side effects you need
to run should be wrapped in an [Effect.t](../guide/02-effects.md), and
almost always dispatched on one of:

-   User interactions with
    [vdom](../guide/01-virtual_dom.md#event-handlers)
-   A [Bonsai lifecycle activation or deactivation](./lifecycles.md)
-   In response to [something changing](./edge_triggered_effects.md)

## Common Bonsai Bugs

### Not scoping your state

Imagine you have a list of users, and you want to show a form for the
"currently selected" user. You probably want to maintain separate state
for each user. Use [Bonsai.scope_model](./state_per_key.md).

### Stale values in effects

`Effect.t`s are run via a queue. This makes it possible for the inputs
used to calculate some `Effect.t` to change while that effect is already
enqueued, so the effect will run with a stale input.

This is usually not what you want, and we have [tools to fetch the
up-to-date value](./effects_and_stale_values.md).

### `Bonsai.Expert.Var` is for tests, and sometimes global state

Don't use it for anything else; see [this article](./var.md).

## Bonsai Performance

### Don't over-incrementalize

As we noted in the
[guide](../guide/03-incrementality.md#incremental-structure-matters),
infrequent and expensive computations can be factored out into
intermediate `Bonsai.t`s.

Often, this isn't actually worth it. Incremental nodes aren't *super*
expensive, but they aren't free to create, fire, and track. In the
"a_typical_function" example at the start of this chapter, it would
probably be more performant to fold the "intermediate" computations into
the main `let%arr` block.

You also can't chain incrementalization forever: Bonsai incremental
graphs have a max height of 1024. This is rarely an issue, but sometimes
comes up when building recursive computations with `Bonsai.fix`.
``{=html}

### `Bonsai.assoc` can be expensive

`Bonsai.assoc` is actually fairly expensive. Most of the cost comes from
needing to maintain separate state / instances of Bonsai primitives for
each key, and then persist that state even if the key is removed. This
grows exponentially for nested assocs.

If the body of an `assoc` doesn't use `local_ Bonsai.graph` to
instantiate any state, lifecycle events, model resetters, `match%sub`s
(implicitly), etc., Bonsai's optimizations will replace it with a
`Incr_map.mapi`, which uses a constant number of incremental nodes
regardless of the size of the input map.

For input maps with thousands of keys, try to avoid instantiating
anything with `graph` inside of your `Bonsai.assoc`s, except for
`Bonsai.path`.

## `Bonsai.Clock.Expert.now`

`Bonsai.Clock.Expert.now` returns a `Bonsai.t` which contains the
current time. This means that any values that depend on it will be
re-evaluated on every frame (usually 60 times a second). This can be
expensive, so there are workarounds for common scenarios:

-   If you only need the current time in order to compute a delta
    against another instant and display the delta to the user, take a
    look at the `vdom_time_ago` library.
-   If you only need the current time inside of a state machine's
    `apply_action` function, the `apply_action`'s
    `Apply_action_context.t` argument has a time source that you can use
    to cheaply access the current time.
-   If you need the current time when computing an `Effect.t` of some
    kind, you can bind on the effect returned by
    `Bonsai.Clock.get_current_time`
-   Otherwise, if you do need the current time, but are ok with the
    returned `Time_ns.t` being *slightly* out of date, you can [use
    `Bonsai.Clock.approx_now`](./time.md#accessing-time) and specify the
    degree of accuracy that you care about.

## List performance in Vdom

Vdom's algorithm for [diffing lists is
naive](../guide/01-virtual_dom.md#diffing-lists): it just compares
elements at corresponding indices. If an element gets inserted into, or
removed from, a list, this can cause subsequent DOM nodes to be
destroyed / recreated unnecessarily.

### Avoid Variable Length / Order Lists

When possible, avoid variable length lists of Vdom nodes, or lists that
may reorder. Some examples of how this might happen:

-   `List.filter_opt`-ing a list of `Vdom.Node.t option`s
-   Concatting multiple `Vdom.Node.t list`s, each of which might change
    from frame to frame
-   Using `Vdom.Node.none_deprecated`, if you conditionally want to show
    a dom node

For DOM elements that are conditionally displayed, consider using
`Vdom.Node.none` when the element is not shown; this will render an
empty comment node, keeping the length of the list constant.

### Use `Vdom.Node.Map_children` for key-able lists

What if you want to display a list of data, whose order might change? If
your list elements can be uniquely identified, using
`Vdom.Node.Map_children.div map` is much better than
`Vdom.Node.div (Map.data map)` because it can diff your list elements in
a smarter way.

## Libraries to know about

### Bonsai_extra

The
[bonsai_extra](https://github.com/janestreet/bonsai/blob/master/extra/bonsai_extra.mli)
library contains a variety of helper functions implemented on top of
Bonsai's primitives.
