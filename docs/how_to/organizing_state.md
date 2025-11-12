# Organizing State

Ideally, each "unit" of state in your app should be defined in a single
place; i.e. should have a single source of truth. If the same state is
defined via multiple `Bonsai.state`s or `Bonsai.state_machine`s, you'll
have to manually keep them in sync. This is bad, because:

-   It's really easy to make mistakes / forget to set something
-   Setting state in [an `on_change`](./edge_triggered_effects.md) or
    [lifecycle event](./lifecycles.md) that uses `after_display` won't
    take effect [until the next frame](./bonsai_runtime.md), so you
    might get flashes of old content, or even behavioral bugs. Using
    `before_display` helps, but each `before_display` can only run once
    per frame, so if your state is being updated by multiple other
    `before_display`s, your mirrored state might become stale.
-   Your code becomes less declarative, more imperative, and therefore
    harder to understand

```{=html}
```
This article provides some suggestions around structuring your state to
avoid synchronization bugs.

## Lift State With Controllable Components

Because each piece of state should have a single source of truth, must
make the (sometimes difficult) decision of which component will "own" a
piece of state. This is true for all states, from simple boolean flags
to complex data structures. Whether a component's state is created
locally or received from a parent leads to the [concept of "controlled"
versus "uncontrolled"
components](https://react.dev/learn/sharing-state-between-components#controlled-and-uncontrolled-components).

For example, a text input element could create its own state via
`let state, set_state = Bonsai.state "" graph`, or it could take a
`state:string Bonsai.t * (string -> unit Effect Bonsai.t)` parameter.

In most frameworks, each component must choose whether it is
uncontrolled (manages its own state), or controlled (gets state from the
caller). Bonsai allows components to offer both!

A common pattern is for components to take an optional
`?state:'state Bonsai.t * ('state -> unit Effect.t) Bonsai.t` parameter.
If provided, it will be controlled, otherwise it will maintain its own
state.

Instead of trying to synchronize states between components, create your
state in some parent code, and pass it to the components that need it.
It's good practice to design components to be controllable.

Some components can't just accept a value + setter for their state,
especially if they are using a complex state machine (e.g. [PRT's
focus](https://github.com/janestreet/bonsai_web_components/blob/c0e4224ff10a1ec59e49d34e476a647fae2dec74/partial_render_table/src/focus.ml)).
Controllable components aren't a total solution, but they cover a lot of
cases.

## Dealing with External State

Some state lives in the browser, or within some other non-Bonsai library
(e.g. Codemirror), in which case you can't "own" it. We deal with this
on a case-by-case basis.

Sometimes (e.g. [the URL](./url_var.md), [local
storage](./var.md#external-global-data)), we can create a `Bonsai.t`
that tracks the value, and maybe the setter, of an external state. If
your components are controllable, you can power them with this external
state.

In other cases (e.g. [focus](./focus.md#getting-focus)), there's no safe
way to track state as a `Bonsai.t`. Try to design your computation so
that you don't need to incrementally depend on these values.

## Don't Store Derived Values

If something *can* be incrementally computed via `let%arr`ing, it
probably should be. For example, the following is bad, because the view
will be stale if the selected comment's data changes:

```{=html}
<!-- $MDX file=../../examples/bonsai_guide_code/organizing_state_examples.ml,part=storing_derived_values -->
```
``` ocaml
let comments (local_ graph) =
  let selected_comment_view, set_selected_comment_view = Bonsai.state None graph in
  let comment_list =
    let%arr all_comments and set_selected_comment_view in
    List.map all_comments ~f:(fun comment ->
      let on_click _ =
        let close = set_selected_comment_view None in
        set_selected_comment_view (Some (comment_view ~close comment))
      in
      {%html|<li on_click=%{on_click}>#{comment.title}</li>|})
    |> Vdom.Node.ul
  in
  let comment_detail =
    let%arr selected_comment_view in
    Option.value selected_comment_view ~default:Vdom.Node.none
  in
  let%arr comment_list and comment_detail in
  Vdom.Node.div [ comment_list; comment_detail ]
;;
```

```{=html}
<iframe data-external="1" src="https://bonsai:8535#storing_derived_values">
```
```{=html}
</iframe>
```
Indeed, if you click on a comment, then close it, and reopen it, you'll
see that the number of likes has incremented!

If we store the computed view, or even the `Comment.t`, we are
duplicating state between the `all_comments` input, and our
`selected_comment` state. Instead, we'll store the simplest thing we
can: the comment's ID:

```{=html}
<!-- $MDX file=../../examples/bonsai_guide_code/organizing_state_examples.ml,part=computing_derived_values -->
```
``` ocaml
let comments (local_ graph) =
  let selected_comment_id, set_selected_comment_id = Bonsai.state None graph in
  let comment_list =
    let%arr all_comments and set_selected_comment_id in
    List.map all_comments ~f:(fun comment ->
      let on_click _ = set_selected_comment_id (Some comment.id) in
      {%html|<li on_click=%{on_click}>#{comment.title}</li>|})
    |> Vdom.Node.ul
  in
  let selected_comment =
    let%arr all_comments and selected_comment_id in
    let%bind.Option selected_comment_id in
    List.find all_comments ~f:(fun comment -> comment.id = selected_comment_id)
  in
  let comment_detail =
    match%sub selected_comment with
    | Some comment ->
      let%arr comment and set_selected_comment_id in
      comment_view ~close:(set_selected_comment_id None) comment
    | None -> Bonsai.return Vdom.Node.none
  in
  let%arr comment_list and comment_detail in
  Vdom.Node.div [ comment_list; comment_detail ]
;;
```

```{=html}
<iframe data-external="1" src="https://bonsai:8535#computing_derived_values">
```
```{=html}
</iframe>
```
```{=html}
```
## Use `Rpc_effect` Helpers for Server State

It might not be immediately obvious, but the server-client boundary is a
common class of state synchronization. You want to show the server's
latest data to your users, and you'd like your users changes to reflect
on the server.

We can't -- and don't want to -- keep server and client state exactly in
sync:

-   Users often want to explicitly commit changes to the server, and
    only do so if their changes are "valid"
-   Users expect some latency / loading time when fetching / refreshing
    data, or performing some operation.

The [`Rpc_effect` library](./rpcs.md) implements tools for polling and
dispatching one-shot actions to RPCs. We highly recommend using it
rather than trying to reimplement client-server communication yourself.

## `Bonsai_extra.Mirror`

Sometimes, despite our best attempts, we might just have to keep two
independent `Bonsai.t` states in sync. This is usually because the
`Bonsai.t`s in question are [external](#dealing-with-external-state), or
come from [uncontrollable
components](#lift-state-with-controllable-components) like the [Partial
Render Table's focus](./partial_render_table.md#focus).

If we have to synchronize state, the best way to do so is probably with
`Bonsai_extra.Mirror.mirror`, which synchronizes the state of an
"interactive" component (e.g. PRT focus) and a "backup store" (e.g. the
URL, local storage).

The gist of this combinator is that if you have two states that you'd
like to be synchronized, you can feed the "current value" and "set
value" functions for both states into `mirror` and they'll automatically
be kept up to date. Either of these can be backed by any kind of
structure, but there are some important differences in their symmetry.

On [activation](./lifecycles.md), `store` has priority, so if the values
are different, `store` wins, and `interactive` has its value "set". From
that point on, if either incoming value changes, the opposite setter is
called. In the case that both `store` and `interactive` change at the
same time, the tie is broken in favor of `interactive`, and `store_set`
is called.

Here's the type signature of `mirror`:

```{=html}
<!-- $MDX file=../../examples/bonsai_guide_code/bonsai_types.mli,part=mirror -->
```
``` ocaml
val mirror
  :  ?sexp_of_model:('m -> Sexp.t)
  -> equal:('m -> 'm -> bool)
  -> store_set:('m -> unit Effect.t) Bonsai.t
  -> store_value:'m Bonsai.t
  -> interactive_set:('m -> unit Effect.t) Bonsai.t
  -> interactive_value:'m Bonsai.t
  -> local_ Bonsai.graph
  -> unit
```

We prefer `mirror` over manual use of `on_change` because `mirror`
handles bi-directional syncing. It's also a clear indication to readers
that state is being synced, whereas `on_change` can be used for all
sorts of side effects.

Note that `mirror` is still not "perfect". State changes are scheduled
during Bonsai's lifecycle phase, and will be applied during the next
frame.

More importantly, `mirror` only works when the setters are synchronous:
if a `set` does not take effect within a frame (e.g. setting
`Bonsai.state`, or a `Bonsai.Expert.Var`), `mirror` will try to reflect
it back, and you'll enter an infinite loop of ping-ponging state between
your two components. For this reason, do not use `mirror` with async, or
setters that are delayed via `wait_after_display` or a [time
delay](./time.md#delaying-effects).
