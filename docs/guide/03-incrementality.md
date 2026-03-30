# Incrementality

In the last 2 chapters, we learned how to build functional web UIs with
`virtual_dom`, and schedule side effects in response to user interaction
with `Effect.t`.

For applications with a large amount of frequently-changing input data,
it's important that we only re-compute the parts of the application that
actually depend on the new data. In this chapter, we'll:

-   Learn how to build and compose incremental computations via the
    `Bonsai.t` type and `let%arr` operator
-   Note that the Directed Acyclical Graph (DAG) of `Bonsai.t`s is
    actually static

## `Bonsai.t`

Bonsai is all about constructing graphs of incremental nodes. Some of
these are stateful, but most are derived as a function of the current
values of other nodes. A good analogy to help understand Bonsai is that
of the spreadsheet. From our blog post introducing the [Incremental
library](https://blog.janestreet.com/introducing-incremental/):

> In a spreadsheet, each cell contains either simple data, or an
> equation that describes how the value in this cell should be derived
> from values in other cells. Collectively, this amounts to a
> graph-structured computation, and one of the critical optimizations in
> Excel is that when some of the cells change, Excel only recomputes the
> parts of the graph that depend on those changed cells.

A `'a Bonsai.t` is a node in the incremental graph, kind of like a cell
in a spreadsheet.

`val Bonsai.return : 'a -> 'a t` wraps a plain OCaml value in a
`Bonsai.t`. This is like an Excel cell that contains some constant
value.

To create a new `Bonsai.t` as a function of other `Bonsai.t`s, we can
use the `let%arr` operator. It works just like [`ppx_let`'s
`let%map`](https://github.com/janestreet/ppx_let), but with some extra
performance optimizations for pattern matching on incremental values.
This is like a formula cell in Excel.

``` ocaml
let int_view (a : int Bonsai.t) : Vdom.Node.t Bonsai.t =
  let%arr (a : int) = (a : int Bonsai.t) in
  {%html|<div>%{a#Int}</div>|}
;;
```

Most of your `let%arr`s will compute a `Bonsai.t` as a function of
multiple `Bonsai.t`s:

``` ocaml
let sum_and_display (a : int Bonsai.t) (b : int Bonsai.t)
  : Vdom.Node.t Bonsai.t
  =
  let%arr a and b in
  Vdom.Node.textf "%d + %d = %d" a b (a + b)
;;
```

### `let%arr` vs `let%map` vs `Bonsai.map` vs `>>|`

`let%arr` is just pretty syntax for
`val Bonsai.map : 'a t -> f:('a -> 'b) -> 'b t`, with an incremental
[cutoff](../how_to/cutoff.md) against any ignored patterns. So if you
`let%arr` on a record, but only care about some of the fields,
`let%arr`s will only recompute when that field changes:

``` ocaml
let%arr { foo; _ } = my_thing in
do_something foo
```

If you don't destructure in the `let%arr`:

``` ocaml
let%arr my_thing in
do_something my_thing.foo
```

Or use `let%map`:

``` ocaml
let%map { foo; _ } = my_thing in
do_something foo
```

`do_something` will be recomputed every time *any* field of `my_thing`
changes.

As a guideline:

-   ALWAYS use `let%arr` instead of `let%map`
-   `Bonsai.map` and `>>|` don't have the cutoff, but are sometimes
    nicer for ergonomic reasons. Only use them for very simple / fast
    operations
-   Every `Bonsai.map` / `>>|` adds incremental nodes, so strongly
    prefer a single `let%arr` over chaining multiple `>>|`.

## `let%arr` Must Be Pure!

It might be tempting to react to changes in a `Bonsai.t` by running side
effects in a `let%arr` that depends on it. For example:

``` ocaml
let print_on_change (a : int Bonsai.t) : int Bonsai.t =
  let%arr a in
  print_endline [%string "state is now %{a#Int}"];
  a
;;
```

or

``` ocaml
let dispatch_query_on_change (a : int Bonsai.t) : int Bonsai.t =
  let%arr a in
  send_http_request [%string "https://example.com?query=%{a#Int}"];
  a
;;
```

Do not do this! A `let%arr`:

-   might run multiple times per frame
-   might run as part of some code that starts as inactive, switches to
    being active, and then becomes [inactive](../how_to/lifecycles.md)
    again, all in the same frame.
-   will only run when its explicit dependencies change
-   not run at all, if it is not linked into the incremental computation
    of your app's result

Note that "get" operations can also be side effects! For example, the
following will recompute when `prefix` or `suffix` changes, but not when
the [document
title](https://developer.mozilla.org/en-US/docs/Web/API/Document/title)
changes:

``` ocaml
let compute_title (prefix : string Bonsai.t) (suffix : string Bonsai.t) : string Bonsai.t =
  let%arr prefix and suffix in
  (* https://developer.mozilla.org/en-US/docs/Web/API/Document/title *)
  let document_title = get_document_title () in
  [%string "%{prefix} %{document_title} %{suffix}"]
;;
```

This is one reason why side effects should be performed within a
`'a Effect.t`: there's a limited set of safe APIs for running
`Effect.t`s, so you can't accidentally run one within a `let%arr`.

If you need to do something whenever a `'a Bonsai.t` changes, use
edge-triggered effects like
[`Edge.on_change`](../how_to/edge_triggered_effects.md) or [lifecycle
events](../how_to/lifecycles.md).

## Don't Do Work While Computing `Effect.t`s

Most [`Effect.t`s](./02-effects.md) you'll see are incrementally
computed, because most side effects you might want to perform depend on
some `'a Bonsai.t`. An easy mistake to make when incrementally computing
an `Effect.t` is to do part of its work during computation. For example:

``` ocaml
let copy_to_clipboard_button
  (data : Big_data.t Bonsai.t)
  (label : string Bonsai.t)
  : Vdom.Node.t Bonsai.t
  =
  let on_click =
    let%arr data in
    let serialized_data = Big_data.sexp_of_t data |> Sexp.to_string in
    Bonsai_web_clipboard.copy_text serialized_data
  in
  let%arr on_click and label in
  {%html|<button on_click=%{fun _ -> on_click}>Copy: #{label}</button>|}
;;
```

Serializing data is pure, so this code isn't *incorrect*, but it's very
*inefficient*, because we're doing an expensive serialization at least
once every time `data` changes, but we don't actually use the result
unless the user clicks the button.

Instead, we can move this work inside the `Effect.t`:

``` ocaml
let copy_to_clipboard_button
  (data : Big_data.t Bonsai.t)
  (label : string Bonsai.t)
  : Vdom.Node.t Bonsai.t
  =
  let on_click =
    let%arr data in
    let%bind.Effect serialized_data =
      Effect.of_thunk (fun () -> Big_data.sexp_of_t data |> Sexp.to_string)
    in
    Bonsai_web_clipboard.copy_text serialized_data
  in
  let%arr on_click and label in
  {%html|<button on_click=%{fun _ -> on_click}>Copy: #{label}</button>|}
;;
```

Now, we only serialize when the user clicks the button!

There's still a potential bug if the `data` changes after `view` was
last rendered, but before the user clicks the button. We can solve this
with a [`Bonsai.peek`](../how_to/effects_and_stale_values.md).

## Incremental Structure Matters

When writing code, it can be beneficial to "factor out" expensive,
reused things.

For instance, in this code, we run `expensive_calculate_exponent` every
loop, even though it will be the same every time:

``` ocaml
let analyze_list (big_list : float list) (risk_parameter : float) : float =
  List.fold big_list ~init:0. ~f:(fun sum x ->
    sum +. (x ** expensive_calculate_exponent ~risk_parameter))
;;
```

Breaking it out into an "intermediate" computation is an easy
performance win:

``` ocaml
let analyze_list (big_list : float list) (risk_parameter : float) : float =
  let exponent = expensive_calculate_exponent ~risk_parameter in
  List.fold big_list ~init:0. ~f:(fun sum x -> sum +. (x ** exponent))
;;
```

You can apply a similar concept to incremental computations. Imagine we
want to implement the following function:

    F(a, b, c) = (a^b) / c

If we don't know anything about `a` and `b`, we would probably write:

``` ocaml
let exp_and_divide (a : float Bonsai.t) (b : float Bonsai.t) (c : float Bonsai.t) =
  let%arr a and b and c in
  (a ** b) /. c
;;
```

But if we know that `c` changes much more frequently than `a` and `b`,
we can break out the expensive float exponentiation into an intermediate
computation:

``` ocaml
let exp_and_divide (a : float Bonsai.t) (b : float Bonsai.t) (c : float Bonsai.t) =
  let dividend =
    let%arr a and b in
    a ** b
  in
  let%arr dividend and c in
  dividend /. c
;;
```

That being said, incremental nodes aren't free, and you should [avoid
over-incrementalizing](../how_to/best_practices_pitfalls.md#dont-over-incrementalize).

For incrementality to be useful, inputs need to actually change. On to
[Chapter 4: state](./04-state.md)!

## The Underlying Machinery

`Bonsai.t` is actually a wrapper around Incremental's `Incr.t`. The
biggest user-facing difference is that there is no `Bonsai.bind`, which
forces the computation graph to have a static shape. This enables some
[useful features and performance
optimizations](../advanced/why_no_bind.md). We'll learn how to write
control flow code without `bind` in a [later
chapter](./05-control_flow.md).

