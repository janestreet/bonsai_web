# Control Flow

In [chapter 3](./03-incrementality.md), we learned how to build and
compose a static graph of incremental `Bonsai.t`s using the `let%arr`
operator. But often, web UIs need to express some dynamic patterns, and
`let%arr` just isn't enough. In this chapter, we'll:

-   Use `match%sub` to conditionally evaluate `Bonsai.t`s
-   Evaluate a collection of `Bonsai.t`s separately for each of a
    dynamically-sized number of inputs
-   Learn what it means for a `Bonsai.t` to be "active" vs "inactive"
-   Remark on higher-order functions in Bonsai

## `match%sub`

Let's say we want to show a counter (built in [the state
chapter](./04-state.md)) only when `show: bool Bonsai.t` is true. With
the functions you've seen so far, you might write:

``` ocaml
let maybe_show_naive show (local_ graph) =
  let counter = counter ~step:(return 1) graph in
  let%arr counter and show in
  match show with
  | false -> Vdom.Node.none
  | true -> counter
;;
```

But because we are `let%arr`-ing on `counter`, the incremental runtime
will continuously recompute it, even when we aren't actually using it.

### Conditional Recomputation

We can avoid this and get a performance boost using Bonsai's
`match%sub`:

``` ocaml
let maybe_show show (local_ graph) =
  let counter = counter ~step:(return 1) graph in
  match%sub show with
  | false -> Bonsai.return Vdom.Node.none
  | true -> counter
;;
```

`match%sub` is like `match`, but for `Bonsai.t`s:

1.  The matched value should be a `'a Bonsai.t` or a literal tuple of
    `Bonsai.t`s.
2.  The values produced by each of the match-arms must be of type
    `'b Bonsai.t`.
3.  Any identifiers bound during matching are available as
    `'c Bonsai.t`s inside the arms. (You can access them as plain `'c`
    in guard clauses though.)
4.  The overall type of the `match%sub` expression has type
    `'b Bonsai.t`.

### Conditional Instantiation

`match%sub` has a superpower: you can use `graph` inside its arms. This
means we can instantiate some state that is local to one arm:

``` ocaml
let maybe_show_2 show (local_ graph) =
  match%sub show with
  | `Count_by_1 -> counter ~step:(return 1) graph
  | `Count_by_2 -> counter ~step:(return 2) graph
  | `No -> Bonsai.return Vdom.Node.none
;;
```

Note that each branch has an independent counter with its own state.
You'll see this if you increment the first counter and then switch to
the second. Interestingly, state does not go away when a branch ceases
to be active: as we noted [last chapter](./04-state.md), this is because
Bonsai maintains a central copy of the entire application state.

> **Note:** Bonsai provides some [lifecycle
> functions](../how_to/lifecycles.md) to schedule effects when a code
> block becomes active or inactive.

### Conditional Data Dependencies

We can also use `match%sub` to pattern-match just like regular `match`,
allowing us to conditionally access data:

``` ocaml
let maybe_show_var show (local_ graph) =
  match%sub show with
  | `Count_by step -> counter ~step graph
  | `No -> Bonsai.return Vdom.Node.none
;;
```

Note that all cases of `Count_by`, share the same counter state. That's
because they all go to the same branch of the `match%sub`. If we wanted
to create separate versions of state for individual cases of `step`, we
could use guard clauses to create multiple branches that match the same
pattern, each with their own locally instantiated state:

``` ocaml
let maybe_show_var_guard show (local_ graph) =
  match%sub show with
  | `Count_by step when Int.equal step 1 -> counter ~step graph
  | `Count_by step when Int.equal step 4 -> counter ~step graph
  | `Count_by step -> counter ~step graph
  | `No -> Bonsai.return Vdom.Node.none
;;
```

This particular case is pretty silly: we're not going to write separate
`match%sub` branches for every potential value of `int`. Instead, we
could use
[`scope_model`](https://github.com/janestreet/bonsai_web/blob/master/docs/how_to/state_per_key.md),
which maintains separate copies of state for some value of a key:

``` ocaml
let maybe_show_var_scope_model show (local_ graph) =
  match%sub show with
  | `Count_by step ->
    Bonsai.scope_model
      (module Int)
      ~on:step
      ~for_:(fun (local_ graph) -> counter ~step graph)
      graph
  | `No -> Bonsai.return Vdom.Node.none
;;
```

## Creating a Dynamic Number of `Bonsai.t`s

In the [last chapter](./04-state.md), we created two separate counters
by calling `counter graph` twice. But what if we want to create `n`
counters, where `n` is an `int Bonsai.t` that can change at runtime?

Let's try to build this with the tools we have:

``` ocaml
let multiple_counters (n : int Bonsai.t) (local_ graph) =
  let%arr n = n in
  let (counters : Vdom.Node.t Bonsai.t list) =
    List.init n ~f:(fun _ -> counter graph)
  in
  let%arr counters = Bonsai.all counters in
  Vdom.Node.div counters
```

As you can see above, this won't even compile: the content of `let%arr`
blocks is runtime code, so the `local_` mode bans you from using `graph`
within them. Furthermore, if this code compiled, the output *would* have
type `Vdom.Node.t Bonsai.t Bonsai.t`, which is illegal: remember, the
Bonsai computation graph has to be static.

Instead, we can use Bonsai's `assoc` primitive:

``` ocaml
val assoc
  :  here:[%call_pos]
  -> ('k, 'cmp) Comparator.Module.t
  -> ('k, 'v, 'cmp) Map.t Bonsai.t
  -> f:('k Bonsai.t -> 'v Bonsai.t -> local_ Bonsai.graph -> 'result Bonsai.t)
  -> local_ Bonsai.graph
  -> ('k, 'result, 'cmp) Map.t Bonsai.t
```

Bonsai will performantly and incrementally apply the transformation
described by `f` on every value in the input map to produce the output
map; think of it like `Map.mapi`, but on a `Map.t Bonsai.t` input, and
with the ability to use `graph` to instantiate things per-key.

> **Note:** `Comparator.Module.t` is a first class module with a
> `type t` and a `sexp_of` function.

Each key/value pair in the output map has its own independent state and
dependencies. This means that if the input map is 100,000 elements
large, but only one of the keys has data that is changing frequently,
only that key's instance will be re-run to recompute the overall output.
Here's an example, which will make multiple copies of the counter we
implemented [last chapter](./04-state.md):

``` ocaml
let multiple_counters (input : unit Int.Map.t Bonsai.t) (local_ graph) =
  let counters =
    Bonsai.assoc
      (module Int)
      input
      ~f:(fun key (_ : unit Bonsai.t) (local_ graph) ->
        let%arr key
        and counter, _ = counter graph in
        {%html|
          <tr>
            <td>counter #%{key#Int}:</td>
            <td>%{counter}</td>
          </tr>
        |})
      graph
  in
  let%arr counters in
  let counters_list = Map.data counters in
  {%html|
    <table>
      *{counters_list}
    </table>
  |}
;;
```

Note that if you add, remove, and re-add a counter, it will retain its
state. **Tip:** If your `Bonsai.assoc` produces `Vdom.Node.t`s, you
might want to use
[`Vdom.Node.Map_children`](./01-virtual_dom.md#diffing-lists) for more
efficient and stable diffing.

## Further Reading

-   `match%sub` and `Bonsai.assoc` are [higher-order
    functions](https://github.com/janestreet/bonsai_web/blob/master/docs/how_to/higher_order_functions.md)
-   The code inside `match%sub` branches or `assoc` can [become
    inactive](https://github.com/janestreet/bonsai_web/blob/master/docs/how_to/lifecycles.md)
    (lifecycle events).

