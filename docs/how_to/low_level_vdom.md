# Low Level Vdom

This article will be most useful for maintainers of low-level tools and
reusable components. The vast majority of web apps should not need to
use hooks/widgets directly.

Some components need more control over their DOM than the standard suite
of `Vdom.Node.*`s and `Vdom.Attr.*`s provides. In particular, you might
need to:

-   Run some code whenever a node is mounted into the DOM, patched, or
    removed from the DOM
-   Take over rendering of some subtree of the DOM

The `virtual_dom` library supports this through "hooks" and "widgets".

## Hooks

A `virtual_dom` hook is a special `Vdom.Attr.t` that allows you to run
custom code:

-   Whenever it is attached to a DOM element (which might not yet be
    mounted in the DOM)
-   After the DOM element it is attached to is mounted into the DOM
-   Whenever the DOM element it is attached to is patched
-   Whenever the hook, or the DOM element it is attached to, is removed
    from the DOM

Hooks can be created with some input value, and can define state.

Some use cases of hooks include:

-   Attaching custom event listeners
-   Using various browser-provided observers
-   Portalling tooltips

Here's an example illustrating the API:

```{=html}
<!-- $MDX file=../../examples/bonsai_guide_code/low_level_vdom_examples.ml,part=noop_hook -->
```
``` ocaml
module Noop_hook = struct
  module T = struct
    module Input = struct
      (* Hooks don't run in [Bonsai_web_test] tests; instead, we display an attribute with
         the input name, and a sexp of the input value. *)
      type t = unit [@@deriving sexp_of]

      (* Multiple hooks of the same "class" get merged by Vdom. You'll need to implement
         receiving multiple sets of inputs. *)
      let combine () () = ()
    end

    module State = Unit

    let init (_input : Input.t) (_element : Dom_html.element Js.t) =
      (* [init] will run once the hook is attached to a DOM node. The DOM node might not
         yet be mounted! This means that you can e.g. attach event listeners, but not
         query the DOM / layout / size / etc.

         [init]'s main job is to create a [State.t], which often has some mutable fields.
      *)
      ()
    ;;

    let on_mount (_input : Input.t) (_state : State.t) (_element : Dom_html.element Js.t) =
      (* [on_mount] will run once the DOM node has been connected. There are 3 options:
         - [`Do_nothing] won't run an [on_mount]
         - [`Schedule_immediately_after_this_dom_patch_completes] will synchronously run
           [on_mount] immediately after the patch completes. This only works if the patch
           is done by Bonsai; calling [Vdom.Node.to_dom] outside of a hook or widget won't
           run [on_mount]
         - [`Schedule_animation_frame] will run [on_mount] via [requestAnimationFrame].
           This is not recommended, because it adds frame delays, and forces
           asynchronicity.
      *)
      ()
    ;;

    let on_mount = `Schedule_immediately_after_this_dom_patch_completes on_mount

    let update
      ~old_input:_
      ~new_input:_
      (_state : State.t)
      (_element : Dom_html.element Js.t)
      =
      (* [update] is called on **every patch** to the DOM node. This is usually where you
         would handle changes to inputs. Most hooks should explicitly check for input
         equality to avoid unnecessary work. *)
      ()
    ;;

    let destroy (_input : Input.t) (_state : State.t) (_element : Dom_html.element Js.t) =
      (* [destroy] will run once the hook is removed from the DOM. This could mean that:
         - The element was removed
         - The hook was removed
         - A diff moved the hook from one element to another *)
      ()
    ;;
  end

  include T

  (* It is important that the hook functor is called exactly once for each "class" of
     hooks. *)
  include Vdom.Attr.Hooks.Make (T)
end

(* Typically, this would be a function that takes inputs, and passes them to
   [Hook_module.create]. *)
let noop_hook : Vdom.Attr.t = Vdom.Attr.create_hook "noop" (Noop_hook.create ())
```

### How do hooks work?

The underlying [vdom
library](https://github.com/Matt-Esch/virtual-dom/blob/master/docs/hooks.md)'s
hook API is much lower-level: it just allows running some `hook`
function whenever a `Vdom.Node.t` gets patched, and another `unhook`
function when it gets removed.

Recall that `Vdom.Node.t`s and `Vdom.Attr.t`s are immutable, so we're
running a different "hook instance" every patch. But the
`Vdom.Hooks.Make` functor creates a `Type_equal.Id.t` that is then
shared between all `Vdom.Attr.t`s created via `My_hook.create`.

Our `virtual_dom` hook implementation uses the lower-level `hook` to
check if there's a prior instance of this hook has ran on this DOM node.
It does so by stashing some state onto the DOM node, containing the
user-defined `State.t`, and the current `Input.t`. The state is keyed by
the `Type_equal.Id.t` shared by all instances of the hook.

If state already exists, we run `update`. Otherwise, we run `init`, and
schedule `on_mount` to run, either via `requestAnimationFrame`, or upon
the completion of the current VDom patch. The latter is preferable
because it will run synchronously and not be delayed by a frame, but its
implementation is custom, and requires that the patch is happening as
part of the Bonsai runtime loop (i.e. not randomly calling
`Vdom.Node.to_dom`).

On `unhook`, we fetch our stashed state, and call `destroy`.

This is why it is important to use the `Vdom.Hooks.Make` functor only
once for each "class" of hook. If you call the functor in the function
that computes your `Vdom.Attr.t`, you'll end up running `destroy` +
`init` + `on_mount` for every frame.

## Widgets

A widget allows you to take over the diffing/patching of a subtree of
the DOM. This is particularly useful for embedding components from
external libraries, e.g. codemirror, charting/plotting library, or a
Canvas implementation of a component.

Similarly to hooks, widgets can take inputs, and maintain state.

Here's an example illustrating the API:

```{=html}
<!-- $MDX file=../../examples/bonsai_guide_code/low_level_vdom_examples.ml,part=trivial_widget -->
```
``` ocaml
open Js_of_ocaml

module Trivial_widget = struct
  (* Hooks force you to define the type of DOM node you're using. If you're not sure, or
     it's dynamic, you can use [Dom_html.element]. *)
  type dom = Dom_html.divElement

  let name = "trivial_widget"

  module Input = Bool
  module State = Int

  (* Just a helper function. *)
  let redraw input state element =
    let display = if input then state else state * -1 in
    element##.innerHTML := Js.string (sprintf "%d" display)
  ;;

  (* [create] will run on patch when the vdom is being converted to a DOM node. Note that
     there are NO SAFEGUARDS against script injection, bad inputs, etc. You are
     responsible for following security best practices. *)
  let create input =
    let state = Random.int 100 in
    let element = Dom_html.createDiv Dom_html.document in
    redraw input state element;
    state, element
  ;;

  (* [update] will run on every subsequent patch. *)
  let update ~prev_input ~input ~state ~element =
    match [%equal: Input.t] prev_input input with
    | true -> state, element
    | false ->
      redraw input state element;
      state, element
  ;;

  (* [destroy] will run when the widget is removed from the DOM. Do any cleanup here; e.g.
     removing event listeners, undoing side effects, etc. *)
  let destroy ~prev_input:_ ~state:_ ~element:_ = ()

  (* As with hooks, widgets don't run in [Bonsai_web_test]. But widgets give us a bit more
     control over how their input is displayed in tests. You could also just pass
     [let to_vdom_for_testing = `Sexp_of_input]. *)
  let to_vdom_for_testing =
    `Custom
      (fun input ->
        Vdom.Node.create "trivial_widget" [ Vdom.Node.textf "Inverted: %b" input ])
  ;;
end

(* As with [Vdom.Attr.Hooks.Make], it is important that [Vdom.Node.widget_of_module] is
   called exactly once for each "class" of widgets. *)
let (trivial_widget : bool -> Vdom.Node.t) =
  unstage (Vdom.Node.widget_of_module (module Trivial_widget))
;;

let app (local_ graph) =
  let invert, toggle_invert = Bonsai.toggle ~default_model:false graph in
  let%arr invert and toggle_invert in
  let widget = trivial_widget invert in
  (* Note that each usage of widget has a separate internal state! *)
  {%html|
    <div>
      <button on_click=%{fun _ -> toggle_invert}>Invert</button>
      %{widget} %{widget} %{widget}
    </div>
  |}
;;
```

```{=html}
<iframe data-external="1" src="https://bonsai:8535#trivial_widget">
```
```{=html}
</iframe>
```
You can use \[Vdom.Node.t\]s in your widgets. Here's an example that
renders a `Vdom.Node.t` that it receives as input, and also counts the
number of patches:

```{=html}
<!-- $MDX file=../../examples/bonsai_guide_code/low_level_vdom_examples.ml,part=widget_using_diff_patch -->
```
``` ocaml
open Js_of_ocaml

module Widget_using_diff_patch = struct
  type dom = Dom_html.element

  let name = "widget_using_diff_patch"

  module Input = struct
    type t =
      { set_num_patches : int -> unit Effect.t
      ; vdom : Vdom.Node.t
      }

    let sexp_of_t = sexp_of_opaque
  end

  module State = struct
    type t = { num_patches : int } [@@deriving sexp_of]
  end

  let create { Input.vdom; _ } =
    let initial_element = Vdom.Node.to_dom vdom in
    let state = { State.num_patches = 0 } in
    state, initial_element
  ;;

  let update ~(prev_input : Input.t) ~(input : Input.t) ~(state : State.t) ~element =
    let num_patches = state.num_patches + 1 in
    (* You might need to use [Effect.Expert.handle_non_dom_event_exn] from within hooks
       and widgets to interop with Bonsai. Do not do so anywhere else! Also, this example
       dispatches an effect on every patch, which is a bad idea. *)
    Effect.Expert.handle_non_dom_event_exn (input.set_num_patches num_patches);
    match phys_equal input.vdom prev_input.vdom with
    | true -> { State.num_patches }, element
    | false ->
      let patch = Vdom.Node.Patch.create ~previous:prev_input.vdom ~current:input.vdom in
      let new_element = Vdom.Node.Patch.apply patch element in
      { num_patches }, new_element
  ;;

  let destroy ~prev_input:_ ~state:_ ~element:_ = ()
  let to_vdom_for_testing = `Sexp_of_input
end

let widget_using_diff_patch =
  unstage (Vdom.Node.widget_of_module (module Widget_using_diff_patch))
;;

let app (local_ graph) =
  let num_clicks, update_num_clicks = Bonsai.state' 0 graph in
  let num_patches, set_num_patches = Bonsai.state 0 graph in
  let view =
    let%arr num_patches and num_clicks in
    {%html|
      <div>
        <p>Clicks: %{num_clicks#Int}</p>
        <p>Patches: %{num_patches#Int}</p>
      </div>
    |}
  in
  let%arr view and set_num_patches and update_num_clicks in
  (* NOTE: If we used this widget more than once, the separate widget instances would be
     independently setting the same state. Usually, you'll want to account for this by
     maintaining state as a keyed map, rather than a single value. *)
  let widget =
    widget_using_diff_patch { Widget_using_diff_patch.Input.vdom = view; set_num_patches }
  in
  {%html|
    <div>
      <button on_click=%{fun _ -> update_num_clicks (fun x -> x + 1)}>
        Click me
      </button>
      %{widget}
    </div>
  |}
;;
```

```{=html}
<iframe data-external="1" src="https://bonsai:8535#widget_using_diff_patch">
```
```{=html}
</iframe>
```
Note that the patch counter only starts increasing when we click. That's
because the widget's \[create\] doesn't update the Bonsai state, and
we'll only redraw when our view has changed.

There's also a lighter-weight `Vdom.Node.widget` API, which takes
functions instead of a module.

### How do widgets work?

As with [hooks](#how-do-hooks-work), we use an underlying primitive from
the [JavaScript vdom
library](https://github.com/Matt-Esch/virtual-dom/blob/master/docs/widget.md).
It lets us use a custom object with `init`, `update`, and `destroy`
methods. Similarly to hooks, we stash our state on the DOM node, so we
can associate widgets into a class via a \[Type_equal.Id.t\], which is
created when `Vdom.Node.widget_of_module` is called.

## Bonsai lifecycle vs DOM lifecycle

The vast majority of Bonsai code is functional computations over
immutable data structures. Remember that `Vdom.Node.t`s are immutable
declarations of what the view should look like.

Hooks and widgets let you tap into the lifecycle of *DOM nodes*, which
are very mutable. Keep in mind that this lifecycle is separate from the
[lifecycles](./lifecycles.md) and
[on_change](./edge_triggered_effects.md) offered by Bonsai.

This is particularly important if your hook / widget contains some state
that you want to interact with (e.g. read, synchronize) from Bonsai.

A few things to keep in mind:

-   A given `Vdom.Node.t` or `Vdom.Attr.t` (which could contain your
    hooks and widgets) could be used in the DOM multiple times. Each
    copy will receive the same `Input.t`s (because it's made from the
    same `Vdom.Node.t`), but will have a separate `State.t`.
-   Virtual DOM does not "move" nodes, it will destroy, re-create, and
    potentially reuse DOM nodes. In particular, if a sibling node is
    added before a `Vdom.Node.t`, or any of its ancestors, it will be
    destroyed and re-created. Do not assume that your `init` or
    `on_mount` will only run once per node, and make sure to clean up
    via `destroy`.
-   Avoid mutating DOM nodes from hooks/widgets, because any mutations
    could clash with `virtual_dom`'s diffing + patching. For instance,
    if your hook adds a CSS class to some element, but then that
    element's corresponding vdom is updated, that CSS class could get
    lost, or might carry over to an unrelated DOM node.

## Bonsai Interop

The `Bonsai_web_ui_low_level_vdom` library wraps the `virtual_dom` hook
and widget APIs with a Bonsai layer, which gives you the ability to
access the inputs and states of hooks / widgets through an `Effect.t`
API.

```{=html}
```
