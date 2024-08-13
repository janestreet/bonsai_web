# Focus

[Focus](https://web.dev/articles/focus) determines where keyboard events
and listeners go.

## Focus and the Active Element

Your OS keeps track of which window has focus, and the browser provides
some APIs for the web apps to know if they are focused, and manage their
focus.

The [`Dom_html.document##activeElement`
property](https://developer.mozilla.org/en-US/docs/Web/API/Document/activeElement)
tells us which element has focus. Importantly, `activeElement` is not
cleared if the window loses focus. This means that if you switch tabs,
windows, open devtools, etc., your focus won't be lost when you come
back.

The [`hasFocus`
method](https://developer.mozilla.org/en-US/docs/Web/API/Document/hasFocus)
of `Dom_html.document` tells us whether the web app has focus. If focus
is on some iframe, `hasFocus` will return `true`, and `activeElement`
will be the iframe. If your app is in an iframe, but focus is an
ancestor page, then `hasFocus` will return `false` and `activeElement`
will be `body`.
````{=html}

You should avoid programmatically setting focus if `document.hasFocus()`
is false or `activeElement` is on some iframe, because that indicates
that user focus is not in your web app, and you might interrupt the user
if they are in an iframe, or your app is currently being iframed.

## Setting Focus

HTML elements have `focus` method, which can be used to programmatically
set and remove focus.

Bonsai provides an effect-based wrapper via `Effect.Focus.on_effect`:

```{=html}
<!-- $MDX file=../../examples/bonsai_guide_code/focus_examples.ml,part=effect_focus_api -->
```
``` ocaml
let effect_focus_demo (local_ graph) =
  let focus = Effect.Focus.on_effect () graph in
  let%arr { attr; focus; blur } = focus in
  Vdom.Node.div
    [ Vdom.Node.button
        ~attrs:[ Vdom.Attr.on_click (fun _ -> focus) ]
        [ Vdom.Node.text "focus input" ]
    ; Vdom.Node.button
        ~attrs:[ Vdom.Attr.on_click (fun _ -> blur) ]
        [ Vdom.Node.text "blur input" ]
    ; Vdom.Node.input ~attrs:[ attr ] ()
    ]
;;
```

```{=html}
<iframe data-external="1" src="https://bonsai:8535#effect_focus_api">
```
```{=html}
</iframe>
```
You can also use `Effect.Focus.on_activate` to focus elements on the
[on_activate lifecycle event](./lifecycles.mdx):

```{=html}
<!-- $MDX file=../../examples/bonsai_guide_code/focus_examples.ml,part=effect_focus_onactivate -->
```
``` ocaml
let effect_focus_onactivate (local_ graph) =
  let visible, set_visible = Bonsai.state false graph in
  let subview =
    match%sub visible with
    | false -> return Vdom.Node.none
    | true ->
      let autofocus = Effect.Focus.on_activate () graph in
      let%arr autofocus in
      Vdom.Node.input ~attrs:[ autofocus ] ()
  in
  let%arr visible and set_visible and subview in
  Vdom.Node.div
    [ Vdom.Node.button
        ~attrs:[ Vdom.Attr.on_click (fun _ -> set_visible (not visible)) ]
        [ Vdom.Node.text "toggle" ]
    ; subview
    ]
;;
```

```{=html}
<iframe data-external="1" src="https://bonsai:8535#effect_focus_onactivate">
```
```{=html}
</iframe>
```
Note that `Effect.Focus.on_activate` must be instantiated inside the
control flow operator whose `on_activate` you care about; the example
above would not work if `Effect.Focus.on_activate` was instantiated
outside the `match%sub`.

`Effect.Focus.on_activate` might also fail if the DOM element you want
to focus isn't created or made visible within one frame after its Bonsai
code becomes active. For example, it won't autofocus elements in the
modals and popovers provided by
[Bonsai_web_ui_toplayer](https://github.com/janestreet/bonsai/tree/master/web_ui/toplayer),
because they aren't displayed for a few frames after they are created.

Instead, you can use the [`Vdom.Attr.autofocus`
attribute](https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/autofocus):

```{=html}
<!-- $MDX file=../../examples/bonsai_guide_code/focus_examples.ml,part=autofocus -->
```
``` ocaml
module Toplayer = Bonsai_web_ui_toplayer

let autofocus (local_ graph) =
  let { Toplayer.Controls.open_; _ } =
    Toplayer.Modal.create
      ~content:(fun ~close:_ _ ->
        return (Vdom.Node.input ~attrs:[ Vdom.Attr.autofocus true ] ()))
      graph
  in
  let%arr open_ in
  Vdom.Node.div
    [ Vdom.Node.button
        ~attrs:[ Vdom.Attr.on_click (fun _ -> open_) ]
        [ Vdom.Node.text "open modal" ]
    ]
;;
```

```{=html}
<iframe data-external="1" src="https://bonsai:8535#autofocus">
```
```{=html}
</iframe>
```
### Focusable Elements

Not all elements are focusable! See [this
table](https://allyjs.io/data-tables/focusable.html) for a fairly
comprehensive list, but as a general rule, the following can be focused:

-   Links with a `href` attribute
-   Buttons, inputs, textareas, and other interactive elements that do
    **not** have a `disabled` attribute.
-   Iframes
-   Any element that sets the [`tabindex`
    attribute](https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/tabindex).

## The Underlying Machinery

### Bonsai Focus Stealer

In Chrome, the default `activeElement` is the `<body />`. If an element
loses focus, typically via the `blur` method or the element being
removed from the DOM, `activeElement` goes back to the `<body />`.

This can cause problems, because Bonsai apps mount into a `<div />`
under `<body />`, not directly into `<body />`. If focus is on
`<body />`, any keyboard event listeners set on the app root will not
work. To counteract this, Bonsai runs a focus stealer on:

-   Initial page load
-   Every frame
-   Every `blur` event where `relatedTarget` is falsey

, which will move focus to the app root if focus has been moved to
`<body />` **AND** `document.hasFocus()` is `true`.
