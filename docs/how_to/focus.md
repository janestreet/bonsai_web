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

## Getting Focus

It is tempting to try and build a
`val focused_element : Dom_html.element Js.t Bonsai.t`, so your Bonsai
code can subscribe to the currently focused element. This is a bad idea!

DOM nodes are mutable, and you [should not put mutable things in
`Bonsai.t`s](./best_practices_pitfalls.md#no-mutable-models): if you
were to `let%arr` on a `Dom_html.element Js.t`, your code would not
re-run on changes to that DOM node. This is particularly important
because the vdom diff/patch algorithm might reuse a DOM node for
something completely different.

Separately, tracking the active element is finnicky; e.g. the `focus`
and `blur` events don't bubble, and `blur` won't fire if the active
element is removed from the DOM.

It's ok to fetch the `Dom_html.document##activeElement` as part of an
`Effect.t` / in event handlers, but don't try to store it anywhere.

Some Bonsai components implement their own state machines simulating
focus in terms of some OCaml key, rather than rely on DOM focus. The
[PRT](./partial_render_table.md#focus) has a fairly advanced focus state
machine.

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
[on_activate lifecycle event](./lifecycles.md):

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
to focus isn't created or made within the same frame that its Bonsai
code becomes active. This probablt means there's a bug / delayed [state
synchronization](./organizing_state.md) in your UI component.

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
