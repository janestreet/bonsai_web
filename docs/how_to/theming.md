# `Bonsai.View` and Theming

NOTE: We are in the process of deprecating the `theme` API and
`Bonsai_web_ui_view` in favor of styled UI component libraries. We do
not recommend using `theme` in new apps.

The `Bonsai_web.View` library defines themable, higher-level helpers for
common UI patterns and elements. It enables the creation of unstyled,
"functional" UI components.

## Simplified Vdom API

The \[Vdom.Node\] and \[Vdom.Attr\] modules contain primitives for
constructing virtual-dom nodes which mirror the browsers DOM API. This
is great for users who know exactly what they want the document to
contain, but all-things-considered, is a pretty low-level interface.

\[Bonsai_web.View\] is - at its core - a higher-level API for building
\[Vdom.\*.t\]. For instance, it can simplify common stuff like [flexbox
containers](https://flexboxfroggy.com/), or even text:

```{=html}
<!-- $MDX file=../../examples/bonsai_guide_code/theming_examples.ml,part=view_unthemed -->
```
``` ocaml
let text_a = Vdom.Node.span [ Vdom.Node.text "A" ]
let text_b = Vdom.Node.span [ Vdom.Node.text "B" ]
let text_c = Vdom.Node.span [ Vdom.Node.text "C" ]

let flex_container =
  Vdom.Node.div
    ~attrs:
      [ {%css|
          flex-direction: row;
          flex-wrap: wrap;
        |}
      ]
    [ text_a; text_b; text_c ]
;;

(* vs *)

let text_a', text_b', text_c' = View.text "A", View.text "B", View.text "C"
let flex_container' = View.hbox_wrap [ text_a'; text_b'; text_c' ]
```

```{=html}
<iframe data-external="1" src="https://bonsai:8535#view_button">
```
```{=html}
</iframe>
```
There are also helper functions with simplified APIs for common UI
elements like buttons:

```{=html}
<!-- $MDX file=../../examples/bonsai_guide_code/theming_examples.ml,part=view_button -->
```
``` ocaml
open Virtual_dom

let button =
  Vdom.Node.button
    ~attrs:[ Vdom.Attr.on_click (fun _ -> do_thing) ]
    [ Vdom.Node.text "click me" ]
;;

(* vs *)

let button' = View.button theme ~on_click:do_thing "click me"
```

```{=html}
<iframe data-external="1" src="https://bonsai:8535#view_button">
```
```{=html}
</iframe>
```
See [these
demos](https://github.com/janestreet/bonsai/tree/master/examples/bonsai_view)
for a full list of utils offered by `Bonsai_web.View`.

## Themability

You may have noticed that the `button` helper took a
`theme : View.Theme.t` argument. We can get the current theme anywhere
we have a `local_ Bonsai.graph` by calling `View.Theme.current`:

```{=html}
<!-- $MDX file=../../examples/bonsai_guide_code/theming_examples.ml,part=button_get_theme -->
```
``` ocaml
let error_button (local_ graph) =
  let theme = View.Theme.current graph in
  let%arr theme in
  View.button theme ~intent:Error ~on_click:do_thing "Error button"
;;
```

```{=html}
<iframe data-external="1" src="https://bonsai:8535#button_get_theme">
```
```{=html}
</iframe>
```
The `View.*` functions themselves are pure OCaml functions, so they can
be ergonomically used in place of `Vdom.Node.*` without having to
`let%arr` on each one separately. That's why we have to pass them a
`theme` explicitly.

### Setting themes

Bonsai themes include constants for colors / sizing, as well as
implementations of the themed `View.*` functions. By switching the
theme, we can change the look + feel of our app.

To set the theme, wrap your app's top-level
`local_ graph -> Vdom.Node.t Bonsai.t` in a call to
`View.Theme.set_for_app`, passing in the theme of your choice. For
example:

```{=html}
<!-- $MDX file=../../examples/bonsai_guide_code/theming_examples.ml,part=set_theme -->
```
``` ocaml
let app (local_ graph) =
  View.Theme.set_for_app (Kado.theme ~version:V1 () |> Bonsai.return) error_button graph
;;
```

```{=html}
<iframe data-external="1" src="https://bonsai:8535#set_theme">
```
```{=html}
</iframe>
```
Note that the theme you set is be a `Bonsai.t`, so we can determine it
dynamically:

```{=html}
<!-- $MDX file=../../examples/bonsai_guide_code/theming_examples.ml,part=theme_toggle -->
```
``` ocaml
let themed_theme_toggler ~toggle_dark (local_ graph) =
  let%arr theme = View.Theme.current graph
  and toggle_dark
  and error_button = error_button graph in
  View.hbox [ View.button theme ~on_click:toggle_dark "Toggle Dark Mode"; error_button ]
;;

let app (local_ graph) =
  let theme_style, set_theme_style = Bonsai.state Kado.Style.Dark graph in
  let theme =
    let%arr theme_style in
    Kado.theme ~style:theme_style ~version:V1 ()
  in
  let toggle_dark =
    let%arr theme_style and set_theme_style in
    set_theme_style
      (match theme_style with
       | Dark -> Light
       | Light -> Dark)
  in
  View.Theme.set_for_app theme (themed_theme_toggler ~toggle_dark) graph
;;
```

```{=html}
<iframe data-external="1" src="https://bonsai:8535#theme_toggle">
```
```{=html}
</iframe>
```
`View.Theme.current` is a [dynamically-scoped
variable](./dynamic_scope.md).

### Overriding Themes

Any part of a theme can be overriden, so you can customize things
without writing [a whole new theme](#for-theme-authors).

Most tweaks to themes can be done by overriding the theme's
[View.Constants.t](https://github.com/janestreet/bonsai/blob/master/web_ui/view/src/constants.ml).
For instance, let's make the error button from before have a purple
color:

```{=html}
<!-- $MDX file=../../examples/bonsai_guide_code/theming_examples.ml,part=override_constants -->
```
``` ocaml
let app (local_ graph) =
  View.Theme.override_constants_for_computation
    error_button
    ~f:(fun constants ->
      { constants with
        intent =
          { constants.intent with
            error = { background = `Hex "#4c0121"; foreground = `Hex "#e39ff6" }
          }
      })
    graph
;;
```

```{=html}
<iframe data-external="1" src="https://bonsai:8535#override_constants">
```
```{=html}
</iframe>
```
But that's not all. `Theme.t`s are internally represented as
[objects](https://dev.realworldocaml.org/objects.html), because their
"open recursion" propert mean that if you override a method `x`, every
other method that references `x` will also be overridden.

For example, let's replace `View.button`'s implementation with something
that's... not a button:

```{=html}
<!-- $MDX file=../../examples/bonsai_guide_code/theming_examples.ml,part=override_theme -->
```
``` ocaml
let app (local_ graph) =
  View.Expert.override_theme_for_computation
    error_button
    ~f:(fun (module S) ->
      (module struct
        class c =
          object
            inherit S.c

            method! button ~attrs:_ ~disabled:_ ~intent:_ ~tooltip:_ ~on_click:_ _content
                =
              Vdom.Node.text "Buttons are banned by the Great Convention."
          end
      end))
    graph
;;
```

```{=html}
<iframe data-external="1" src="https://bonsai:8535#override_theme">
```
```{=html}
</iframe>
```
To see everything overridable via theme, see
[view/src/underlying_intf.ml](https://github.com/janestreet/bonsai/blob/master/web_ui/view/src/underlying_intf.ml).
Note that some themable things are broken up into multiple methods; for
instance, you can completely replace `View.tooltip_attr` by overriding
the `tooltip_attr` method, or you can override just the style attribute
by overriding the `toplayer_tooltip_styles` method.

So if you want to override something in a theme, the order of places you
look should be:

1.  `Constants.t`
2.  "helper" methods in the `Theme.t` object
3.  The "main" related method in the `Theme.t` object

## Unstyled Functionality Components

It's common to build general-purpose components that solve hard
problems, like popovers, [performant tables](./partial_render_table.md),
code editors, form elements, etc.

Ideally, we'd like to use these in any app while preserving the
look+feel of that app. We could add "appearance" parameters to the
function signature. But an unstyled component that composes other
unstyled components needs to expose *all of their* config parameters in
its API, which quickly becomes unwieldy. Consistently versioning the
appearances of all these components is also dificult.

Instead, unstyled components should:

-   Be written in terms of `View.*` functions where possible, so any
    buttons/cards/badges inside them are automatically themed.
-   Pull any universal appearance constants and config from the current
    `View.Theme.t`. Any per-instance config (like position of a tooltip)
    should be a function argument, but app-wide appearance (like
    padding, border color, etc) should come from the theme.

"Unstyled" means that these components don't come with a fixed set of
styles, not that they cant be styled. The whole point is that they are
styleable through themes.

## Styled Component Libraries

The `View.*` functions are convenient helpers, but they are inherently
simplified, and not as powerful as what you can build with
`Vdom.Node.*`.

Most apps will develop some app-specific reusable components with
styles, an API, and behavior specialized to that app. Similarly,
multiple apps might use a shared library of UI components.

We recommend that styled component libraries implement a theme. Then,
any functionality components should automatically look consistent with
the rest of your app.

## Creating a Theme

The `View.Expert` module can be used to build new themes by overriding
an existing theme with new constants, or by replacing the theme object
entirely.

Most new themes should be derived from `View.Expert.default_theme`,
although you can base a new theme off of any existing one.

Most themes actually consist of multiple `Theme.t`s, for:

-   Light vs dark mode (and potential other color schemes)
-   High vs low contrast
-   Different versions of the theme

We recommend that themes intended to be used by critical web apps
release changes in new versions (e.g. `My_theme.V1`, `My_theme.V2`),
because even benign changes could potentially disrupt the layouts.
