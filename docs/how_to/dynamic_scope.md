# Dynamic Scope

`Bonsai.Dynamic_scope` allows you to pass some `Bonsai.t` to an entire
subgraph of your Bonsai app without explicitly threading it through.
This is quite similar to [React's
Context](https://react.dev/learn/passing-data-deeply-with-context). Some
usecases are:

-   Keyboard shortcuts
-   Getting / setting the current [Url.t](./url_var.md)
-   Accumumating errors / notifications somewhere central in your UI
-   [Theming](./theming.md)

## Reading `Dynamic_scope.t`s

For instance, we might want to configure a theme for a whole app:

```{=html}
<!-- $MDX file=../../examples/bonsai_guide_code/dynamic_scope_examples.ml,part=theme_dynamic_scope -->
```
``` ocaml
module Theme = struct
  type t =
    | Light
    | Dark

  let styles = function
    | Light ->
      {%css|
        background-color: black;
        color: white;
      |}
    | Dark ->
      {%css|
        background-color: white;
        color: black;
      |}
  ;;
end

let theme = Bonsai.Dynamic_scope.create ~name:"theme" ~fallback:Theme.Dark ()

let thing_1 (local_ graph) =
  let%arr theme = Bonsai.Dynamic_scope.lookup theme graph in
  Vdom.Node.div ~attrs:[ Theme.styles theme ] [ Vdom.Node.text "Thing 1" ]
;;

let thing_2 (local_ graph) =
  let%arr theme = Bonsai.Dynamic_scope.lookup theme graph in
  Vdom.Node.div
    [ Vdom.Node.button ~attrs:[ Theme.styles theme ] [ Vdom.Node.text "Thing 2" ] ]
;;

let app (local_ graph) =
  let%arr thing_1 = thing_1 graph
  and thing_2 = thing_2 graph in
  Vdom.Node.div [ thing_1; thing_2 ]
;;
```

```{=html}
<iframe data-external="1" src="https://bonsai:8535#theme_dynamic_scope">
```
```{=html}
</iframe>
```
## Setting `Dynamic_scope.t`s

We didn't set anything in the example above, so the `fallback` value was
used. But we can provide a value for a `Dynamic_scope.t` inside any
`local_ graph -> 'a Bonsai.t`:

```{=html}
<!-- $MDX file=../../examples/bonsai_guide_code/dynamic_scope_examples.ml,part=theme_set -->
```
``` ocaml
let app (local_ graph) =
  Bonsai.Dynamic_scope.set theme (Bonsai.return Theme.Light) ~inside:app graph
;;
```

```{=html}
<iframe data-external="1" src="https://bonsai:8535#theme_set">
```
```{=html}
</iframe>
```
## Downsides of Dynamic Scope

Dynamic Scope is powerful, but it can make your code more difficult to
reason about, because the values of your variables depend on where a
function is *called*, not where it is *defined*.
