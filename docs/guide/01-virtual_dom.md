# Virtual DOM

Browser interfaces are described by a tree of HTML *elements*, each of
which can have some *attributes* attached. The `virtual_dom` library
provides an OCaml interface for constructing these trees.

In this chapter, we'll learn how to write HTML in OCaml using
`virtual_dom` and `ppx_html`, and style it with `ppx_css`.

## Vdom.Node.t

This wouldn't be a programming tutorial without a hello world example,
which introduces the `Vdom.Node.text` node constructor.

``` ocaml
let hello_world : Vdom.Node.t = Vdom.Node.text "hello world!"
```

> **Aside:** For the example above and throughout the rest of this
> guide, try using [inspect
> element](https://developer.chrome.com/docs/devtools/open#inspect) on
> the demo to see the generated HTML.

The text node will frequently be the "leaf" of a view (there are no
"children" of a text node). Let's put some text inside a bulleted list
by using some more node constructors:

``` ocaml
let bulleted_list : Vdom.Node.t =
  let open Vdom.Node in
  div
    [ h3 [ text "Norwegian Pancakes" ]
    ; ul
        [ li [ text "3 eggs" ]
        ; li [ text "2 cups of milk" ]
        ; li [ text "1 cup of flour" ]
        ]
    ]
;;
```

For the bulleted list, we use the `ul` and `li` functions. These
correspond to the [ul
element](https://developer.mozilla.org/en-US/docs/Web/HTML/Element/ul)
and the [li
element](https://developer.mozilla.org/en-US/docs/Web/HTML/Element/li),
which MDN helpfully tells us stands for **U**nordered **L**ist and
**L**ist **I**tem.

`h3` is short for "header level 3", and is responsible for the larger
font in the title text, and `div` is a ["content
division"](https://developer.mozilla.org/en-US/docs/Web/HTML/Element/div)
and serves as a useful wrapper for the rest of the content.

> **Aside:** There is a `Vdom.Node.*` node constructor function for
> *almost* every [DOM
> node](https://developer.mozilla.org/en-US/docs/Web/HTML/Element). If a
> constructor is missing, `Vdom.Node.create` is available to manually
> specify the tag, but the `Virtual_dom` maintainers gladly accept
> contributions back to the main library!

`ppx_html` lets you build vdom nodes using HTML syntax:

``` ocaml
let bulleted_list =
  {%html|
    <div>
      <h3>Norwegian Pancakes</h3>
      <ul>
        <li>3 eggs</li>
        <li>2 cups of milk</li>
        <li>1 cup of flour</li>
      </ul>
    </div>
  |}
;;
```

You can read more about `ppx_html` in the [ppx_html
repository](https://github.com/janestreet/ppx_html).

## Vdom.Attr.t

An optional argument to the `Vdom.Node.*` constructor functions is a
`Vdom.Attr.t list`. These `Attr.t` correspond to [DOM
attributes](https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes),
[DOM
properties](https://developer.mozilla.org/en-US/docs/Web/API/Element#properties),
and [DOM
event_handlers](https://developer.mozilla.org/en-US/docs/Web/Guide/Events/Event_handlers).

Attributes can be used to tweak the appearance and behavior of the nodes
that they are attached to, for instance, by adding placeholder text to a
textbox:

``` ocaml
let input_placeholder : Vdom.Node.t =
  Vdom.Node.input
    ~attrs:[ Vdom.Attr.placeholder "placeholder text here" ]
    ()
;;
```

Or color text with inline css:

``` ocaml
let css : Vdom.Node.t =
  Vdom.Node.span
    ~attrs:[ {%css|color: red;|} ]
    [ Vdom.Node.text "this text is red" ]
;;
```

> **Aside:** We have docs on [using ppx_css in
> Bonsai](https://github.com/janestreet/ppx_css)!

### Event Handlers

An important group of `Vdom.Attr.t`s register "event handlers" for user
interaction (like clicking on buttons or typing into a text box).

They usually receive a browser-level event value (which is almost always
ignored), alongside any useful data extracted from that event. For
example:

``` ocaml
val Vdom.Attr.on_click : (Dom_html.mouseEvent Js.t -> unit Effect.t) -> Vdom.Attr.t
val Vdom.Attr.on_input : (Dom_html.event Js.t -> string -> unit Effect.t) -> Vdom.Attr.t
```

Here's how we can use `on_click`:

``` ocaml
let clicky : Vdom.Node.t =
  Vdom.Node.button
    ~attrs:
      [ Vdom.Attr.on_click (fun _ ->
          (* Alerts are generally bad UI; there's an `Effect.print_s` for
             logging *)
          Effect.alert "hello there!")
      ]
    [ Vdom.Node.text "click me!" ]
;;
```

We'll learn about `Effect.t` --- our abstraction for side effects --- in
[chapter 2](./02-effects.md).

> **Aside:** Sometimes, you want to listen for events across your entire
> app. The functions in `Virtual_dom.Global_listeners` provide `Attr.t`s
> that attach listeners to the window while the attr is attached to some
> vdom node that is currently on the page.

## The Underlying Machinery

A virtual-DOM is an immutable tree of immutable data structures that
represents the view of the application at a point in time. This is in
contrast to [the DOM (Document Object
Model)](https://developer.mozilla.org/en-US/docs/Web/API/Document_Object_Model),
which is a mutable tree of mutable UI elements.

> **Aside:** The term "virtual DOM" is not unique to Bonsai. Many other
> UI libraries like
> [React](https://reactjs.org/docs/faq-internals.html),
> [Elm](https://github.com/elm/virtual-DOM), and
> [Vue](https://vuejs.org/v2/guide/render-function.html#The-Virtual-DOM)
> use the "virtual DOM" approach for similar reasons.

When we first compute our `Vdom.Node.t`, `virtual_dom` creates a
matching DOM tree in the browser. On further recomputations,
`virtual_dom` diffs the new virtual-DOM tree against its previous
version, and updates the DOM elements that have changed. Bonsai
schedules this diffing for you, so all you need to worry about is
producing your desired `Vdom.Node.t`.

> **Aside:** Virtual-DOM diffing does *not* check against the actual
> DOM; it assumes that the DOM will not be changed underneath it.

Creating virtual-DOM is much, much cheaper than real DOM, so only
modifying the DOM we need to is a big performance win. But since
virtual-DOM is immutable, doesn't that mean we need to create an entire
new tree every time we recalculate view? That seems scary, but because
Bonsai computes view *incrementally*, and shares work between
subcomputations, we can build pretty big and complicated web apps with
great performance.

### Diffing Lists

Virtual-dom's algorithm for diffing lists is somewhat naive: it just
zips over the old and new lists, and performs a pairwise diff for each
index. Most of the time, this works fine, and allows for fast diffing.
But consider the following lists of nodes to be diffed:

  Old   New
  ----- -----
  A     B
  B     C
  C     \-

Instead of producing the obviously minimal diff `["delete A"]`, it
instead computes `["diff A and B"; "diff B and C"; "delete C"]`. This
can lead to a variety of problems:

-   On large lists, this can lead to gigantic diffs and patches,
    significantly slowing performance: DOM operations are expensive!
-   If something is added / removed before a DOM node that has browser
    focus, that focus will be lost.
-   Event listeners might not be transferred properly to sibling nodes,
    which will just break your UI

This is likely to occur if using a dynamically changing list of child
nodes, which isn't just growing or shrinking at the tail. There are a
few ways to mitigate it:

The `Vdom.Node.Map_children` module takes a `Vdom.Node.t Map.t` instead
of a `Vdom.Node.t list`, and will perform efficient diffing and stable
association of input elements to DOM nodes.

You can also annotate nodes in a list with a `key` parameter to
`Vdom.Node.t` functions, which serves as an optimization hint for
`virtual_dom` to reorder nodes instead of patching them. However, if a
node has 2 children with the same `key`, your app will crash, so
`vdom_node_with_map_children` is preferred.

