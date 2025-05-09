# Bonsai Docs: Guide (Introduction)

This guide will teach you how to build web UIs in OCaml. We'll learn how
to:

-   Write [HTML with `virtual_dom`](./01-virtual_dom.md), with
    interactivity powered by side-effects [encapsulated as
    `Effect.t`s](./02-effects.md)
-   Structure our web UI as a graph of composable, [incremental
    computations with `Bonsai.t`](./03-incrementality.md)
-   Instantiate and use [state](./04-state.md)
-   Conditionally evaluate Bonsai code, or create a dynamic number of
    `Bonsai.t`s, with [`match%sub` and `assoc`](./05-control_flow.md)

These are the basic tools of writing OCaml web UIs. To learn how to
[style with ppx_css](../how_to/css.md), [send RPCs to a
server](../how_to/rpcs.md), [test your Bonsai
code](../how_to/testing.md), and more, see the [Bonsai
how-tos](../how_to/readme.md).

```{=html}
<aside>
```
These docs do not attempt to teach
[HTML](https://developer.mozilla.org/en-US/docs/Learn/HTML/Introduction_to_HTML)
or [CSS](https://developer.mozilla.org/en-US/docs/Learn/CSS/First_steps)
proficiency; there are plenty of resources on the internet that do it
better than we could.
```{=html}
</aside>
```
This guide is not intended to replace
[bonsai.mli](https://github.com/janestreet/bonsai/blob/master/src/bonsai.mli),
which lists and documents everything that Bonsai provides.

## Web Apps at 10,000 Feet

### What is the Web?

#### The part you already know

The Web is a network of hypertext documents [built on top
of](https://developer.mozilla.org/en-US/docs/Learn/Common_questions/Web_mechanics/How_does_the_Internet_work)
the Internet. To fetch a document, you make an HTTP request to a
computer somewhere (a "server"). When you do this via curl on the
command line, you get an HTTP response full of HTML:

    $ curl https://www.google.com/
    <!doctype html><html itemscope="" itemtype="http://schema.org/WebPage" lang="en"><head><meta content="Search the world's information, including webpages, images, videos and more...

The browser then parses this HTML and turns it into a tree called the
Document Object Model, or DOM. This is the "runtime representation" of a
web UI.

#### The part you might not know

HTML `<tags />` form the "skeleton" of a web UI, but there are 2 other
languages involved:

CSS (Cascading Style Sheets) allow you to style DOM nodes, giving you
control over how things look.

JavaScript allows running code / logic in the browser; both when the
page is first loaded, and in response to "events" triggered by the user
(e.g. clicks, scrolls, keyboard nav). The browser provides a bunch of
APIs for syscall-like things (setting timeouts, sending network
requests, etc), but also manipulating the DOM by
adding/removing/modifying nodes.

![](https://i.imgur.com/w7dbFM0.jpeg)

### Single Page Applications

Historically, each page of a web UI was self-contained. Every time you
navigated to a new page, the web server sent you the HTML for that page.
This is called a "server-side rendering" (SSR).

But if you right click in a Bonsai page, and "View Page Source", there's
almost nothing there. That's because we send across a very basic
"skeleton" HTML, as well as a bunch of JavaScript that sets up (and then
powers) the UI. This allows the server to focus on serving RPCs, not
rendering UIs.

This is called a "Single Page Application" (SPA), and was made popular
by the [React](https://react.dev/) and [Angular](https://angular.io/)
frameworks.

### What about Servers?

Because all the UI stuff happens in the SPA, the server just needs to
serve the `index.html` skeleton and a `main.bc.js` file, which contains
our OCaml code compiled to JavaScript by
[`js_of_ocaml`](https://ocsigen.org/js_of_ocaml/latest/manual/overview).

If our client needs to get data from the server, it can do so with [HTTP
requests](https://www.smashingmagazine.com/2018/01/understanding-using-rest-api/)
or a [Websockets
connection](https://developer.mozilla.org/en-US/docs/Web/API/WebSockets_API).

### `Virtual_dom` and `Bonsai`

`Virtual_dom` is an OCaml library that allows you to declaratively
describe what the DOM of your web UI should look like. Every frame, it
will diff the previous and current `Vdom.Node.t` for your app, and
update the browser's actual DOM accordingly.

Bonsai is a functional OCaml library + framework for incrementally
computing virtual DOM, so that when input data changes, we only need to
recalculate the parts of the output that actually change. Bonsai also
provides APIs for creating and composing state.

The rest of this intro previews building a simple web app in OCaml with
`Virtual_dom` and Bonsai. We'll discuss each step in depth in the guide
chapters.

## OCaml Web Apps at 1,000 Feet

Functional web UIs are functions from *data* to a *view*.

The *data* can be client-side state, data embedded in the URL, data from
the server, etc.

The *view* is the part users see. In web UIs, the view is HTML (provided
by `virtual_dom`), styled by CSS.

For example, a web UI that tells a user how many unread emails they have
might look like:

```{=html}
<!-- $MDX file=../../examples/bonsai_guide_code/intro_examples.mli,part=message_vdom -->
```
``` ocaml
val message_vdom : name:string -> new_emails:int -> Vdom.Node.t
```

```{=html}
<!-- $MDX file=../../examples/bonsai_guide_code/intro_examples.ml,part=message_vdom -->
```
``` ocaml
open! Core
open Virtual_dom

let message_vdom ~name ~new_emails =
  Vdom.Node.div
    ~attrs:[ {%css|font-size: 16px;|} ]
    [ Vdom.Node.textf "hello %s! you have %d new emails" name new_emails ]
;;
```

```{=html}
<iframe data-external="1" src="https://bonsai:8535#message_vdom">
```
```{=html}
</iframe>
```
We'll talk more about `Virtual_dom` in [Chapter 1](./01-virtual_dom.md).

User interactions, state updates, and RPC server calls are just *side
effects* of an otherwise pure function. We wrap these side effects in an
`Effect.t` type.

For example, we could add a button that "reads" an email to our UI:

```{=html}
<!-- $MDX file=../../examples/bonsai_guide_code/intro_examples.mli,part=read_email_button -->
```
``` ocaml
val read_email_button : on_click:unit Effect.t -> Vdom.Node.t
```

```{=html}
<!-- $MDX file=../../examples/bonsai_guide_code/intro_examples.ml,part=read_email_button -->
```
``` ocaml
let read_email_button ~on_click =
  Vdom.Node.button
    ~attrs:[ Vdom.Attr.on_click (fun _ -> on_click) ]
    [ Vdom.Node.text "Read an email!" ]
;;
```

```{=html}
<iframe data-external="1" src="https://bonsai:8535#read_email_button">
```
```{=html}
</iframe>
```
We'll talk more about `Effect.t` in [Chapter 2](./02-effects.md).

A desirable property is incrementality: when something changes, we only
recompute stuff that depends on it. We can do so by wrapping our inputs
and outputs in the incremental `Bonsai.t` type:

```{=html}
<!-- $MDX file=../../examples/bonsai_guide_code/intro_examples.mli,part=emails_bonsai -->
```
``` ocaml
val emails_bonsai
  :  name:string Bonsai.t
  -> new_emails:int Bonsai.t
  -> read_email_effect:unit Effect.t Bonsai.t
  -> Vdom.Node.t Bonsai.t
```

We can compose `Bonsai.t`s with the `let%arr` operator, Bonsai's
equivalent of `let%map`:

```{=html}
<!-- $MDX file=../../examples/bonsai_guide_code/intro_examples.ml,part=emails_bonsai -->
```
``` ocaml
open! Bonsai_web
open Bonsai.Let_syntax

let emails_bonsai ~name ~new_emails ~read_email_effect =
  let message : Vdom.Node.t Bonsai.t =
    let%arr (name : string) = (name : string Bonsai.t)
    and (new_emails : int) = (new_emails : int Bonsai.t) in
    message_vdom ~name ~new_emails
  in
  let%arr message and read_email_effect in
  Vdom.Node.div [ message; read_email_button ~on_click:read_email_effect ]
;;
```

```{=html}
<iframe data-external="1" src="https://bonsai:8535#emails_bonsai">
```
```{=html}
</iframe>
```
In the code above, `message` will not be recomputed if only
`read_email_effect` changes.

We'll talk more about `Bonsai.t` and incrementality in [Chapter
3](./03-incrementality.md).

We can use `Bonsai.state`, which creates a simple getter/setter state,
to make our app stateful.

```{=html}
<aside>
```
To use `Bonsai.state` and other `Bonsai.*` primitives, we need a
`local_ Bonsai.graph` "graph-builder", which Bonsai will pass into your
top-level `app` function. We'll discuss this more in the [state
chapter](04-state.md).
```{=html}
</aside>
```
In our email example, we can use `Bonsai.state` to keep track of how
many unread emails the user has and modify that count whenever they
"read" one:

```{=html}
<!-- $MDX file=../../examples/bonsai_guide_code/intro_examples.mli,part=emails_stateful -->
```
``` ocaml
val emails_stateful : name:string Bonsai.t -> local_ Bonsai.graph -> Vdom.Node.t Bonsai.t
```

```{=html}
<!-- $MDX file=../../examples/bonsai_guide_code/intro_examples.ml,part=emails_stateful -->
```
``` ocaml
let emails_stateful ~name (local_ graph) =
  let default_count = 999 in
  let (count : int Bonsai.t), (set_count : (int -> unit Effect.t) Bonsai.t) =
    Bonsai.state default_count graph
  in
  let read_email_effect =
    let%arr count and set_count in
    set_count (count - 1)
  in
  emails_bonsai ~name ~new_emails:count ~read_email_effect
;;
```

```{=html}
<iframe data-external="1" src="https://bonsai:8535#emails_stateful">
```
```{=html}
</iframe>
```
Note that the state "setter" is an incrementally computed function that
produces a `unit Effect.t`. When this effect is scheduled via an event
handler, the state will update.

We'll talk more about Bonsai's state tools in [Chapter
4](./04-state.md).

And since our ultimate goal is to produce a single
incrementally-computed `Vdom.Node.t`, with state managed by Bonsai, a
complete app looks like:

```{=html}
<!-- $MDX file=../../examples/bonsai_guide_code/intro_examples.mli,part=app -->
```
``` ocaml
val app : local_ Bonsai.graph -> Vdom.Node.t Bonsai.t
```

```{=html}
<!-- $MDX file=../../examples/bonsai_guide_code/intro_examples.ml,part=app -->
```
``` ocaml
let app (local_ graph) = emails_stateful ~name:(Bonsai.return "User") graph
```

```{=html}
<iframe data-external="1" src="https://bonsai:8535#app">
```
```{=html}
</iframe>
```
We can run it with:

```{=html}
<!-- $MDX skip -->
```
``` ocaml
let () = Bonsai_web.Start.start app
```

## Bonsai is Generic

Bonsai isn't actually web-specific: it's a library for building,
composing, and running pure, incremental, state-machines. It works
particularly well for web UIs, but it could also power other UI
backends, or even stateful, incremental computation on servers.

That's why instead of `open! Bonsai`, you'll `open! Bonsai_web`:
`Bonsai_web` contains a bunch of web-specific utils and helpers, in
addition to the core functionality in `Bonsai`.
