# Thinking In Bonsai

# Thinking In Bonsai

> 💡 **About this document:** This is a loose grouping of questions and
> concepts to take you from a Bonsai beginner to someone who can
> reasonably understand and implement an end to end Bonsai application.
> You can either read this document all at once or read individual
> questions as you progress through your front-end journey.

## What is `Bonsai.t` and `let%arr`?

Bonsai defines an incremental computation graph that produces a view
(e.g. a `Vdom.Node.t` for `bonsai_web` and a `View.t` for
`bonsai_term`). In this graph, `Bonsai.t` values are the nodes. We
create new nodes that depend on existing nodes using `let%arr`.

For example, here we're taking some text from a user's input and
returning a string based on its value:

``` ocaml
let%arr text in
if String.is_empty text
then "Type something above!"
else
  [%string
    "Uppercased: %{String.uppercase text} (length: %{String.length \
     text#Int})"]
```

The `let%arr` is creating a new Bonsai node that depends on `text`. When
the value of `text` changes, this new node recomputes its value.

## When is my code run?

Bonsai code runs at two distinct times:

1.  **Initialization**: The Bonsai graph is first created (runs once).

    *You're in this phase when:* calling functions that take `graph` as
    an argument, or writing code outside of `let%arr` blocks.

2.  **Stabilization**: The graph executes as inputs and state change
    (runs repeatedly).

    *You're in this phase when:* inside the body of a `let%arr`.

The key mechanism for writing code that runs during stabilization is
`let%arr`. Here's an example:

``` ocaml
let state, set_state = Bonsai.state' 0 graph in
let%arr state and set_state in
{%html|<div>%{state#Int}</div>|}
```

In this code:

-   `Bonsai.state' 0 graph` runs **once** during initialization (creates
    the state)
-   `{%html|<div>%{state#Int}</div>|}` runs **once initially, then on
    every update** during stabilization

Let's go over the different times using a demo:

``` ocaml
module Counter = struct
  let component ~counter ~set_counter =
    let ( (* Outside the [let%arr] block *) ) =
      print_s [%message "outside"]
    in
    let%arr counter and set_counter in
    let ( (* Inside the [let%arr] block *) ) =
      print_s [%message "inside"]
    in
    {%html|
      <div %{counter_style}>
        <button on_click=%{fun _ -> set_counter (fun counter -> counter - 1)}>-</button>
        <span>%{counter#Int}</span>
        <button on_click=%{fun _ -> set_counter (fun counter -> counter + 1)}>+</button>
      </div>
    |}
  ;;
end
```

> **Exercise:** Open your browser's developer tools pane. How many
> "outside" and "inside"'s do you see? Now click one of buttons. What
> gets printed?

``` ocaml
{|You should see "outside" printed exactly once, but "inside" printed every
    time a button is pressed. This is because
    `print_s [%message "outside"]`
    runs during initialization and
    `print_s [%message "inside"]`
    runs during graph stabilization.|}
```

## Why can't I embed a `Vdom.Node.t Bonsai.t` in `ppx_html`?

`Vdom.Node.t Bonsai.t` is **reactive** HTML in the Bonsai graph, while
`ppx_html` produces a `Vdom.Node.t`, which is a single snapshot of HTML.

The mismatch: - `Vdom.Node.t Bonsai.t` is a reactive value that produces
different HTML on each stabilization - `Vdom.Node.t` is a single
snapshot of HTML - an immutable value representing the DOM at one moment

`let%arr` extracts the current Vdom.Node.t snapshot from the
`Vdom.Node.t Bonsai.t` reactive value, making it available for use in
`ppx_html`. Whenever the snapshot changes, `let%arr` re-evaluates the
`ppx_html` expression with the new snapshot.

## Why does `Bonsai.t` not have a `bind` function?

A `bind` function would make `Bonsai.t` a monad instead of an arrow.
This would allow Bonsai nodes to conditionally create other Bonsai
nodes, effectively making the graph structure modifiable at runtime.
This would prevent features like:

-   Optimizing and condensing the Bonsai graph. We're able to run an
    optimization pass on initialization that significantly condenses the
    graph, which saves a lot of memory. If an app were to dynamically
    generate Bonsai values, we would have to do the condensation at
    runtime every time the graph changes, which would be much slower.
    Also, dynamism is fairly slow in Incremental, the underlying library
    that Bonsai uses.
-   Attaching instrumentation for debugging to nodes in the computation
    graph, or even adding specialized debugging nodes. In practice, the
    [`match%sub`](https://github.com/janestreet/bonsai/tree/master/ppx_bonsai#matchsub)
    and `Bonsai.assoc` primitives allow you to do similar things to what
    you'd do with bind, such as conditional rendering or rendering an
    arbitrary amount of nodes.

## Why can't I use `let%arr` inside a `let%arr`?

When using two `Bonsai.t` values, it's tempting to write:

    let%arr name in
    let%arr age in
    {%html| <div>#{name} is %{age#Int} years old </div>|}

This will give you an error saying that nested let%arr blocks are not
allowed. Instead, you should write:

    let%arr name and age in
    {%html| <div>#{name} is %{age#Int} years old </div>|}

Each let%arr block evaluates to a `Bonsai.t` value. So in the first
example `let%arr age in...` returns a `Vdom.Node.t Bonsai.t`, but then
the `let%arr name in...` block wraps that in another `Bonsai.t` to
produce a `Vdom.Node.t Bonsai.t Bonsai.t`. This is not allowed as a type
for the same reason that we don't allow a bind function; it permits
Bonsai nodes to be constructed at runtime. By using `let%arr...and`, we
get a `Vdom.Node.t Bonsai.t` with no nesting.

Why does a `'a Bonsai.t Bonsai.t` permit Bonsai nodes to be constructed
at runtime? We can think of a `Bonsai.t` as a value that changes over
time. Therefore, a `'a Bonsai.t Bonsai.t` is a value that changes a
Bonsai node over time, which in turn would permit it to initialize or
destroy nodes.

## What is `Bonsai.graph`?

Bonsai programs construct a *static* graph that cannot be mutated after
it's constructed. The `Bonsai.graph` type enforces this at compile time
through `local` types, which prevent `graph` from being stored in
closures or memory.

Without the `graph` parameter, restricting dynamic graph construction
would require runtime checks instead of compile-time guarantees. (Note:
`let%arr` accesses `graph` under the hood, despite not taking it as an
explicit input, but this is solely for ergonomic reasons)

## How do I use components that take `Bonsai.graph` in `ppx_html`?

Let's say we have a component `Counter` that takes in `graph` as a
parameter. You may be tempted to write:

``` ocaml
{%html| <%{Counter.component graph} /> |}
```

However, this doesn't work because any component that takes in `graph`
will return a `Bonsai.t` which, [as mentioned
above](#why-cant-i-embed-a-vdom.node.t-bonsai.t-in-ppx-html), you can't
embed in `ppx_html`. Instead, call the component first, use `let%arr` on
the result, and interpolate that result in `ppx_html`:

``` ocaml
let component (local_ graph) =
  let%arr shuffle_component = Shuffle.component graph in
  {%html|<div>%{shuffle_component}</div>|}
;;
```

## What's the relationship between `bonsai`, `bonsai_web`, and `bonsai_term`?

Bonsai is "just" a library for building incremental computations.
There's nothing specific to web development in Bonsai. This core
functionality lives in `bonsai`, while the web-specific APIs on top live
in `bonsai_web`. In addition to building web apps in Bonsai, we also
build terminal user interfaces (TUIs) in Bonsai using
[`bonsai_term`](https://github.com/janestreet/bonsai_term). Like
`bonsai_web`, this contains the terminal-specific functionality on top
of `bonsai`. We recommend that you give both Bonsai Term and Bonsai Web
a try! \## What are lifecycle events?

Even though a Bonsai program's graph structure is fixed at
initialization, individual nodes can be *active* (involved in the
current computation) or *inactive* (skipped). `match%sub` and
`Bonsai.assoc` are primitives that control activation of different
branches. Neither primitive changes the graph's structure, but instead
can be thought of as "toggling" which part of the graph is active.

Bonsai has lifecycle events that fire when a node transitions between
these states:

-   `on_activate` - when a node becomes active
-   `on_deactivate` - when a node becomes inactive

These events can be subscribed to using `Bonsai.Edge.lifecycle`.

**Note:** Most nodes are active for the entire lifetime of the app. For
those nodes, `on_activate` runs once at startup and `on_deactivate`
never runs. The exceptions are nodes underneath `match%sub` or
`Bonsai.assoc`, whose activation tracks the matched branch or the keys
present in the input map.

There's also the `before_display` and `after_display` events which run
on **every** frame while a `Bonsai.t` is active. They are almost never
what you want to use.

#### Demo

In this example, we use `match%sub` to conditionally show a node if
`show` is `true`. We log `on_activate` and `on_deactivate` for the node.

``` ocaml
let component ~log (graph @ local) =
  let show, toggle_show = Bonsai.toggle ~default_model:false graph in
  let match_sub_view =
    match%sub show with
    | true ->
      Bonsai.Edge.lifecycle
        ~on_activate:
          (let%arr log in
           log "Activated")
        ~on_deactivate:
          (let%arr log in
           log "Deactivated")
        graph;
      let color, set_color = Bonsai.state (random_color ()) graph in
      let%arr color and set_color in
      {%html|
        <div
          style="
            width: 100%;
            height: 100px;
            background-color: %{color#Css_gen.Color};
            display: flex;
            justify-content: center;
            align-items: center;
          "
        >
          <button on_click=%{fun _ -> set_color (random_color ())}>
            #{" Shuffle color "}
          </button>
        </div>
      |}
    | false -> Bonsai.return Vdom.Node.none
  in
  let%arr toggle_show and match_sub_view in
  {%html|
    <div style="width: 384px; display: flex; flex-direction: column">
      <div style="margin-bottom: 16px; height: 100px">%{match_sub_view}</div>
      <button on_click=%{fun _ -> toggle_show}>Toggle show</button>
    </div>
  |}
;;
```

## How does state interact with a node's lifecycle?

Bonsai keeps a node's state even while the node is inactive. Increment
the counter, then toggle it. The counter's state persists even after the
counter is deactivated and later reactivated.

If you wish to reset a node's state when it becomes inactive, check out
either [`Bonsai.with_model_resetter`](../how_to/resetting_state.md) or
[`Bonsai.scope_model`](../how_to/resetting_state.md).

``` ocaml
module Counter = struct
  let component (graph @ local) =
    let counter, set_counter = Bonsai.state' 0 graph in
    let%arr counter and set_counter in
    {%html|
      <div style="display: flex; gap: 8px; justify-content: space-between">
        <button on_click=%{fun _ -> set_counter (fun c -> c - 1)}>-</button>
        <div style="width: 4ch; text-align: center">%{counter#Int}</div>
        <button on_click=%{fun _ -> set_counter (fun c -> c + 1)}>+</button>
      </div>
    |}
  ;;
end

let component (graph @ local) =
  let show, toggle_show = Bonsai.toggle ~default_model:true graph in
  let counter =
    match%sub show with
    | true -> Counter.component graph
    | false -> Bonsai.return Vdom.Node.none
  in
  let%arr toggle_show and counter in
  {%html|
    <div>
      <div style="padding: 8px 0; height: 36px">%{counter}</div>
      <button on_click=%{fun _ -> toggle_show}>Toggle counter</button>
    </div>
  |}
;;
```

## Why are state setters `Bonsai.t` values?

A Bonsai app's state and queue of pending state updates don't get
instantiated until after graph initialization. Therefore we can't call
setters before the graph is done being initialized. By wrapping the
setter in a `Bonsai.t`, we can ensure it is only used during the
stabilization phase.

## How do I implement routing in Bonsai?

Routing in Bonsai is implemented on the client-side, that is, your
**frontend** code is responsible for rendering different page content
depending on your URL. This is in contrast to server-side routing, where
the **server** sends different page content based on your URL.

To implement routing, create a single `Url_var.t` at your Bonsai
application's entrypoint that is then passed throughout your
application. The `Url_var` can access your page's URL and modify it just
like any piece of state in Bonsai (i.e. you can set and get the URL).
When creating the `Url_var.t`, you'll pass in a `parse_exn` (string →
your type) function and an `unparse` (your type → string) function.
These are used to convert the URL string into a data structure that your
OCaml code can use to conditionally render Bonsai nodes. Typically this
is an OCaml variant for the different routes, tuples for url segments,
and records for query parameters.

For instance, if we had a simple app with a home route, a route for blog
posts with the id, and a route for search results, we'd have a variant
like this:

``` ocaml
module Route = struct
  type t =
    | Homepage [@index]
    | Post of int
    | Search of { query : string }
end
```

Usually we implement the parsing/unparsing functions with the
[`Uri_parsing`](../how_to/uri_parsing.md) library. The Uri_parsing
library can either be called manually, or a URI parser can be derived
from a type definition using
[`ppx_uri_parsing`](../how_to/uri_parsing.md). We recommend deriving
when possible, since it reduces the amount of code needed.

Here's an example of `Uri_parsing` in action, but with the `Url_var`
part mocked out to avoid changing this page's URL.

``` ocaml
open Uri_parsing

module Route = struct
  type t =
    | Homepage [@index]
    | Post of int
    | Search of { query : string }
  [@@deriving typed_variants, sexp, equal, uri_parsing]
end

module Navigation_buttons = struct
  let component ~set_uri () =
    let%arr set_uri in
    {%html|
      <div style="display: flex; gap: 8px; margin-bottom: 16px; flex-wrap: wrap">
        <button on_click=%{fun _ -> set_uri Route.Homepage }>#{"Go Home"}</button
        ><button on_click=%{fun _ -> set_uri (Post 42)}>#{"View Post 42"}</button
        ><button on_click=%{fun _ -> set_uri (Post 123)}>#{"View Post 123"}</button
        ><button on_click=%{fun _ -> set_uri (Search { query = "bonsai"})}>
          #{" Search \"bonsai\" "}</button
        ><button on_click=%{fun _ -> set_uri (Search { query = "routing" })}>
          #{" Search \"routing\" "}
        </button>
      </div>
    |}
  ;;
end

let component (local_ graph) =
  let current_route, set_uri = Url_helper.create (module Route) graph in
  let page_content =
    match%sub current_route with
    | Route.Homepage -> Bonsai.return Home_page.view
    | Post post_id ->
      let%arr post_id in
      Post_page.view ~post_id
    | Search { query } ->
      let%arr query in
      Search_page.view ~query
  in
  let string_of_route = unstage (Parser.to_string Route.parser) in
  let%arr current_url_display =
    Url_display.component ~current_route ~string_of_route ()
  and nav_buttons = Navigation_buttons.component ~set_uri ()
  and page_content in
  {%html|
    <div style="max-width: 600px">
      <h3 style="margin-top: 0">URL-Based Routing Demo</h3>
      %{current_url_display} %{nav_buttons} %{page_content}
    </div>
  |}
;;
```

Since this app is a demo, we're using a custom `Url_helper` to avoid
actually changing the page's URL state. In practice, you'd typically
construct a `Url_var.t` using the
[`Url_var.Typed.make`](../how_to/url_var.md) function.

## When should I use `Bonsai.state` or `Bonsai.state'`?

Only use `Bonsai.state` when state updates have no dependency on the
previous state. For example, don't use `Bonsai.state` when setting a
field on a record. The updated record has a dependency on the previous
state, since the other fields on the record come from the previous
state.

Here's a demonstration of how updating a field on a record with
`Bonsai.state` can create bugs. Two functions, `fetch_username` and
`fetch_email`, fetch their respective data and set it in their state.

> **Exercise:** Click `start`. What happens? Why does it happen? Explain
> what about `Bonsai.state` creates this problem.

``` ocaml
module User_profile = struct
  type t =
    { username : string option
    ; email : string option
    }

  (* PROBLEMATIC: Two setters can race and overwrite each other *)
  let component (graph @ local) ~fetch_username ~fetch_email =
    let profile, set_profile =
      Bonsai.state { username = None; email = None } graph
    in
    let%arr profile
    and set_profile
    and reset_logs
    and fetch_username
    and fetch_email in
    let update_username_from_api =
      (* PROBLEM: Overwrites concurrent email updates with stale value *)
      let%bind.Effect new_username = fetch_username in
      set_profile { profile with username = Some new_username }
    in
    let update_email_from_api =
      let%bind.Effect new_email = fetch_email in
      (* PROBLEM: Overwrites concurrent username updates with stale value *)
      set_profile { profile with email = Some new_email }
    in
    let reset =
      let%bind.Effect () = reset_logs in
      set_profile { username = None; email = None }
    in
    let username = Option.value profile.username ~default:"<none>" in
    let email = Option.value profile.email ~default:"<none>" in
    {%html|
      <div style="display: flex; flex-direction: column">
        <button
          style="width: 200px; margin: 10px"
          on_click=%{fun _ -> Effect.all_parallel_unit [update_username_from_api; update_email_from_api]}
        >
          #{" Start "}</button
        ><button style="width: 200px; margin: 10px" on_click=%{fun _ -> reset}>
          #{" Reset "}
        </button>
        <div>#{" Username: "}#{username}<br />#{" Email: "}#{email}</div>
      </div>
    |}
  ;;
end
```

``` ocaml
{|The email field is set, then once the username field is set, the email field
  is reset. This happens because the setter updates the username from
  the stale `profile` value that existed before the email field was set|}
```

We can solve this problem by using `Bonsai.state'`. Instead of taking a
new state value, the setter takes an update function
`(old_state -> new_state)`. Bonsai will always pass the setter the most
recent state with all the previous changes applied.

``` ocaml
module User_profile = struct
  type t =
    { username : string option
    ; email : string option
    }

  let component (graph @ local) ~fetch_username ~fetch_email =
    let profile, set_profile =
      Bonsai.state' { username = None; email = None } graph
    in
    let%arr profile
    and set_profile
    and reset_logs
    and fetch_username
    and fetch_email in
    let update_username_from_api =
      let%bind.Effect new_username = fetch_username in
      set_profile (fun profile ->
        { profile with username = Some new_username })
    in
    let update_email_from_api =
      let%bind.Effect new_email = fetch_email in
      set_profile (fun profile -> { profile with email = Some new_email })
    in
    let reset =
      let%bind.Effect () = reset_logs in
      set_profile (fun _ -> { username = None; email = None })
    in
    let username = Option.value profile.username ~default:"<none>" in
    let email = Option.value profile.email ~default:"<none>" in
    {%html|
      <div style="display: flex; flex-direction: column">
        <button
          style="width: 200px; margin: 10px"
          on_click=%{fun _ -> Effect.all_parallel_unit [update_username_from_api; update_email_from_api]}
        >
          #{" Start "}</button
        ><button style="width: 200px; margin: 10px" on_click=%{fun _ -> reset}>
          #{" Reset "}
        </button>
        <div>#{" Username: "}#{username}<br />#{" Email: "}#{email}</div>
      </div>
    |}
  ;;
end
```

## What are the differences between views and components?

The standard convention when writing UI components is to divide them
into two categories:

-   **Views** take in non-`Bonsai.t` parameters and return
    non-`Bonsai.t` values, e.g. a button that takes in a `~text:string`
    and returns a `Vdom.Node.t`
-   **Components** take in `Bonsai.t` parameters and return a
    `Bonsai.t`, e.g. a counter that takes in a `~state:int Bonsai.t` and
    returns an `Vdom.Node.t Bonsai.t`

Confusingly, the combined group of both views and components is called
"UI components". We recommend always specifying "**UI** components" when
referring to the superset of views and components.

**Rules**: - Using a component requires `let%arr`-ing it. Views don't
need to be `let%arr`'d. - If it has `let%arr`, returns a `Bonsai.t` or
takes in `graph`, it's a component - If it returns a `Vdom.Node.t` (not
wrapped in `Bonsai.t`), it's a view - Views can only call other views.
Components can call both views and components

Modules should export a `view` or a `component` function for views and
components respectively. Modules can export both in cases where a UI
component can be declared as both a view or as a component.

> **Exercise:** Look at the example below. Which are components and
> which are views?

``` ocaml
module Increment_button = struct
  let view ~set_counter () =
    {%html|
      <button
        on_click=%{fun _ -> set_counter (fun c -> c + 1)}
        %{increment_button_style}
      >
        #{" + "}
      </button>
    |}
  ;;
end

module Counter = struct
  let component (graph @ local) =
    let counter, set_counter = Bonsai.state' 0 graph in
    let%arr counter and set_counter in
    {%html|
      <div %{counter_container_style}>
        <Increment_button.view ~set_counter />
        <div %{count_style}>
          #{" Count: "}<span %{count_number_style}>%{counter#Int}</span>
        </div>
      </div>
    |}
  ;;
end

module Page = struct
  let component (graph @ local) =
    let%arr counter = Counter.component graph in
    {%html|
      <div>
        <h1>Press the button</h1>
        %{counter}
      </div>
    |}
  ;;
end
```

``` ocaml
{|`Page` and `Counter` are both components. `Increment_button` is a view|}
```

