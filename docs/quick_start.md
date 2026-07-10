# Quick Start

This quick start outlines many commonly used tools for building a Bonsai
app and the situations in which you may want to use them. It isn't
intended to be exhaustive, and is purposefully terse so that it can be
easily parsed.

## Writing HTML with `ppx_html`

`ppx_html` is a syntax extension that lets you write HTML-like markup
directly in OCaml code, similar to JSX in React. It converts your HTML
tags into calls to `Vdom.Node` functions at compile time.

In Bonsai, we create reusable functions that return markup. These
functions can be composed together to build user interfaces.

**Important constraint:** A `ppx_html` block must return a single root
node. If you need multiple sibling nodes, wrap them in a parent element
like `<div></div>` or use a fragment `<></>`.

``` ocaml
(* Multiple root nodes - use a fragment *)
{%html|
  <>
    <h1>Welcome</h1>
    <p>This is my page</p>
  </>
|}
```

Here's an example of creating and using a reusable component. We
recommend structuring components as modules with a `view` function:

``` ocaml
module Greeting = struct
  let view () = {%html|<div class="greeting">Hello, world!</div>|}
end

module Home_page = struct
  let view () = {%html|<Greeting.view />|}
end
```

### HTML Tags

Any HTML tag with a corresponding function in `Vdom.Node` can be used in
`ppx_html`.

``` ocaml
{%html|
  <div>
    <h1>Title</h1>
    <span>Here is a span</span>
    <form><label>Here is a label</label><input type="text" /></form>
  </div>
|}
```

### Using OCaml Values

You can use OCaml values in your markup using different syntaxes
depending on the value's type.

**Rendering text:** - `#{string_value}` - renders a string variable as
text - `%{value#Module}` - calls `Module.to_string value` and renders
the result as text (works like `ppx_string`)

**Rendering nodes:** - `%{node}` - inserts a `Vdom.Node.t` value -
`*{node_list}` - inserts a `Vdom.Node.t list` - `?{node_option}` -
inserts a `Vdom.Node.t option` (renders nothing if `None`)

``` ocaml
let name = "Alice"
let age = 25
let greeting_node = {%html|<strong>Welcome!</strong>|}
let optional_subtitle = Some {%html|<em>(new user)</em>|}

let view =
  {%html|
    <div>
      #{name} is %{age#Int} years old. %{greeting_node}
      ?{optional_subtitle}
    </div>
  |}
;;
```

Here's an example rendering a list of nodes:

``` ocaml
let items = [ "Apples"; "Bananas"; "Oranges" ]

let view =
  let item_nodes =
    List.map items ~f:(fun item -> {%html|<li>#{item}</li>|})
  in
  {%html|
    <ul>
      *{item_nodes}
    </ul>
  |}
;;
```

### Rendering Components

Components are reusable functions that return markup. There are two
syntaxes for rendering them in `ppx_html`:

**`<Module.path>` syntax** - Only works with literal module paths like
`Card.view` - Cannot pass positional (unlabeled) arguments

``` ocaml
module Card = struct
  (* Component function with standard signature *)
  let view ?(attrs = []) children = {%html|<div *{attrs}>*{children}</div>|}
end

let view = {%html|<Card.view>Hello, world!</>|}
```

**`<%{expression}>` syntax** (general form) - Can use any OCaml
expression: local variables, conditionals, function calls - Can pass
positional arguments

``` ocaml
(* Note the positional `header` argument *)
let card (header : string) ?(attrs = []) children =
  {%html|
    <div *{attrs}>
      <h1>#{header}</h1>
      *{children}
    </div>
  |}
;;

let view =
  {%html|
    <%{card "Hello, world!"} class="greeting"
      >#{" Card content here "}</>
  |}
;;
```

When you use a component in `ppx_html`, the syntax you use determines
what function signature `ppx_html` expects. This is how `ppx_html` knows
how to call your function.

**Self-closing syntax:** `<Foo.view />` requires the function to take
`unit` as its last positional parameter:

``` ocaml
module Self_closing_component = struct
  (* Must end with unit *)
  let view () = {%html|<div>No children</div>|}
end

let view = {%html|<Self_closing_component.view />|}
```

**Opening and closing tags:** `<Foo.view></>` requires the function to
take `Vdom.Node.t list` as its last positional parameter.

You can also use `<Foo.view></Foo.view>` as alternate syntax for
`<Foo.view></>`

``` ocaml
module Component_with_children = struct
  (* Must end with Vdom.Node.t list *)
  let view (children : Vdom.Node.t list) = {%html|<div>*{children}</div>|}
end

let view =
  {%html|
    <Component_with_children.view>
      <Component_with_children.view>Child 1</Component_with_children.view>
      <div>Child 2</div>
    </>
  |}
;;
```

### Attributes and Arguments

Components can take in both attributes and arguments.

Attributes are the standard [HTML
attributes](https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Attributes)
such as `class`, `id`, `placeholder`, etc. As long as a component has an
optional `attrs` argument, it accepts all attributes (which are defined
in `Vdom.Attr`).

Attributes can be passed in using `attribute=value` syntax where `value`
is either a string or an OCaml value wrapped in `%{}`.

Some attributes such as `on_click`, `on_change`, etc. take in functions
that are called when events happen. These can be used to trigger
[effects](#effects).

``` ocaml
module Greeting = struct
  let view ?(attrs = []) () = {%html|<div *{attrs}>Hello there!</div>|}
end

let id = "my-id"

let view =
  (* [id] is being passed to the function [Vdom.Attr.id] *)
  {%html|<Greeting.view id=%{id} style="background-color: yellow" />|}
;;
```

You can put `Vdom.Attr.t` values into your markup using different
syntaxes depending on whether you're working with a single attribute, an
optional attribute, or a list of attributes:

-   `%{attr}` - adds a single `Vdom.Attr.t` value
-   `?{attr_option}` - adds an `Attr.t option` (nothing is added if
    `None`)
-   `*{attr_list}` - adds an `Attr.t list`

These can be mixed and matched on the same element.

``` ocaml
let class_attr = Vdom.Attr.class_ "greeting"
let optional_id = Some (Vdom.Attr.id "my-div")
let extra_attrs = [ Vdom.Attr.style (Css_gen.color (`Name "red")) ]

let view =
  {%html|<div %{class_attr} ?{optional_id} *{extra_attrs}>Hello, world!</div>|}
;;
```

Arguments are defined by components as named or optional function
parameters. You can pass in arguments in `ppx_html` using the
`~arg:%{value}` syntax. Like in OCaml, if `arg` and `value` have the
same name, you can use the `~arg` shorthand

``` ocaml
module Name_input = struct
  let view ?(placeholder_name = "") ~label () =
    {%html|
      <div %{style}>
        <label>#{label}</label>
        <input type="text" placeholder=%{placeholder_name} />
      </div>
    |}
  ;;
end

let placeholder_name = "Alice"

let view =
  {%html|<Name_input.view ~label:%{"What's your name?"} ~placeholder_name />|}
;;
```

If a component accepts a named or optional argument of type
`Vdom.Node.t`, you can pass it in directly as inline `ppx_html` using
the `~arg:(<></>)` syntax.

``` ocaml
module Container = struct
  let view ?(bottom = Vdom.Node.None) ~top () =
    {%html|
      <div>
        <div>%{top}</div>
        <div>%{bottom}</div>
      </div>
    |}
  ;;
end

let view =
  {%html|<Container.view ~bottom:(<p>Bottom</p>) ~top:(<>Top</>) />|}
;;
```

### Comments

`ppx_html` uses HTML-style comments

``` ocaml
{%html|
  <!-- this is a comment! -->
  <div></div>
|}
```

## Styling with `ppx_css`

[`ppx_css`](https://github.com/janestreet/ppx_css) is a syntax extension
that validates CSS at compile time. There are three ways to use it:

**1. Inline `style` attribute** - For simple, one-off styles on a single
element

**2. `{%css| |}` blocks** - For reusable styles with pseudo-selectors
like `:hover`

**3. `[%css stylesheet {| |}]`** - For named classes with media queries,
relative selectors, and other @at-rules

### Inline `style` attribute

The simplest way to add styles is using the `style` attribute directly
in your markup. You can embed OCaml values into the styles:

``` ocaml
let height = `Px 20 in
{%html|
  <div
    style="
      background-color: #ff0099;
      width: 300px;
      height: %{height#Css_gen.Length};
    "
  >
    #{" On Wednesdays We Wear Pink "}
  </div>
|}
```

### `{%css| |}` blocks

Use `{%css| |}` to define CSS separately. This is useful when you want
to reuse styles or use pseudo-selectors like `:hover`. This format also
supports embedding OCaml values.

``` ocaml
let hover_color = `Hex "#9900ff" in
let style =
  {%css|
    background-color: #fefefe;
    width: 300px;
    height: 20px;

    &:hover {
      background-color: %{hover_color#Css_gen.Color};
      color: white;
    }
  |}
in
{%html|<div %{style}>Hover Over Me</div>|}
```

### `[%css stylesheet {| |}]`

Use `[%css stylesheet {| |}]` to define named CSS classes that are
exported as a module. This is useful for more complex styling needs like
relative selectors (styling child elements), media queries, and other
[@at-rules](https://developer.mozilla.org/en-US/docs/Web/CSS/CSS_syntax/At-rule)
like `@keyframes`. The class names are hashed to avoid collisions
between different stylesheets.

``` ocaml
module Styles =
  [%css
  stylesheet
    {|
      .greeting {
        background: gray;
        padding: 20px;
        color: white;

        &:hover .child {
          outline: 1px solid blue;
        }
      }

      @media (max-width: 800px) {
        .greeting {
          padding: 10px;
          background: green;
        }
      }
    |}]

let view =
  {%html|<div %{Styles.greeting}><span %{Styles.child}>Good morning!</span></div>|}
;;
```

## Mapping and Control Flow

### Transforming values with `let%arr`

Use this when you want to map a `Bonsai.t` to another `Bonsai.t`. This
is equivalent to `Bonsai.map` plus an optimization that adds an
[incremental
cutoff](https://github.com/janestreet/bonsai_web/blob/master/docs/how_to/cutoff.md)
when destructuring a type.

``` ocaml
module Student_badge = struct
  let component student =
    let%arr student in
    {%html|<div class="student-badge">My name is #{Student.name student}</div>|}
  ;;
end

let component =
  let student =
    Bonsai.return
      (Student.Masters
         { name = "Bob"; thesis = Some "incrementally typed languages" })
  in
  Student_badge.component student
;;
```

### Pattern matching: `match%arr` vs `match%sub`

When you need to conditionally render different content based on a
`Bonsai.t` value, you have two options:

**Use `match%arr`** (simpler, preferred) when each branch just returns a
different value: - You're transforming data, not creating new
computations - No branch needs state, effects, or access to
[`graph`](https://github.com/janestreet/bonsai_web/blob/master/docs/thinking_in_bonsai.md) -
It's syntactic sugar for `let%arr value in match value with ...`

**Use `match%sub`** (more powerful, can have significant overhead) when
each branch may need their own state: - You need to use
[`graph`](https://github.com/janestreet/bonsai_web/blob/master/docs/thinking_in_bonsai.md)
within the match arms - Each match arm creates a separate Bonsai node
with state local to the arm

**Rule of thumb:** Start with `match%arr`. Only use `match%sub` when you
need stateful branches or computations that depend on `graph`. **Example
using `match%arr`** (just transforming values):

``` ocaml
module Student_category = struct
  let component (student : Student.t Bonsai.t) =
    (* match%arr responds to state changes but doesn't create stateful
       branches *)
    match%arr student with
    | Student.Phd _ ->
      {%html|<div>You're a grad student doing a PhD</div>|}
    | Student.Masters _ ->
      {%html|<div>You're a grad student doing a Masters</div>|}
    | Bachelors _ -> {%html|<div>You're an undergrad</div>|}
  ;;
end
```

**Example using `match%sub`** (branches with different state
requirements):

``` ocaml
let component (student_name : string Bonsai.t) (graph @ local) =
  match%sub student_name with
  | "Bob" ->
    (* Bob's page needs toggle state *)
    let is_light_mode, toggle_light_mode =
      Bonsai.toggle ~default_model:true graph
    in
    let%arr student_name and is_light_mode and toggle_light_mode in
    let style =
      if is_light_mode then light_on_style else light_off_style
    in
    {%html|
      <div %{style}>
        #{student_name}#{" is an electrician "}<button
          on_click=%{fun _ -> toggle_light_mode}
        >
          #{"toggle light"}
        </button>
      </div>
    |}
  | "Alice" ->
    (* Alice's page needs counter state *)
    let count, set_count = Bonsai.state' 0 graph in
    let%arr count and set_count in
    {%html|
      <div>
        #{" Alice is an accountant "}<button
          on_click=%{fun _ -> set_count (fun count -> count - 1)}
        >
          #{"-"}</button
        >%{count#Int}<button
          on_click=%{fun _ -> set_count (fun count -> count + 1)}
        >
          #{"+"}
        </button>
      </div>
    |}
  | student ->
    (* This branch doesn't need state, just returns markup *)
    let%arr student in
    {%html|<div>#{student} does not attend this school</div>|}
;;
```

### Lazy conditional rendering with `match%sub [%lazy]`

This version of `match%sub` defers the construction of the Bonsai nodes
in a match arm until it is matched on.

In other words, this allows you to only construct the Bonsai nodes for
the parts of your app that the user actually visits.

**When to use it:** Use sparingly, as lazy construction introduces
overhead. We've seen it useful when branching on URLs, where each route
represents a large page and users typically access one route at a time.

`match%sub [%lazy]` requires that `graph` be in scope.

``` ocaml
module Student_page = struct
  let component degree (graph @ local) =
    match%sub [%lazy] degree with
    | Student.Phd { name; thesis } ->
      (* Imagine this is a computationally intensive page *)
      let%arr name and thesis in
      Grad_student_page.component ~name ~thesis:(Some thesis)
    | Masters { name; thesis } ->
      let%arr name and thesis in
      Grad_student_page.component ~name ~thesis
    | Bachelors { name; major } ->
      let%arr name and major in
      {%html|<div>#{name} is a Bachelors student majoring in #{major}!</div>|}
  ;;
end

let component =
  Student_page.component
    (Bonsai.return
       (Student.Masters
          { name = "Charlie"; thesis = Some "programming languages" }))
    graph
;;
```

### Create a dynamic number of Bonsai nodes with `Bonsai.assoc`

Use `Bonsai.assoc` when you want to create a Bonsai node for each entry
in a data structure, such as when you're rendering a list of items that
need to have their own state, or if you need to recompute a single node
without recomputing the entire list.

**Caution**: `Bonsai.assoc` can add significant overhead cost,
especially if called inside another `Bonsai.assoc`. Be mindful of your
usage.

``` ocaml
let component ~todos (graph @ local) =
  let assoc_view =
    Bonsai.assoc
      (module Int)
      todos
      ~f:(fun _key todo (graph @ local) ->
        let is_finished, set_is_finished = Bonsai.state false graph in
        let%arr todo and is_finished and set_is_finished in
        let icon =
          match is_finished with
          | true -> {%html|<span %{icon_style}>✓</span>|}
          | false ->
            {%html|
              <button %{button_style} on_click=%{fun _ ->
                set_is_finished true
              }>
                #{" Complete "}
              </button>
            |}
        in
        {%html|<div %{todo_style}>%{icon}<span> #{todo}</span></div>|})
      graph
  in
  let%arr assoc_view in
  Vdom.Node.Map_children.div assoc_view
;;
```

## State

State is how you add interaction to your Bonsai application. When users
click buttons, type in forms, or navigate between pages, state tracks
what has changed and allows your UI to respond dynamically.

Bonsai provides several state APIs depending on your needs. Each takes a
[`graph`
parameter](https://github.com/janestreet/bonsai_web/blob/master/docs/thinking_in_bonsai.md)
and returns both the current state value and a way to update it (either
a setter function or an action dispatcher).

### Simple state: `Bonsai.state`

Use `Bonsai.state` if all updates to state completely replace the old
state and do not depend on the old state

``` ocaml
module Student_major = struct
  let majors =
    [| "Cinema Studies"; "Computer Science"; "Underwater Basketweaving" |]
  ;;

  let component (graph @ local) =
    let major, set_major = Bonsai.state (Array.get majors 0) graph in
    let%arr major and set_major in
    {%html|
      <div>
        #{" Your major is "}#{major}<button
          style="margin-left: 8px; margin-right: 8px"
          on_click=%{fun _ ->
            let index = Random.int (Array.length majors) in
            set_major (Array.get majors index)}
        >
          #{" Change Major "}
        </button>
      </div>
    |}
  ;;
end
```

### State based on previous value: `Bonsai.state'`

Use `Bonsai.state'` if all updates to state completely replace the old
state, and the new state depends on the old state (e.g. counters).

``` ocaml
let component (graph @ local) =
  let classes, set_classes = Bonsai.state' 0 graph in
  let%arr classes and set_classes in
  {%html|
    <div>
      #{" You are enrolled in "}#{Int.to_string classes}#{" classes. "}<button
        on_click=%{fun _ -> set_classes (fun classes -> classes + 1)}
      >
        #{"Enroll"}
      </button>
    </div>
  |}
;;
```

### Boolean state: `Bonsai.toggle` and `Bonsai.toggle'`

Use `Bonsai.toggle` when you need a boolean state that toggles between
true and false. It returns the current boolean state and an
[effect](#effects) to toggle it.

Use `Bonsai.toggle'` when you also need to set the boolean state
directly to a specific value (in addition to toggling). It returns a
record with `state`, `toggle`, and `set_state`.

``` ocaml
module Theme_toggle = struct
  let component (graph @ local) =
    let is_light_mode, toggle_theme =
      Bonsai.toggle ~default_model:true graph
    in
    let%arr is_light_mode and toggle_theme in
    let style, mode_text =
      if is_light_mode
      then light_mode_styles, "Light Mode"
      else dark_mode_styles, "Dark Mode"
    in
    {%html|
      <div %{style}>
        #{" Current theme: "}#{mode_text}<button
          style="margin-left: 16px"
          on_click=%{fun _ -> toggle_theme}
        >
          #{" Toggle Theme "}
        </button>
      </div>
    |}
  ;;
end
```

When you need direct control over the boolean value, use
`Bonsai.toggle'`:

``` ocaml
module Settings_panel = struct
  let component (graph @ local) =
    let { Bonsai.Toggle.state = notifications_enabled
        ; set_state = set_notifications
        ; toggle = toggle_notifications
        }
      =
      Bonsai.toggle' ~default_model:true graph
    in
    let%arr notifications_enabled
    and set_notifications
    and toggle_notifications in
    let status_text =
      if notifications_enabled then "Enabled" else "Disabled"
    in
    {%html|
      <div style="padding: 16px; border: 1px solid #d1d5db">
        <div>Notifications: #{status_text}</div>
        <div style="display: flex; gap: 8px; margin-top: 8px">
          <button on_click=%{fun _ -> toggle_notifications}>Toggle</button>
          <button on_click=%{fun _ -> set_notifications true}>Enable</button>
          <button on_click=%{fun _ -> set_notifications false}>Disable</button>
        </div>
      </div>
    |}
  ;;
end
```

### Complex state with actions: `Bonsai.state_machine`

Use `Bonsai.state_machine` when you need multiple distinct ways to
update state. Instead of setting state directly, you dispatch actions
(typically variants like `Add_grade of float` or `Reset`), and an
`apply_action` function determines how each action transforms the state.

This is useful when your state has multiple fields or complex update
logic, or when you want to ensure state can only change in specific,
controlled ways.

``` ocaml
module Student_progress = struct
  type action =
    | Add_grade of float
    | Reset

  type model =
    { total_points : float
    ; num_grades : int
    }
  [@@deriving equal]

  let component (graph @ local) =
    let state, inject =
      Bonsai.state_machine
        ~default_model:{ total_points = 0.0; num_grades = 0 }
        ~apply_action:(fun _ model action ->
          match action with
          | Add_grade grade ->
            { total_points = model.total_points +. grade
            ; num_grades = model.num_grades + 1
            }
          | Reset -> { total_points = 0.0; num_grades = 0 })
        graph
    in
    let%arr { total_points; num_grades } = state
    and inject in
    let average =
      if num_grades = 0
      then "N/A"
      else Float.to_string (total_points /. Float.of_int num_grades)
    in
    {%html|
      <div style="display: flex; flex-direction: column">
        <span style="padding: 16px">#{"Average grade: "}#{average}</span
        ><button style="padding: 16px" on_click=%{fun _ -> inject (Add_grade 90.0)}>
          #{" Add A "}</button
        ><button style="padding: 16px" on_click=%{fun _ -> inject (Add_grade 80.0)}>
          #{" Add B "}</button
        ><button style="padding: 16px" on_click=%{fun _ -> inject Reset}>
          #{" Reset "}
        </button>
      </div>
    |}
  ;;
end
```

### State machine that returns values: `Bonsai.actor`

Use `Bonsai.actor` when you need a state machine where actions can
return values. Like `Bonsai.state_machine`, you dispatch actions and an
`apply_action` function updates the state. However, `apply_action` also
returns a value that gets sent back through the effect when it
completes.

This is useful when you need confirmation or results from state updates,
such as validation results, generated IDs, or computed values based on
the new state.

``` ocaml
module Todo_list = struct
  type action =
    | Add_todo of string
    | Remove_todo of int
  [@@deriving sexp_of]

  type model =
    { todos : (int * string) list
    ; next_id : int
    }
  [@@deriving equal]

  let component (graph @ local) =
    let state, inject =
      Bonsai.actor
        ~default_model:{ todos = []; next_id = 0 }
        ~recv:(fun _ model action ->
          match action with
          | Add_todo text ->
            let new_id = model.next_id in
            ( { todos = (new_id, text) :: model.todos
              ; next_id = new_id + 1
              }
            , new_id )
          | Remove_todo id ->
            ( { model with
                todos = List.filter model.todos ~f:(fun (i, _) -> i <> id)
              }
            , id ))
        graph
    in
    let%arr { todos; _ } = state
    and inject in
    let add_todo_effect =
      let%bind.Effect new_id = inject (Add_todo "New todo") in
      Effect.print_s [%message "Added todo with ID" (new_id : int)]
    in
    let todo_items =
      List.map todos ~f:(fun (id, text) ->
        {%html|
          <li>
            #{text}<button
              style="margin-left: 8px"
              on_click=%{fun _ ->
                Effect.ignore_m (inject (Remove_todo id))}
            >
              #{" Remove "}
            </button>
          </li>
        |})
    in
    {%html|
      <div style="padding: 16px">
        <button on_click=%{fun _ -> add_todo_effect}>Add Todo</button>
        <ul style="margin-top: 8px">
          *{todo_items}
        </ul>
      </div>
    |}
  ;;
end
```

### State machine with input: `Bonsai.state_machine_with_input`

Use `Bonsai.state_machine_with_input` when your state machine's behavior
needs to depend on external values that can change. The `~input`
parameter provides the current value to `apply_action`, allowing actions
to use up-to-date external data.

This is useful when state updates need access to dynamic values like
user preferences, configuration settings, or other changing context.

``` ocaml
module Counter_with_step = struct
  type action =
    | Increment
    | Decrement
  [@@deriving sexp_of]

  let component (graph @ local) =
    let step_size, set_step_size = Bonsai.state 1 graph in
    let count, update_count =
      Bonsai.state_machine_with_input
        ~default_model:0
        ~apply_action:(fun _ step_size_status count action ->
          let step_size =
            match step_size_status with
            | Active step_size -> step_size
            | Inactive -> 1
          in
          match action with
          | Increment -> count + step_size
          | Decrement -> count - step_size)
        step_size
        graph
    in
    let%arr count and update_count and step_size and set_step_size in
    {%html|
      <div style="padding: 16px">
        <div style="margin-bottom: 16px">
          <span>Step size: %{step_size#Int}</span>
          <Step_button.view ~set_step_size ~step_size:%{1} />
          <Step_button.view ~set_step_size ~step_size:%{5} />
          <Step_button.view ~set_step_size ~step_size:%{10} />
        </div>
        <div>
          <button on_click=%{fun _ -> update_count Decrement}>-</button>
          <span style="margin: 0 16px">Count: %{count#Int}</span>
          <button on_click=%{fun _ -> update_count Increment}>+</button>
        </div>
      </div>
    |}
  ;;
end

let component = Counter_with_step.component graph
```

### Per-key state: `Bonsai.scope_model`

Use `Bonsai.scope_model` to maintain separate state for different values
of a key. Each unique key gets its own independent state that persists
when you switch back to it.

``` ocaml
module Form = Bonsai_web_form.With_manual_view

module Counter = struct
  let component (graph @ local) =
    let count, set_count = Bonsai.state' 0 graph in
    let%arr count and set_count in
    {%html|
      <div>
        #{" Count: "}#{Int.to_string count}<button
          style="margin-left: 16px; margin-right: 16px"
          on_click=%{fun _ -> set_count (fun c -> c + 1)}
        >
          #{" Increment "}
        </button>
      </div>
    |}
  ;;
end

module Scoped_counters = struct
  let component (graph @ local) =
    let form =
      Form.Elements.Dropdown.list
        (module String)
        ~equal:[%equal: String.t]
        (Bonsai.return [ "Alice"; "Bob"; "Charlie" ])
        graph
    in
    let active_user =
      let%arr form in
      Form.value_or_default form ~default:"Alice"
    in
    Bonsai.scope_model
      (module String)
      ~on:active_user
      graph
      ~for_:(fun graph ->
        let%arr counter = Counter.component graph
        and form in
        {%html|
          <div>
            <div>User: %{Form.view form}</div>
            %{counter}
          </div>
        |})
  ;;
end

let component = Scoped_counters.component graph
```

## Effects

Effects are how you trigger side effects in Bonsai code. They are values
that your code can emit in event handlers and other places, and may be
run asynchronously.

An effect will run every time it is *scheduled* by the Bonsai runtime.

**How it works:** - You specify what should happen (update state,
network request, DOM manipulation) - The Bonsai runtime schedules it
(typically from event handlers like `on_click`)

### Scheduling Effects

Effects represent side effects that will run when received by the Bonsai
runtime (e.g., from an event handler). For example, the setter function
returned from `Bonsai.state` returns an `Effect.t` when called. However,
this `Effect.t` doesn't do anything until we return it in our `on_click`
handler.

``` ocaml
let component (graph @ local) =
  let counter, set_counter = Bonsai.state' 0 graph in
  let%arr counter and set_counter in
  (* Effects are constructed here but don't run yet *)
  let decrement : unit Effect.t = set_counter (fun c -> c - 1) in
  let increment : unit Effect.t = set_counter (fun c -> c + 1) in
  {%html|
    <div>
      <!-- Effects are scheduled (run) when the button is clicked -->
      <button on_click=%{fun _ -> decrement}>-</button>
      <span>%{counter#Int}</span>
      <button on_click=%{fun _ -> increment}>+</button>
    </div>
  |}
;;
```

### Chaining Effects

Effects can be chained together using the `Effect` monad.

``` ocaml
module Multi_step_form = struct
  let component (graph @ local) =
    let message, set_message = Bonsai.state "" graph in
    let submit_form =
      let%arr set_message in
      let%bind.Effect () =
        Effect.print_s [%message "Validating form..."]
      in
      let%bind.Effect () =
        Effect.print_s [%message "Processing submission..."]
      in
      set_message "Form submitted successfully!"
    in
    let%arr message and submit_form in
    {%html|
      <div style="padding: 16px">
        <button on_click=%{fun _ -> submit_form}>Submit Form</button>
        <div style="margin-top: 8px">#{message}</div>
      </div>
    |}
  ;;
end
```

### Converting synchronous functions into effects

Use `Effect.of_sync_fun` to convert a synchronous function into an
effect. This is useful when you want to perform computations or side
effects (like logging or local storage operations) in response to user
actions.

``` ocaml
module Random_number_generator = struct
  let component (graph @ local) =
    let current_number, set_number = Bonsai.state 0 graph in
    let generate_random =
      let%arr set_number in
      let%bind.Effect random_number =
        Effect.of_sync_fun (fun () -> Random.int 100) ()
      in
      set_number random_number
    in
    let%arr current_number and generate_random in
    {%html|
      <div style="padding: 16px">
        <div>#{"Current number: "}%{current_number#Int}</div>
        <button style="margin-top: 8px" on_click=%{fun _ -> generate_random}>
          #{" Generate Random Number "}
        </button>
      </div>
    |}
  ;;
end
```

### Combining multiple effects: `Effect.all_parallel` and `Effect.all_parallel_unit`

Use `Effect.all_parallel` to run multiple effects at once and return a
list of their results when all complete. If these effects return `unit`,
then use `Effect.all_parallel_unit` instead. This is commonly used with
`Effect.Prevent_default` to prevent default browser behavior while also
performing your custom logic.

``` ocaml
module Custom_link = struct
  let component (graph @ local) =
    let clicked_count, set_count = Bonsai.state' 0 graph in
    let%arr clicked_count and set_count in
    let handle_click =
      Effect.all_parallel_unit
        [ (Effect.Prevent_default [@alert "-deprecated"])
        ; set_count (fun count -> count + 1)
        ; Effect.print_s [%message "Link clicked!"]
        ]
    in
    {%html|
      <div style="padding: 16px">
        <a
          href="https://example.com"
          on_click=%{fun _ -> handle_click}
          style="color: #3b82f6; text-decoration: underline"
          >#{" Click me (won't navigate) "}</a
        >
        <div style="margin-top: 8px">
          #{" Clicked "}#{Int.to_string clicked_count}#{" times "}
        </div>
      </div>
    |}
  ;;
end
```

## Error Handling

We recommended wrapping fallible values in `Or_error.t Bonsai.t` and
(almost) **never** raising exceptions. Exceptions are extremely slow in
`js_of_ocaml` and can cause extreme performance degradation.

You can match on these values using `match%arr` and handle the error
accordingly.

``` ocaml
module Student_profile = struct
  let component ~(student : Student.t Or_error.t) () =
    match student with
    | Ok student ->
      {%html|<div %{student_style}>Hi I'm #{Student.name student}</div>|}
    | Error error ->
      {%html|<div %{error_style}>Failed to fetch student: #{Error.to_string_hum error}</div>|}
  ;;
end

let eve = Student.Phd { name = "Eve"; thesis = "Dedekind Cuts" }

module Student_profile_page = struct
  let component (graph @ local) =
    let student, set_student = Bonsai.state (Ok eve) graph in
    let%arr student and set_student in
    let has_error = Or_error.is_error student in
    {%html|
      <div %{container_style}><div %{button_wrapper_style}><button
            disabled'=%{has_error}
            on_click=%{fun _ -> set_student (Or_error.error_string "Student not found")}
          >#{" Error "}</button><button
            disabled'=%{not has_error}
            on_click=%{fun _ -> set_student (Ok eve)}
          >#{" Success "}</button></div><Student_profile.component ~student /></div>
    |}
  ;;
end
```

