# Partial Render Table

A very common problem is performantly displaying lots of tabular data.

If your list has tens or even hundreds of entries, you might get away
with a simple HTML table. But once we get to thousands, the browser
starts to struggle.

`bonsai_web_ui_partial_render_table` implements a reusable table
component, with a bunch of features:

-   Focusable rows and cells, with support for keyboard navigation
-   Stateful cells and a dynamic set of columns
-   Sorting by one or multiple columns
-   Customizable initial column width
-   Reorderable columns

## Client-side (Basic) PRT

Client-side PRTs receive the entire dataset from the server, and render
only the range visible on the screen.

Let's walk through setting up a PRT.

### Row Key and Data

We need a `Row_key` module that implements `Comparator`. The key for
each row must be unique.

Our row data type (`'data`) can be anything. Most tables use a record
type for `'data`. For example:

```{=html}
<!-- $MDX file=../../examples/bonsai_guide_code/prt_examples.ml,part=row_type -->
```
``` ocaml
(* Our "row key" type is [Symbol.t], which we implement as a string. *)
module Symbol = String

module Row = struct
  type t =
    { symbol : Symbol.t
    ; price : float
    ; num_owned : int
    ; last_updated : Time_ns.t
    }
  [@@deriving sexp, compare, equal, bin_io, typed_fields]
end
```

It's common to have the `Row_key.t` appear somewhere in the `'data`
type.

Then, we'll need to get a `(Row_key.t, 'data, 'cmp) Map.t Bonsai.t`,
which is the data that powers the PRT. In most real apps, you'll get
this data from your server with a
[`Rpc_effect.Polling_state_rpc.poll`](./rpcs.md).

### Defining Columns

Next, we need to define a "column id" type. If we use a variant type:

```{=html}
<!-- $MDX file=../../examples/bonsai_guide_code/prt_examples.ml,part=variant_col_id -->
```
``` ocaml
  module Col_id = struct
    module T = struct
      type t =
        | Symbol
        | Price
        | Num_owned
        | Last_updated
      [@@deriving sexp, compare, enumerate]
    end

    include T
    include Comparator.Make (T)
  end
```

### Column Structure

We then create a `Column_structure.t`, which defines the order and
grouping of columns.

```{=html}
<!-- $MDX file=../../examples/bonsai_guide_code/prt_examples.ml,part=variant_structure -->
```
``` ocaml
  module Structure = Bonsai_web_ui_partial_render_table.Column_structure

  let structure =
    Structure.Group.(
      [ leaf Col_id.Symbol
      ; group
          ~label:(return {%html|Position|})
          [ leaf Col_id.Price; leaf Col_id.Num_owned ]
      ; leaf Col_id.Last_updated
      ]
      |> lift)
  ;;
```

Alternatively, you could have a flat column structure:

```{=html}
<!-- $MDX file=../../examples/bonsai_guide_code/prt_examples.ml,part=flat_structure -->
```
``` ocaml
  let structure = Structure.flat Col_id.all
```

You can also use `Column_structure.flat_dynamic` or
`Column_structure.Group_dynamic` to provide your structure as a
`Bonsai.t`. This allows you to dynamically reorder, add, remove, or
group columns, e.g. with `bonsai_web_ui_reorderable_list`. But it
requires more incremental nodes.

You can also specify initial widths for your columns, and whether they
can be resized by dragging:

```{=html}
<!-- $MDX file=../../examples/bonsai_guide_code/prt_examples.ml,part=variant_structure_mods -->
```
``` ocaml
  let structure =
    structure
    |> Structure.with_initial_widths
         ~f:
           (Bonsai.return (function
             | Col_id.Symbol -> `Px 75
             | Last_updated -> `Px 150
             | _ -> Structure.default_initial_width))
    |> Structure.with_is_resizable
         ~f:
           (Bonsai.return (function
             | Col_id.Symbol | Price -> true
             | Num_owned | Last_updated -> false))
  ;;
```

### Rendering Cells and Headers

Then, we specify how the cells and headers should be rendered:

```{=html}
<!-- $MDX file=../../examples/bonsai_guide_code/prt_examples.ml,part=variant_columns -->
```
``` ocaml
  module Table = Bonsai_web_ui_partial_render_table.Basic

  let columns : (Symbol.t, Row.t, Col_id.t) Table.Columns.t =
    Table.Columns.build
      (module Col_id)
      ~columns:structure
      ~render_cell:
        (Stateful_rows
           (fun _key data (local_ _graph) ->
             let%arr { Row.symbol; price; num_owned; last_updated } = data in
             fun col ->
               match col with
               | Col_id.Symbol -> Vdom.Node.text symbol
               | Price -> Vdom.Node.text (sprintf "%.2f" price)
               | Num_owned -> Vdom.Node.text (string_of_int num_owned)
               | Last_updated -> Vdom.Node.text (Time_ns.to_string last_updated)))
      ~render_header:(fun col (local_ _graph) ->
        match%arr col with
        | Symbol -> Vdom.Node.text "Symbol"
        | Price -> Vdom.Node.text "Price"
        | Num_owned -> Vdom.Node.text "Num_owned"
        | Last_updated -> Vdom.Node.text "Last Updated")
  ;;
```

You have 3 options for your `render_cell` function, with different
[performance](https://github.com/janestreet/bonsai_web_components/blob/c0e4224ff10a1ec59e49d34e476a647fae2dec74/partial_render_table/bench/bin/main.ml).

`Pure` is the fastest and requires the least amount of memory. Use
`Pure` if you don't need access to `local_ graph` inside your cells.

If you do need to use APIs that take `local_ graph` in your cells, you
should try to use `Stateful_rows` instead of `Stateful_cells`, which is
slower, and makes it easy to accidentally create incremental nodes you
don't actually need.

#### `render_cell`, In Depth

1.  `Pure` is a simple
    `('column_id -> 'key -> 'data -> Vdom.Node.t) Bonsai.t`. This is the
    most performant option by far for simple tables, and requires the
    fewest incremental nodes.
2.  `Stateful_rows` allows you to instantiate state at the level of each
    row, but not in the individual cells:
    `'key Bonsai.t -> 'data Bonsai.t -> local_ Bonsai.graph -> ('column_id -> Vdom.Node.t) Bonsai.t`.
3.  `Stateful_cells` is the most powerful, but also least performant
    option, allowing you to instantiate state in any cell:
    `'column_id Bonsai.t -> 'key Bonsai.t -> 'data Bonsai.t -> local_ Bonsai.graph -> Vdom.Node.t Bonsai.t`.

If you don't need to use `local_ graph` in your rows / cells, you should
use `Pure`, because it is the fastest, and requires the least memory. We
should have used `Pure` in the example above, and we'll do so in later
examples.

If you do (e.g. if you have a form or chart in your cells), you should
try to use `Stateful_rows` over `Stateful_cells`: in addition to just
being faster, this ensures that you are instantiating the stateful
components you need for your cells once per row, not once per cell!

It's worth noting that many things can be done with a less powerful API.
For example, let's say you want to have a counter in each cell of your
table. You could instantiate a separate `Bonsai.state` storing `int` in
each cell, or you could maintain a single `Bonsai.state` that stores a
`int (Row_id.t * Col_id.t).Map.t`, which you `let%arr` over to create a
single `Pure` rendering function for the whole table. The downside is
that any time any cell's count changes, the content of all cells must be
recomputed. That's why `Stateful_rows` is usually the right compromise
for tables that need stateful elements.

#### Popovers + Modals

A common element to put inside table cells is popovers / modals. Instead
of instantiating a `Bonsai_web_ui_toplayer.Popover` per cell / row, you
might want to instantiate a single
`Bonsai_web_ui_toplayer.Popover.For_external_state.t`, and have the
"open popover" effect set some `Bonsai.state`, which stores a row key.
Then, you can attach the popover positioning attr to the cell whose
`Row_id.t` matches your open state.

### Initializing the Table

Finally, we glue all our pieces together:

```{=html}
<!-- $MDX file=../../examples/bonsai_guide_code/prt_examples.ml,part=table_no_focus -->
```
``` ocaml
  let component (local_ graph) ~data =
    let table =
      Table.component
        (module Symbol)
        ~focus:None
        ~row_height:(Bonsai.return (`Px 30))
        ~columns
        data
        graph
    in
    let%arr { view; _ } = table in
    view
  ;;
```

```{=html}
<iframe style="max-height: 1000px" data-external="1" src="https://bonsai:8535#prt">
```
```{=html}
</iframe>
```
### Sorting

The `Table.Basic.Columns.build` function takes an optional `sorts`
argument, which allows you to specify a `Sort_kind.t option` for every
`column_id`. A `Sort_kind.t` consists of 2 functions: `forward` for
"ascending" sorts, and `reverse` for "descending" sorts.

Most sort functions are reversible, so you can use
`Sort_kind.reversible` to generate a `Sort_kind.t` from just an
"ascending" sorter. Let's make our table sortable by `symbol` and
`price`, but not by `num_owned`:

```{=html}
<!-- $MDX file=../../examples/bonsai_guide_code/prt_examples.ml,part=sort_variant -->
```
``` ocaml
  module Sort_kind = Bonsai_web_ui_partial_render_table.Sort_kind

  let sorts (col_id : Col_id.t Bonsai.t) (local_ _graph) =
    let%arr col_id in
    match col_id with
    | Symbol ->
      Some
        (Sort_kind.reversible ~forward:(fun (_a_key, a) (_b_key, b) ->
           [%compare: string] a.Row.symbol b.Row.symbol))
    | Price ->
      Some
        (Sort_kind.reversible ~forward:(fun (_a_key, a) (_b_key, b) ->
           [%compare: float] a.Row.price b.Row.price))
    | Num_owned -> None
    | Last_updated ->
      Some
        (Sort_kind.reversible ~forward:(fun (_a_key, a) (_b_key, b) ->
           [%compare: Time_ns.t] a.Row.last_updated b.Row.last_updated))
  ;;
```

If we pass this as a `~sorts` argument to our column definition, we can
click on column headers to sort by that column, or Shift+click to sort
by multiple columns at once:

```{=html}
<iframe style="max-height: 1000px" data-external="1" src="https://bonsai:8535#sort">
```
```{=html}
</iframe>
```
### Focus

PRT supports focusing by row or cell. It returns a "handle", which can
be used to implement keyboard navigation by listening to keyboard
events:

```{=html}
<!-- $MDX file=../../examples/bonsai_guide_code/prt_examples.ml,part=focus_variant -->
```
``` ocaml
  let component (local_ graph) ~data =
    let table =
      Table.component
        (module Symbol)
        ~focus:
          (Table.Focus.By_cell
             { on_change =
                 Bonsai.return (fun (_ : (Symbol.t * Col_id.t) option) -> Effect.Ignore)
             })
        ~row_height:(Bonsai.return (`Px 30))
        ~columns
        data
        graph
    in
    let%arr { view; focus; num_filtered_rows; _ } = table in
    Vdom.Node.div
      ~attrs:
        [ Vdom.Attr.on_keydown (fun kbc ->
            let binding =
              let current_or_first_column =
                match Table.Focus.By_cell.focused focus with
                | None -> Col_id.Symbol
                | Some (_, c) -> c
              in
              match Js_of_ocaml.Dom_html.Keyboard_code.of_event kbc with
              | ArrowDown | KeyJ -> Some (Table.Focus.By_cell.focus_down focus)
              | ArrowUp | KeyK -> Some (Table.Focus.By_cell.focus_up focus)
              | ArrowLeft | KeyH -> Some (Table.Focus.By_cell.focus_left focus)
              | ArrowRight | KeyL -> Some (Table.Focus.By_cell.focus_right focus)
              | PageDown -> Some (Table.Focus.By_cell.page_down focus)
              | PageUp -> Some (Table.Focus.By_cell.page_up focus)
              | Escape -> Some (Table.Focus.By_cell.unfocus focus)
              | Home ->
                Some (Table.Focus.By_cell.focus_index focus 0 current_or_first_column)
              | End ->
                Some
                  (Table.Focus.By_cell.focus_index
                     focus
                     num_filtered_rows
                     current_or_first_column)
              | _ -> None
            in
            match binding with
            | Some b -> Effect.Many [ Effect.Prevent_default; b ]
            | None -> Effect.Ignore)
          (* [tabindex=0] allows browser focus to be set on the table.
             We then remove the default focus ring with [outline: none] css. *)
        ; Vdom.Attr.tabindex 0
        ; {%css|outline: none;|}
        ]
      [ view ]
  ;;
```

```{=html}
<iframe style="max-height: 1000px" data-external="1" src="https://bonsai:8535#focus_variant">
```
```{=html}
</iframe>
```
In practice, you might want to attach the listener attr somewhere higher
up, or as a `Vdom.Attr.Global_listeners`.

### Styling

The PRT accepts a `~styling` argument, which allows configuring its
appearance.

You can configure colors, borders, and fonts explicitly:

```{=html}
<!-- $MDX file=../../examples/bonsai_guide_code/prt_examples.ml,part=prt_styling -->
```
``` ocaml
    let table =
      Table.component
        (module Symbol)
        ~styling:
          (This_one
             (Bonsai.return
                Bonsai_web_ui_partial_render_table_styling.(
                  create
                    { colors =
                        { page_bg = `Hex "#f0f4f8"
                        ; page_fg = `Hex "#333333"
                        ; header_bg = `Hex "#2c3e50"
                        ; header_fg = `Hex "#ecf0f1"
                        ; header_cell_focused_bg = `Hex "#2c3e50"
                        ; header_cell_focused_fg = `Hex "#2980b9"
                        ; row_even_bg = `Hex "#ffffff"
                        ; row_even_fg = `Hex "#333333"
                        ; row_odd_bg = `Hex "#e8eef2"
                        ; row_odd_fg = `Hex "#333333"
                        ; cell_focused_bg = `Hex "#3498db"
                        ; cell_focused_fg = `Hex "#ffffff"
                        ; cell_focused_outline = Some (`Hex "#2980b9")
                        ; row_focused_bg = `Hex "#d6eaf8"
                        ; row_focused_fg = `Hex "#2980b9"
                        ; row_focused_border = `Hex "#2980b9"
                        ; row_of_focused_cell_fg = None
                        ; row_of_focused_cell_bg = None
                        ; header_header_border = `Hex "#34495e"
                        ; body_body_border = `Hex "#bdc3c7"
                        ; header_body_border = `Hex "#7f8c8d"
                        }
                    ; lengths = Params.Lengths.default
                    ; fonts = Params.Fonts.default
                    })))
        ~focus:
          (Table.Focus.By_cell
             { on_change =
                 Bonsai.return (fun (_ : (Symbol.t * Col_id.t) option) -> Effect.Ignore)
             })
        ~row_height:(Bonsai.return (`Px 30))
        ~columns
        data
        graph
    in
```

```{=html}
<iframe style="max-height: 1000px" data-external="1" src="https://bonsai:8535#styling">
```
```{=html}
</iframe>
```
By default, styling config will be pulled from the
[theme](./theming.md). We recommend explicitly passing a `Styling.t`,
because dynamically scoped theming is implicit, and can be difficult to
follow.

## Typed Fields

If your `Row.t` is a record type, you can derive
[`typed_fields`](https://github.com/janestreet/ppx_typed_fields) on your
`Row.t` to get a `Col_id.t` that will always match your `Row.t`
structure:

```{=html}
<!-- $MDX file=../../examples/bonsai_guide_code/prt_examples.ml,part=typed_fields_col_id -->
```
``` ocaml
  module Col_id = struct
    include Row.Typed_field.Packed
    include Comparator.Make (Row.Typed_field.Packed)
  end
```

It reduces boilerplate when implementing `sorts`:

```{=html}
<!-- $MDX file=../../examples/bonsai_guide_code/prt_examples.ml,part=typed_fields_sorts -->
```
``` ocaml
  module Sort_kind = Bonsai_web_ui_partial_render_table.Sort_kind

  let sort (type a) (module S : Comparable with type t = a) (field : a Row.Typed_field.t) =
    Some
      (Sort_kind.reversible ~forward:(fun (_a_key, a) (_b_key, b) ->
         S.compare (Row.Typed_field.get field a) (Row.Typed_field.get field b)))
  ;;

  let sorts (col_id : Col_id.t Bonsai.t) (local_ _graph) =
    let%arr { f = T field } = col_id in
    match field with
    | Symbol -> sort (module String) field
    | Price -> sort (module Float) field
    | Num_owned -> None
    | Last_updated -> sort (module Time_ns) field
  ;;
```

and cell rendering logic:

```{=html}
<!-- $MDX file=../../examples/bonsai_guide_code/prt_examples.ml,part=typed_fields_columns -->
```
``` ocaml
  module Table = Bonsai_web_ui_partial_render_table.Basic

  let columns : (Symbol.t, Row.t, Col_id.t) Table.Columns.t =
    Table.Columns.build
      (module Col_id)
      ~sorts
      ~columns:structure
      ~render_cell:
        (Pure
           (return (fun { Col_id.f = T field } _key data ->
              let value = Row.Typed_field.get field data in
              match field with
              | Symbol -> Vdom.Node.text value
              | Price -> Vdom.Node.text (sprintf "%.2f" value)
              | Num_owned -> Vdom.Node.text (string_of_int value)
              | Last_updated -> Vdom.Node.text (Time_ns.to_string value))))
      ~render_header:(fun col (local_ _graph) ->
        let%arr { f = T field } = col in
        Vdom.Node.text (Row.Typed_field.name field))
  ;;
```

The biggest downside of using typed fields is that it's tricky to add
additional columns. You can do this by having your `Col_id.t` be a
variant of the `Row.Typed_fields.Packed.t`, or your additional fields.

## Server-side (Expert) PRT

Client-side PRTs work great for tens of thousands of rows, but as we get
into hundreds of thousands or millions, shipping all that data to the
client becomes a performance bottleneck. This is amplified if your data
changes frequently.

With server-side PRTs, we only send the currently viewed range of data,
so the type of our input data changes from
`(Row_key.t, Data.t, 'cmp) Map.t` to
`(Row_key.t, Data.t) Incr_map_collate.Collated.t`. Collation consists of
filtering, sorting, and range-restricting data. The `Collated.t` type
contains the currently viewed range of data, in addition to some
metadata about the total number of rows before / after filtering and
range restriction.

A server-side table requires some additional setup. This is currently
very boilerplate-heavy, and we hope to improve it in the future.

### Protocol Changes

With a client-side PRT, we collate on the client. Our RPC to get data
from the server might just use `unit` for its query type, since we're
fetching everything.

With server-side collation, we will need to provide:

-   Filtering params, if filtering is supported. The type you'll use
    here depends on how you implement filtering. For example, you might
    use a string for matching against one of the fields, a collection of
    rules for how to filter each column, or some other arbitrary
    `Filter_params.t` type.

```{=html}
<!-- -->
```
-   Sorting params, if sorting is supported. If your columns support
    `Asc | Desc | None` sorting, consider using
    `Bonsai_web_ui_partial_render_table_protocol.Stable.Order.t`
-   The currently viewed range of rows. This should probably be a
    `start:int * end:int` tuple or record.

### Client-side Changes

#### Columns

The process of creating columns is mostly the same, except that you need
to use the `Expert` module, and instantiate column sorting state
yourself:

```{=html}
<!-- $MDX file=../../examples/bonsai_guide_code/prt_examples.ml,part=server_side_columns -->
```
``` ocaml
  module Table = Bonsai_web_ui_partial_render_table.Expert

  module Col_id = struct
    include Row.Typed_field.Packed
    include Comparator.Make (Row.Typed_field.Packed)
  end

  module Structure = Bonsai_web_ui_partial_render_table.Column_structure

  let component (local_ graph) =
    (* We need to create the sortable state outside of the table. *)
    let sortable_state =
      Table.Columns.Sortable.state ~equal:[%equal: Col_id.t] () graph
    in
    let columns : (Symbol.t, Row.t, Col_id.t) Table.Columns.t =
      Table.Columns.build
        (module Col_id)
        ~columns:(Structure.flat Col_id.all)
        ~render_cell:
          (Pure
             (return (fun { Col_id.f = T field } _key data ->
                let value = Row.Typed_field.get field data in
                match field with
                | Symbol -> Vdom.Node.text value
                | Price -> Vdom.Node.text (sprintf "%.2f" value)
                | Num_owned -> Vdom.Node.text (string_of_int value)
                | Last_updated -> Vdom.Node.text (Time_ns.to_string value))))
        ~render_header:(fun col (local_ _graph) ->
          let%arr ({ f = T field } as col) = col
          and sortable_state in
          Table.Columns.Sortable.Header.Expert.default_click_handler
            ~sortable:true
            ~column_id:col
            sortable_state
            (Table.Columns.Sortable.Header.with_icon
               (Vdom.Node.text (Row.Typed_field.name field))))
    in
```

This is because you'll need to send the sort order to the server as part
of your query, so it can collate.

#### Focus

The `~focus` configuration records for `By_row` and `By_cell` have some
additional arguments.

Because the source of truth for the rows is on the server, there's no
way to tell if a focused row that's off screen still exists on the
server, or what its index is.

You can supply a `key_rank : ('key -> int option Effect.t) Bonsai.t`,
which should ping a server endpoint and get the index corresponding to
the key, if it exists.

Similarly, the `compute_presence` function allows the user to make
`Focus.By_row/cell.focused` return \[None\] if a row that's focused but
off screen doesn't actually exist anymore. Most commonly, `Fn.id` is
used, so the type of `presence` is `'key option`. Note that
`compute_presence` does not impact the visually displayed focused
row/cell.

#### Query + Table Instantiation

Creating the table is a bit tricky. We need to know how many rows fit on
the screen to build our query, and we need our query to get data, and
render the table. But the table itself reports how many rows fit on the
screen.

Here's how we might get around this:

```{=html}
<!-- $MDX file=../../examples/bonsai_guide_code/prt_examples.ml,part=server_side_query -->
```
``` ocaml
    let copied_range, set_copied_range = Bonsai.state (0, 0) graph in
    let query =
      let%arr copied_range and sortable_state and filter_params in
      { Query.filter_params
      ; sort_order = Table.Columns.Sortable.order sortable_state
      ; visible_range = copied_range
      }
    in
    (* In practice, this would probably need some kind of error handling.*)
    let data = fetch_data_polling_rpc query in
    let table =
      Table.component
        (module Symbol)
        ~focus
        ~row_height:(Bonsai.return (`Px 30))
        ~columns
        data
        graph
    in
    let%sub { range = table_range; _ } = table in
    Bonsai.Edge.on_change
      ~equal:[%equal: int * int]
      table_range
      ~callback:
        (let%arr set_copied_range in
         fun table_range -> set_copied_range table_range)
```

It's unfortunate that syncing state like this is necessary. We are
hoping to improve this in the future.

```{=html}
```
## Your Table Might Start Empty

The data displayed in your table is probably loaded via an RPC. If you
try to run some side effect (e.g. focusing the first row) via a
`Bonsai.Edge.lifecycle ~on_activate`, this will likely run before the
server has responded with data. Consider using an `on_change` on the
data instead.

## Beware Buttons in Tables

A common design pattern is putting an "Actions" column with a bunch of
buttons at the end of your table. This can be dangerous if your table is
dynamic, because content might shift unexpectedly, and you might [click
on the wrong thing](https://web.dev/articles/cls).

Some best practices:

-   Place buttons in a separate "details" pane, which shows up when you
    focus a row.
-   Require a confirmation modal / dialog for important buttons, that
    states which row will be affected.
-   Keyboard-controlled forms that submit on enter are often preferable,
    if you **need** to do dangerous in a table.
