# Partial Render Table

A very common problem is displaying a large quantity of tabular data
performantly.

If your list has tens or even hundreds of entries, you might get away
with a simple `View.table`. But once we get to thousands, the browser
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

### Row Data Type

The only type you strictly need is a `Row_key` module that implements
`Bonsai.comparator`, where `Row_key.t` is the row key. From there, you
need to:

-   Provide a `(Row_key.t, 'data, 'cmp) Map.t Bonsai.t` of data
-   Specify a list of columns, including a function for rendering each
    cell from the `'data`

In practice, the vast majority of tables define a `Row.t` for `'data`,
typically as a record. For example:

```{=html}
<!-- $MDX file=../../examples/bonsai_guide_code/prt_examples.ml,part=row_type -->
```
``` ocaml
module Symbol = String

module Row = struct
  type t =
    { symbol : Symbol.t
    ; price : float
    ; num_owned : int
    }
  [@@deriving sexp, compare, equal, bin_io, typed_fields]
end
```

If we key this table by `symbol`, our `Row_key` module is just `String`.

### Defining Columns

Next, we need to define a "column id" type, which we'll use to declare
which columns our table should have, and how they should be displayed.
If we use a variant type:

```{=html}
<!-- $MDX file=../../examples/bonsai_guide_code/prt_examples.ml,part=dynamic_experimental_variant_col_id -->
```
``` ocaml
  module Table = Bonsai_web_ui_partial_render_table.Basic

  module Col_id = struct
    module T = struct
      type t =
        | Symbol
        | Price
        | Num_owned
      [@@deriving sexp, compare]
    end

    include T
    include Comparator.Make (T)
  end

  let columns : (Symbol.t, Row.t, Col_id.t) Table.Columns.t =
    Table.Columns.Dynamic_experimental.build
      (module Col_id)
      ~columns:(Bonsai.return [ Col_id.Symbol; Price; Num_owned ])
      ~render_cell:(fun col _key data (local_ _graph) ->
        match%sub col with
        | Symbol ->
          let%arr { Row.symbol; _ } = data in
          Vdom.Node.text symbol
        | Price ->
          let%arr { price; _ } = data in
          Vdom.Node.text (sprintf "%.2f" price)
        | Num_owned ->
          let%arr { num_owned; _ } = data in
          Vdom.Node.text (string_of_int num_owned))
      ~render_header:(fun col (local_ _graph) ->
        let%arr col in
        let name =
          match col with
          | Symbol -> Vdom.Node.text "Symbol"
          | Price -> Vdom.Node.text "Price"
          | Num_owned -> Vdom.Node.text "Num_owned"
        in
        Table.Columns.Dynamic_columns.Sortable.Header.with_icon name)
  ;;
```

Then, we just need to call the table function:

```{=html}
<!-- $MDX file=../../examples/bonsai_guide_code/prt_examples.ml,part=dynamic_experimental_table_no_focus -->
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
<iframe style="max-height: 1000px" data-external="1" src="https://bonsai:8535#dynamic_experimental">
```
```{=html}
</iframe>
```
### Sorting

The `Table.Columns.Dynamic_experimental.build` function takes an
optional `sorts` argument, which allows you to specify a
`Sort_kind.t option` for every `column_id`. A `Sort_kind.t` consists of
2 functions: `forward` for "ascending" sorts, and `reverse` for
"descending" sorts.

Most sort functions are reversible, so you can use
`Sort_kind.reversible` to generate a `Sort_kind.t` from just an
"ascending" sorter. Let's make our table sortable by `symbol` and
`price`, but not by `num_owned`:

```{=html}
<!-- $MDX file=../../examples/bonsai_guide_code/prt_examples.ml,part=dynamic_experimental_sort_variant -->
```
``` ocaml
  module Sort_kind = Table.Columns.Dynamic_experimental.Sort_kind

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
  ;;
```

If we pass this as a `~sorts` argument to our column definition, we can
click on column headers to sort by that column, or Shift+click to sort
by multiple columns at once:

```{=html}
<iframe style="max-height: 1000px" data-external="1" src="https://bonsai:8535#dynamic_experimental_sort">
```
```{=html}
</iframe>
```
### Focus

PRT supports focusing by row or cell. It returns a "handle", which can
be used to implement keyboard navigation by listening to keyboard
events:

```{=html}
<!-- $MDX file=../../examples/bonsai_guide_code/prt_examples.ml,part=dynamic_experimental_focus_variant -->
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
          (* Allows browser focus to be set on the table. *)
        ; Vdom.Attr.tabindex 0 (* Unsets default browser styling for focused elements. *)
        ; {%css|outline: none;|}
        ]
      [ view ]
  ;;
```

```{=html}
<iframe style="max-height: 1000px" data-external="1" src="https://bonsai:8535#dynamic_experimental_focus_variant">
```
```{=html}
</iframe>
```
In practice, you might want to attach the listener attr somewhere higher
up, or as a `Vdom.Attr.Global_listeners`.

## Typed Fields

If your `Row.t` is a record type, you can derive
[`typed_fields`](https://github.com/janestreet/ppx_typed_fields) on your
`Row.t` to get a `Col_id.t` that will always match your `Row.t`
structure:

```{=html}
<!-- $MDX file=../../examples/bonsai_guide_code/prt_examples.ml,part=dynamic_experimental_typed_fields_col_id -->
```
``` ocaml
  module Table = Bonsai_web_ui_partial_render_table.Basic

  module Col_id = struct
    include Row.Typed_field.Packed
    include Comparator.Make (Row.Typed_field.Packed)
  end
```

It reduces boilerplate when implementing `sorts`:

```{=html}
<!-- $MDX file=../../examples/bonsai_guide_code/prt_examples.ml,part=dynamic_experimental_typed_fields_sorts -->
```
``` ocaml
  module Sort_kind = Table.Columns.Dynamic_experimental.Sort_kind

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
  ;;
```

and columns:

```{=html}
<!-- $MDX file=../../examples/bonsai_guide_code/prt_examples.ml,part=dynamic_experimental_typed_fields_columns -->
```
``` ocaml
  let all_columns = Bonsai.return Row.Typed_field.Packed.all

  let columns : (Symbol.t, Row.t, Col_id.t) Table.Columns.t =
    Table.Columns.Dynamic_experimental.build
      (module Col_id)
      ~sorts
      ~columns:all_columns
      ~render_cell:(fun col _key data (local_ _graph) ->
        let%arr { f = T field } = col
        and data in
        let value = Row.Typed_field.get field data in
        match field with
        | Symbol -> Vdom.Node.text value
        | Price -> Vdom.Node.textf "%f" value
        | Num_owned -> Vdom.Node.textf "%d" value)
      ~render_header:(fun col (local_ _graph) ->
        let%arr { f = T field } = col in
        Table.Columns.Dynamic_columns.Sortable.Header.with_icon
          (Vdom.Node.text (Row.Typed_field.name field)))
  ;;
```

## Server-side (Expert) PRT

Client-side PRTs work great for tens of thousands of rows, but as we get
into hundreds of thousands or millions, shipping all that data to the
client becomes a performance bottleneck. This is amplified by how
frequently your data is changing.

With server-side PRTs, we only send the currently viewed range of data,
so the type of our input data changes from
`(Row_key.t, Data.t, 'cmp) Map.t` to
`(Row_key.t, Data.t) Incr_map_collate.Collated.t`. Collation consists of
filtering, sorting, and range-restricting data. The `Collated.t` type
contains the currently viewed range of data, in addition to some
metadata about the total number of rows before / after filtering and
range restriction.

### Changes vs Client-side PRT

A server-side table requires some additional configuration.

#### Columns

The process of creating columns is mostly the same, except that you need
to use the `Expert` module, and instantiate column sorting state
yourself:

```{=html}
<!-- $MDX file=../../examples/bonsai_guide_code/prt_examples.ml,part=server_side_columns -->
```
``` ocaml
  module Table = Bonsai_web_ui_partial_render_table.Expert
  module Column = Table.Columns.Dynamic_experimental

  module Col_id = struct
    include Row.Typed_field.Packed
    include Comparator.Make (Row.Typed_field.Packed)
  end

  let all_columns = Bonsai.return Row.Typed_field.Packed.all

  let component (local_ graph) ~data =
    let sortable_state = Column.Sortable.state ~equal:[%equal: Col_id.t] () graph in
    let columns : (Symbol.t, Row.t, Col_id.t) Table.Columns.t =
      Column.build
        (module Col_id)
        ~columns:all_columns
        ~render_cell:(fun col _key data (local_ _graph) ->
          let%arr { f = T field } = col
          and data in
          let value = Row.Typed_field.get field data in
          match field with
          | Symbol -> Vdom.Node.text value
          | Price -> Vdom.Node.textf "%f" value
          | Num_owned -> Vdom.Node.textf "%d" value)
        ~render_header:(fun col (local_ _graph) ->
          let%arr ({ f = T field } as col) = col
          and sortable_state in
          Column.Sortable.Header.Expert.default_click_handler
            ~sortable:true
            ~column_id:col
            sortable_state
            (Column.Sortable.Header.with_icon
               (Vdom.Node.text (Row.Typed_field.name field))))
    in
```

This is because you'll need to send the sort order to the server as part
of your query, so it can collate.

### Focus

The `~focus` configuration records for `By_row` and `By_cell` have some
additional arguments.

Because the source of truth for the rows is on the server, there's no
way to tell if a focused row that's off screen still exists on the
server. The `compute_presence` function allows the user to customize
what `Focus.By_row/cell.focused` returns. Most commonly, `Fn.id` is
used, so the type of `presence` is `'key option`. Note that
`compute_presence` does not impact the visually displayed focused
row/cell.

For similar reasons, if you attempt to `focus` by key, and that key
isn't currently being displayed, the table won't know which index to
scroll to. You can supply a
`key_rank : ('key -> int option) unit Effect.t`, which should ping a
server endpoint and get the index corresponding to the key, if it
exists.

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
