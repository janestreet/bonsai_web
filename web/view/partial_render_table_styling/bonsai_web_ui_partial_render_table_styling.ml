open! Core
module Vdom = Virtual_dom.Vdom

module Config = struct
  type t =
    { header_cell : Vdom.Attr.t
    ; header_cell_focused : Vdom.Attr.t
    ; header_row : Vdom.Attr.t
    ; header : Vdom.Attr.t
    ; autosize_table_cell_wrapper : Vdom.Attr.t
    ; autosize_table_cell_wrapper_focused : Vdom.Attr.t
    ; cell : Vdom.Attr.t
    ; cell_focused : Vdom.Attr.t
    ; row : Vdom.Attr.t
    ; row_focused : Vdom.Attr.t
    ; row_of_focused_cell : Vdom.Attr.t
    ; body : Vdom.Attr.t
    ; table : Vdom.Attr.t
    ; table_vars : Vdom.Attr.t
    }
end

module Params = struct
  module Colors = struct
    type t =
      { page_bg : Css_gen.Color.t
      ; page_fg : Css_gen.Color.t
      ; header_bg : Css_gen.Color.t
      ; header_fg : Css_gen.Color.t
      ; header_cell_focused_bg : Css_gen.Color.t
      ; header_cell_focused_fg : Css_gen.Color.t
      ; row_even_bg : Css_gen.Color.t
      ; row_even_fg : Css_gen.Color.t
      ; row_odd_bg : Css_gen.Color.t
      ; row_odd_fg : Css_gen.Color.t
      ; cell_focused_bg : Css_gen.Color.t
      ; cell_focused_fg : Css_gen.Color.t
      ; cell_focused_outline : Css_gen.Color.t option
      ; row_focused_bg : Css_gen.Color.t
      ; row_focused_fg : Css_gen.Color.t
      ; row_of_focused_cell_bg : Css_gen.Color.t option
      ; row_of_focused_cell_fg : Css_gen.Color.t option
      ; row_focused_border : Css_gen.Color.t
      ; header_header_border : Css_gen.Color.t
      ; header_body_border : Css_gen.Color.t
      ; body_body_border : Css_gen.Color.t
      }
  end

  module Lengths = struct
    type t =
      { body_body_border_width_x : Css_gen.Length.t
      ; body_body_border_width_y : Css_gen.Length.t
      ; cell_focused_outline_width : Css_gen.Length.t
      ; header_cell_padding_y : Css_gen.Length.t
      ; header_cell_padding_x : Css_gen.Length.t
      ; body_cell_padding_y : Css_gen.Length.t
      ; body_cell_padding_x : Css_gen.Length.t
      }

    let default =
      { body_body_border_width_x = `Px 1
      ; body_body_border_width_y = `Px 1
      ; cell_focused_outline_width = `Px 1
      ; header_cell_padding_y = `Rem 0.3
      ; header_cell_padding_x = `Rem 0.5
      ; body_cell_padding_y = `Rem 0.3
      ; body_cell_padding_x = `Rem 0.5
      }
    ;;
  end

  module Fonts = struct
    type t =
      { header_cell_font_size : Css_gen.Length.t
      ; body_cell_font_size : Css_gen.Length.t
      }

    let default = { header_cell_font_size = `Rem 0.8; body_cell_font_size = `Rem 0.8 }
  end

  type t =
    { colors : Colors.t
    ; lengths : Lengths.t
    ; fonts : Fonts.t
    }
end

type t = resize_column_widths_to_fit:bool -> Config.t

module Style =
  [%css
  stylesheet
    {|
      /* Repeating the class selector is a specificity bump */
      .row_of_focused_cell_bg.row_of_focused_cell_bg {
        background-color: var(--row_of_focused_cell_bg_color);
      }

      .row_of_focused_cell_fg.row_of_focused_cell_fg {
        background-color: var(--row_of_focused_cell_fg_color);
      }
    |}]

let table_vars_attr
  (module Stylesheet : Default_table_styling.S)
  ({ colors; lengths; fonts } : Params.t)
  =
  let color = Css_gen.Color.to_string_css in
  let len = Css_gen.Length.to_string_css in
  let vars =
    Stylesheet.Variables.set_all
      ~bg:(color colors.page_bg)
      ~fg:(color colors.page_fg)
      ~header_bg:(color colors.header_bg)
      ~header_fg:(color colors.header_fg)
      ~header_cell_focused_bg:(color colors.header_cell_focused_bg)
      ~header_cell_focused_fg:(color colors.header_cell_focused_fg)
      ~row_even_bg:(color colors.row_even_bg)
      ~row_even_fg:(color colors.row_even_fg)
      ~row_odd_bg:(color colors.row_odd_bg)
      ~row_odd_fg:(color colors.row_odd_fg)
      ~cell_focused_bg:(color colors.cell_focused_bg)
      ~cell_focused_fg:(color colors.cell_focused_fg)
      ~row_focused_bg:(color colors.row_focused_bg)
      ~row_focused_fg:(color colors.row_focused_fg)
      ~row_focused_border:(color colors.row_focused_border)
      ~header_header_border:(color colors.header_header_border)
      ~body_body_border:(color colors.body_body_border)
      ~header_body_border:(color colors.header_body_border)
      ~body_border_width_x:(len lengths.body_body_border_width_x)
      ~body_border_width_y:(len lengths.body_body_border_width_y)
      ~header_cell_padding_x:(len lengths.header_cell_padding_x)
      ~header_cell_padding_y:(len lengths.header_cell_padding_y)
      ~body_cell_padding_x:(len lengths.body_cell_padding_x)
      ~body_cell_padding_y:(len lengths.body_cell_padding_y)
      ~header_cell_font_size:(len fonts.header_cell_font_size)
      ~body_cell_font_size:(len fonts.body_cell_font_size)
  in
  let row_of_focused_cell_vars =
    Style.Variables.set
      ?row_of_focused_cell_bg_color:(Option.map colors.row_of_focused_cell_bg ~f:color)
      ?row_of_focused_cell_fg_color:(Option.map colors.row_of_focused_cell_fg ~f:color)
      ()
  in
  Vdom.Attr.many [ vars; row_of_focused_cell_vars ]
;;

let create (params : Params.t) ~resize_column_widths_to_fit =
  let style_module =
    if resize_column_widths_to_fit
    then (module Autosize_cols_table_styling : Default_table_styling.S)
    else (module Default_table_styling : Default_table_styling.S)
  in
  let module Stylesheet = (val style_module) in
  let cell_outline, wrapper_cell_outline =
    let attr =
      match params.colors.cell_focused_outline with
      | None -> Vdom.Attr.empty
      | Some outline_color ->
        let width = params.lengths.cell_focused_outline_width in
        (* By using an outline instead of a border, we avoid shifting the layout of the
           table as the focused cell changes. But we still want to contain the outline
           within the cell, so we use a negative offset. *)
        {%css|
          outline: %{width#Css_gen.Length} solid %{outline_color#Css_gen.Color};
          outline-offset: calc(%{width#Css_gen.Length} * -1);
        |}
    in
    if resize_column_widths_to_fit then Vdom.Attr.empty, attr else attr, Vdom.Attr.empty
  in
  let row_of_focused_cell =
    Vdom.Attr.many
      [ Option.value_map
          params.colors.row_of_focused_cell_fg
          ~f:(fun _ -> Style.row_of_focused_cell_fg)
          ~default:Vdom.Attr.empty
      ; Option.value_map
          params.colors.row_of_focused_cell_bg
          ~f:(fun _ -> Style.row_of_focused_cell_bg)
          ~default:Vdom.Attr.empty
      ]
  in
  { Config.header_cell = Stylesheet.header_cell
  ; header_cell_focused = Stylesheet.header_cell_focused
  ; header_row = Stylesheet.header_row
  ; header = Stylesheet.header
  ; autosize_table_cell_wrapper = Vdom.Attr.empty
  ; autosize_table_cell_wrapper_focused = wrapper_cell_outline
  ; cell = Stylesheet.body_cell
  ; cell_focused = Vdom.Attr.many [ cell_outline; Stylesheet.body_cell_focused ]
  ; row = Stylesheet.body_row
  ; row_focused = Stylesheet.body_row_focused
  ; row_of_focused_cell
  ; body = Vdom.Attr.empty
  ; table = Stylesheet.table
  ; table_vars = table_vars_attr style_module params
  }
;;

let default =
  create
    { colors =
        { page_bg = `Name "white"
        ; page_fg = `Name "black"
        ; header_bg = `Name "black"
        ; header_fg = `Name "white"
        ; header_cell_focused_bg = `Name "black"
        ; header_cell_focused_fg = `Name "white"
        ; row_even_bg = `Name "#e6e6e6"
        ; row_even_fg = `Name "black"
        ; row_odd_bg = `Name "white"
        ; row_odd_fg = `Name "black"
        ; cell_focused_bg = `Hex "#e0f7ff"
        ; cell_focused_fg = `Name "black"
        ; cell_focused_outline = Some (`Hex "#1f7eeb")
        ; row_of_focused_cell_bg = None
        ; row_of_focused_cell_fg = None
        ; row_focused_bg = `Hex "#e0f7ff"
        ; row_focused_fg = `Name "black"
        ; row_focused_border = `Hex "#0a90bf"
        ; header_header_border = `Name "grey"
        ; header_body_border = `Name "grey"
        ; body_body_border = `Name "grey"
        }
    ; lengths = Params.Lengths.default
    ; fonts = Params.Fonts.default
    }
;;

module Expert = struct
  include Config

  let lift' f = f
  let lift t = lift' (fun ~resize_column_widths_to_fit:_ -> t)

  let map t ~f =
    let g ~resize_column_widths_to_fit = f (t ~resize_column_widths_to_fit) in
    g
  ;;

  let combine original extra =
    match extra with
    | None -> original
    | Some extra -> Vdom.Attr.many (original :: extra)
  ;;

  let add_attrs
    ?header_cell
    ?header_cell_focused
    ?header_row
    ?header
    ?autosize_table_cell_wrapper
    ?autosize_table_cell_wrapper_focused
    ?cell
    ?cell_focused
    ?row
    ?row_focused
    ?row_of_focused_cell
    ?body
    ?table
    ?table_vars
    t
    =
    map t ~f:(fun t ->
      { header_cell = combine t.header_cell header_cell
      ; header_cell_focused = combine t.header_cell_focused header_cell_focused
      ; header_row = combine t.header_row header_row
      ; header = combine t.header header
      ; autosize_table_cell_wrapper =
          combine t.autosize_table_cell_wrapper autosize_table_cell_wrapper
      ; autosize_table_cell_wrapper_focused =
          combine
            t.autosize_table_cell_wrapper_focused
            autosize_table_cell_wrapper_focused
      ; cell = combine t.cell cell
      ; cell_focused = combine t.cell_focused cell_focused
      ; row = combine t.row row
      ; row_focused = combine t.row_focused row_focused
      ; row_of_focused_cell = combine t.row_of_focused_cell row_of_focused_cell
      ; body = combine t.body body
      ; table = combine t.table table
      ; table_vars = combine t.table_vars table_vars
      })
  ;;
end

module Private = struct
  let resolve t ~resize_column_widths_to_fit = t ~resize_column_widths_to_fit
end
