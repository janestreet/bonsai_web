open! Core
module Vdom = Virtual_dom.Vdom

module Config = struct
  type t =
    { header_cell : Vdom.Attr.t
    ; header_row : Vdom.Attr.t
    ; header : Vdom.Attr.t
    ; autosize_table_cell_wrapper : Vdom.Attr.t
    ; autosize_table_bottom_border_element : Vdom.Attr.t
    ; cell : Vdom.Attr.t
    ; cell_focused : Vdom.Attr.t
    ; row : Vdom.Attr.t
    ; row_focused : Vdom.Attr.t
    ; body : Vdom.Attr.t
    ; table : Vdom.Attr.t
    }
end

module Params = struct
  module Colors = struct
    type t =
      { page_bg : Css_gen.Color.t
      ; page_fg : Css_gen.Color.t
      ; header_bg : Css_gen.Color.t
      ; header_fg : Css_gen.Color.t
      ; row_even_bg : Css_gen.Color.t
      ; row_even_fg : Css_gen.Color.t
      ; row_odd_bg : Css_gen.Color.t
      ; row_odd_fg : Css_gen.Color.t
      ; cell_focused_bg : Css_gen.Color.t
      ; cell_focused_fg : Css_gen.Color.t
      ; row_focused_bg : Css_gen.Color.t
      ; row_focused_fg : Css_gen.Color.t
      ; row_focused_border : Css_gen.Color.t
      ; header_header_border : Css_gen.Color.t
      ; body_body_border : Css_gen.Color.t
      ; header_body_border : Css_gen.Color.t
      }
  end

  type t = { colors : Colors.t }
end

type t = resize_column_widths_to_fit:bool -> Config.t

let table_attr (module Stylesheet : Default_table_styling.S) ({ colors } : Params.t) =
  let to_string = Css_gen.Color.to_string_css in
  let vars =
    Stylesheet.Variables.set
      ~bg:(to_string colors.page_bg)
      ~fg:(to_string colors.page_fg)
      ~header_bg:(to_string colors.header_bg)
      ~header_fg:(to_string colors.header_fg)
      ~row_even_bg:(to_string colors.row_even_bg)
      ~row_even_fg:(to_string colors.row_even_fg)
      ~row_odd_bg:(to_string colors.row_odd_bg)
      ~row_odd_fg:(to_string colors.row_odd_fg)
      ~cell_focused_bg:(to_string colors.cell_focused_bg)
      ~cell_focused_fg:(to_string colors.cell_focused_fg)
      ~row_focused_bg:(to_string colors.row_focused_bg)
      ~row_focused_fg:(to_string colors.row_focused_fg)
      ~row_focused_border:(to_string colors.row_focused_border)
      ~header_header_border:(to_string colors.header_header_border)
      ~body_body_border:(to_string colors.body_body_border)
      ~header_body_border:(to_string colors.header_body_border)
      ()
  in
  Vdom.Attr.many [ vars; Stylesheet.table ]
;;

let create params ~resize_column_widths_to_fit =
  let style_module =
    if resize_column_widths_to_fit
    then (module Autosize_cols_table_styling : Default_table_styling.S)
    else (module Default_table_styling : Default_table_styling.S)
  in
  let module Stylesheet = (val style_module) in
  { Config.header_cell = Stylesheet.header_cell
  ; header_row = Stylesheet.header_row
  ; header = Stylesheet.header
  ; autosize_table_cell_wrapper = Stylesheet.autosize_table_cell_wrapper
  ; autosize_table_bottom_border_element = Stylesheet.autosize_table_bottom_border_element
  ; cell = Stylesheet.body_cell
  ; cell_focused = Stylesheet.body_cell_focused
  ; row = Stylesheet.body_row
  ; row_focused = Stylesheet.body_row_focused
  ; body = Stylesheet.body
  ; table = table_attr style_module params
  }
;;

let default =
  create
    { colors =
        { page_bg = `Name "white"
        ; page_fg = `Name "black"
        ; header_bg = `Name "black"
        ; header_fg = `Name "white"
        ; row_even_bg = `Name "#e6e6e6"
        ; row_even_fg = `Name "black"
        ; row_odd_bg = `Name "white"
        ; row_odd_fg = `Name "black"
        ; cell_focused_bg = `Hex "#e0f7ff"
        ; cell_focused_fg = `Name "black"
        ; row_focused_bg = `Hex "#e0f7ff"
        ; row_focused_fg = `Name "black"
        ; row_focused_border = `Hex "#0a90bf"
        ; header_header_border = `Name "grey"
        ; header_body_border = `Name "grey"
        ; body_body_border = `Name "grey"
        }
    }
;;

module Expert = struct
  include Config

  let lift f = f

  let map t ~f =
    let g ~resize_column_widths_to_fit = f (t ~resize_column_widths_to_fit) in
    g
  ;;
end

module Private = struct
  let resolve t ~resize_column_widths_to_fit = t ~resize_column_widths_to_fit
end
