open! Core
open! Import

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

let table_attr autosize (constants : Constants.t) =
  let style_module =
    if autosize
    then (module Autosize_table_styling : Default_table_styling.S)
    else (module Default_table_styling : Default_table_styling.S)
  in
  let module Stylesheet = (val style_module) in
  let vars =
    Stylesheet.Variables.set
      ~bg:(Css_gen.Color.to_string_css constants.primary.background)
      ~fg:(Css_gen.Color.to_string_css constants.primary.foreground)
      ~header_bg:(Css_gen.Color.to_string_css constants.table.header_row.background)
      ~header_fg:(Css_gen.Color.to_string_css constants.table.header_row.foreground)
      ~row_even_bg:(Css_gen.Color.to_string_css constants.table.body_row_even.background)
      ~row_even_fg:(Css_gen.Color.to_string_css constants.table.body_row_even.foreground)
      ~row_odd_bg:(Css_gen.Color.to_string_css constants.table.body_row_odd.background)
      ~row_odd_fg:(Css_gen.Color.to_string_css constants.table.body_row_odd.foreground)
      ~cell_focused_bg:
        (Css_gen.Color.to_string_css constants.table.body_cell_focused.background)
      ~cell_focused_fg:
        (Css_gen.Color.to_string_css constants.table.body_cell_focused.foreground)
      ~row_focused_bg:
        (Css_gen.Color.to_string_css constants.table.body_row_focused.background)
      ~row_focused_fg:
        (Css_gen.Color.to_string_css constants.table.body_row_focused.foreground)
      ~row_focused_border:
        (Css_gen.Color.to_string_css constants.table.body_row_focused_border)
      ~header_header_border:
        (Css_gen.Color.to_string_css constants.table.header_header_border)
      ~body_body_border:(Css_gen.Color.to_string_css constants.table.body_body_border)
      ~header_body_border:(Css_gen.Color.to_string_css constants.table.header_body_border)
      ()
  in
  Vdom.Attr.many [ vars; Stylesheet.table ]
;;

let default ?(autosize = false) constants =
  let style_module =
    if autosize
    then (module Autosize_table_styling : Default_table_styling.S)
    else (module Default_table_styling : Default_table_styling.S)
  in
  let module Stylesheet = (val style_module) in
  { header_cell = Stylesheet.header_cell
  ; header_row = Stylesheet.header_row
  ; header = Stylesheet.header
  ; autosize_table_cell_wrapper = Stylesheet.autosize_table_cell_wrapper
  ; autosize_table_bottom_border_element = Stylesheet.autosize_table_bottom_border_element
  ; cell = Stylesheet.body_cell
  ; cell_focused = Stylesheet.body_cell_focused
  ; row = Stylesheet.body_row
  ; row_focused = Stylesheet.body_row_focused
  ; body = Stylesheet.body
  ; table = table_attr autosize constants
  }
;;
