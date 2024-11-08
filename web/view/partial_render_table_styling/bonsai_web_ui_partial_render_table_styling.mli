open! Core
module Vdom = Virtual_dom.Vdom

module Params : sig
  module Colors : sig
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

type t

val create : Params.t -> t

(** [default] is intended to be a reasonably good looking table. *)
val default : t

module Expert : sig
  type styling := t

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

  val lift : (resize_column_widths_to_fit:bool -> t) -> styling
  val map : styling -> f:(t -> t) -> styling
end

module Private : sig
  val resolve : t -> resize_column_widths_to_fit:bool -> Expert.t
end
