open! Core
module Vdom = Virtual_dom.Vdom

module Params : sig
  module Colors : sig
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

  module Lengths : sig
    (** [x] means the left + right sides, [y] means the top + bottom sides. *)
    type t =
      { body_body_border_width_x : Css_gen.Length.t
      ; body_body_border_width_y : Css_gen.Length.t
      ; cell_focused_outline_width : Css_gen.Length.t
      ; header_cell_padding_y : Css_gen.Length.t
      ; header_cell_padding_x : Css_gen.Length.t
      ; body_cell_padding_y : Css_gen.Length.t
      ; body_cell_padding_x : Css_gen.Length.t
      }

    val default : t
  end

  module Fonts : sig
    type t =
      { header_cell_font_size : Css_gen.Length.t
      ; body_cell_font_size : Css_gen.Length.t
      }

    val default : t
  end

  type t =
    { colors : Colors.t
    ; lengths : Lengths.t
    ; fonts : Fonts.t
    }
end

type t

val create : Params.t -> t

(** [default] is intended to be a reasonably good looking table. *)
val default : t

module Expert : sig
  type styling := t

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

  (** [lift] creates a new [Styling.t], with an entirely custom set of attrs. *)
  val lift : t -> styling

  val add_attrs
    :  ?header_cell:Vdom.Attr.t list
    -> ?header_cell_focused:Vdom.Attr.t list
    -> ?header_row:Vdom.Attr.t list
    -> ?header:Vdom.Attr.t list
    -> ?autosize_table_cell_wrapper:Vdom.Attr.t list
    -> ?autosize_table_cell_wrapper_focused:Vdom.Attr.t list
    -> ?cell:Vdom.Attr.t list
    -> ?cell_focused:Vdom.Attr.t list
    -> ?row:Vdom.Attr.t list
    -> ?row_focused:Vdom.Attr.t list
    -> ?row_of_focused_cell:Vdom.Attr.t list
    -> ?body:Vdom.Attr.t list
    -> ?table:Vdom.Attr.t list
    -> ?table_vars:Vdom.Attr.t list
    -> styling
    -> styling
end

module Private : sig
  val resolve : t -> resize_column_widths_to_fit:bool -> Expert.t
end
