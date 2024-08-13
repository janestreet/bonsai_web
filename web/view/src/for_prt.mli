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

(** [default] styles PRTs like the simple HTML table generated by [View.Table].

    Styling PRT, especially the borders, is unexpectedly difficult to do performantly.
    For most use cases not solvable by customizing theme constants, it is probably
    a good idea to extend these default stylings rather than replace them completely. *)
val default : ?autosize:bool -> Constants.t -> t
