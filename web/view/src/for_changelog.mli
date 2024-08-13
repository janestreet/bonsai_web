(** Extra styling options for the Changelog component [lib/bonsai/web_ui/changelog] *)

open! Core
open! Import

module Entry_type : sig
  (** Categorization of the changelog entry. *)
  type t =
    | Feature
    | Bug_fix
    | Update
  [@@deriving sexp]
end

type t =
  { entry : Vdom.Attr.t
  ; notification : Vdom.Attr.t
  ; header : Vdom.Attr.t
  ; footer : Vdom.Attr.t
  ; chip_color : Entry_type.t -> Css_gen.Color.t
  ; close_x_button : Vdom.Node.t
  }

val default : Constants.t -> t
