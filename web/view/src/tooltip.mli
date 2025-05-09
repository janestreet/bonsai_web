open! Core
open! Import

module Position : sig
  type t =
    | Auto
    | Top
    | Bottom
    | Left
    | Right
  [@@deriving sexp, sexp_grammar, equal, compare, enumerate]
end

module Alignment : sig
  type t =
    | Center
    | Start
    | End
  [@@deriving sexp, sexp_grammar, equal, compare, enumerate]
end

module Direction : sig
  type t =
    | Top
    | Right
    | Bottom
    | Left
end

val make
  :  Constants.t
  -> container_attrs:Vdom.Attr.t list
  -> tooltip_attrs:Vdom.Attr.t list
  -> direction:Direction.t
  -> tipped:Vdom.Node.t
  -> tooltip:Vdom.Node.t
  -> Vdom.Node.t
