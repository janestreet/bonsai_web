open! Core
open! Import
include module type of Bonsai_web_test_selector with type t = Bonsai_web_test_selector.t

(** Returns an attribute which adds a data attr in tests, but is a no-op in production. *)
val attr : t -> Virtual_dom.Vdom.Attr.t

val attr_of_opt : t option -> Virtual_dom.Vdom.Attr.t
