open! Core

(** A test selector type that avoids users from passing strings around and crafting css
    selectors via string interpolation. We've made it unix-compatible so that test
    selectors can be shared between both JS-based tests and native tests that interact
    with a browser via API. *)
type t

(** Creates a unique test selector that keeps track of its creation location.

    If provided, [name] will appear in the printed attr, and can be helpful for debugging. *)
val make : here:[%call_pos] -> ?name:string -> unit -> t

module Keyed : sig
  type test_selector = t

  (** A [Keyed.t] is a "parametrized" selector: it will return the same value for every
      distinct value of ['a]. In contrast, [Test_selector.make] return a new selector each
      time it's called.

      [Keyed.t] is useful if you have many "instances" of the same vdom, but need to
      select one; e.g. table cells, lists, etc. *)
  type 'a t

  module type Of_sexpable = sig
    type t [@@deriving sexp_of]
  end

  (** [create] creates a "parametrized" selector; calling [get] will return the same
      [Test_selector.t] for every distinct value of ['a].

      If provided, [name] will appear in the printed attr, and can be helpful for
      debugging. *)
  val create
    :  here:[%call_pos]
    -> ?name:string
    -> (module Of_sexpable with type t = 'a)
    -> 'a t

  val get : 'a t -> 'a -> test_selector
end

module For_bonsai_web : sig
  (** Returns the underlying string representation of a test selector for Attrs. *)
  val to_selector_data : t -> string

  (** Generates a CSS selector that can be used in tests to target the given test
      selector. *)
  val css_selector : t -> string

  (** Returns a string representation of the test selector. This will not be a valid CSS
      selector. *)
  val display : t -> string

  val filter_printed_attributes : key:string -> data:string -> bool

  val filter_printed_attributes_with_test_selector_filtering
    :  filter_printed_attributes:(key:string -> data:string -> bool) option
    -> key:string
    -> data:string
    -> bool
end
