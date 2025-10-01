open! Core

(** Like Persistent_var, but reads directly from session storage all the time instead
    using an intermediate var after the initial read. *)
module Session_storage_var : sig
  type 'a t

  val create
    :  (module Sexpable with type t = 'a)
    -> unique_id:string
    -> default:'a
    -> 'a t

  val get : 'a t -> 'a
  val set : 'a t -> 'a -> unit
  val add_on_set_listener : 'a t -> (unit -> unit) -> unit
  val to_incr : 'a t -> 'a Ui_incr.t
end
