open! Core
open Bonsai_introspection_protocol

(** This includes all of the introspection functions used internally in
    [Rpc_effect_kernel]. *)
include Rpc_effect_kernel.S

(** [run_top_level_side_effects] registers the callbacks necessary for introspection to
    work/sets up the communication necessary for the chrome dev tool panel. It does not
    actually start doing any introspection itself. Introspection is enabled/controlled by
    the panel via the communication mechanisms setup by [run_top_level_side_effects]. *)
val run_top_level_side_effects : unit Lazy.t

module For_testing : sig
  val get_is_recording : unit -> bool
  val pop_events : unit -> Js_of_ocaml.Js.js_string Js_of_ocaml.Js.t

  (** [pop_event'] is like [pop_events] but gives the actual events for tests where the
      thing that is desired to test is not protocol/serialization, but rather more logical
      things. *)
  val pop_events' : unit -> Rpc_effect_protocol.Event.t list

  val start_recording : unit -> unit
  val stop_recording : unit -> unit
end
