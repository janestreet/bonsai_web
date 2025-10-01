open! Core
module Incr_node = Incr_node
module Profiling = Profiling
module Rpc_effect_introspection = Rpc_effect_introspection

(** [run_top_level_side_effects] registers the callbacks necessary for introspection to
    work/sets up the communication necessary for the chrome dev tool panel. It does not
    actually start do any introspection itself. Introspection is enabled/controlled by the
    panel via the communication mechanisms setup by [run_top_level_side_effects]. *)
val run_top_level_side_effects : unit Lazy.t

val set_revision : string -> unit
