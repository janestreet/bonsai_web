open! Core
module Incr_node = Incr_node
module Profiling = Profiling
module Rpc_effect_introspection = Rpc_effect_introspection
open Js_of_ocaml

class type global = object
  method bonsaiWebRevision : Js.js_string Js.t Js.optdef Js.prop
end

let global : global Js.t = Js.Unsafe.global

let run_top_level_side_effects =
  lazy
    (Lazy.force Rpc_effect_introspection.run_top_level_side_effects;
     Lazy.force Ppx_module_timer.run_top_level_side_effects;
     Lazy.force Incr_node.run_top_level_side_effects;
     Lazy.force Profiling.run_top_level_side_effects)
;;

let set_revision revision =
  global##.bonsaiWebRevision := Js.Optdef.return (Js.string revision)
;;
