open! Core
open Js_of_ocaml
open Bonsai_introspection_protocol

class type global = object
  method ppxModuleTimerIntrospectionSupported : Js.number Js.t Js.prop
  method ppxModuleTimerReadEvents : (unit -> Js.js_string Js.t) Js.callback Js.prop
end

let global : global Js.t = Js.Unsafe.global

let readModuleTimerEvents () =
  Ppx_module_timer_runtime.For_introspection.timing_events_in_reverse_chronological_order
    ()
  |> List.map ~f:Startup_timing_protocol.Timing_event.Stable.of_latest
  |> [%sexp_of: Startup_timing_protocol.Timing_event.Stable.t list]
  |> Sexp.to_string_mach
  |> Js.string
;;

let init_global () =
  global##.ppxModuleTimerIntrospectionSupported := Js.number_of_float 1.;
  global##.ppxModuleTimerReadEvents := Js.wrap_callback readModuleTimerEvents
;;

let run_top_level_side_effects = lazy (init_global ())
