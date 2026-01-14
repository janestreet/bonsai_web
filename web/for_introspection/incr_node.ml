open! Core
open Js_of_ocaml
open Bonsai_introspection_protocol

class type global = object
  (* [ bonsaiNodeIntrospectionSupported ] is used to distinguish between non-bonsai apps
     vs bonsai apps that have not yet picked up the introspection changes yet. *)
  method bonsaiIncrNodeIntrospectionSupported : Js.number Js.t Js.prop

  method bonsaiIncrNodeIntrospectionPopEvents :
    (unit -> Js.js_string Js.t) Js.callback Js.prop
end

let global : global Js.t = Js.Unsafe.global
let queue : Incr_node_protocol.Event.Stable.t Queue.t = Queue.create ()

let enqueue ~here kind =
  let kind = Bonsai.Private.Annotate_incr.Kind.name kind in
  let event =
    Incr_node_protocol.Event.Stable.of_latest (Node_created { node = { here; kind } })
  in
  Queue.enqueue queue event
;;

let pop_events' () =
  let events = Queue.to_list queue in
  Queue.clear queue;
  events
;;

let pop_events () =
  pop_events' ()
  |> [%sexp_of: Incr_node_protocol.Event.Stable.t list]
  |> Sexp.to_string_mach
  |> Js.string
;;

let is_recording =
  let is_recording_var =
    Utils.Session_storage_var.create
      (module Bool)
      ~unique_id:"incrNodeIsRecording"
      ~default:false
  in
  fun () -> Utils.Session_storage_var.get is_recording_var
;;

let () =
  Bonsai.Private.Annotate_incr.on_incr_annotation (fun ~here kind _ ->
    if is_recording () then enqueue ~here kind)
;;

let init_global () =
  global##.bonsaiIncrNodeIntrospectionSupported := Js.number_of_float 2.;
  global##.bonsaiIncrNodeIntrospectionPopEvents := Js.wrap_callback pop_events
;;

let run_top_level_side_effects = lazy (init_global ())
