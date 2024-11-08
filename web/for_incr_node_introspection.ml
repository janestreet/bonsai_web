open! Core
open Js_of_ocaml

class type global = object
  (* [ bonsaiNodeIntrospectionSupported ] is used to distinguish between
     non-bonsai apps vs bonsai apps that have not yet picked up the introspection changes
   yet. *)
  method bonsaiIncrNodeIntrospectionSupported : bool Js.t Js.prop

  (* [ bonsaiIncrNodeIntrospectionIsRecording ] is whether incr node creation events
     are recorded/not recorded; i.e. put into the queue that
     [bonsaiIncrNodeIntrospectionPopEvents] reads. *)
  method bonsaiIncrNodeIntrospectionIsRecording : bool Js.t Js.optdef Js.prop

  method bonsaiIncrNodeIntrospectionPopEvents :
    (unit -> Js.js_string Js.t) Js.callback Js.prop

  method bonsaiIncrNodeIntrospectionStartRecording : (unit -> unit) Js.callback Js.prop
  method bonsaiIncrNodeIntrospectionStopRecording : (unit -> unit) Js.callback Js.prop
end

let global : global Js.t = Js.Unsafe.global

let is_recording () =
  match Js.Optdef.to_option global##.bonsaiIncrNodeIntrospectionIsRecording with
  | None -> false
  | Some x -> Js.to_bool x
;;

let queue
  : Bonsai_introspection_protocol.For_incr_node_introspection.Event.Stable.t Queue.t
  =
  Queue.create ()
;;

let enqueue ~here kind =
  let kind = Bonsai.Private.Annotate_incr.Kind.name kind in
  let event =
    Bonsai_introspection_protocol.For_incr_node_introspection.Event.Stable.of_latest
      (Node_created { node = { here; kind } })
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
  |> [%sexp_of:
       Bonsai_introspection_protocol.For_incr_node_introspection.Event.Stable.t list]
  |> Sexp.to_string_mach
  |> Js.string
;;

let () =
  Bonsai.Private.Annotate_incr.on_incr_annotation (fun ~here kind _ ->
    if is_recording () then enqueue ~here kind)
;;

let start_recording () =
  global##.bonsaiIncrNodeIntrospectionIsRecording := Js.Optdef.return (Js.bool true)
;;

let stop_recording () =
  global##.bonsaiIncrNodeIntrospectionIsRecording := Js.Optdef.return (Js.bool false)
;;

let init_global () =
  global##.bonsaiIncrNodeIntrospectionSupported := Js.bool true;
  global##.bonsaiIncrNodeIntrospectionStartRecording := Js.wrap_callback start_recording;
  global##.bonsaiIncrNodeIntrospectionStopRecording := Js.wrap_callback stop_recording;
  (match is_recording () with
   | true ->
     (* In order to catch incr node creations from the very beginning, the devtool panel
        api will set is_recording to true before any javascript in the page runs.
        Otherwise, we start it at [false]. *)
     ()
   | false ->
     global##.bonsaiIncrNodeIntrospectionIsRecording := Js.Optdef.return (Js.bool false));
  global##.bonsaiIncrNodeIntrospectionPopEvents := Js.wrap_callback pop_events
;;

let run_top_level_side_effects = lazy (init_global ())
