open! Core
open Js_of_ocaml

(* The below variables are read by the devtool panel as global variables. *)
class type global = object
  (* [ bonsaiBugInstrospectionSupported ] is used to distinguish between
     non-bonsai apps or bonsai apps that have not yet picked up the introspection
     changes. This is read by the devtool panel to provide a nice error message
     when attempting to inspect wikipedia/a non-bonsai app/an app that is on an
     outdated version of bonsai. *)
  method bonsaiBugIntrospectionSupported : bool Js.t Js.prop
  method bonsaiBugShouldRecordFromBeginning : bool Js.t Js.optdef Js.prop
  method bonsaiBugIsProfiling : (unit -> bool Js.t) Js.callback Js.prop
  method bonsaiBugStartProfiling : (unit -> unit) Js.callback Js.prop
  method bonsaiBugStopProfiling : (unit -> unit) Js.callback Js.prop
  method bonsaiBugPopEvents : (unit -> Js.js_string Js.t) Js.callback Js.prop
  method bonsaiBugLatestGraphInfo : (unit -> Js.js_string Js.t) Js.callback Js.prop
end

let global : global Js.t = Js.Unsafe.global

type debugging_state =
  | Not_debugging
  | Debugging

let should_record_from_beginning () =
  match Js.Optdef.to_option global##.bonsaiBugShouldRecordFromBeginning with
  | None -> false
  | Some x -> Js.to_bool x
;;

let is_profiling_var =
  Ui_incr.Var.create
    (match should_record_from_beginning () with
     | true -> Debugging
     | false -> Not_debugging)
;;

let is_profiling = Ui_incr.Var.watch is_profiling_var

let iter_entries performance_observer_entry_list ~f =
  performance_observer_entry_list##getEntries
  |> Js.to_array
  |> Array.iter ~f:(fun entry ->
    let label =
      let label = entry##.name |> Js.to_string in
      match Bonsai.Private.Instrumentation.extract_node_path_from_entry_label label with
      | None -> `Other label
      | Some node_id -> `Bonsai node_id
    in
    let entry_type = entry##.entryType |> Js.to_bytestring in
    let start_time = Js.float_of_number entry##.startTime in
    let duration = Js.float_of_number entry##.duration in
    f { Bonsai_protocol.Entry.label; entry_type; start_time; duration })
;;

let performance_observer_ref = ref None
let queue : Bonsai_protocol.Worker_message.t Queue.t = Queue.create ()
let latest_graph_info = ref (Some Bonsai.Private.Graph_info.empty)
let graph_info_changed = ref true

let enqueue_performance_measure (performance_measure : Bonsai_protocol.Entry.t) =
  Queue.enqueue queue (Message (Performance_measure performance_measure))
;;

let set_latest_graph_info (graph_info : Bonsai.Private.Graph_info.Stable.V3.t) : unit =
  latest_graph_info := Some graph_info;
  graph_info_changed := true
;;

let bonsai_protocol_message_to_string
  : Bonsai_protocol.Versioned_message.t -> Js.js_string Js.t
  =
  fun message ->
  Js.bytestring
    (Bin_prot.Writer.to_string Bonsai_protocol.Versioned_message.bin_writer_t message)
;;

let pop_events () =
  let messages = Queue.to_list queue in
  Queue.clear queue;
  let messages =
    if !graph_info_changed
    then (
      graph_info_changed := false;
      match !latest_graph_info with
      | None -> messages
      | Some graph_info -> Message (Graph_info graph_info) :: messages)
    else messages
  in
  Javascript_profiling.clear_marks ();
  Javascript_profiling.clear_measures ();
  bonsai_protocol_message_to_string (V4 messages)
;;

let latest_graph_info () =
  let messages =
    match !latest_graph_info with
    | None -> []
    | Some graph_info ->
      [ Bonsai_protocol.Worker_message.Message (Graph_info graph_info) ]
  in
  bonsai_protocol_message_to_string (V4 messages)
;;

let commence_debugger () =
  Ui_incr.Var.set is_profiling_var Debugging;
  let performance_observer =
    let f new_entries observer =
      observer##takeRecords
      |> (ignore : PerformanceObserver.performanceEntry Js.t Js.js_array Js.t -> unit);
      iter_entries new_entries ~f:(fun entry -> enqueue_performance_measure entry)
    in
    PerformanceObserver.observe ~entry_types:[ "measure" ] ~f
  in
  performance_observer_ref := Some performance_observer;
  Ui_incr.stabilize ()
;;

let start_profiling () =
  match Ui_incr.Var.value is_profiling_var with
  | Debugging -> print_endline "Already debugging."
  | Not_debugging ->
    print_endline "Starting the debugger.";
    commence_debugger ()
;;

let stop_profiling () =
  Ui_incr.Var.set is_profiling_var Not_debugging;
  Option.iter !performance_observer_ref ~f:(fun performance_observer ->
    performance_observer##disconnect);
  Javascript_profiling.clear_marks ();
  Javascript_profiling.clear_measures ();
  Ui_incr.stabilize ()
;;

let init_global =
  let is_profiling () =
    match Ui_incr.Var.value is_profiling_var with
    | Not_debugging -> Js.bool false
    | Debugging -> Js.bool true
  in
  fun () ->
    global##.bonsaiBugIntrospectionSupported := Js.bool true;
    global##.bonsaiBugIsProfiling := Js.wrap_callback is_profiling;
    global##.bonsaiBugStartProfiling := Js.wrap_callback start_profiling;
    global##.bonsaiBugStopProfiling := Js.wrap_callback stop_profiling;
    global##.bonsaiBugPopEvents := Js.wrap_callback pop_events;
    global##.bonsaiBugLatestGraphInfo := Js.wrap_callback latest_graph_info;
    (* We commence the bonsai profiler if we were told to debug upon app startup. *)
    match Ui_incr.Var.value is_profiling_var with
    | Debugging -> commence_debugger ()
    | Not_debugging -> ()
;;

let run_top_level_side_effects () = init_global ()
