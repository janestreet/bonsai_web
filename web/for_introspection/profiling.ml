open! Core
open Js_of_ocaml
open Bonsai_introspection_protocol

(* The below variables are read by the devtool panel as global variables. *)
class type global = object
  (* [ bonsaiBugInstrospectionSupported ] is used to distinguish between non-bonsai apps
     or bonsai apps that have not yet picked up the introspection changes. This is read by
     the devtool panel to provide a nice error message when attempting to inspect
     wikipedia/a non-bonsai app/an app that is on an outdated version of bonsai. *)
  method bonsaiBugIntrospectionSupported : Js.number Js.t Js.prop
  method bonsaiBugPopEvents : (unit -> Js.js_string Js.t) Js.callback Js.prop
  method bonsaiBugLatestGraphInfo : (unit -> Js.js_string Js.t) Js.callback Js.prop
  method bonsaiBugStartComputationWatcher : (unit -> unit) Js.callback Js.prop
  method bonsaiBugStopComputationWatcher : (unit -> unit) Js.callback Js.prop
end

let global : global Js.t = Js.Unsafe.global

let is_profiling_var =
  Utils.Session_storage_var.create
    (module Bonsai.Private.Instrumentation.Profiling)
    ~unique_id:"bonsaiBugIsProfiling"
    ~default:Not_profiling
;;

let is_profiling = Utils.Session_storage_var.to_incr is_profiling_var

let computation_watcher_status_var =
  Ui_incr.Var.create Bonsai.Private.Instrumentation.Watching.Not_watching
;;

let computation_watcher_status = Ui_incr.Var.watch computation_watcher_status_var

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
    f { Bonsai_bug_protocol.Entry.label; entry_type; start_time; duration })
;;

let performance_observer_ref = ref None
let queue : Bonsai_bug_protocol.Worker_message.t Queue.t = Queue.create ()
let computation_watcher_queue = Queue.create ()
let latest_graph_info = ref (Some Bonsai.Private.Node_path.Map.empty)
let graph_info_changed = ref true

let enqueue_performance_measure (performance_measure : Bonsai_bug_protocol.Entry.t) =
  Queue.enqueue queue (Message (Performance_measure performance_measure))
;;

let set_latest_graph_info (graph_info : Bonsai.Private.Graph_info.Stable.V3.t) : unit =
  (* We only use the info field, so we don't need to serialize the other parts. *)
  latest_graph_info := Some graph_info.info;
  graph_info_changed := true
;;

let bonsai_protocol_message_to_string
  : Bonsai_bug_protocol.Versioned_message.t -> Js.js_string Js.t
  =
  fun message ->
  Js.bytestring
    (Bin_prot.Writer.to_string Bonsai_bug_protocol.Versioned_message.bin_writer_t message)
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
  bonsai_protocol_message_to_string (V5 messages)
;;

let latest_graph_info () =
  let messages =
    match !latest_graph_info with
    | None -> []
    | Some graph_info ->
      [ Bonsai_bug_protocol.Worker_message.Message (Graph_info graph_info) ]
  in
  bonsai_protocol_message_to_string (V5 messages)
;;

let start_profiling () =
  print_endline "Starting the Bonsai Bug profiler.";
  let performance_observer =
    let f new_entries observer =
      observer##takeRecords
      |> (ignore : PerformanceObserver.performanceEntry Js.t Js.js_array Js.t -> unit);
      iter_entries new_entries ~f:(fun entry -> enqueue_performance_measure entry)
    in
    PerformanceObserver.observe ~entry_types:[ "measure" ] ~f
  in
  performance_observer_ref := Some performance_observer
;;

let stop_profiling () =
  Option.iter !performance_observer_ref ~f:(fun performance_observer ->
    performance_observer##disconnect);
  Javascript_profiling.clear_marks ();
  Javascript_profiling.clear_measures ();
  Ui_incr.stabilize ()
;;

let update_profiling () =
  match Utils.Session_storage_var.get is_profiling_var with
  | Profiling -> start_profiling ()
  | Not_profiling -> stop_profiling ()
;;

let start_computation_watcher () =
  Ui_incr.Var.set computation_watcher_status_var Watching;
  Ui_incr.Incr.stabilize ();
  (* Clear the watcher queue after the first stabilize so that the initialization of the
     incrementals don't log to console *)
  Queue.clear computation_watcher_queue
;;

let stop_computation_watcher () =
  Ui_incr.Var.set computation_watcher_status_var Not_watching;
  Ui_incr.Incr.stabilize ()
;;

let listen_for_profiling_changes () =
  Utils.Session_storage_var.add_on_set_listener is_profiling_var update_profiling
;;

let log_all_computation_watcher_nodes_in_javascript_console () =
  Bonsai.Private.Computation_watcher.Output_queue.process_queue
    ~f:(fun node ->
      let open Js_of_ocaml in
      let stringified_node = Bonsai.Private.Computation_watcher.Node.to_string node in
      Js.Unsafe.global##.console##log
        (Js.Optdef.return (Js.string "%O"))
        (Js.Optdef.return (Js.string stringified_node)))
    computation_watcher_queue
;;

let init_global () =
  listen_for_profiling_changes ();
  global##.bonsaiBugIntrospectionSupported := Js.number_of_float 2.;
  global##.bonsaiBugPopEvents := Js.wrap_callback pop_events;
  global##.bonsaiBugLatestGraphInfo := Js.wrap_callback latest_graph_info;
  global##.bonsaiBugStartComputationWatcher := Js.wrap_callback start_computation_watcher;
  global##.bonsaiBugStopComputationWatcher := Js.wrap_callback stop_computation_watcher;
  (* We commence the bonsai profiler if we were told to debug upon app startup. *)
  match Utils.Session_storage_var.get is_profiling_var with
  | Profiling -> start_profiling ()
  | Not_profiling -> ()
;;

let run_top_level_side_effects = lazy (init_global ())

type timer = string * Javascript_profiling.Timer.t

let default_instrumentation_for_incr_dom_start_app =
  { Bonsai.Private.Instrumentation.Config.instrument_for_profiling = is_profiling
  ; instrument_for_computation_watcher = computation_watcher_status
  ; set_latest_graph_info
  ; computation_watcher_queue
  ; start_timer = (fun s -> s, Javascript_profiling.Timer.start ())
  ; stop_timer =
      (fun (s, timer) ->
        let measurement = Javascript_profiling.Timer.stop timer in
        Javascript_profiling.measure s measurement)
  }
;;

module For_testing = struct
  let start_profiling () = Utils.Session_storage_var.set is_profiling_var Profiling
  let stop_profiling () = Utils.Session_storage_var.set is_profiling_var Not_profiling
  let start_computation_watcher = start_computation_watcher
  let stop_computation_watcher = stop_computation_watcher
end
