open! Core
open Js_of_ocaml
open Bonsai_introspection_protocol
module Or_no_sexp_of_provided = Rpc_effect_protocol.Or_no_sexp_of_provided
module Effect = Ui_effect

let get_id = Effect.of_thunk Rpc_effect_protocol.Rpc_id.create
let queue : Rpc_effect_protocol.Event.t Queue.t = Queue.create ()

class type global = object
  (* [ rpcEffectInstrospectionSupported ] is used to distinguish between non-bonsai apps
     or bonsai apps that have not yet picked up the introspection changes. This is read by
     the devtool panel to provide a nice error message when attempting to inspect
     wikipedia/a non-bonsai app/an app that is on an outdated version of bonsai. *)
  method rpcEffectIntrospectionSupported : Js.number Js.t Js.prop
  method rpcEffectPopEvents : (unit -> Js.js_string Js.t) Js.callback Js.prop
end

let global : global Js.t = Js.Unsafe.global

let pop_events' () =
  let events = Queue.to_list queue in
  Queue.clear queue;
  events
;;

let pop_events () =
  pop_events' ()
  |> List.map ~f:Rpc_effect_protocol.Event.Stable.of_latest
  |> [%sexp_of: Rpc_effect_protocol.Event.Stable.t list]
  |> Sexp.to_string_mach
  |> Js.string
;;

let is_recording_var =
  Utils.Session_storage_var.create
    (module Bool)
    ~unique_id:"rpcEffectIsRecording"
    ~default:false
;;

let is_recording () = Utils.Session_storage_var.get is_recording_var
let should_record_effect = Effect.of_thunk is_recording

let maybe_record_event (event : Rpc_effect_protocol.Event.t lazy_t) =
  match is_recording () with
  | false -> ()
  | true ->
    let event = force event in
    Queue.enqueue queue event
;;

let init_global () =
  global##.rpcEffectIntrospectionSupported := Js.number_of_float 2.;
  global##.rpcEffectPopEvents := Js.wrap_callback pop_events
;;

let run_top_level_side_effects = lazy (init_global ())

let send_and_track_rpc
  ~rpc_kind
  ~get_current_time
  ~sexp_of_query
  ~sexp_of_response
  ~path
  ~send_rpc:actually_send_rpc
  ~query
  ~get_response
  ~response_to_event
  ~here
  =
  let open Effect.Let_syntax in
  let%bind.Effect id = get_id in
  let%bind start_time = get_current_time in
  let start_event =
    lazy
      (let query =
         match sexp_of_query with
         | None -> Or_no_sexp_of_provided.No_sexp_of_provided
         | Some sexp_of_query ->
           Or_no_sexp_of_provided.Sexp_of_provided (sexp_of_query query)
       in
       Rpc_effect_protocol.Event.V1.Started
         { id; rpc_kind; start_time; query; path; here = Some here })
  in
  maybe_record_event start_event;
  let%bind response = actually_send_rpc (query, Some id) in
  let%bind end_time = get_current_time in
  let duration = Time_ns.diff end_time start_time in
  let () =
    maybe_record_event (response_to_event response ~id ~duration ~sexp_of_response)
  in
  Effect.return (get_response response)
;;

let send_and_track_rpc_from_poller
  ~rpc_kind
  ~get_current_time
  ~sexp_of_query
  ~sexp_of_response
  ~path
  ~send_rpc:actually_send_rpc
  ~get_response
  ~query
  ~here
  =
  send_and_track_rpc
    ~here
    ~rpc_kind
    ~get_current_time
    ~sexp_of_query
    ~sexp_of_response
    ~path
    ~send_rpc:actually_send_rpc
    ~get_response:(function
      | Bonsai.Effect_throttling.Poll_result.Aborted ->
        Bonsai.Effect_throttling.Poll_result.Aborted
      | Finished x -> Finished (Or_error.map ~f:get_response x))
    ~query
    ~response_to_event:(fun response ~id ~duration ~sexp_of_response ->
      match (response : 'response Or_error.t Bonsai.Effect_throttling.Poll_result.t) with
      | Aborted -> lazy (Rpc_effect_protocol.Event.V1.Aborted { id; duration })
      | Finished response ->
        lazy
          (let response =
             Or_error.map response ~f:(fun response ->
               match sexp_of_response with
               | None -> Or_no_sexp_of_provided.No_sexp_of_provided
               | Some sexp_of_response -> Sexp_of_provided (sexp_of_response response))
           in
           Finished { id; duration; response }))
;;

let send_and_track_rpc_from_dispatch
  ~rpc_kind
  ~get_current_time
  ~sexp_of_query
  ~sexp_of_response
  ~path
  ~send_rpc:actually_send_rpc
  ~get_response
  ~query
  ~here
  =
  send_and_track_rpc
    ~here
    ~rpc_kind
    ~get_current_time
    ~sexp_of_query
    ~sexp_of_response
    ~path
    ~send_rpc:actually_send_rpc
    ~query
    ~get_response:(fun x -> Or_error.map ~f:get_response x)
    ~response_to_event:(fun response ~id ~duration ~sexp_of_response ->
      lazy
        (let response =
           Or_error.map response ~f:(fun response ->
             match sexp_of_response with
             | None -> Or_no_sexp_of_provided.No_sexp_of_provided
             | Some sexp_of_response -> Sexp_of_provided (sexp_of_response response))
         in
         Rpc_effect_protocol.Event.V1.Finished { id; duration; response }))
;;

let last_sent_tracing_event_id = ref None
let tracing_event_id_to_rpc_id = Int63.Table.create ()

let handle_tracing_event (event @ local) =
  let open Async_rpc_kernel in
  let%tydi { event; rpc; id = tracing_event_id; payload_bytes } =
    Tracing_event.globalize event
  in
  match event with
  | Sent Query -> last_sent_tracing_event_id := Some tracing_event_id
  | Received (Response _) ->
    (* Get rpc message ID from tracing event ID *)
    (match Hashtbl.find tracing_event_id_to_rpc_id tracing_event_id with
     | Some id ->
       maybe_record_event
         (Lazy.return (Rpc_effect_protocol.Event.V1.Response_size { id; payload_bytes }))
     | None ->
       if payload_bytes = 7
       then
         (* This is the size of abort, forget on server, and cancel current query
            messages. We probably don't care about this extra message too much, so we can
            just ignore it/not print anything. *)
         ()
       else
         print_s
           [%message
             "RPC response not tracked by RPC Effect Inspector"
               (rpc : Rpc.Description.t)
               (payload_bytes : int)])
  | _ -> ()
;;

let trace_connection conn =
  let open Async_rpc_kernel in
  Bus.subscribe_permanently_exn
    (Async_rpc_kernel_private.Connection.tracing_events conn)
    ~f:(fun event ->
      (match is_recording () with
       | true -> handle_tracing_event event
       | false -> ());
      (* We remove the entry upon receiving a finished response even if we aren't
         recording to prevent memory leaks. *)
      match event.event with
      | Received (Response Partial_response) -> ()
      | Received (Response _) -> Hashtbl.remove tracing_event_id_to_rpc_id event.id
      | _ -> ())
;;

let just_sent_query_with_id id =
  match id, is_recording (), !last_sent_tracing_event_id with
  | None, _, _ | _, false, _ -> ()
  | Some _, true, None -> print_endline "No tracing event recorded when sending query."
  | Some id, true, Some tracing_event_id ->
    (match Hashtbl.add tracing_event_id_to_rpc_id ~key:tracing_event_id ~data:id with
     | `Duplicate -> print_endline "Tracing event ID already in use."
     | `Ok -> ())
;;

let rpc_name ~rpc_kind =
  match rpc_kind with
  | Rpc_effect_protocol.Rpc_kind.Normal { name; _ }
  | Babel { descriptions = { name; _ } :: _; _ }
  | Streamable { name; _ }
  | Polling_state_rpc { name; _ }
  | Babel_polling_state_rpc { descriptions = { name; _ } :: _; _ } -> name
;;

let time_rpc_effect
  ~(rpc_kind : Rpc_effect_protocol.Rpc_kind.t)
  (effect : 'query -> 'response Effect.t)
  =
  let name = rpc_name ~rpc_kind in
  fun query ->
    let%bind.Effect timer = Effect.of_sync_fun Javascript_profiling.Timer.start () in
    let%map.Effect response = effect query in
    let () =
      let measurement = Javascript_profiling.Timer.stop timer in
      Javascript_profiling.measure ~track:"RPC Effect" ~color:Tertiary name measurement
    in
    response
;;

module For_testing = struct
  let get_is_recording = is_recording
  let start_recording () = Utils.Session_storage_var.set is_recording_var true
  let stop_recording () = Utils.Session_storage_var.set is_recording_var false
  let pop_events = pop_events
  let pop_events' = pop_events'
end
