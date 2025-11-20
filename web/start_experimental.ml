open! Core
open! Async_kernel
open! Import
open Js_of_ocaml

let () = Lazy.force For_introspection.run_top_level_side_effects

module type Result_spec_s = sig
  type t
  type extra
  type incoming

  val view : t -> Vdom.Node.t
  val extra : t -> extra
  val incoming : t -> incoming -> unit Vdom.Effect.t
end

module App_result = struct
  type ('extra, 'incoming) t =
    { view : Vdom.Node.t
    ; extra : 'extra
    ; inject_incoming : 'incoming -> unit Vdom.Effect.t
    }
  [@@deriving fields ~iterators:create]

  let of_result_spec
    (type result extra incoming)
    (module Result : Result_spec_s
      with type t = result
       and type extra = extra
       and type incoming = incoming)
    (r : Result.t)
    =
    { view = Result.view r; extra = Result.extra r; inject_incoming = Result.incoming r }
  ;;
end

module Handle = struct
  module Injector = struct
    type 'a t =
      | Before_app_start of 'a Queue.t
      | Inject of ('a -> unit Vdom.Effect.t)
  end

  type ('extra, 'incoming) t =
    { mutable injector : 'incoming Injector.t
    ; stop : unit Ivar.t
    ; started : unit Ivar.t
    ; extra : ('extra -> unit) Bus.Read_write.t
    ; last_extra : 'extra Moption.t
    }

  let create () =
    let extra =
      Bus.create_exn
        Arity1
        ~on_subscription_after_first_write:Allow_and_send_last_value
        ~on_callback_raise:(fun error -> eprint_s [%sexp (error : Error.t)])
    in
    let last_extra = Moption.create () in
    Bus.subscribe_permanently_exn extra ~f:(fun extra ->
      Moption.set_some last_extra extra);
    { injector = Before_app_start (Queue.create ())
    ; stop = Ivar.create ()
    ; started = Ivar.create ()
    ; extra
    ; last_extra
    }
  ;;

  let stop t = Ivar.fill_if_empty t.stop ()
  let started t = Ivar.read t.started

  let schedule t a =
    match t.injector with
    | Inject f -> f a |> Vdom.Effect.Expert.handle_non_dom_event_exn
    | Before_app_start queue -> Queue.enqueue queue a
  ;;

  let set_started t = Ivar.fill_if_empty t.started ()

  let set_inject t inject =
    let prev = t.injector in
    t.injector <- Inject inject;
    match prev with
    | Inject _ -> ()
    | Before_app_start queue -> Queue.iter queue ~f:(schedule t)
  ;;

  let extra t = Bus.read_only t.extra
  let last_extra t = Moption.get t.last_extra
end

module Result_spec = Start_via_incr_dom.Result_spec

let default_custom_connector _connector =
  raise_s
    [%message
      "The Bonsai app used a custom connector, but none was provided when the app was \
       started. To fix this, use the [~custom_connector] argument when calling \
       [Bonsai_web.Start.start]"]
;;

let computation_with_rpc_and_result_spec computation ~custom_connector ~result_spec =
  let computation =
    Rpc_effect.Private.with_connector
      (function
        | Custom (Rpc_effect.Where_to_connect.Url_connector.T { on_conn_failure; url }) ->
          Rpc_effect.Private.url_connector ~on_conn_failure url
        | Custom
            (Rpc_effect.Where_to_connect.Self_connector.T
              { Rpc_effect.Where_to_connect.Self.on_conn_failure }) ->
          Rpc_effect.Private.self_connector ~on_conn_failure ()
        | Custom custom -> custom_connector custom)
      computation
  in
  fun (local_ graph) ->
    Bonsai.arr1 graph (computation graph) ~f:(App_result.of_result_spec result_spec)
;;

module Timer = struct
  module Pm = Incr_dom.Start_app.Private.Performance_measure

  type t =
    | Profiling_timing of string * Javascript_profiling.Timer.t
    | Performance_measure of Incr_dom.Start_app.Private.Performance_measure.timer

  let start ~should_debug ~should_profile event =
    let pm_start evt =
      Performance_measure
        (Pm.timer_start ~debug:(should_debug ()) ~profile:(should_profile ()) evt)
    in
    match event with
    | Driver.Timeable_event.From_driver (Profiling_entry s) ->
      Profiling_timing (s, Javascript_profiling.Timer.start ())
    | From_driver Graph_application -> pm_start Bonsai_graph_application
    | From_driver Preprocess -> pm_start Bonsai_preprocess
    | From_driver Gather -> pm_start Bonsai_gather
    | From_driver Run_eval_fun -> pm_start Incr_app_creation
    | From_driver First_stabilization -> pm_start First_stabilization
    | Mount_initial_dom -> pm_start Mount_initial_dom
    | Whole_animation_frame_loop -> pm_start Whole_animation_frame_loop
    | From_driver Stabilize_for_clock -> pm_start Stabilize_for_clock
    | From_driver Apply_actions -> pm_start Apply_actions
    | From_driver Stabilize_for_action -> pm_start Stabilize_for_action
    | From_driver Stabilize_after_all_apply_actions ->
      pm_start Stabilize_after_all_apply_actions
    | Diff_vdom -> pm_start Diff_vdom
    | Patch_vdom -> pm_start Patch_vdom
    | On_display_handlers -> pm_start On_display_handlers
    | Start_of_frame_to_start_of_next_frame ->
      pm_start Start_of_frame_to_start_of_next_frame
    | End_of_frame_to_start_of_next_frame -> pm_start End_of_frame_to_start_of_next_frame
  ;;

  let stop = function
    | Profiling_timing (s, timer) ->
      let measurement = Javascript_profiling.Timer.stop timer in
      Javascript_profiling.measure s measurement
    | Performance_measure timer -> Pm.timer_stop timer
  ;;
end

let start_and_get_handle
  result_spec
  ?optimize
  ?(custom_connector = default_custom_connector)
  ?simulate_body_focus_on_root_element
  ?(time_source = Bonsai.Time_source.create ~start:(Time_ns.now ()))
  ~bind_to_element_with_id
  computation
  =
  Async_js.init ();
  let pre_startup = () in
  let force_profile = false in
  let bonsai_handle =
    let handle = Handle.create () in
    let computation (local_ graph) =
      let result =
        computation_with_rpc_and_result_spec
          computation
          ~custom_connector
          ~result_spec
          graph
      in
      let%arr.Bonsai result in
      Bus.write handle.extra (App_result.extra result);
      Handle.set_inject handle (App_result.inject_incoming result);
      result
    in
    let web_driver_unstarted =
      Driver.create
        ?optimize
        ~instrumentation:(fun ~should_debug ~should_profile ->
          { For_introspection.Profiling.default_instrumentation_for_incr_dom_start_app with
            start_timer =
              Timer.start ~should_debug ~should_profile:(fun () ->
                force_profile || should_profile ())
          ; stop_timer = Timer.stop
          })
        ~time_source
        ~bind_to_element_with_id
        computation
    in
    let stop = Ivar.read handle.stop in
    don't_wait_for
      (let%bind () = Async_js.document_loaded () in
       let web_driver =
         Driver.start
           ?simulate_body_focus_on_root_element
           ~on_display_for_marking_started:(fun () -> Handle.set_started handle)
           ~get_vdom:App_result.view
           web_driver_unstarted
       in
       upon stop (fun () -> Driver.destroy web_driver);
       Incr_dom.Start_app.Private.Frame_loop.start_looping
         ~is_stopped:(fun () -> Deferred.is_determined stop)
         ~perform_update:(fun () ->
           (* The clock is set only once per call to perform_update, so that all actions
              that occur before each display update occur "at the same time." *)
           let now =
             let date = new%js Js.date_now in
             Time_ns.Span.of_ms (Js.to_float date##getTime) |> Time_ns.of_span_since_epoch
           in
           Incr.Clock.advance_clock Incr.clock ~to_:now;
           let time_source = Driver.Expert.time_source web_driver in
           Bonsai.Time_source.advance_clock time_source ~to_:now;
           Driver.recompute web_driver);
       Deferred.never ());
    let bonsai_bug_moved_message =
      {| Bonsai-bug has moved to the bonsai chrome extension. To use bonsai bug:
           1. Install the bonsai chrome extension if you haven't already! https://chrome-extensions/
           2. Open the dev-tool panel.
           3. A new pane titled "Bonsai Developer Tools" should appear. Click on it!
           4. Click on "Bonsai Profiling"
           5. Start profiling!
      |}
    in
    let start_bonsai_debugger () = print_endline bonsai_bug_moved_message in
    let stop_bonsai_debugger () = print_endline bonsai_bug_moved_message in
    Js.Unsafe.global##.startBonsaiDebugger := Js.Unsafe.callback start_bonsai_debugger;
    Js.Unsafe.global##.stopBonsaiDebugger := Js.Unsafe.callback stop_bonsai_debugger;
    handle
  in
  (* Note: duplicated between [start_experimental] and [start_via_incr_dom]. *)
  Deferred.upon (Handle.started bonsai_handle) (fun () ->
    Js.Unsafe.global##.bonsaiHasStarted := Js.bool true);
  let () =
    Deferred.upon (Handle.started bonsai_handle) (fun () ->
      (* NOTE: We set [bonsaiHasStarted] as a witness that bonsai has started successfully
         for use in browser testing. *)
      Js.Unsafe.global##.bonsaiHasStarted := Js.bool true);
    ignore (pre_startup : unit)
  in
  bonsai_handle
;;

let start
  ?custom_connector
  ?(bind_to_element_with_id = "app")
  ?simulate_body_focus_on_root_element
  ?time_source
  ?optimize
  component
  =
  let (_ : _ Handle.t) =
    start_and_get_handle
      Result_spec.just_the_view
      ~bind_to_element_with_id
      ?simulate_body_focus_on_root_element
      ?custom_connector
      ?time_source
      ?optimize
      component
  in
  ()
;;
