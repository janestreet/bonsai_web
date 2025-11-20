open! Core
open! Async_kernel
open! Import
open Js_of_ocaml
module App_root_registry = Incr_dom.Start_app.Private.App_root_registry
module Debug_logging = Incr_dom.Start_app.Private.Debug_logging
module Focus_stealer = Incr_dom.Start_app.Private.Focus_stealer

let warn_if_quirks_mode = Incr_dom.Start_app.Private.warn_if_quirks_mode

module Timeable_event = struct
  type t =
    | From_driver of Bonsai_driver.Instrumentation.Timeable_event.t
    | Mount_initial_dom
    | Whole_animation_frame_loop
    | Diff_vdom
    | Patch_vdom
    | On_display_handlers
    | Start_of_frame_to_start_of_next_frame
    | End_of_frame_to_start_of_next_frame
end

type 'r unstarted =
  | T :
      { bonsai_driver : 'r Bonsai_driver.t
      ; timer : 'a. Timeable_event.t -> f:(unit -> 'a) -> 'a
      ; instrumentation :
          (Timeable_event.t, 'timer) Bonsai.Private.Instrumentation.Config.t
      ; action_logging : Sexp.t Lazy.t Debug_logging.Action_logging.t
      ; should_debug : unit -> bool
      ; bind_to_element_with_id : string
      }
      -> 'r unstarted

type 'r t =
  | T :
      { bonsai_driver : 'r Bonsai_driver.t
      ; focus_stealer : Focus_stealer.t
      ; timer : 'a. Timeable_event.t -> f:(unit -> 'a) -> 'a
      ; action_logging : Sexp.t Lazy.t Debug_logging.Action_logging.t
      ; should_debug : unit -> bool
      ; instrumentation :
          (Timeable_event.t, 'timer) Bonsai.Private.Instrumentation.Config.t
      ; on_display_for_marking_started : unit -> unit
      ; start_of_frame_to_start_of_next_frame_timer : 'timer option ref
      ; end_of_frame_to_start_of_next_frame_timer : 'timer option ref
      ; get_vdom : 'r -> Vdom.Node.t
      ; on_stop : unit -> unit
      ; prev_vdom : Vdom.Node.t ref
      ; prev_elt : Dom_html.element Js.t ref
      }
      -> 'r t

let create
  (type r)
  ?optimize
  ~instrumentation
  ~time_source
  ~bind_to_element_with_id
  (computation : local_ Bonsai.graph -> r Bonsai.t)
  =
  let { Debug_logging.Flags.should_debug; should_profile }, action_logging =
    Debug_logging.init_app
      ~named_logging_filters:[]
      ~app_id:bind_to_element_with_id
      ~initial_debug:false
  in
  Util.For_bonsai_internal.set_stack_overflow_exception_check ();
  let (instrumentation : (Timeable_event.t, _) Bonsai.Private.Instrumentation.Config.t) =
    instrumentation ~should_debug ~should_profile
  in
  let bonsai_driver =
    Bonsai_driver.create
      ~instrumentation:
        { instrumentation with
          start_timer = (fun evt -> instrumentation.start_timer (From_driver evt))
        }
      ?optimize
      ~time_source
      computation
  in
  let timer event ~f =
    let timer = instrumentation.start_timer event in
    let result = f () in
    instrumentation.stop_timer timer;
    result
  in
  (T
     { bonsai_driver
     ; timer
     ; instrumentation
     ; action_logging
     ; should_debug
     ; bind_to_element_with_id
     }
   : _ unstarted)
;;

let start
  ?(simulate_body_focus_on_root_element = true)
  ~on_display_for_marking_started
  ~get_vdom
  (T
     { bonsai_driver
     ; timer
     ; instrumentation
     ; action_logging
     ; should_debug
     ; bind_to_element_with_id
     } :
    _ unstarted)
  =
  let focus_stealer = Focus_stealer.create ~enabled:simulate_body_focus_on_root_element in
  warn_if_quirks_mode ();
  let vdom, elt =
    timer Mount_initial_dom ~f:(fun () ->
      let vdom = Bonsai_driver.result bonsai_driver |> get_vdom in
      let elt =
        Vdom.Node.For_changing_dom.with_on_mount_at_end (fun () ->
          let elt = Vdom.Node.to_dom vdom in
          let elem = Dom_html.getElementById_exn bind_to_element_with_id in
          let parent = Option.value_exn (Js.Opt.to_option elem##.parentNode) in
          Dom.replaceChild parent elt elem;
          elt)
      in
      vdom, elt)
  in
  let prev_vdom = ref vdom in
  let prev_elt = ref elt in
  Focus_stealer.maybe_refocus_on_blur focus_stealer prev_elt;
  let app_root_handle = App_root_registry.register_app_root prev_elt in
  T
    { bonsai_driver
    ; action_logging
    ; should_debug
    ; timer
    ; instrumentation
    ; get_vdom
    ; focus_stealer
    ; prev_vdom
    ; prev_elt
    ; on_display_for_marking_started
    ; start_of_frame_to_start_of_next_frame_timer = ref None
    ; end_of_frame_to_start_of_next_frame_timer = ref None
    ; on_stop =
        (fun () ->
          Bonsai_driver.Expert.invalidate_observers bonsai_driver;
          App_root_registry.unregister_app_root app_root_handle;
          Debug_logging.cleanup_app ~app_id:bind_to_element_with_id)
    }
;;

let recompute
  (T
    { bonsai_driver
    ; action_logging
    ; should_debug
    ; timer
    ; instrumentation
    ; focus_stealer
    ; get_vdom
    ; on_display_for_marking_started
    ; start_of_frame_to_start_of_next_frame_timer
    ; end_of_frame_to_start_of_next_frame_timer
    ; prev_vdom
    ; prev_elt
    ; _
    })
  =
  Option.iter !start_of_frame_to_start_of_next_frame_timer ~f:instrumentation.stop_timer;
  Option.iter !end_of_frame_to_start_of_next_frame_timer ~f:instrumentation.stop_timer;
  start_of_frame_to_start_of_next_frame_timer
  := Some (instrumentation.start_timer Start_of_frame_to_start_of_next_frame);
  let animation_frame_loop_timer =
    instrumentation.start_timer Whole_animation_frame_loop
  in
  Bonsai_driver.flush
    ~log_before_action_application:(fun ~action_sexp ->
      Debug_logging.maybe_log_action action_logging ~sexp_of_action:Lazy.force action_sexp)
    ~log_on_skipped_stabilization:(fun ~action_sexp:(_ : Sexp.t Lazy.t) ->
      Ui_metrics.Counters.observe Incr_skipped_stabilizations;
      if should_debug ()
      then Console.console##debug (Js.string "action applied without stabilizing"))
    bonsai_driver;
  let vdom =
    Bonsai_driver.result bonsai_driver
    |> get_vdom
    |> Focus_stealer.maybe_wrap_root_element focus_stealer
  in
  let patch =
    timer Diff_vdom ~f:(fun () ->
      Vdom.Node.Patch.create ~previous:!prev_vdom ~current:vdom)
  in
  timer Patch_vdom ~f:(fun () ->
    Vdom.Node.For_changing_dom.with_on_mount_at_end (fun () ->
      let elt = Vdom.Node.Patch.apply patch !prev_elt in
      (* [!prev_elt] should almost always refer to the current app root element, and be
         connected. The exceptions are if something external has mutated DOM, which we
         can't control, and if a patch changes the tag of the root element, forcing it to
         be replaced rather than updated.

         To protect against the latter case, we update the [prev_elt] ref immediately
         after patching, before any [on_mount] handlers or lifecycles run. *)
      prev_vdom := vdom;
      prev_elt := elt));
  timer On_display_handlers ~f:(fun () ->
    on_display_for_marking_started ();
    Bonsai_driver.trigger_lifecycles bonsai_driver);
  For_introspection.Profiling.log_all_computation_watcher_nodes_in_javascript_console ();
  if should_debug () then Console.console##debug (Js.string "-------");
  (* Restoring focus from the [<body />] to the app root should mostly be handled by the
     [blur] listener above, but we additionally run this check every frame because:

     - We want the root element to start out focused, so perform an initial update/render,
       then immediately focus the root (unless a non-body element already has focus).
     - [blur] doesn't run if the currently focused element is removed from the DOM, so we
       might need to possibly focus-steal after every frame.

     We still want [refocus_on_blur], so that we can respond immediately to most blurs
     without waiting ~16ms for the next frame. *)
  Focus_stealer.maybe_refocus_root_element focus_stealer !prev_elt;
  instrumentation.stop_timer animation_frame_loop_timer;
  start_of_frame_to_start_of_next_frame_timer
  := Some (instrumentation.start_timer End_of_frame_to_start_of_next_frame)
;;

let result (T { bonsai_driver; _ }) = Bonsai_driver.result bonsai_driver

let schedule_event (T { bonsai_driver; _ }) effect =
  Bonsai_driver.schedule_event bonsai_driver effect
;;

let destroy (T { on_stop; _ }) = on_stop ()

module Expert = struct
  let time_source (T { bonsai_driver; _ }) =
    Bonsai_driver.Expert.time_source bonsai_driver
  ;;
end
