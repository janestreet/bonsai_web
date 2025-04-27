open! Core
open! Async_kernel
open! Import

module Timeable_event : sig
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

(** A [Driver.t] factors out the logic for running a Bonsai web app, in a browser-like
    context; i.e. it assumes that DOM APIs are available.

    It is built on top of [Bonsai_driver.t], and is intended to power both runtime Bonsai
    apps, and JSDom/Browser testing + benchmarking. *)
type 'r t

type 'r unstarted

(** Builds a new driver for a bonsai component. This will instantiate the Bonsai
    computation graph, and perform an initial stabilization, but will not yet touch the
    DOM. *)
val create
  :  ?optimize:bool
  -> instrumentation:
       (should_debug:(unit -> bool)
        -> should_profile:(unit -> bool)
        -> (Timeable_event.t, _) Bonsai.Private.Instrumentation.Config.t)
  -> time_source:Bonsai.Time_source.t
  -> bind_to_element_with_id:string
  -> (local_ Bonsai.graph -> 'r Bonsai.t)
  -> 'r unstarted

(** Mounts the initial DOM from the first stabilization.

    [start] should be called only once [Async_js.document_loaded] for real Bonsai apps. *)
val start
  :  ?simulate_body_focus_on_root_element:bool
  -> on_display_for_marking_started:(unit -> unit)
  -> get_vdom:('r -> Vdom.Node.t)
  -> 'r unstarted
  -> 'r t

(** Runs a "frame" of the Bonsai runtime. This is mostly:
    1. [Bonsai_driver.flush]
    2. diffing + patching the DOM
    3. [Bonsai_driver.trigger_lifecycles] *)
val recompute : 'a t -> unit

(** retreives the computed result of the bonsai application *)
val result : 'a t -> 'a

(** Schedules an event *)
val schedule_event : _ t -> unit Effect.t -> unit

(** [destroy] shuts down the incremental computation. It will not remove the app's DOM. *)
val destroy : 'a t -> unit

module Expert : sig
  val time_source : _ t -> Bonsai.Time_source.t
end
