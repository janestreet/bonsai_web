open! Core

(** [For_profiling] provides the needed hooks for tooling to "profile" bonsai apps. This
    is used by the bonsai debugger devtool panel to profile web apps. *)

(** [run_top_level_side_effects] should run at the very beginning/top level of the app. *)
val run_top_level_side_effects : unit -> unit

type debugging_state =
  | Not_debugging
  | Debugging

(** [is_profiling] is the "source-of-truth" of whether performance is on or off.
    It can be changed with [start_recording] and [stop_recording]. *)
val is_profiling : debugging_state Ui_incr.t

(** [set_latest_graph_info] should get called anytime that the graph info changes. *)
val set_latest_graph_info : Bonsai.Private.Graph_info.Stable.V3.t -> unit
