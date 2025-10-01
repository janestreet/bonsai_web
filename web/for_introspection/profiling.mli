open! Core

(** [For_profiling] provides the needed hooks for tooling to "profile" bonsai apps. This
    is used by the bonsai debugger devtool panel to profile web apps. *)

(** [run_top_level_side_effects] should run at the very beginning/top level of the app. *)
val run_top_level_side_effects : unit Lazy.t

(** [is_profiling] reflects whether performance profiling is on or off as indicated by
    session storage. This is set by the Bonsai chrome devtools extension. *)
val is_profiling : Bonsai.Private.Instrumentation.Profiling.t Ui_incr.t

(** [set_latest_graph_info] should get called anytime that the graph info changes. *)
val set_latest_graph_info : Bonsai.Private.Graph_info.Stable.V3.t -> unit

val computation_watcher_queue : Bonsai.Private.Computation_watcher.Node.t Queue.t

(** [log_all_computation_watcher_nodes_in_javascript_console] only works in web. Using
    print_endline in Js_of_ocaml seems to separate the input string on newline characters
    and then log them individually, which is not the behavior we want.

    The default behavior of print_endline/console.log will turn `lib/test/file` into a
    url-ish string that looks like `lib<a href="/test/file" />`. It will also shorten
    really long URLs into shorter ones somewhat arbitrarily (ex:
    `lib/super/long/name/or/something` -> `lib/sup......./something`)

    This function prevents the above behavior *)
val log_all_computation_watcher_nodes_in_javascript_console : unit -> unit

type timer

val default_instrumentation_for_incr_dom_start_app
  : (string, timer) Bonsai.Private.Instrumentation.Config.t

module For_testing : sig
  val start_profiling : unit -> unit
  val stop_profiling : unit -> unit
  val start_computation_watcher : unit -> unit
  val stop_computation_watcher : unit -> unit
end
