(** [Start] handles the entire lifecycle of web-based Bonsai application.

    Put another way, [Start] is like {!Core.Command} for Bonsai_web apps. *)

open! Core
open! Async_kernel
open! Import

(** [start] takes a value of type ['result Bonsai_proc.Computation.t] and runs it.

    [bind_to_element_with_id] should be the HTML id of the element in the document that
    Bonsai will take control of. For most apps, you'll have html that looks like this:
    [<html><body><div id="app"></div></body></html>], so the value passed to
    [bind_to_element_with_id] should be the string ["app"]. The default value of this
    parameter is "app".

    [custom_connector] is for expessing custom logic for connecting to RPC endpoints. Read
    more about it in rpc_effect.mli.

    [time_source] should only be passed in tests. *)
val start
  :  ?custom_connector:(Rpc_effect.Where_to_connect.Custom.t -> Rpc_effect.Connector.t)
  -> ?bind_to_element_with_id:string
  -> ?simulate_body_focus_on_root_element:bool
  -> ?time_source:Bonsai.Time_source.t
  -> ?optimize:bool
  -> (local_ Bonsai.graph -> Vdom.Node.t Bonsai.t)
  -> unit

(** {2 Starting with a Handle} *)

module Handle : sig
  (** When a Bonsai app is started, a [Handle.t] is returned to the user. *)
  type ('extra, 'incoming) t

  (** [stop] ends the incremental computation performed by the app, and prevents the
      application from modifying the page. *)
  val stop : _ t -> unit

  (** The [Deferred.t] returned by [started] completes once the application has written to
      the page for the first time. *)
  val started : _ t -> unit Deferred.t

  (** If the application provides a way to inject actions (see [Result_spec.S.incoming]),
      then you can schedule those actions with [schedule] *)
  val schedule : (_, 'incoming) t -> 'incoming -> unit

  (** If the application provides some "extra data" that is computed alongside the view of
      the application, (see [Result_spec.S.extra]), then you can subscribe to those values
      using the bus returned by [extra]

      A value is placed into the Bus on every frame regardless of if it changed or not. *)
  val extra : ('extra, _) t -> ('extra -> unit) Bus.Read_only.t

  (** Like [extra], but only fetches the last ['extra] produced by the computation. If the
      [Deferred.t] returned by [started] has completed, then the [option] returned by
      [last_extra] will always be [Some]. *)
  val last_extra : ('extra, _) t -> 'extra option
end

module Result_spec = Start_via_incr_dom.Result_spec

(** [start_and_get_handle] takes a [local_ Bonsai.graph -> 'result Bonsai.t] and runs it.

    The first parameter to [start_and_get_handle] is a first-class module that defines a
    [view] function of type ['result -> Vdom.Node.t]. If the computation has type
    [Vdom.Node.t Bonsai_proc.Computation.t], then {!Result_spec.just_the_view} is a
    suggested first argument. Read the docs in {!Result_spec} for more details.

    [bind_to_element_with_id] should be the HTML id of the element in the document that
    Bonsai will take control of. For most apps, you'll have html that looks like this:
    [<html><body><div id="app"></div></body></html>], so the value passed to
    [bind_to_element_with_id] should be the string ["app"].

    [optimize] configures Bonsai's computation optimizations, and defaults to [true]

    [time_source] should only be passed in tests. *)
val start_and_get_handle
  :  ('result, 'extra, 'incoming) Result_spec.t
  -> ?optimize:bool
  -> ?custom_connector:(Rpc_effect.Where_to_connect.Custom.t -> Rpc_effect.Connector.t)
  -> ?simulate_body_focus_on_root_element:bool
  -> ?time_source:Bonsai.Time_source.t
  -> bind_to_element_with_id:string
  -> (local_ Bonsai.graph -> 'result Bonsai.t)
  -> ('extra, 'incoming) Handle.t
