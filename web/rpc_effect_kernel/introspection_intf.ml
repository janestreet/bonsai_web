open! Core
open Bonsai
open Bonsai_introspection_protocol

(** This module contains functions and effects for "introspecting" rpc effect rpc's.

    This is how the Rpc_effect module communicates rpc information to the bonsai chrome
    extension's devtool panel. This module is meant to be used by bonsai internals and not
    by bonsai apps directly. *)
module type S = sig
  val should_record_effect : bool Effect.t

  (** [send_and_track_rpc_from_poller] sends the [send_rpc] rpc, but it also records its
      start/finish event messages and "enqueue"'s the messages so that they can be read by
      the devtool panel.

      This function is specialized for "polling" functions that return a ('query,
      'response) Poll_result.t . For "dispatch" equivalents refer to
      [send_and_track_rpc_from_dispatch]. *)
  val send_and_track_rpc_from_poller
    :  rpc_kind:Rpc_effect_protocol.Rpc_kind.t
    -> get_current_time:Time_ns.t Effect.t
    -> sexp_of_query:('query -> Sexp.t) option
    -> sexp_of_response:('underlying -> Sexp.t) option
    -> path:string
    -> send_rpc:
         ('query * Rpc_effect_protocol.Rpc_id.t option
          -> 'underlying Or_error.t Bonsai.Effect_throttling.Poll_result.t Effect.t)
    -> get_response:('underlying -> 'response)
    -> query:'query
    -> here:Source_code_position.t
    -> 'response Or_error.t Bonsai.Effect_throttling.Poll_result.t Effect.t

  (** [send_and_track_rpc_from_dispatch] is like [send_and_track_rpc_from_poller] but for
      one-off dispatch functions. (e.g. [Rpc_effect.Rpc.dispatcher]). *)
  val send_and_track_rpc_from_dispatch
    :  rpc_kind:Rpc_effect_protocol.Rpc_kind.t
    -> get_current_time:Time_ns.t Effect.t
    -> sexp_of_query:('query -> Sexp.t) option
    -> sexp_of_response:('underlying -> Sexp.t) option
    -> path:string
    -> send_rpc:
         ('query * Rpc_effect_protocol.Rpc_id.t option -> 'underlying Or_error.t Effect.t)
    -> get_response:('underlying -> 'response)
    -> query:'query
    -> here:Source_code_position.t
    -> 'response Or_error.t Effect.t

  (** Listen for extra data, e.g. RPC payload sizes, on the given connection. *)
  val trace_connection : Async_rpc_kernel.Rpc.Connection.t -> unit

  (** This function should be called immediately after an RPC is dispatched. *)
  val just_sent_query_with_id : Rpc_effect_protocol.Rpc_id.t option -> unit

  (** [time_rpc_effect] will time the individual call to an RPC Effect function. This is
      used in bonsai_web for adding performance metrics to the devtool panel. *)
  val time_rpc_effect
    :  rpc_kind:Rpc_effect_protocol.Rpc_kind.t
    -> ('query -> 'response Effect.t)
    -> ('query -> 'response Effect.t)
end

module No_instrospection : S = struct
  let should_record_effect = Effect.return false

  let send_and_track_rpc_from_poller
    ~rpc_kind:_
    ~get_current_time:_
    ~sexp_of_query:_
    ~sexp_of_response:_
    ~path:_
    ~send_rpc
    ~get_response
    ~query
    ~here:_
    =
    match%map.Effect send_rpc (query, None) with
    | Effect_throttling.Poll_result.Aborted -> Effect_throttling.Poll_result.Aborted
    | Finished response -> Finished (Or_error.map ~f:get_response response)
  ;;

  let send_and_track_rpc_from_dispatch
    ~rpc_kind:_
    ~get_current_time:_
    ~sexp_of_query:_
    ~sexp_of_response:_
    ~path:_
    ~send_rpc
    ~get_response
    ~query
    ~here:_
    =
    let%map.Effect response = send_rpc (query, None) in
    Or_error.map ~f:get_response response
  ;;

  let trace_connection _ = ()
  let just_sent_query_with_id _ = ()
  let time_rpc_effect ~rpc_kind:_ effect = effect
end
