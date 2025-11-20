open! Core
open! Import
open! Async_kernel
open Async_rpc_kernel
open Bonsai.For_open
module On_conn_failure = Rpc_effect_kernel.On_conn_failure

(** The place that an RPC should be sent. *)
module Where_to_connect : sig
  include
    module type of Rpc_effect_kernel.Where_to_connect
    with type Custom.t = Rpc_effect_kernel.Where_to_connect.Custom.t
     and type t = Rpc_effect_kernel.Where_to_connect.t

  module Self : sig
    type t = private { on_conn_failure : On_conn_failure.t }
  end

  module Self_connector : Registered1 with type arg = Self.t

  module Url : sig
    type t = private
      { on_conn_failure : On_conn_failure.t
      ; url : string
      }
  end

  module Url_connector : Registered1 with type arg = Url.t

  val self : on_conn_failure:On_conn_failure.t -> unit -> t
  val url : on_conn_failure:On_conn_failure.t -> string -> t
end

module Poll_result = Rpc_effect_kernel.Poll_result
module Shared_poller = Rpc_effect_kernel.Shared_poller
module Inflight_query_id = Rpc_effect_kernel.Inflight_query_id

module Rpc : sig
  (** An effect for sending a particular RPC to a particular place.

      [where_to_connect] defaults to [self ~on_conn_failure:Retry_until_success]. *)
  val dispatcher
    :  here:[%call_pos]
    -> ?sexp_of_query:('query -> Sexp.t)
    -> ?sexp_of_response:('response -> Sexp.t)
    -> ('query, 'response) Rpc.Rpc.t
    -> ?where_to_connect:Where_to_connect.t Bonsai.t
    -> local_ Bonsai.graph
    -> ('query -> 'response Or_error.t Effect.t) Bonsai.t

  val babel_dispatcher
    :  here:[%call_pos]
    -> ?sexp_of_query:('query -> Sexp.t)
    -> ?sexp_of_response:('response -> Sexp.t)
    -> ('query -> 'response Or_error.t Deferred.t) Babel.Caller.t
    -> ?where_to_connect:Where_to_connect.t Bonsai.t
    -> local_ Bonsai.graph
    -> ('query -> 'response Or_error.t Effect.t) Bonsai.t

  val streamable_dispatcher
    :  here:[%call_pos]
    -> ?sexp_of_query:('query -> Sexp.t)
    -> ?sexp_of_response:('response -> Sexp.t)
    -> ('query, 'response) Streamable.Plain_rpc.t
    -> ?where_to_connect:Where_to_connect.t Bonsai.t
    -> local_ Bonsai.graph
    -> ('query -> 'response Or_error.t Effect.t) Bonsai.t

  (** A computation that periodically dispatches on an RPC and keeps track of the most
      recent response. Only one request will be in-flight at any point in time.

      [clear_when_deactivated] determines whether the most recent response should be
      discarded when the component is deactivated. Default is true.

      [where_to_connect] defaults to [self ~on_conn_failure:Surface_error_to_rpc]. *)
  val poll
    :  here:[%call_pos]
    -> ?sexp_of_query:('query -> Sexp.t)
    -> ?sexp_of_response:('response -> Sexp.t)
    -> equal_query:('query -> 'query -> bool)
    -> ?equal_response:('response -> 'response -> bool)
    -> ?clear_when_deactivated:bool
    -> ?intercept_query:('query -> Inflight_query_id.t -> 'query Effect.t) Bonsai.t
    -> ?on_response_received:('query -> 'response Or_error.t -> unit Effect.t) Bonsai.t
    -> ('query, 'response) Rpc.Rpc.t
    -> ?where_to_connect:Where_to_connect.t Bonsai.t
    -> every:Time_ns.Span.t Bonsai.t
    -> output_type:('query, 'response, 'output) Poll_result.Output_type.t
    -> 'query Bonsai.t
    -> local_ Bonsai.graph
    -> 'output Bonsai.t

  (** Analagous to [poll] for babel RPCs. See [poll] for details. *)
  val babel_poll
    :  here:[%call_pos]
    -> ?sexp_of_query:('query -> Sexp.t)
    -> ?sexp_of_response:('response -> Sexp.t)
    -> equal_query:('query -> 'query -> bool)
    -> ?equal_response:('response -> 'response -> bool)
    -> ?clear_when_deactivated:bool
    -> ?intercept_query:('query -> Inflight_query_id.t -> 'query Effect.t) Bonsai.t
    -> ?on_response_received:('query -> 'response Or_error.t -> unit Effect.t) Bonsai.t
    -> ('query -> 'response Or_error.t Deferred.t) Babel.Caller.t
    -> ?where_to_connect:Where_to_connect.t Bonsai.t
    -> every:Time_ns.Span.t Bonsai.t
    -> output_type:('query, 'response, 'output) Poll_result.Output_type.t
    -> 'query Bonsai.t
    -> local_ Bonsai.graph
    -> 'output Bonsai.t

  (** Analagous to [poll] for Streamable plain RPCs. See [poll] for details. *)
  val streamable_poll
    :  here:[%call_pos]
    -> ?sexp_of_query:('query -> Sexp.t)
    -> ?sexp_of_response:('response -> Sexp.t)
    -> equal_query:('query -> 'query -> bool)
    -> ?equal_response:('response -> 'response -> bool)
    -> ?clear_when_deactivated:bool
    -> ?intercept_query:('query -> Inflight_query_id.t -> 'query Effect.t) Bonsai.t
    -> ?on_response_received:('query -> 'response Or_error.t -> unit Effect.t) Bonsai.t
    -> ('query, 'response) Streamable.Plain_rpc.t
    -> ?where_to_connect:Where_to_connect.t Bonsai.t
    -> every:Time_ns.Span.t Bonsai.t
    -> output_type:('query, 'response, 'output) Poll_result.Output_type.t
    -> 'query Bonsai.t
    -> local_ Bonsai.graph
    -> 'output Bonsai.t

  (** Analagous to [poll_until_ok] for Streamable plain RPCs. See [poll_until_ok] for
      details. *)
  val streamable_poll_until_ok
    :  here:[%call_pos]
    -> ?sexp_of_query:('query -> Sexp.t)
    -> ?sexp_of_response:('response -> Sexp.t)
    -> equal_query:('query -> 'query -> bool)
    -> ?equal_response:('response -> 'response -> bool)
    -> ?clear_when_deactivated:bool
    -> ?intercept_query:('query -> Inflight_query_id.t -> 'query Effect.t) Bonsai.t
    -> ?on_response_received:('query -> 'response Or_error.t -> unit Effect.t) Bonsai.t
    -> ('query, 'response) Streamable.Plain_rpc.t
    -> ?where_to_connect:Where_to_connect.t Bonsai.t
    -> retry_interval:Time_ns.Span.t Bonsai.t
    -> output_type:('query, 'response, 'output) Poll_result.Output_type.t
    -> 'query Bonsai.t
    -> local_ Bonsai.graph
    -> 'output Bonsai.t

  val shared_poller
    :  here:[%call_pos]
    -> ('query, _) Comparator.Module.t
    -> ?sexp_of_response:('response -> Sexp.t)
    -> ?equal_response:('response -> 'response -> bool)
    -> ?clear_when_deactivated:bool
    -> ?intercept_query:('query -> Inflight_query_id.t -> 'query Effect.t) Bonsai.t
    -> ?on_response_received:('query -> 'response Or_error.t -> unit Effect.t) Bonsai.t
    -> ('query, 'response) Rpc.Rpc.t
    -> ?where_to_connect:Where_to_connect.t Bonsai.t
    -> every:Time_ns.Span.t Bonsai.t
    -> local_ Bonsai.graph
    -> ('query, 'response) Shared_poller.t Bonsai.t

  (** Like [poll], but stops polling the same input query after an ok response. If the
      query changes, the computation will resume polling until it receives another ok
      response. If the computation receives an error response, it will retry sending the
      RPC after waiting [retry_interval]. *)
  val poll_until_ok
    :  here:[%call_pos]
    -> ?sexp_of_query:('query -> Sexp.t)
    -> ?sexp_of_response:('response -> Sexp.t)
    -> equal_query:('query -> 'query -> bool)
    -> ?equal_response:('response -> 'response -> bool)
    -> ?clear_when_deactivated:bool
    -> ?intercept_query:('query -> Inflight_query_id.t -> 'query Effect.t) Bonsai.t
    -> ?on_response_received:('query -> 'response Or_error.t -> unit Effect.t) Bonsai.t
    -> ('query, 'response) Rpc.Rpc.t
    -> ?where_to_connect:Where_to_connect.t Bonsai.t
    -> retry_interval:Time_ns.Span.t Bonsai.t
    -> output_type:('query, 'response, 'output) Poll_result.Output_type.t
    -> 'query Bonsai.t
    -> local_ Bonsai.graph
    -> 'output Bonsai.t

  (** Similar to [poll_until_ok], but will continue polling until [condition response]
      indicates to [`Stop_polling] on an ok response. Also like [poll_until_ok], it will
      resume polling when the query changes, the condition changes, or the computation
      receives an error response. *)
  val poll_until_condition_met
    :  here:[%call_pos]
    -> ?sexp_of_query:('query -> Sexp.t)
    -> ?sexp_of_response:('response -> Sexp.t)
    -> equal_query:('query -> 'query -> bool)
    -> ?equal_response:('response -> 'response -> bool)
    -> ?clear_when_deactivated:bool
    -> ?intercept_query:('query -> Inflight_query_id.t -> 'query Effect.t) Bonsai.t
    -> ?on_response_received:('query -> 'response Or_error.t -> unit Effect.t) Bonsai.t
    -> ('query, 'response) Rpc.Rpc.t
    -> ?where_to_connect:Where_to_connect.t Bonsai.t
    -> every:Time_ns.Span.t Bonsai.t
    -> condition:('response -> [ `Continue | `Stop_polling ]) Bonsai.t
    -> output_type:('query, 'response, 'output) Poll_result.Output_type.t
    -> 'query Bonsai.t
    -> local_ Bonsai.graph
    -> 'output Bonsai.t

  val babel_poll_until_ok
    :  here:[%call_pos]
    -> ?sexp_of_query:('query -> Sexp.t)
    -> ?sexp_of_response:('response -> Sexp.t)
    -> equal_query:('query -> 'query -> bool)
    -> ?equal_response:('response -> 'response -> bool)
    -> ?clear_when_deactivated:bool
    -> ?intercept_query:('query -> Inflight_query_id.t -> 'query Effect.t) Bonsai.t
    -> ?on_response_received:('query -> 'response Or_error.t -> unit Effect.t) Bonsai.t
    -> ('query -> 'response Or_error.t Deferred.t) Babel.Caller.t
    -> ?where_to_connect:Where_to_connect.t Bonsai.t
    -> retry_interval:Time_ns.Span.t Bonsai.t
    -> output_type:('query, 'response, 'output) Poll_result.Output_type.t
    -> 'query Bonsai.t
    -> local_ Bonsai.graph
    -> 'output Bonsai.t

  val babel_poll_until_condition_met
    :  here:[%call_pos]
    -> ?sexp_of_query:('query -> Sexp.t)
    -> ?sexp_of_response:('response -> Sexp.t)
    -> equal_query:('query -> 'query -> bool)
    -> ?equal_response:('response -> 'response -> bool)
    -> ?clear_when_deactivated:bool
    -> ?intercept_query:('query -> Inflight_query_id.t -> 'query Effect.t) Bonsai.t
    -> ?on_response_received:('query -> 'response Or_error.t -> unit Effect.t) Bonsai.t
    -> ('query -> 'response Or_error.t Deferred.t) Babel.Caller.t
    -> ?where_to_connect:Where_to_connect.t Bonsai.t
    -> every:Time_ns.Span.t Bonsai.t
    -> condition:('response -> [ `Continue | `Stop_polling ]) Bonsai.t
    -> output_type:('query, 'response, 'output) Poll_result.Output_type.t
    -> 'query Bonsai.t
    -> local_ Bonsai.graph
    -> 'output Bonsai.t

  (** Like [poll], but returns the poll result and querying effect separately. This allows
      for more flexible composition and custom handling of the state. The returned effect
      can be scheduled to send/re-send the RPC.

      Unlike [poll], this function does not automatically schedule the effect - no RPCs
      occur unless you explicitly schedule the returned effect yourself. *)
  val manual_poll
    :  here:[%call_pos]
    -> ?sexp_of_query:('query -> Sexp.t)
    -> ?sexp_of_response:('response -> Sexp.t)
    -> equal_query:('query -> 'query -> bool)
    -> ?equal_response:('response -> 'response -> bool)
    -> ?clear_when_deactivated:bool
    -> ?intercept_query:('query -> Inflight_query_id.t -> 'query Effect.t) Bonsai.t
    -> ?on_response_received:('query -> 'response Or_error.t -> unit Effect.t) Bonsai.t
    -> ('query, 'response) Rpc.Rpc.t
    -> ?where_to_connect:Where_to_connect.t Bonsai.t
    -> output_type:('query, 'response, 'output) Poll_result.Output_type.t
    -> local_ Bonsai.graph
    -> ('output * ('query -> 'response Or_error.t Effect.t)) Bonsai.t
end

module Polling_state_rpc : sig
  (** An effect for dispatching on a particular Polling_state_rpc with a particular query.
      When the computation is deactivated, it asks the server to cleanup any cached data,
      so that there is no memory leak. If this cleanup fails, then
      [on_forget_client_error] is called with the error. *)
  val dispatcher
    :  here:[%call_pos]
    -> ?sexp_of_query:('query -> Sexp.t)
    -> ?sexp_of_response:('response -> Sexp.t)
    -> ?on_forget_client_error:(Error.t -> unit Effect.t)
    -> ('query, 'response) Polling_state_rpc.t
    -> ?where_to_connect:Where_to_connect.t Bonsai.t
    -> local_ Bonsai.graph
    -> ('query -> 'response Or_error.t Effect.t) Bonsai.t

  val babel_dispatcher
    :  here:[%call_pos]
    -> ?sexp_of_query:('query -> Sexp.t)
    -> ?sexp_of_response:('response -> Sexp.t)
    -> ?on_forget_client_error:(Error.t -> unit Effect.t)
    -> ('query, 'response) Versioned_polling_state_rpc.Client.caller
    -> ?where_to_connect:Where_to_connect.t Bonsai.t
    -> local_ Bonsai.graph
    -> ('query -> 'response Or_error.t Effect.t) Bonsai.t

  (** A computation that periodically dispatches on a polling_state_rpc and keeps track of
      the most recent response. To explicitly re-send the RPC, schedule the [refresh]
      field of the result. It also keeps track of the current query that is in-flight. *)
  val poll
    :  here:[%call_pos]
    -> ?sexp_of_query:('query -> Sexp.t)
    -> ?sexp_of_response:('response -> Sexp.t)
    -> equal_query:('query -> 'query -> bool)
    -> ?equal_response:('response -> 'response -> bool)
    -> ?clear_when_deactivated:bool
    -> ?intercept_query:('query -> Inflight_query_id.t -> 'query Effect.t) Bonsai.t
    -> ?on_response_received:('query -> 'response Or_error.t -> unit Effect.t) Bonsai.t
    -> ('query, 'response) Polling_state_rpc.t
    -> ?where_to_connect:Where_to_connect.t Bonsai.t
    -> ?when_to_start_next_effect:
         [ `Wait_period_after_previous_effect_starts_blocking
         | `Wait_period_after_previous_effect_finishes_blocking
         | `Every_multiple_of_period_non_blocking
         | `Every_multiple_of_period_blocking
         ]
    -> every:Time_ns.Span.t Bonsai.t
    -> output_type:('query, 'response, 'output) Poll_result.Output_type.t
    -> 'query Bonsai.t
    -> local_ Bonsai.graph
    -> 'output Bonsai.t

  val babel_poll
    :  here:[%call_pos]
    -> ?sexp_of_query:('query -> Sexp.t)
    -> ?sexp_of_response:('response -> Sexp.t)
    -> equal_query:('query -> 'query -> bool)
    -> ?equal_response:('response -> 'response -> bool)
    -> ?clear_when_deactivated:bool
    -> ?intercept_query:('query -> Inflight_query_id.t -> 'query Effect.t) Bonsai.t
    -> ?on_response_received:('query -> 'response Or_error.t -> unit Effect.t) Bonsai.t
    -> ('query, 'response) Versioned_polling_state_rpc.Client.caller
    -> ?where_to_connect:Where_to_connect.t Bonsai.t
    -> ?when_to_start_next_effect:
         [ `Wait_period_after_previous_effect_starts_blocking
         | `Wait_period_after_previous_effect_finishes_blocking
         | `Every_multiple_of_period_non_blocking
         | `Every_multiple_of_period_blocking
         ]
    -> every:Time_ns.Span.t Bonsai.t
    -> output_type:('query, 'response, 'output) Poll_result.Output_type.t
    -> 'query Bonsai.t
    -> local_ Bonsai.graph
    -> 'output Bonsai.t

  (** Like [poll], but returns the poll result and querying effect separately. This allows
      for more flexible composition and custom handling of the state. The returned effect
      can be scheduled to send/re-send the RPC.

      Unlike [poll], this function does not automatically schedule the effect - no RPCs
      occur unless you explicitly schedule the returned effect yourself. *)
  val manual_poll
    :  here:[%call_pos]
    -> ?sexp_of_query:('query -> Sexp.t)
    -> ?sexp_of_response:('response -> Sexp.t)
    -> equal_query:('query -> 'query -> bool)
    -> ?equal_response:('response -> 'response -> bool)
    -> ?clear_when_deactivated:bool
    -> ?intercept_query:('query -> Inflight_query_id.t -> 'query Effect.t) Bonsai.t
    -> ?on_response_received:('query -> 'response Or_error.t -> unit Effect.t) Bonsai.t
    -> ('query, 'response) Polling_state_rpc.t
    -> ?where_to_connect:Where_to_connect.t Bonsai.t
    -> output_type:('query, 'response, 'output) Poll_result.Output_type.t
    -> local_ Bonsai.graph
    -> ('output * ('query -> 'response Or_error.t Effect.t)) Bonsai.t

  val shared_poller
    :  here:[%call_pos]
    -> ('query, _) Comparator.Module.t
    -> ?sexp_of_response:('response -> Sexp.t)
    -> ?equal_response:('response -> 'response -> bool)
    -> ?clear_when_deactivated:bool
    -> ?intercept_query:('query -> Inflight_query_id.t -> 'query Effect.t) Bonsai.t
    -> ?on_response_received:('query -> 'response Or_error.t -> unit Effect.t) Bonsai.t
    -> ('query, 'response) Polling_state_rpc.t
    -> ?where_to_connect:Where_to_connect.t Bonsai.t
    -> every:Time_ns.Span.t Bonsai.t
    -> local_ Bonsai.graph
    -> ('query, 'response) Shared_poller.t Bonsai.t
end

module Status : sig
  module State : sig
    (** The status of an RPC connection.

        state diagram:
        {v
      START
       |       .------------------.
       v       v                   \
      Connecting -> Connected <-> Disconnected
       |  ^          ^
       v  |          |
      Failed_to_connect
        v} *)
    type t =
      | Connecting
      | Connected
      | Disconnected of Error.t
      | Failed_to_connect of Error.t
    [@@deriving equal, sexp]
  end

  type t =
    { state : State.t
    ; connecting_since : Time_ns.t option
    }
  [@@deriving sexp_of]

  (** A component whose output tracks the state of a connection to a host. *)
  val state
    :  where_to_connect:Where_to_connect.t Bonsai.t
    -> local_ Bonsai.graph
    -> t Bonsai.t

  (** [on_change] triggers effects when the connection state changes. *)
  val on_change
    :  where_to_connect:Where_to_connect.t Bonsai.t
    -> callback:(State.t -> unit Effect.t) Bonsai.t
    -> local_ Bonsai.graph
    -> unit
end

module Connector = Rpc_effect_kernel.Connector

module Private : sig
  (** This module contains functions intended for use by Bonsai's internal startup code.
      Ordinarily, you shouldn't need to call any of them.

      More specifically, in tests, [with_connector] is called when a test handle is
      created, using an optional, user-provided function to select the connector.
      Similarly, when an app is actually being run, we take a function of type
      [Custom.t -> Connector.t] and default the [Self] and [Url] cases to [self_connector]
      and [url_connector] declared below. *)

  (** Turns a computation into a new computation that has access to some sort of
      connection. This is the primitive and most powerful way of providing access to a
      connection. Since it has access to the [Where_to_connect.t], it can create different
      kinds of connections based on what is being connected to. *)
  val with_connector
    :  (Where_to_connect.t -> Connector.t)
    -> (local_ Bonsai.graph -> 'a Bonsai.t)
    -> local_ Bonsai.graph
    -> 'a Bonsai.t

  (** The connector for the server hosting the web page. *)
  val self_connector : on_conn_failure:On_conn_failure.t -> unit -> Connector.t

  (** The connector for an arbitrary URL. *)
  val url_connector : on_conn_failure:On_conn_failure.t -> string -> Connector.t

  (** Determines whether the connector is the test fallback connector. This is used by the
      testing library to swap out the [test_fallback] connector with a different connector
      controlled by other parameters. *)
  val is_test_fallback : Connector.t -> bool
end

module Mock : sig
  (** Turns a computation into a new computation that has access to some sort of
      connection. This is the primitive and most powerful way of providing access to a
      connection. Since it has access to the [Where_to_connect.t], it can create different
      kinds of connections based on what is being connected to. This can be useful when
      you want to run an existing client app in a mocked context, like for sandbox
      testing. *)
  val with_connector
    :  (Where_to_connect.t -> Connector.t)
    -> (local_ Bonsai.graph -> 'a Bonsai.t)
    -> local_ Bonsai.graph
    -> 'a Bonsai.t
end

module For_introspection = For_introspection.Rpc_effect_introspection
