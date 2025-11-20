open! Core
open! Async_kernel
open Async_rpc_kernel
open Bonsai.For_open

module type S = Introspection_intf.S

module Poll_result = Poll_result

module On_conn_failure : sig
  (** Persistent connections reuse a single connection. If there's a failure to connect,
      it will wait a bit, then attempt to re-establish the connection.

      On connection failure, our RPC can either wait until some retry attempt succeeds, or
      treat the failure as an error.

      For almost all polling RPCs, and most one-shot RPCs, [Surface_error_to_rpc] is
      preferable. However, with one-shot RPCs, you might then want to repeatedly retry the
      RPC until it succeeds. The [Retry_until_success] option can be useful here, but if
      the connection never succeeds, the effect will never resolve. *)
  type t =
    | Surface_error_to_rpc
    | Retry_until_success
  [@@deriving sexp_of, compare, equal]
end

(** The place that an RPC should be sent. *)
module Where_to_connect : sig
  module Custom : sig
    type t = ..
  end

  (** The place that an RPC should be sent.

      [t] MUST be private, to ensure that all [Custom] values come through [Register] or
      [Register1], so we always have a comparison function. *)
  type t = private Custom of Custom.t [@@deriving compare, sexp_of, equal]

  module type Registered = sig
    type Custom.t += T

    val where_to_connect : t
  end

  (** [Register] allows you to use a custom [Connector.t]. To do so:
      - Use the resulting [M.where_to_connect] as [~where_to_connect] when dispatching
        RPCs
      - Pass a [~custom_connector] arg to [Bonsai_web.Start.start], which, when given your
        [M.T], returns your custom connector. *)
  module Register () : Registered

  module type Registered1 = sig
    type arg
    type Custom.t += T of arg

    val where_to_connect : arg -> t
  end

  (** [Register1] is like [Register], but takes an argument. *)
  module Register1 (Arg : sig
      type t [@@deriving compare, sexp_of]
    end) : Registered1 with type arg = Arg.t
end

module Inflight_query_id : Unique_id.Id

module Shared_poller : sig
  (** A [Shared_poller] is a handle to a polling-style RPC whose RPCs can be shared
      between multiple components that might have an interest in polling values with the
      same types.

      To create a [Shared_poller], use either [Rpc_effect.Rpc.shared_poller] or
      [Rpc_effect.Polling_state_rpc.shared_poller]. With the value returned by those
      functions, you can call [Shared_poller.lookup] with a query value to get access to
      the results of the given RPC with the provided query. *)

  type ('query, 'response) t

  (** Uses a shared-poller to either start polling an RPC, or if another user of the same
      shared-poller is already polling with the same query, it'll immediately return the
      most recent value. *)
  val lookup
    :  here:[%call_pos]
    -> ('query, 'response) t Bonsai.t
    -> 'query Bonsai.t
    -> output_type:('query, 'response, 'output) Poll_result.Output_type.t
    -> local_ Bonsai.graph
    -> 'output Bonsai.t

  (** You can use [custom_create] to build a shared-poller if the
      [Rpc_effect.Rpc.shared_poller] and [Rpc_effect.Polling_state_rpc.shared_poller]
      aren't sufficient. You'll likely want to wrap any shared poller in a
      [Bonsai.scope_model] on [~where_to_connect]. *)
  val custom_create
    :  here:[%call_pos]
    -> ('query, _) Comparator.Module.t
    -> f:
         ('query Bonsai.t
          -> local_ Bonsai.graph
          -> ('query, 'response) Poll_result.t Bonsai.t)
    -> local_ Bonsai.graph
    -> ('query, 'response) t Bonsai.t
end

module Rpc : sig
  (** An effect for sending a particular RPC to a particular place.

      [where_to_connect] defaults to [self ~on_conn_failure:Retry_until_success]. *)
  val dispatcher
    :  here:[%call_pos]
    -> ?sexp_of_query:('query -> Sexp.t)
    -> ?sexp_of_response:('response -> Sexp.t)
    -> ('query, 'response) Rpc.Rpc.t
    -> where_to_connect:Where_to_connect.t Bonsai.t
    -> local_ Bonsai.graph
    -> ('query -> 'response Or_error.t Effect.t) Bonsai.t

  val babel_dispatcher
    :  here:[%call_pos]
    -> ?sexp_of_query:('query -> Sexp.t)
    -> ?sexp_of_response:('response -> Sexp.t)
    -> ('query -> 'response Or_error.t Deferred.t) Babel.Caller.t
    -> where_to_connect:Where_to_connect.t Bonsai.t
    -> local_ Bonsai.graph
    -> ('query -> 'response Or_error.t Effect.t) Bonsai.t

  val streamable_dispatcher
    :  here:[%call_pos]
    -> ?sexp_of_query:('query -> Sexp.t)
    -> ?sexp_of_response:('response -> Sexp.t)
    -> ('query, 'response) Streamable.Plain_rpc.t
    -> where_to_connect:Where_to_connect.t Bonsai.t
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
    -> where_to_connect:Where_to_connect.t Bonsai.t
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
    -> where_to_connect:Where_to_connect.t Bonsai.t
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
    -> where_to_connect:Where_to_connect.t Bonsai.t
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
    -> where_to_connect:Where_to_connect.t Bonsai.t
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
    -> where_to_connect:Where_to_connect.t Bonsai.t
    -> every:Time_ns.Span.t Bonsai.t
    -> local_ Bonsai.graph
    -> ('query, 'response) Shared_poller.t Bonsai.t

  val shared_babel_poller
    :  here:[%call_pos]
    -> ('query, _) Comparator.Module.t
    -> ?sexp_of_response:('response -> Sexp.t)
    -> ?equal_response:('response -> 'response -> bool)
    -> ?clear_when_deactivated:bool
    -> ?intercept_query:('query -> Inflight_query_id.t -> 'query Effect.t) Bonsai.t
    -> ?on_response_received:
         ('query -> 'response Or_error.t -> unit Bonsai.Effect.t) Bonsai.t
    -> ('query -> 'response Or_error.t Deferred.t) Babel.Caller.t
    -> where_to_connect:Where_to_connect.t Bonsai.t
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
    -> where_to_connect:Where_to_connect.t Bonsai.t
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
    -> where_to_connect:Where_to_connect.t Bonsai.t
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
    -> where_to_connect:Where_to_connect.t Bonsai.t
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
    -> where_to_connect:Where_to_connect.t Bonsai.t
    -> every:Time_ns.Span.t Bonsai.t
    -> condition:('response -> [ `Continue | `Stop_polling ]) Bonsai.t
    -> output_type:('query, 'response, 'output) Poll_result.Output_type.t
    -> 'query Bonsai.t
    -> local_ Bonsai.graph
    -> 'output Bonsai.t

  (** Like [poll], but returns the poll result and querying effect separately. This allows
      for more flexible composition and custom handling of the state. The returned effect
      can be scheduled to send/re-send the RPC.

      Unlike [poll], this function does not automatically schedule the effect - no polling
      happens unless you explicitly schedule the returned effect yourself. *)
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
    -> where_to_connect:Where_to_connect.t Bonsai.t
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
    -> where_to_connect:Where_to_connect.t Bonsai.t
    -> local_ Bonsai.graph
    -> ('query -> 'response Or_error.t Effect.t) Bonsai.t

  val babel_dispatcher
    :  here:[%call_pos]
    -> ?sexp_of_query:('query -> Sexp.t)
    -> ?sexp_of_response:('response -> Sexp.t)
    -> ?on_forget_client_error:(Error.t -> unit Effect.t)
    -> ('query, 'response) Versioned_polling_state_rpc.Client.caller
    -> where_to_connect:Where_to_connect.t Bonsai.t
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
    -> where_to_connect:Where_to_connect.t Bonsai.t
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
    -> where_to_connect:Where_to_connect.t Bonsai.t
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

      Unlike [poll], this function does not automatically schedule the effect - no polling
      happens unless you explicitly schedule the returned effect yourself. *)
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
    -> where_to_connect:Where_to_connect.t Bonsai.t
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
    -> where_to_connect:Where_to_connect.t Bonsai.t
    -> every:Time_ns.Span.t Bonsai.t
    -> local_ Bonsai.graph
    -> ('query, 'response) Shared_poller.t Bonsai.t

  val shared_babel_poller
    :  here:[%call_pos]
    -> ('query, _) Comparator.Module.t
    -> ?sexp_of_response:('response -> Sexp.t)
    -> ?equal_response:('response -> 'response -> bool)
    -> ?clear_when_deactivated:bool
    -> ?intercept_query:('query -> Inflight_query_id.t -> 'query Effect.t) Bonsai.t
    -> ?on_response_received:('query -> 'response Or_error.t -> unit Effect.t) Bonsai.t
    -> ('query, 'response) Versioned_polling_state_rpc.Client.caller
    -> where_to_connect:Where_to_connect.t Bonsai.t
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

module Persistent_connection_packed : sig
  type t

  val create
    :  (module Persistent_connection.S
          with type conn = Async_rpc_kernel_private.Connection.t
           and type t = 'conn)
    -> 'conn
    -> t
end

module Connector : sig
  (** A connector specifies a way of creating a connection. This module is exposed to
      cover exceptional cases; ordinarily, you should prefer to use the [Self] and [Url]
      constructors of [Where_to_connect.t], which have a connector backing them that you
      don't need to explicitly provide. *)

  module Rpc := Async_rpc_kernel.Rpc

  type t

  val persistent_connection
    :  on_conn_failure:On_conn_failure.t
    -> (module Persistent_connection.S
          with type t = 'conn
           and type conn = Rpc.Connection.t)
    -> 'conn
    -> t

  val of_packed_persistent_connection
    :  on_conn_failure:On_conn_failure.t
    -> Persistent_connection_packed.t
    -> t

  val async_durable : Rpc.Connection.t Async_durable.t -> t

  val for_test
    :  's Rpc.Implementations.t
    -> connection_state:(Rpc.Connection.t -> 's)
    -> t

  val for_preview
    :  's Rpc.Implementations.t
    -> connection_state:(Rpc.Connection.t -> 's)
    -> t

  val test_fallback : t
end

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

  (** [set_introspection] will let you register an [introspection] module. This module
      provides / forwards the information necessary for the bonsai devtool panel to work.
      Because there are cases where we do not have a devtool panel (e.g. bonsai_term
      apps), [rpc_effect_kernel] defaults to not doing introspection. You can call this
      function to implement introspection support. *)
  val set_introspection : (module Introspection_intf.S) -> unit

  (** Determines whether the connector is the test fallback connector. This is used by the
      testing library to swap out the [test_fallback] connector with a different connector
      controlled by other parameters. *)
  val is_test_fallback : Connector.t -> bool

  module For_tests : sig
    module Rvar : sig
      type 'a t

      val create : (unit -> 'a Deferred.Or_error.t) -> 'a t
      val invalidate : 'a t -> unit
      val contents : 'a t -> 'a Deferred.Or_error.t
    end
  end
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
