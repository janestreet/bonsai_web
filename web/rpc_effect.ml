open! Core
open! Import
open Async_kernel
open Async_rpc_kernel
module On_conn_failure = Rpc_effect_kernel.On_conn_failure
module Connector = Rpc_effect_kernel.Connector

let () =
  Rpc_effect_kernel.Private.set_introspection
    (module For_introspection.Rpc_effect_introspection)
;;

module Where_to_connect = struct
  include Rpc_effect_kernel.Where_to_connect

  module Self = struct
    type t = { on_conn_failure : On_conn_failure.t } [@@deriving sexp_of, compare]
  end

  module Self_connector = Rpc_effect_kernel.Where_to_connect.Register1 (Self)

  module Url = struct
    type t =
      { on_conn_failure : On_conn_failure.t
      ; url : string
      }
    [@@deriving sexp_of, compare]
  end

  module Url_connector = Rpc_effect_kernel.Where_to_connect.Register1 (Url)

  let self ~on_conn_failure () = Self_connector.where_to_connect { on_conn_failure }
  let url ~on_conn_failure url = Url_connector.where_to_connect { on_conn_failure; url }
end

let default_for_one_shot =
  Where_to_connect.self ~on_conn_failure:Retry_until_success () |> Bonsai.return
;;

let default_for_polling =
  Where_to_connect.self ~on_conn_failure:Surface_error_to_rpc () |> Bonsai.return
;;

module Private = struct
  include Rpc_effect_kernel.Private

  let self_persistent_connection_packed =
    lazy
      (Rpc_effect_kernel.Persistent_connection_packed.create
         (module Persistent_connection.Rpc)
         (Persistent_connection.Rpc.create
            ~server_name:"self-ws-server"
            ~address:(module Unit)
            ~connect:(fun () -> Async_js.Rpc.Connection.client ())
            Deferred.Or_error.return))
  ;;

  let url_persistent_connection_packed =
    Memo.of_comparator (module String) (fun url ->
      Rpc_effect_kernel.Persistent_connection_packed.create
        (module Persistent_connection.Rpc)
        (Persistent_connection.Rpc.create
           ~server_name:url
           ~address:(module String)
           ~connect:(fun url ->
             Async_js.Rpc.Connection.client ~uri:(Uri.of_string url) ())
           (fun () -> Deferred.Or_error.return url)))
  ;;

  let self_connector ~on_conn_failure () =
    Connector.of_packed_persistent_connection
      ~on_conn_failure
      (force self_persistent_connection_packed)
  ;;

  let url_connector ~on_conn_failure url =
    Connector.of_packed_persistent_connection
      ~on_conn_failure
      (url_persistent_connection_packed url)
  ;;
end

module Mock = struct
  let with_connector = Private.with_connector
end

module Poll_result = Rpc_effect_kernel.Poll_result
module Shared_poller = Rpc_effect_kernel.Shared_poller
module Inflight_query_id = Rpc_effect_kernel.Inflight_query_id

module Rpc = struct
  open Rpc_effect_kernel

  let dispatcher
    ~(here : [%call_pos])
    ?sexp_of_query
    ?sexp_of_response
    rpc
    ?(where_to_connect = default_for_one_shot)
    (local_ graph)
    =
    Rpc.dispatcher ~here ?sexp_of_query ?sexp_of_response rpc ~where_to_connect graph
  ;;

  let babel_dispatcher
    ~(here : [%call_pos])
    ?sexp_of_query
    ?sexp_of_response
    caller
    ?(where_to_connect = default_for_one_shot)
    (local_ graph)
    =
    Rpc.babel_dispatcher
      ~here
      ?sexp_of_query
      ?sexp_of_response
      caller
      ~where_to_connect
      graph
  ;;

  let streamable_dispatcher
    ~(here : [%call_pos])
    ?sexp_of_query
    ?sexp_of_response
    rpc
    ?(where_to_connect = default_for_one_shot)
    (local_ graph)
    =
    Rpc.streamable_dispatcher
      ~here
      ?sexp_of_query
      ?sexp_of_response
      rpc
      ~where_to_connect
      graph
  ;;

  let poll
    ~(here : [%call_pos])
    ?sexp_of_query
    ?sexp_of_response
    ~equal_query
    ?equal_response
    ?clear_when_deactivated
    ?intercept_query
    ?on_response_received
    rpc
    ?(where_to_connect = default_for_polling)
    ~every
    ~output_type
    query
    (local_ graph)
    =
    Rpc.poll
      ~here
      ?sexp_of_query
      ?sexp_of_response
      ~equal_query
      ?equal_response
      ?clear_when_deactivated
      ?intercept_query
      ?on_response_received
      rpc
      ~where_to_connect
      ~every
      ~output_type
      query
      graph
  ;;

  let babel_poll
    ~(here : [%call_pos])
    ?sexp_of_query
    ?sexp_of_response
    ~equal_query
    ?equal_response
    ?clear_when_deactivated
    ?intercept_query
    ?on_response_received
    caller
    ?(where_to_connect = default_for_polling)
    ~every
    ~output_type
    query
    (local_ graph)
    =
    Rpc.babel_poll
      ~here
      ?sexp_of_query
      ?sexp_of_response
      ~equal_query
      ?equal_response
      ?clear_when_deactivated
      ?intercept_query
      ?on_response_received
      caller
      ~where_to_connect
      ~every
      ~output_type
      query
      graph
  ;;

  let streamable_poll
    ~(here : [%call_pos])
    ?sexp_of_query
    ?sexp_of_response
    ~equal_query
    ?equal_response
    ?clear_when_deactivated
    ?intercept_query
    ?on_response_received
    rpc
    ?(where_to_connect = default_for_polling)
    ~every
    ~output_type
    query
    (local_ graph)
    =
    Rpc.streamable_poll
      ~here
      ?sexp_of_query
      ?sexp_of_response
      ~equal_query
      ?equal_response
      ?clear_when_deactivated
      ?intercept_query
      ?on_response_received
      rpc
      ~where_to_connect
      ~every
      ~output_type
      query
      graph
  ;;

  let streamable_poll_until_ok
    ~(here : [%call_pos])
    ?sexp_of_query
    ?sexp_of_response
    ~equal_query
    ?equal_response
    ?clear_when_deactivated
    ?intercept_query
    ?on_response_received
    rpc
    ?(where_to_connect = default_for_polling)
    ~retry_interval
    ~output_type
    query
    (local_ graph)
    =
    Rpc.streamable_poll_until_ok
      ~here
      ?sexp_of_query
      ?sexp_of_response
      ~equal_query
      ?equal_response
      ?clear_when_deactivated
      ?intercept_query
      ?on_response_received
      rpc
      ~where_to_connect
      ~retry_interval
      ~output_type
      query
      graph
  ;;

  let shared_poller
    (type q cmp)
    ~(here : [%call_pos])
    (module Q : Comparator.S with type t = q and type comparator_witness = cmp)
    ?sexp_of_response
    ?equal_response
    ?clear_when_deactivated
    ?intercept_query
    ?on_response_received
    rpc
    ?(where_to_connect = default_for_polling)
    ~every
    (local_ graph)
    =
    Rpc.shared_poller
      ~here
      (module Q)
      ?sexp_of_response
      ?equal_response
      ?clear_when_deactivated
      ?intercept_query
      ?on_response_received
      rpc
      ~where_to_connect
      ~every
      graph
  ;;

  let poll_until_ok
    ~(here : [%call_pos])
    ?sexp_of_query
    ?sexp_of_response
    ~equal_query
    ?equal_response
    ?clear_when_deactivated
    ?intercept_query
    ?on_response_received
    rpc
    ?(where_to_connect = default_for_polling)
    ~retry_interval
    ~output_type
    query
    (local_ graph)
    =
    Rpc.poll_until_ok
      ~here
      ?sexp_of_query
      ?sexp_of_response
      ~equal_query
      ?equal_response
      ?clear_when_deactivated
      ?intercept_query
      ?on_response_received
      rpc
      ~where_to_connect
      ~retry_interval
      ~output_type
      query
      graph
  ;;

  let poll_until_condition_met
    ~(here : [%call_pos])
    ?sexp_of_query
    ?sexp_of_response
    ~equal_query
    ?equal_response
    ?clear_when_deactivated
    ?intercept_query
    ?on_response_received
    rpc
    ?(where_to_connect = default_for_polling)
    ~every
    ~condition
    ~output_type
    query
    (local_ graph)
    =
    Rpc.poll_until_condition_met
      ~here
      ?sexp_of_query
      ?sexp_of_response
      ~equal_query
      ?equal_response
      ?clear_when_deactivated
      ?intercept_query
      ?on_response_received
      rpc
      ~where_to_connect
      ~every
      ~condition
      ~output_type
      query
      graph
  ;;

  let babel_poll_until_ok
    ~(here : [%call_pos])
    ?sexp_of_query
    ?sexp_of_response
    ~equal_query
    ?equal_response
    ?clear_when_deactivated
    ?intercept_query
    ?on_response_received
    caller
    ?(where_to_connect = default_for_polling)
    ~retry_interval
    ~output_type
    query
    (local_ graph)
    =
    Rpc.babel_poll_until_ok
      ~here
      ?sexp_of_query
      ?sexp_of_response
      ~equal_query
      ?equal_response
      ?clear_when_deactivated
      ?intercept_query
      ?on_response_received
      caller
      ~where_to_connect
      ~retry_interval
      ~output_type
      query
      graph
  ;;

  let babel_poll_until_condition_met
    ~(here : [%call_pos])
    ?sexp_of_query
    ?sexp_of_response
    ~equal_query
    ?equal_response
    ?clear_when_deactivated
    ?intercept_query
    ?on_response_received
    caller
    ?(where_to_connect = default_for_polling)
    ~every
    ~condition
    ~output_type
    query
    (local_ graph)
    =
    Rpc.babel_poll_until_condition_met
      ~here
      ?sexp_of_query
      ?sexp_of_response
      ~equal_query
      ?equal_response
      ?clear_when_deactivated
      ?intercept_query
      ?on_response_received
      caller
      ~where_to_connect
      ~every
      ~condition
      ~output_type
      query
      graph
  ;;

  let manual_poll
    ~(here : [%call_pos])
    ?sexp_of_query
    ?sexp_of_response
    ~equal_query
    ?equal_response
    ?clear_when_deactivated
    ?intercept_query
    ?on_response_received
    rpc
    ?(where_to_connect = default_for_polling)
    ~output_type
    (local_ graph)
    =
    Rpc.manual_poll
      ~here
      ?sexp_of_query
      ?sexp_of_response
      ~equal_query
      ?equal_response
      ?clear_when_deactivated
      ?intercept_query
      ?on_response_received
      rpc
      ~where_to_connect
      ~output_type
      graph
  ;;
end

module Polling_state_rpc = struct
  open Rpc_effect_kernel

  let dispatcher
    ~(here : [%call_pos])
    ?sexp_of_query
    ?sexp_of_response
    ?on_forget_client_error
    rpc
    ?(where_to_connect = default_for_one_shot)
    (local_ graph)
    =
    Polling_state_rpc.dispatcher
      ~here
      ?sexp_of_query
      ?sexp_of_response
      ?on_forget_client_error
      rpc
      ~where_to_connect
      graph
  ;;

  let babel_dispatcher
    ~(here : [%call_pos])
    ?sexp_of_query
    ?sexp_of_response
    ?on_forget_client_error
    caller
    ?(where_to_connect = default_for_one_shot)
    (local_ graph)
    =
    Polling_state_rpc.babel_dispatcher
      ~here
      ?sexp_of_query
      ?sexp_of_response
      ?on_forget_client_error
      caller
      ~where_to_connect
      graph
  ;;

  let poll
    ~(here : [%call_pos])
    ?sexp_of_query
    ?sexp_of_response
    ~equal_query
    ?equal_response
    ?clear_when_deactivated
    ?intercept_query
    ?on_response_received
    rpc
    ?(where_to_connect = default_for_polling)
    ?when_to_start_next_effect
    ~every
    ~output_type
    query
    (local_ graph)
    =
    Polling_state_rpc.poll
      ~here
      ?sexp_of_query
      ?sexp_of_response
      ~equal_query
      ?equal_response
      ?clear_when_deactivated
      ?intercept_query
      ?on_response_received
      rpc
      ~where_to_connect
      ?when_to_start_next_effect
      ~every
      ~output_type
      query
      graph
  ;;

  let babel_poll
    ~(here : [%call_pos])
    ?sexp_of_query
    ?sexp_of_response
    ~equal_query
    ?equal_response
    ?clear_when_deactivated
    ?intercept_query
    ?on_response_received
    caller
    ?(where_to_connect = default_for_polling)
    ?when_to_start_next_effect
    ~every
    ~output_type
    query
    (local_ graph)
    =
    Polling_state_rpc.babel_poll
      ~here
      ?sexp_of_query
      ?sexp_of_response
      ~equal_query
      ?equal_response
      ?clear_when_deactivated
      ?intercept_query
      ?on_response_received
      caller
      ~where_to_connect
      ?when_to_start_next_effect
      ~every
      ~output_type
      query
      graph
  ;;

  let manual_poll
    ~(here : [%call_pos])
    ?sexp_of_query
    ?sexp_of_response
    ~equal_query
    ?equal_response
    ?clear_when_deactivated
    ?intercept_query
    ?on_response_received
    rpc
    ?(where_to_connect = default_for_polling)
    ~output_type
    (local_ graph)
    =
    Polling_state_rpc.manual_poll
      ~here
      ?sexp_of_query
      ?sexp_of_response
      ~equal_query
      ?equal_response
      ?clear_when_deactivated
      ?intercept_query
      ?on_response_received
      rpc
      ~where_to_connect
      ~output_type
      graph
  ;;

  let shared_poller
    ~(here : [%call_pos])
    m
    ?sexp_of_response
    ?equal_response
    ?clear_when_deactivated
    ?intercept_query
    ?on_response_received
    rpc
    ?(where_to_connect = default_for_polling)
    ~every
    (local_ graph)
    =
    Polling_state_rpc.shared_poller
      ~here
      m
      ?sexp_of_response
      ?equal_response
      ?clear_when_deactivated
      ?intercept_query
      ?on_response_received
      rpc
      ~where_to_connect
      ~every
      graph
  ;;
end

module Status = Rpc_effect_kernel.Status
module For_introspection = For_introspection.Rpc_effect_introspection
