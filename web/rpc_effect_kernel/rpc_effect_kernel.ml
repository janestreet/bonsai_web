open! Core
open Async_kernel
open Async_rpc_kernel
open Bonsai_introspection_protocol
open Bonsai.For_open

module type S = Introspection_intf.S

module Poll_result = Poll_result

let introspection =
  ref (module Introspection_intf.No_instrospection : Introspection_intf.S)
;;

let set_introspection new_introspection = introspection := new_introspection

module On_conn_failure = struct
  type t =
    | Surface_error_to_rpc
    | Retry_until_success
  [@@deriving sexp_of, compare, equal]
end

module Where_to_connect = struct
  module Custom = struct
    type t = ..

    module Registered = struct
      type ext = t

      type t =
        | T :
            { get : ext -> 'a option
            ; compare : 'a -> 'a -> int
            ; sexp_of_arg : 'a -> Sexp.t
            }
            -> t

      let table : (extension_constructor, t) Hashtbl.t =
        Hashtbl.create
          (module struct
            type t = extension_constructor

            let sexp_of_t t = Sexp.Atom (Obj.Expert.Extension_constructor.name t)

            let compare =
              Comparable.lift Int.compare ~f:Obj.Expert.Extension_constructor.id
            ;;

            let hash = Obj.Expert.Extension_constructor.id
          end)
      ;;
    end

    let compare (t1 : t) (t2 : t) =
      let ext1 = Obj.Expert.Extension_constructor.of_val t1 in
      let ext2 = Obj.Expert.Extension_constructor.of_val t2 in
      match Poly.compare ext1 ext2 with
      | 0 ->
        let (T { get; compare; sexp_of_arg = _ }) =
          Hashtbl.find_exn Registered.table ext1
        in
        let payload1 = get t1 |> Option.value_exn in
        let payload2 = get t2 |> Option.value_exn in
        compare payload1 payload2
      | n -> n
    ;;

    let sexp_of_t (t : t) =
      let ext = Obj.Expert.Extension_constructor.of_val t in
      let (T { sexp_of_arg; get; compare = _ }) = Hashtbl.find_exn Registered.table ext in
      let arg = get t |> Option.value_exn in
      sexp_of_arg arg
    ;;

    let equal = Comparable.equal compare
  end

  type t = Custom of Custom.t [@@deriving compare, equal, sexp_of]

  module type Registered = sig
    type Custom.t += T

    val where_to_connect : t
  end

  module Register () : Registered = struct
    type Custom.t += T

    let get t =
      match t with
      | T -> Some ()
      | _ -> None
    ;;

    let () =
      Hashtbl.add_exn
        Custom.Registered.table
        ~key:[%extension_constructor T]
        ~data:(T { get; compare = Unit.compare; sexp_of_arg = sexp_of_unit })
    ;;

    let where_to_connect = Custom T
  end

  module type Registered1 = sig
    type arg
    type Custom.t += T of arg

    val where_to_connect : arg -> t
  end

  module Register1 (Arg : sig
      type t [@@deriving compare, sexp_of]
    end) : Registered1 with type arg = Arg.t = struct
    type arg = Arg.t
    type Custom.t += T of Arg.t

    let get t =
      match t with
      | T arg -> Some arg
      | _ -> None
    ;;

    let () =
      Hashtbl.add_exn
        Custom.Registered.table
        ~key:[%extension_constructor T]
        ~data:(T { get; compare = Arg.compare; sexp_of_arg = Arg.sexp_of_t })
    ;;

    let where_to_connect arg = Custom (T arg)
  end
end

module Rvar : sig
  (** A "Refreshable" var. *)
  type 'a t

  val const : 'a -> 'a t

  (** Makes a new container that asynchronously computes its contents on demand. *)
  val create : (unit -> 'a Deferred.Or_error.t) -> 'a t

  (** Mark the current contents of the container as being no longer valid, which means
      that the next time anyone wants to look at it, it must be re-computed. *)
  val invalidate : 'a t -> unit

  (** Computes the container's contents in order to return them. Getting the contents of
      the same ['a t] twice should only force computation once, unless it is invalidated
      between the calls to [contents], or the first call returns an error before the
      second call begins.

      If [invalidate] is called in the middle of computing the result, the computation
      starts over. *)
  val contents : 'a t -> 'a Deferred.Or_error.t

  val derived : 'a t -> ('a -> 'b Deferred.Or_error.t) -> 'b t
  val destroy : _ t -> unit
end = struct
  type 'a state =
    | Invalid
    | Pending
    | Value of 'a

  type 'a common =
    { mutable state : 'a state
    ; f : unit -> 'a Deferred.Or_error.t
    ; finished : ('a Or_error.t, read_write) Bvar.t
    ; invalidated : (unit -> unit, read_write) Bus.t
    }

  type 'a t =
    | Standard of 'a common
    | Derived of
        { common : 'a common
        ; on_destroy : unit -> unit
        }
    | Const of 'a

  let create_common f =
    let invalidated =
      Bus.create_exn
        Arity1
        ~on_subscription_after_first_write:Allow
        ~on_callback_raise:Error.raise
    in
    { state = Invalid; f; finished = Bvar.create (); invalidated }
  ;;

  let const v = Const v
  let create f = Standard (create_common f)

  let return_result t result =
    Deferred.return
      (match t with
       | Const _ -> result
       | Standard t | Derived { common = t; _ } ->
         (match result with
          | Ok value ->
            t.state <- Value value;
            Bvar.broadcast t.finished (Ok value);
            Ok value
          | Error e ->
            t.state <- Invalid;
            Bvar.broadcast t.finished (Error e);
            Error e))
  ;;

  let rec contents = function
    | Const v -> Deferred.Or_error.return v
    | (Standard t | Derived { common = t; _ }) as self ->
      (match t.state with
       | Invalid ->
         t.state <- Pending;
         (match%bind.Async_kernel.Deferred Monitor.try_with_join_or_error t.f with
          | Ok value ->
            (match t.state with
             | Invalid ->
               (* If [t] has been invalidated in the middle of computing its
                  result, try again. This recursive call shouldn't cause an infinite
                  loop because [t.f] is passed when the [t] is created, which
                  means it cannot possibly unconditionally call [invalidate]
                  on itself. Undoubtedly there is a way around this that will cause
                  an infinite loop, but in that case the infinite loop is not
                  surprising. *)
               contents self
             | Pending -> return_result self (Ok value)
             | Value value ->
               eprint_s
                 [%message
                   "BUG: Skipped computing Rvar result because it has already been \
                    computed."];
               return_result self (Ok value))
          | Error e -> return_result self (Error e))
       | Pending -> Bvar.wait t.finished
       | Value value -> Deferred.Or_error.return value)
  ;;

  let invalidate = function
    | Const _ -> ()
    | Standard t | Derived { common = t; _ } ->
      t.state <- Invalid;
      Bus.write t.invalidated ()
  ;;

  let derived inner f =
    match inner with
    | Const v -> create (fun () -> f v)
    | Standard { invalidated = inner_invalidated; _ }
    | Derived { common = { invalidated = inner_invalidated; _ }; _ } ->
      let f () = Deferred.Or_error.bind (contents inner) ~f in
      let rec me =
        lazy
          (let subscriber = Lazy.force subscriber in
           let on_destroy () = Bus.unsubscribe inner_invalidated subscriber in
           Derived { common = create_common f; on_destroy })
      and subscriber =
        lazy
          (Bus.subscribe_exn inner_invalidated ~f:(fun () -> invalidate (Lazy.force me)))
      in
      Lazy.force me
  ;;

  let destroy = function
    | Const _ -> ()
    | Standard _ as t -> invalidate t
    | Derived { on_destroy; _ } as t ->
      invalidate t;
      on_destroy ()
  ;;
end

(* This is factored out because we want to be able to share a connection between clients
   using different values of [retry_silently_on_conn_failure]. *)
module Persistent_connection_packed = struct
  type t =
    | T :
        { connection_module :
            (module Persistent_connection.S
               with type t = 'conn
                and type conn = Rpc.Connection.t)
        ; connection : 'conn
        ; menu : Versioned_rpc.Menu.t Rvar.t
        }
        -> t

  let create
    (type conn)
    (module Conn : Persistent_connection.S
      with type t = conn
       and type conn = Rpc.Connection.t)
    (connection : conn)
    =
    let (module Rpc_effect_introspection) = !introspection in
    let menu =
      Rvar.create (fun () ->
        (* The menu [Rvar.t] is only used once a connection has been established,
           so we want to bind on [Conn.connected] regardless of [retry_silently_on_conn_failure]. *)
        let%bind.Async_kernel.Deferred connection = Conn.connected connection in
        Versioned_rpc.Menu.request connection)
    in
    Bus.subscribe_permanently_exn (Conn.event_bus connection) ~f:(function
      | Disconnected -> Rvar.invalidate menu
      | Connected conn -> Rpc_effect_introspection.trace_connection conn
      | _ -> ());
    T { connection_module = (module Conn); connection; menu }
  ;;
end

module Connector = struct
  type t =
    | Async_durable of
        { connection : Rpc.Connection.t Async_durable.t
        ; menu : Versioned_rpc.Menu.t Rvar.t
        }
    | Persistent_connection of
        { connection : Persistent_connection_packed.t
        ; on_conn_failure : On_conn_failure.t
        }
    | Connection of
        { connection : Rpc.Connection.t Deferred.t
        ; menu : Versioned_rpc.Menu.t Rvar.t
        }
    | Test_fallback

  let menu_rvar = function
    | Async_durable { menu : Versioned_rpc.Menu.t Rvar.t; _ } -> Some menu
    | Persistent_connection
        { connection = T { menu : Versioned_rpc.Menu.t Rvar.t; _ }; _ } -> Some menu
    | Connection { menu : Versioned_rpc.Menu.t Rvar.t; _ } -> Some menu
    | Test_fallback -> None
  ;;

  let persistent_connection
    (type conn)
    ~on_conn_failure
    (module Conn : Persistent_connection.S
      with type t = conn
       and type conn = Rpc.Connection.t)
    (connection : conn)
    =
    Persistent_connection
      { connection = Persistent_connection_packed.create (module Conn) connection
      ; on_conn_failure
      }
  ;;

  let of_packed_persistent_connection ~on_conn_failure connection =
    Persistent_connection { connection; on_conn_failure }
  ;;

  let async_durable (connection : Rpc.Connection.t Async_durable.t) =
    let (module Rpc_effect_introspection) = !introspection in
    let menu =
      Rvar.create (fun () ->
        Async_durable.with_ connection ~f:(fun conn ->
          Rpc_effect_introspection.trace_connection conn;
          Versioned_rpc.Menu.request conn))
    in
    Bus.subscribe_permanently_exn
      (Async_durable.is_intact_bus connection)
      ~f:(fun is_intact -> if not is_intact then Rvar.invalidate menu);
    Async_durable { connection; menu }
  ;;

  let make_fake_connection ?time_source implementations ~connection_state =
    let open Async_rpc_kernel in
    let open Async_kernel in
    let (module Rpc_effect_introspection) = !introspection in
    let to_server = Pipe.create () in
    let to_client = Pipe.create () in
    let one_connection implementations ~connection_state pipe_to pipe_from =
      let transport =
        Pipe_transport.create Pipe_transport.Kind.string (fst pipe_to) (snd pipe_from)
      in
      let%bind conn =
        Rpc.Connection.create
          ?time_source
          ?implementations
          ~heartbeat_config:Rpc.Connection.Heartbeat_config.never_heartbeat
          ~connection_state
          transport
      in
      let conn = Result.ok_exn conn in
      Rpc_effect_introspection.trace_connection conn;
      return conn
    in
    don't_wait_for
      (let%bind server_conn =
         one_connection (Some implementations) ~connection_state to_server to_client
       in
       Rpc.Connection.close_finished server_conn);
    one_connection None ~connection_state:(fun _conn -> ()) to_client to_server
  ;;

  let for_test implementations ~connection_state =
    let connection =
      make_fake_connection
        ~time_source:
          (Synchronous_time_source.create ~now:Time_ns.epoch ()
           |> Synchronous_time_source.read_only)
        implementations
        ~connection_state
    in
    Connection
      { connection
      ; menu =
          Rvar.create (fun () ->
            let%bind.Async_kernel.Deferred connection in
            Versioned_rpc.Menu.request connection)
      }
  ;;

  let for_preview implementations ~connection_state =
    let connection =
      Async_rpc_kernel.Persistent_connection.Rpc.create
        ~server_name:"preview"
        ~connect:(fun () ->
          let open Async_kernel.Deferred.Let_syntax in
          make_fake_connection implementations ~connection_state >>| Result.return)
        ~address:(module Unit)
        Deferred.Result.return
    in
    Persistent_connection
      { connection =
          Persistent_connection_packed.create
            (module Async_rpc_kernel.Persistent_connection.Rpc)
            connection
      ; on_conn_failure = Retry_until_success
      }
  ;;

  let test_fallback = Test_fallback

  let with_connection f ~where_to_connect ~callback =
    match f where_to_connect with
    | Async_durable { connection; menu = _ } -> Async_durable.with_ connection ~f:callback
    | Persistent_connection
        { connection = T { connection_module = (module Conn); connection; menu = _ }
        ; on_conn_failure
        } ->
      let%bind.Deferred.Or_error connection =
        match on_conn_failure with
        | Surface_error_to_rpc -> Conn.connected_or_failed_to_connect connection
        | Retry_until_success ->
          Conn.connected connection |> Deferred.map ~f:Or_error.return
      in
      callback connection
    | Connection { connection; menu = _ } ->
      let%bind.Async_kernel.Deferred connection in
      callback connection
    | Test_fallback ->
      Deferred.Or_error.error_string
        "RPC not handled because no connector has been provided."
    | exception e -> Deferred.Or_error.of_exn e
  ;;

  let with_connection_with_menu f ~where_to_connect ~callback =
    match f where_to_connect with
    | Async_durable { connection; menu } ->
      Async_durable.with_ connection ~f:(fun connection ->
        let%bind.Deferred.Or_error menu = Rvar.contents menu in
        callback (Versioned_rpc.Connection_with_menu.create_directly connection menu))
    | Persistent_connection
        { connection = T { connection_module = (module Conn); connection; menu }
        ; on_conn_failure
        } ->
      let%bind.Deferred.Or_error connection =
        match on_conn_failure with
        | Surface_error_to_rpc -> Conn.connected_or_failed_to_connect connection
        | Retry_until_success ->
          Conn.connected connection |> Deferred.map ~f:Or_error.return
      in
      let%bind.Deferred.Or_error menu = Rvar.contents menu in
      callback (Versioned_rpc.Connection_with_menu.create_directly connection menu)
    | Connection { connection; menu } ->
      let%bind.Deferred connection in
      let%bind.Deferred.Or_error menu = Rvar.contents menu in
      callback (Versioned_rpc.Connection_with_menu.create_directly connection menu)
    | Test_fallback ->
      Deferred.Or_error.error_string
        "RPC not handled because no connector has been provided."
    | exception e -> Deferred.Or_error.of_exn e
  ;;
end

let connector_var =
  Bonsai.Dynamic_scope.create
    ~name:"Bonsai_web.Rpc_effect.connector_var"
    ~fallback:(fun _ -> failwith "BUG: no bonsai-rpc handler installed")
    ()
;;

module Private = struct
  let with_connector connector computation =
    Bonsai.Dynamic_scope.set connector_var (Bonsai.return connector) ~inside:computation
  ;;

  let set_introspection = set_introspection

  let is_test_fallback connector =
    match connector with
    | Connector.Test_fallback -> true
    | Async_durable _ | Persistent_connection _ | Connection _ -> false
  ;;

  module For_tests = struct
    module Rvar = Rvar
  end
end

module Mock = struct
  let with_connector = Private.with_connector
end

module Shared_poller = struct
  open Bonsai.Let_syntax

  type ('query, 'response) t = ('query, ('query, 'response) Poll_result.t) Bonsai.Memo.t

  let create = Bonsai.Memo.create
  let custom_create = create

  let lookup ~(here : [%call_pos]) memo query ~output_type (local_ graph) =
    let res = Bonsai.Memo.lookup ~here memo query graph in
    let%arr res in
    Poll_result.get_output (Option.value res ~default:Poll_result.empty) ~output_type
  ;;
end

module Inflight_query_key = Unique_id.Int ()

module Poll_accumulator = struct
  type ('query, 'response) t =
    { last_ok_response : ('query * 'response * Time_ns.t) option
    ; last_error : ('query * Error.t * Time_ns.t) option
    ; inflight_query : ('query * Time_ns.t) option
    }
  [@@deriving sexp_of]
end

module Poll_behavior = struct
  type 'response t =
    | Always (* Sends an rpc on every clock tick. *)
    | Until_ok
    (* Sends an rpc repeatedly until an ok response arrives. Stops polling
       once an error occurs.*)
    | Until_condition_met of
        (* Sends an rpc repeatedly until the user-provided function returns
           [`Stop_polling] on an ok response *)
        ('response -> [ `Continue | `Stop_polling ]) Bonsai.t
end

(* This returns ONLY the state machine model and effect that returns the response.
   No Bonsai.Edge.* APIs are used here - the effect must be manually scheduled. *)
let generic_polling_state_machine
  (type query response)
  ~(rpc_kind : Rpc_effect_protocol.Rpc_kind.t Bonsai.t)
  ~sexp_of_query
  ~sexp_of_underlying
  ~sexp_of_response
  ~equal_query
  ?(equal_response = [%eta2 phys_equal])
  ~clear_when_deactivated
  ~on_response_received
  dispatcher
  ~get_response
  ~here
  (local_ graph)
  =
  let module Query = struct
    type t = query

    let sexp_of_t = Option.value ~default:sexp_of_opaque sexp_of_query
  end
  in
  let module Response = struct
    type t = response

    let sexp_of_t = Option.value ~default:sexp_of_opaque sexp_of_response
  end
  in
  let open Bonsai.Let_syntax in
  let module Model = struct
    let sexp_of_query = Query.sexp_of_t
    let sexp_of_response = Response.sexp_of_t

    type t =
      { last_ok_response : (query * response * Time_ns.t) option
      ; last_error : (query * Error.t * Time_ns.t) option
      ; inflight_queries : (query * Time_ns.t) Inflight_query_key.Map.t
      }
    [@@deriving sexp_of, equal]
  end
  in
  let module Action = struct
    type t =
      | Finish of
          { query : Query.t
          ; response : Response.t Or_error.t Bonsai.Effect_throttling.Poll_result.t
          ; inflight_query_key : Inflight_query_key.t
          }
      | Start of
          { query : Query.t
          ; inflight_query_key : Inflight_query_key.t
          }
    [@@deriving sexp_of]
  end
  in
  let default_model =
    { Model.last_ok_response = None
    ; last_error = None
    ; inflight_queries = Inflight_query_key.Map.empty
    }
  in
  let response, inject_response =
    (* using a state_machine1 is important because we need add check the Computation_status
       to see if we should drop the action (due to [clear_when_responded]) *)
    Bonsai.state_machine_with_input
      (* Use a var here to prevent bonsai from optimizing the [state_machine1] down to a
         [state_machine0] *)
      Bonsai.Expert.Var.(create () |> value)
      ~sexp_of_model:[%sexp_of: Model.t]
      ~sexp_of_action:[%sexp_of: Action.t]
      ~equal:[%equal: Model.t]
      ~default_model
      ~apply_action:(fun context computation_status model action ->
        let should_ignore =
          match computation_status with
          | Inactive -> clear_when_deactivated
          | Active () -> false
        in
        let timestamp =
          Bonsai.Apply_action_context.time_source context |> Bonsai.Time_source.now
        in
        if should_ignore
        then default_model
        else (
          match action with
          | Finish { query; response; inflight_query_key } ->
            let last_ok_response, last_error =
              match response with
              | Finished (Ok response) -> Some (query, response, timestamp), None
              | Finished (Error error) ->
                model.last_ok_response, Some (query, error, timestamp)
              | Aborted -> model.last_ok_response, model.last_error
            in
            { last_ok_response
            ; last_error
            ; inflight_queries = Map.remove model.inflight_queries inflight_query_key
            }
          | Start { query; inflight_query_key } ->
            { model with
              inflight_queries =
                Map.add_exn
                  model.inflight_queries
                  ~key:inflight_query_key
                  ~data:(query, timestamp)
            }))
      graph
  in
  let underlying_to_response = function
    | Bonsai.Effect_throttling.Poll_result.Aborted ->
      Bonsai.Effect_throttling.Poll_result.Aborted
    | Finished x -> Finished (Or_error.map x ~f:get_response)
  in
  let (module Rpc_effect_introspection) = !introspection in
  let effect =
    let path = Bonsai.path_id graph in
    let get_current_time = Bonsai.Clock.get_current_time graph in
    let%arr dispatcher
    and inject_response
    and on_response_received
    and get_current_time
    and path
    and rpc_kind in
    let open Effect.Let_syntax in
    let actually_send_rpc (query, id) =
      let%bind inflight_query_key = Effect.of_sync_fun Inflight_query_key.create () in
      let%bind () = inject_response (Start { query; inflight_query_key }) in
      let%bind response = dispatcher (query, id) in
      let%bind () =
        inject_response
          (Finish
             { query; response = underlying_to_response response; inflight_query_key })
      in
      Effect.return response
    in
    fun query ->
      let%bind response =
        match%bind Rpc_effect_introspection.should_record_effect with
        | false ->
          let%map.Effect response = actually_send_rpc (query, None) in
          underlying_to_response response
        | true ->
          Rpc_effect_introspection.send_and_track_rpc_from_poller
            ~rpc_kind
            ~get_current_time
            ~sexp_of_query
            ~sexp_of_response:sexp_of_underlying
            ~path
            ~send_rpc:actually_send_rpc
            ~get_response
            ~query
            ~here
      in
      let%bind () =
        match response with
        | Bonsai.Effect_throttling.Poll_result.Aborted -> Effect.Ignore
        | Bonsai.Effect_throttling.Poll_result.Finished response ->
          on_response_received query response
      in
      match response with
      | Bonsai.Effect_throttling.Poll_result.Aborted ->
        Effect.return (Error (Error.of_string "Request aborted"))
      | Bonsai.Effect_throttling.Poll_result.Finished response -> Effect.return response
  in
  let effect =
    let%arr rpc_kind and effect in
    Rpc_effect_introspection.time_rpc_effect ~rpc_kind effect
  in
  let%arr { last_ok_response; last_error; inflight_queries } = response
  and effect in
  let inflight_query = Option.map ~f:snd (Map.max_elt inflight_queries) in
  { Poll_accumulator.last_ok_response; last_error; inflight_query }, effect
;;

(*
   This adds scheduling (Edge APIs) on top of the accumulator API and returns a Poll_result.t
*)
let generic_poll_or_error
  ~(rpc_kind : Rpc_effect_protocol.Rpc_kind.t Bonsai.t)
  ~sexp_of_query
  ~sexp_of_underlying
  ~sexp_of_response
  ~equal_query
  ?equal_response
  ~clear_when_deactivated
  ~on_response_received
  dispatcher
  ~where_to_connect
  ~when_to_start_next_effect
  ~every
  ~poll_behavior
  ~get_response
  ~output_type
  ~here
  query
  (local_ graph)
  =
  let open Bonsai.Let_syntax in
  let%sub poll_accumulator, effect_with_response =
    generic_polling_state_machine
      ~rpc_kind
      ~sexp_of_query
      ~sexp_of_underlying
      ~sexp_of_response
      ~equal_query
      ?equal_response
      ~clear_when_deactivated
      ~on_response_received
      dispatcher
      ~get_response
      ~here
      graph
  in
  let effect =
    let%arr effect_with_response in
    fun query ->
      let open Effect.Let_syntax in
      (* Ignore the response. We don't expose it in the [poll] API *)
      let%map (_ : 'response Or_error.t) = effect_with_response query in
      ()
  in
  (* Below are four constructs that schedule the effect to run:

     * [on_activate]
     * When the [query] changes
     * When the [where_to_connect] changes
     * On an interval

     The tricky part is that [Clock.every] and [Edge.on_change] both run effects on
     activate by default. To avoid the redundancy, we make neither of them
     trigger on activate, and only use [on_activate] for running effects on
     activation. *)
  let () =
    Bonsai.Edge.on_change'
      ~sexp_of_model:(Option.value ~default:sexp_of_opaque sexp_of_query)
      ~equal:equal_query
      query
      ~callback:
        (let%arr effect in
         fun prev query ->
           match prev with
           | Some _ -> effect query
           | None -> Effect.Ignore)
      graph
  in
  let send_rpc_effect =
    let%arr effect and query in
    effect query
  in
  let () =
    Bonsai.Edge.on_change'
      ~equal:[%equal: Where_to_connect.t]
      where_to_connect
      ~callback:
        (let%arr send_rpc_effect in
         fun prev _new ->
           match prev with
           | None -> Effect.Ignore
           | Some _ -> send_rpc_effect)
      graph
  in
  let%sub () =
    let clock =
      Bonsai.Clock.every
        ~when_to_start_next_effect
        ~trigger_on_activate:false
        every
        send_rpc_effect
    in
    let poll_until_condition_met condition (local_ graph) =
      let should_poll =
        let%arr condition and poll_accumulator in
        let { Poll_accumulator.last_ok_response; last_error; inflight_query } =
          poll_accumulator
        in
        match last_ok_response, last_error with
        | None, _ | _, Some _ -> Option.is_none inflight_query
        | Some (_, response, _), None ->
          (match condition response with
           | `Stop_polling -> false
           | `Continue -> true)
      in
      match%sub should_poll with
      | true ->
        clock graph;
        Bonsai.return ()
      | false -> Bonsai.return ()
    in
    match poll_behavior with
    | Poll_behavior.Always ->
      clock graph;
      Bonsai.return ()
    | Until_ok -> poll_until_condition_met (Bonsai.return (fun _ -> `Stop_polling)) graph
    | Until_condition_met condition -> poll_until_condition_met condition graph
  in
  let () = Bonsai.Edge.lifecycle ~on_activate:send_rpc_effect graph in
  let send_rpc_effect =
    let%arr effect and query in
    effect query
  in
  let%arr poll_accumulator and send_rpc_effect in
  let { Poll_accumulator.last_ok_response; last_error; inflight_query } =
    poll_accumulator
  in
  Poll_result.Private.create
    { last_ok_response
    ; last_error
    ; inflight_query
    ; refresh = send_rpc_effect
    ; equal_query
    }
  |> Poll_result.get_output ~output_type
;;

(* This [generic_polling_state_machine] wrapper adds reset-on-deactivate *)
let generic_polling_state_machine
  ~rpc_kind
  ~sexp_of_query
  ~sexp_of_underlying
  ~sexp_of_response
  ~equal_query
  ?equal_response
  ?(clear_when_deactivated = true)
  ?(on_response_received = Bonsai.return (fun _ _ -> Effect.Ignore))
  dispatcher
  ~get_response
  ~here
  (local_ graph)
  =
  let c =
    generic_polling_state_machine
      ~rpc_kind
      ~sexp_of_query
      ~sexp_of_underlying
      ~sexp_of_response
      ~equal_query
      ?equal_response
      ~clear_when_deactivated
      ~on_response_received
      dispatcher
      ~get_response
      ~here
  in
  if clear_when_deactivated
  then (
    let result, reset = Bonsai.with_model_resetter ~f:(fun graph -> c graph) graph in
    let () = Bonsai.Edge.lifecycle ~on_deactivate:reset graph in
    result)
  else c graph
;;

(* This [generic_poll_or_error] refines the [generic_poll_or_error] above by
   resetting on deactivate to avoid leaking memory (after all, an important
   feature of [Polling_state_rpc.dispatcher] is that doesn't cause a memory
   leak on the server, so it would be shame if we didn't also defend against
   memory leaks on the client. *)
let generic_poll_or_error
  ~rpc_kind
  ~sexp_of_query
  ~sexp_of_underlying
  ~sexp_of_response
  ~equal_query
  ?equal_response
  ?(clear_when_deactivated = true)
  ?(on_response_received = Bonsai.return (fun _ _ -> Effect.Ignore))
  ~where_to_connect
  ?(when_to_start_next_effect = `Wait_period_after_previous_effect_starts_blocking)
  dispatcher
  ~every
  ~poll_behavior
  query
  ~get_response
  ~output_type
  ~here
  (local_ graph)
  =
  let c =
    generic_poll_or_error
      ~rpc_kind
      ~sexp_of_query
      ~sexp_of_underlying
      ~sexp_of_response
      ~equal_query
      ?equal_response
      ~on_response_received
      ~clear_when_deactivated
      dispatcher
      ~where_to_connect
      ~when_to_start_next_effect
      ~every
      ~poll_behavior
      ~get_response
      ~output_type
      ~here
      query
  in
  if clear_when_deactivated
  then (
    let result, reset = Bonsai.with_model_resetter ~f:c graph in
    let () = Bonsai.Edge.lifecycle ~on_deactivate:reset graph in
    result)
  else c graph
;;

let sexp_of_polling_state_rpc_underlying_response (_, sexp) = Lazy.force sexp

module Our_rpc = struct
  let generic_dispatcher (type request response) dispatcher ~where_to_connect
    : local_ Bonsai.graph -> (request -> response Effect.t) Bonsai.t
    =
    fun (local_ graph) ->
    let open Bonsai.Let_syntax in
    let connector = Bonsai.Dynamic_scope.lookup connector_var graph in
    let%arr connector and where_to_connect in
    Ui_effect_of_deferred.of_deferred_fun (dispatcher connector ~where_to_connect)
  ;;

  let dispatcher_internal rpc ~where_to_connect =
    let (module Rpc_effect_introspection) = !introspection in
    generic_dispatcher ~where_to_connect (fun connector ~where_to_connect (query, id) ->
      Connector.with_connection connector ~where_to_connect ~callback:(fun connection ->
        let d = Rpc.Rpc.dispatch rpc connection query in
        Rpc_effect_introspection.just_sent_query_with_id id;
        d))
  ;;

  let babel_dispatcher_internal rpc ~where_to_connect =
    let (module Rpc_effect_introspection) = !introspection in
    generic_dispatcher ~where_to_connect (fun connector ~where_to_connect (query, id) ->
      Connector.with_connection_with_menu
        connector
        ~where_to_connect
        ~callback:(fun connection ->
          let d = Babel.Caller.Rpc.dispatch_multi rpc connection query in
          Rpc_effect_introspection.just_sent_query_with_id id;
          d))
  ;;

  let streamable_dispatcher_internal rpc ~where_to_connect =
    let (module Rpc_effect_introspection) = !introspection in
    generic_dispatcher ~where_to_connect (fun connector ~where_to_connect (query, id) ->
      Connector.with_connection connector ~where_to_connect ~callback:(fun connection ->
        let d = Streamable.Plain_rpc.dispatch rpc connection query in
        Rpc_effect_introspection.just_sent_query_with_id id;
        d))
  ;;

  let poll
    ~(here : [%call_pos])
    ?sexp_of_query
    ?sexp_of_response
    ~equal_query
    ?equal_response
    ?clear_when_deactivated
    ?on_response_received
    rpc
    ~where_to_connect
    ~every
    ~output_type
    query
    (local_ graph)
    =
    let dispatcher = dispatcher_internal rpc ~where_to_connect graph in
    let dispatcher = Bonsai.Effect_throttling.poll dispatcher graph in
    generic_poll_or_error
      ~rpc_kind:
        (let%arr.Bonsai every in
         Rpc_effect_protocol.Rpc_kind.Normal
           { name = Rpc.Rpc.name rpc
           ; version = Rpc.Rpc.version rpc
           ; interval = Poll { every }
           })
      ~sexp_of_query
      ~sexp_of_underlying:sexp_of_response
      ~sexp_of_response
      ~equal_query
      ?equal_response
      ?clear_when_deactivated
      ?on_response_received
      ~where_to_connect
      dispatcher
      ~every
      ~poll_behavior:Always
      ~get_response:Fn.id
      ~here
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
    ?on_response_received
    rpc
    ~where_to_connect
    ~every
    ~output_type
    query
    (local_ graph)
    =
    let dispatcher = babel_dispatcher_internal rpc ~where_to_connect graph in
    let dispatcher = Bonsai.Effect_throttling.poll dispatcher graph in
    generic_poll_or_error
      ~rpc_kind:
        (let%arr.Bonsai every in
         Rpc_effect_protocol.Rpc_kind.Babel
           { descriptions = Babel.Caller.descriptions rpc; interval = Poll { every } })
      ~sexp_of_query
      ~sexp_of_underlying:sexp_of_response
      ~sexp_of_response
      ~equal_query
      ?equal_response
      ?clear_when_deactivated
      ?on_response_received
      ~where_to_connect
      dispatcher
      ~every
      ~get_response:Fn.id
      ~poll_behavior:Always
      ~output_type
      ~here
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
    ?on_response_received
    rpc
    ~where_to_connect
    ~every
    ~output_type
    query
    (local_ graph)
    =
    let dispatcher = streamable_dispatcher_internal rpc ~where_to_connect graph in
    let dispatcher = Bonsai.Effect_throttling.poll dispatcher graph in
    generic_poll_or_error
      ~rpc_kind:
        (let%arr.Bonsai every in
         let%tydi { name; version } = Streamable.Plain_rpc.description rpc in
         Rpc_effect_protocol.Rpc_kind.Streamable
           { name; version; interval = Poll { every } })
      ~sexp_of_query
      ~sexp_of_underlying:sexp_of_response
      ~sexp_of_response
      ~equal_query
      ?equal_response
      ?clear_when_deactivated
      ?on_response_received
      ~where_to_connect
      dispatcher
      ~every
      ~poll_behavior:Always
      ~get_response:Fn.id
      ~output_type
      ~here
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
    ?on_response_received
    rpc
    ~where_to_connect
    ~retry_interval
    ~output_type
    query
    (local_ graph)
    =
    let dispatcher = streamable_dispatcher_internal rpc ~where_to_connect graph in
    let dispatcher = Bonsai.Effect_throttling.poll dispatcher graph in
    generic_poll_or_error
      ~rpc_kind:
        (let%arr.Bonsai retry_interval in
         let%tydi { name; version } = Streamable.Plain_rpc.description rpc in
         Rpc_effect_protocol.Rpc_kind.Streamable
           { name; version; interval = Poll_until_ok { retry_interval } })
      ~sexp_of_query
      ~sexp_of_underlying:sexp_of_response
      ~sexp_of_response
      ~equal_query
      ?equal_response
      ?clear_when_deactivated
      ?on_response_received
      ~where_to_connect
      dispatcher
      ~every:retry_interval
      ~poll_behavior:Until_ok
      ~get_response:Fn.id
      ~output_type
      ~here
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
    ?on_response_received
    rpc
    ~where_to_connect
    ~every
    =
    let module M = struct
      include Q

      let equal a b = (Comparator.compare Q.comparator) a b = 0
    end
    in
    Shared_poller.create (module Q) ~f:(fun query ->
      poll
        ~here
        ~sexp_of_query:(Comparator.sexp_of_t M.comparator)
        ?sexp_of_response
        ~equal_query:M.equal
        ?equal_response
        ?clear_when_deactivated
        ?on_response_received
        rpc
        ~where_to_connect
        ~every
        ~output_type:Abstract
        query)
  ;;

  let poll_until_ok
    ~(here : [%call_pos])
    ?sexp_of_query
    ?sexp_of_response
    ~equal_query
    ?equal_response
    ?clear_when_deactivated
    ?on_response_received
    rpc
    ~where_to_connect
    ~retry_interval
    ~output_type
    query
    (local_ graph)
    =
    let dispatcher = dispatcher_internal rpc ~where_to_connect graph in
    let dispatcher = Bonsai.Effect_throttling.poll dispatcher graph in
    generic_poll_or_error
      ~rpc_kind:
        (let%arr.Bonsai retry_interval in
         Rpc_effect_protocol.Rpc_kind.Normal
           { name = Rpc.Rpc.name rpc
           ; version = Rpc.Rpc.version rpc
           ; interval = Poll_until_ok { retry_interval }
           })
      ~sexp_of_query
      ~sexp_of_underlying:sexp_of_response
      ~sexp_of_response
      ~equal_query
      ?equal_response
      ?clear_when_deactivated
      ?on_response_received
      ~where_to_connect
      dispatcher
      ~every:retry_interval
      ~poll_behavior:Until_ok
      ~get_response:Fn.id
      ~output_type
      ~here
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
    ?on_response_received
    rpc
    ~where_to_connect
    ~every
    ~condition
    ~output_type
    query
    (local_ graph)
    =
    let dispatcher = dispatcher_internal rpc ~where_to_connect graph in
    let dispatcher = Bonsai.Effect_throttling.poll dispatcher graph in
    generic_poll_or_error
      ~rpc_kind:
        (let%arr.Bonsai every in
         Rpc_effect_protocol.Rpc_kind.Normal
           { name = Rpc.Rpc.name rpc
           ; version = Rpc.Rpc.version rpc
           ; interval = Poll_until_condition_met { every }
           })
      ~sexp_of_query
      ~sexp_of_underlying:sexp_of_response
      ~sexp_of_response
      ~equal_query
      ?equal_response
      ?clear_when_deactivated
      ?on_response_received
      ~where_to_connect
      dispatcher
      ~every
      ~poll_behavior:(Until_condition_met condition)
      ~get_response:Fn.id
      ~output_type
      query
      ~here
      graph
  ;;

  let babel_poll_until_ok
    ~(here : [%call_pos])
    ?sexp_of_query
    ?sexp_of_response
    ~equal_query
    ?equal_response
    ?clear_when_deactivated
    ?on_response_received
    rpc
    ~where_to_connect
    ~retry_interval
    ~output_type
    query
    (local_ graph)
    =
    let dispatcher = babel_dispatcher_internal rpc ~where_to_connect graph in
    let dispatcher = Bonsai.Effect_throttling.poll dispatcher graph in
    generic_poll_or_error
      ~rpc_kind:
        (let%arr.Bonsai retry_interval in
         Rpc_effect_protocol.Rpc_kind.Babel
           { descriptions = Babel.Caller.descriptions rpc
           ; interval = Poll_until_ok { retry_interval }
           })
      ~sexp_of_query
      ~sexp_of_underlying:sexp_of_response
      ~sexp_of_response
      ~equal_query
      ?equal_response
      ?clear_when_deactivated
      ?on_response_received
      ~where_to_connect
      dispatcher
      ~every:retry_interval
      ~poll_behavior:Until_ok
      ~get_response:Fn.id
      ~output_type
      ~here
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
    ?on_response_received
    rpc
    ~where_to_connect
    ~every
    ~condition
    ~output_type
    query
    (local_ graph)
    =
    let dispatcher = babel_dispatcher_internal rpc ~where_to_connect graph in
    let dispatcher = Bonsai.Effect_throttling.poll dispatcher graph in
    generic_poll_or_error
      ~rpc_kind:
        (let%arr.Bonsai every in
         Rpc_effect_protocol.Rpc_kind.Babel
           { descriptions = Babel.Caller.descriptions rpc
           ; interval = Poll_until_condition_met { every }
           })
      ~sexp_of_query
      ~sexp_of_underlying:sexp_of_response
      ~sexp_of_response
      ~equal_query
      ?equal_response
      ?clear_when_deactivated
      ?on_response_received
      ~where_to_connect
      dispatcher
      ~every
      ~poll_behavior:(Until_condition_met condition)
      ~get_response:Fn.id
      ~output_type
      ~here
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
    ?on_response_received
    rpc
    ~where_to_connect
    (local_ graph)
    =
    let open Bonsai.Let_syntax in
    let dispatcher = dispatcher_internal rpc ~where_to_connect graph in
    let dispatcher =
      let%arr dispatcher in
      fun query ->
        match%map.Effect dispatcher query with
        | Ok result -> Bonsai.Effect_throttling.Poll_result.Finished (Ok result)
        | Error _ as error -> Finished error
    in
    generic_polling_state_machine
      ~here
      ~rpc_kind:
        (Bonsai.return
           (Rpc_effect_protocol.Rpc_kind.Normal
              { name = Rpc.Rpc.name rpc
              ; version = Rpc.Rpc.version rpc
              ; interval = Dispatch
              }))
      ~sexp_of_query
      ~sexp_of_response
      ~sexp_of_underlying:sexp_of_response
      ~equal_query
      ?equal_response
      ?clear_when_deactivated
      ?on_response_received
      dispatcher
      ~get_response:Fn.id
      graph
  ;;

  let maybe_track
    ~here
    ~sexp_of_query
    ~sexp_of_response
    ~rpc_kind
    ~get_response
    dispatcher
    (local_ graph)
    =
    let open Bonsai.Let_syntax in
    let (module Rpc_effect_introspection) = !introspection in
    let get_current_time = Bonsai.Clock.get_current_time graph in
    let path = Bonsai.path_id graph in
    let effect =
      let%arr dispatcher and get_current_time and path and rpc_kind in
      fun query ->
        match%bind.Effect Rpc_effect_introspection.should_record_effect with
        | false ->
          let%map.Effect response = dispatcher (query, None) in
          Or_error.map response ~f:get_response
        | true ->
          Rpc_effect_introspection.send_and_track_rpc_from_dispatch
            ~rpc_kind
            ~get_current_time
            ~sexp_of_query
            ~sexp_of_response
            ~path
            ~send_rpc:dispatcher
            ~get_response
            ~query
            ~here
    in
    let%arr rpc_kind and effect in
    Rpc_effect_introspection.time_rpc_effect ~rpc_kind effect
  ;;

  let dispatcher
    ~(here : [%call_pos])
    ?sexp_of_query
    ?sexp_of_response
    rpc
    ~where_to_connect
    (local_ graph)
    =
    let dispatcher = dispatcher_internal rpc ~where_to_connect graph in
    maybe_track
      ~sexp_of_query
      ~sexp_of_response
      ~rpc_kind:
        (Bonsai.return
           (Rpc_effect_protocol.Rpc_kind.Normal
              { name = Rpc.Rpc.name rpc
              ; version = Rpc.Rpc.version rpc
              ; interval = Dispatch
              }))
      ~get_response:Fn.id
      dispatcher
      ~here
      graph
  ;;

  let streamable_dispatcher
    ~(here : [%call_pos])
    ?sexp_of_query
    ?sexp_of_response
    rpc
    ~where_to_connect
    (local_ graph)
    =
    let dispatcher = streamable_dispatcher_internal rpc ~where_to_connect graph in
    maybe_track
      ~sexp_of_query
      ~sexp_of_response
      ~rpc_kind:
        (Bonsai.return
           (let%tydi { name; version } = Streamable.Plain_rpc.description rpc in
            Rpc_effect_protocol.Rpc_kind.Streamable { name; version; interval = Dispatch }))
      ~get_response:Fn.id
      ~here
      dispatcher
      graph
  ;;

  let babel_dispatcher
    ~(here : [%call_pos])
    ?sexp_of_query
    ?sexp_of_response
    rpc
    ~where_to_connect
    (local_ graph)
    =
    let dispatcher = babel_dispatcher_internal rpc ~where_to_connect graph in
    maybe_track
      ~sexp_of_query
      ~sexp_of_response
      ~rpc_kind:
        (Bonsai.return
           (Rpc_effect_protocol.Rpc_kind.Babel
              { descriptions = Babel.Caller.descriptions rpc; interval = Dispatch }))
      ~get_response:Fn.id
      ~here
      dispatcher
      graph
  ;;
end

module Polling_state_rpc = struct
  let dispatcher'
    ~sexp_of_response
    ?(on_forget_client_error = fun _ -> Effect.Ignore)
    create_client_rvar
    ~destroy_after_forget
    ~where_to_connect
    (local_ graph)
    =
    let open Bonsai.Let_syntax in
    let connector = Bonsai.Dynamic_scope.lookup connector_var graph in
    let (module Rpc_effect_introspection) = !introspection in
    let client_rvar = create_client_rvar ~connector graph in
    let forget_client_on_server =
      let perform_dispatch (connector, client_rvar, where_to_connect) =
        Connector.with_connection connector ~where_to_connect ~callback:(fun connection ->
          let%bind.Eager_deferred.Or_error client = Rvar.contents client_rvar in
          match%map.Deferred
            Polling_state_rpc.Client.forget_on_server client connection
          with
          | Ok () -> Ok ()
          | Error _ when Rpc.Connection.is_closed connection ->
            (* If the connection is closed, then any data for this
               connection has been forgotten by the server anyway, so
               the error is moot. *)
            Ok ()
          | Error error -> Error error)
      in
      let%arr connector and client_rvar and where_to_connect in
      let%bind.Effect () =
        match%bind.Effect
          Ui_effect_of_deferred.of_deferred_fun
            perform_dispatch
            (connector, client_rvar, where_to_connect)
        with
        | Ok () -> Effect.Ignore
        | Error error -> on_forget_client_error error
      in
      if destroy_after_forget
      then Effect.of_thunk (fun () -> Rvar.destroy client_rvar)
      else Effect.Ignore
    in
    let () = Bonsai.Edge.lifecycle ~on_deactivate:forget_client_on_server graph in
    let () =
      Bonsai.Edge.on_change'
        ~equal:[%equal: Where_to_connect.t]
        where_to_connect
        ~callback:
          (let%arr forget_client_on_server in
           fun prev _new ->
             match prev with
             | None -> Effect.Ignore
             | Some _ -> forget_client_on_server)
        graph
    in
    let perform_query (connector, client, where_to_connect) (query, id) =
      Connector.with_connection connector ~where_to_connect ~callback:(fun connection ->
        let%bind.Eager_deferred.Or_error client = Rvar.contents client in
        match%map.Eager_deferred
          Polling_state_rpc.Client.For_introspection.dispatch_with_underlying_diff_as_sexp
            ?sexp_of_response
            ~on_dispatch:(fun () -> Rpc_effect_introspection.just_sent_query_with_id id)
            client
            connection
            query
        with
        | `Ok (Ok result) -> Ok (Bonsai.Effect_throttling.Poll_result.Finished result)
        | `Ok (Error _ as error) -> error
        | `Raised exn -> Error (Error.of_exn exn)
        | `Aborted -> Ok Bonsai.Effect_throttling.Poll_result.Aborted)
    in
    let%arr connector and client_rvar and where_to_connect in
    Ui_effect_of_deferred.of_deferred_fun
      (perform_query (connector, client_rvar, where_to_connect))
  ;;

  let babel_dispatcher_internal ?on_forget_client_error caller ~where_to_connect =
    let create_client_rvar ~connector (local_ _graph) =
      let%arr.Bonsai connector and where_to_connect in
      match Connector.menu_rvar (connector where_to_connect) with
      | None -> raise_s [%message [%here]]
      | Some menu_rvar ->
        Rvar.derived menu_rvar (fun _ ->
          Connector.with_connection_with_menu
            connector
            ~where_to_connect
            ~callback:(fun connection_with_menu ->
              Versioned_polling_state_rpc.Client.negotiate_client
                caller
                connection_with_menu
              |> Deferred.return))
    in
    dispatcher'
      ?on_forget_client_error
      ~destroy_after_forget:true
      ~where_to_connect
      create_client_rvar
  ;;

  let dispatcher_internal ?on_forget_client_error rpc ~where_to_connect =
    let create_client_rvar ~connector:_ (local_ graph) =
      Bonsai.Expert.thunk
        ~f:(fun () -> Rvar.const (Polling_state_rpc.Client.create rpc))
        graph
    in
    dispatcher'
      ?on_forget_client_error
      ~destroy_after_forget:false
      ~where_to_connect
      create_client_rvar
  ;;

  let generic_poll
    ~(here : [%call_pos])
    ?sexp_of_query
    ?sexp_of_response
    ~sexp_of_underlying
    ~equal_query
    ~rpc_kind
    ?equal_response
    ?clear_when_deactivated
    ?on_response_received
    ~where_to_connect
    ?when_to_start_next_effect
    ~every
    ~get_response
    ~output_type
    query
    ~dispatcher
    (local_ graph)
    =
    let open Bonsai.Let_syntax in
    let dispatcher =
      let%arr dispatcher in
      fun query ->
        match%map.Effect dispatcher query with
        | Ok (Bonsai.Effect_throttling.Poll_result.Aborted as aborted) -> aborted
        | Ok (Finished result) -> Finished (Ok result)
        | Error _ as error -> Finished error
    in
    generic_poll_or_error
      ~here
      ~rpc_kind
      ~sexp_of_query
      ~sexp_of_underlying
      ~sexp_of_response
      ~equal_query
      ?equal_response
      ?clear_when_deactivated
      ?on_response_received
      dispatcher
      ~where_to_connect
      ?when_to_start_next_effect
      ~every
      ~poll_behavior:Always
      ~get_response
      ~output_type
      query
      graph
  ;;

  let poll
    ~(here : [%call_pos])
    ?sexp_of_query
    ?sexp_of_response
    ~equal_query
    ?equal_response
    ?clear_when_deactivated
    ?on_response_received
    rpc
    ~where_to_connect
    ?when_to_start_next_effect
    ~every
    ~output_type
    query
    (local_ graph)
    =
    let dispatcher = dispatcher_internal ~sexp_of_response rpc ~where_to_connect graph in
    generic_poll
      ~here
      ~rpc_kind:
        (let%arr.Bonsai every in
         Rpc_effect_protocol.Rpc_kind.Polling_state_rpc
           { name = Polling_state_rpc.name rpc
           ; version = Polling_state_rpc.version rpc
           ; interval = Poll { every }
           })
      ?sexp_of_query
      ?sexp_of_response
      ~sexp_of_underlying:(Some sexp_of_polling_state_rpc_underlying_response)
      ~equal_query
      ?equal_response
      ?clear_when_deactivated
      ?on_response_received
      ~where_to_connect
      ?when_to_start_next_effect
      ~every
      ~get_response:fst
      ~output_type
      query
      ~dispatcher
      graph
  ;;

  let babel_poll
    ~(here : [%call_pos])
    ?sexp_of_query
    ?sexp_of_response
    ~equal_query
    ?equal_response
    ?clear_when_deactivated
    ?on_response_received
    rpc
    ~where_to_connect
    ?when_to_start_next_effect
    ~every
    ~output_type
    query
    (local_ graph)
    =
    let dispatcher =
      babel_dispatcher_internal ~sexp_of_response rpc ~where_to_connect graph
    in
    generic_poll
      ~rpc_kind:
        (let%arr.Bonsai every in
         Rpc_effect_protocol.Rpc_kind.Babel
           { descriptions = Babel.Caller.descriptions rpc; interval = Poll { every } })
      ?sexp_of_query
      ?sexp_of_response
      ~sexp_of_underlying:(Some sexp_of_polling_state_rpc_underlying_response)
      ~equal_query
      ?equal_response
      ?clear_when_deactivated
      ?on_response_received
      ~where_to_connect
      ?when_to_start_next_effect
      ~every
      ~output_type
      ~here
      query
      ~dispatcher
      ~get_response:fst
      graph
  ;;

  let collapse_sequencer_error dispatcher query =
    match%map.Effect dispatcher query with
    | Error _ as error -> error
    | Ok (Bonsai.Effect_throttling.Poll_result.Finished x) -> Ok x
    | Ok Aborted -> Or_error.error_string "Request aborted"
  ;;

  let dispatcher
    ~(here : [%call_pos])
    ?sexp_of_query
    ?sexp_of_response
    ?on_forget_client_error
    rpc
    ~where_to_connect
    (local_ graph)
    =
    let open Bonsai.Let_syntax in
    let dispatcher =
      dispatcher_internal
        ~sexp_of_response
        ?on_forget_client_error
        rpc
        ~where_to_connect
        graph
      >>| collapse_sequencer_error
    in
    Our_rpc.maybe_track
      ~here
      ~sexp_of_query
      ~sexp_of_response:(Some sexp_of_polling_state_rpc_underlying_response)
      ~rpc_kind:
        (Bonsai.return
           (Rpc_effect_protocol.Rpc_kind.Polling_state_rpc
              { name = Polling_state_rpc.name rpc
              ; version = Polling_state_rpc.version rpc
              ; interval = Dispatch
              }))
      ~get_response:fst
      dispatcher
      graph
  ;;

  let babel_dispatcher
    ~(here : [%call_pos])
    ?sexp_of_query
    ?sexp_of_response
    ?on_forget_client_error
    caller
    ~where_to_connect
    (local_ graph)
    =
    let open Bonsai.Let_syntax in
    let dispatcher =
      babel_dispatcher_internal
        ~sexp_of_response
        ?on_forget_client_error
        caller
        ~where_to_connect
        graph
      >>| collapse_sequencer_error
    in
    Our_rpc.maybe_track
      ~here
      ~sexp_of_query
      ~sexp_of_response:(Some sexp_of_polling_state_rpc_underlying_response)
      ~rpc_kind:
        (Bonsai.return
           (Rpc_effect_protocol.Rpc_kind.Babel_polling_state_rpc
              { descriptions = Babel.Caller.descriptions caller; interval = Dispatch }))
      ~get_response:fst
      dispatcher
      graph
  ;;

  let shared_poller
    (type q cmp)
    ~(here : [%call_pos])
    (module Q : Comparator.S with type t = q and type comparator_witness = cmp)
    ?sexp_of_response
    ?equal_response
    ?clear_when_deactivated
    ?on_response_received
    rpc
    ~where_to_connect
    ~every
    =
    let module M = struct
      include Q

      let equal a b = (Comparator.compare Q.comparator) a b = 0
    end
    in
    Shared_poller.create (module Q) ~f:(fun query ->
      poll
        ~sexp_of_query:(Comparator.sexp_of_t M.comparator)
        ?sexp_of_response
        ~equal_query:[%equal: M.t]
        ?equal_response
        ?clear_when_deactivated
        ?on_response_received
        rpc
        ~where_to_connect
        ~every
        ~output_type:Abstract
        ~here
        query)
  ;;

  let manual_poll
    ~(here : [%call_pos])
    ?sexp_of_query
    ?sexp_of_response
    ~equal_query
    ?equal_response
    ?clear_when_deactivated
    ?on_response_received
    rpc
    ~where_to_connect
    (local_ graph)
    =
    let open Bonsai.Let_syntax in
    let dispatcher = dispatcher_internal ~sexp_of_response rpc ~where_to_connect graph in
    let dispatcher =
      let%arr dispatcher in
      fun query ->
        match%map.Effect dispatcher query with
        | Ok (Bonsai.Effect_throttling.Poll_result.Aborted as aborted) -> aborted
        | Ok (Finished result) -> Finished (Ok result)
        | Error _ as error -> Finished error
    in
    let%sub poll_accumulator, poll_effect_with_response =
      generic_polling_state_machine
        ~here
        ~rpc_kind:
          (Bonsai.return
             (Rpc_effect_protocol.Rpc_kind.Polling_state_rpc
                { name = Polling_state_rpc.name rpc
                ; version = Polling_state_rpc.version rpc
                ; interval = Dispatch
                }))
        ~sexp_of_query
        ~sexp_of_response
        ~sexp_of_underlying:(Some sexp_of_polling_state_rpc_underlying_response)
        ~equal_query
        ?equal_response
        ?clear_when_deactivated
        ?on_response_received
        dispatcher
        ~get_response:fst
        graph
    in
    let%arr poll_accumulator and poll_effect_with_response in
    poll_accumulator, poll_effect_with_response
  ;;
end

module Status = struct
  open Bonsai.Let_syntax

  module State = struct
    type t =
      | Connecting
      | Connected
      | Disconnected of Error.t
      | Failed_to_connect of Error.t
    [@@deriving sexp, equal]
  end

  (* This is a weird "dispatcher" component because it doesn't try to send an RPC
     at all; it only tries to make the connection, making not of all the events
     that occurred in the process. *)
  let dispatcher ~where_to_connect =
    Our_rpc.generic_dispatcher
      ~where_to_connect
      (fun connector ~where_to_connect (writeback : State.t -> unit) ->
         match%map.Deferred
           Connector.with_connection
             connector
             ~where_to_connect
             ~callback:(fun connection ->
               writeback Connected;
               upon
                 (Rpc.Connection.close_reason connection ~on_close:`started)
                 (fun reason -> writeback (Disconnected (Error.of_info reason)));
               Deferred.Or_error.return ())
         with
         | Ok () -> ()
         | Error error ->
           (* We know that an error indicates a failure to connect because
           [callback] never returns an error of its own. *)
           writeback (Failed_to_connect error))
  ;;

  module Model = struct
    type state =
      | Initial
      | State of State.t
    [@@deriving sexp, equal]

    type nonrec t =
      { state : state
      ; clock : (Bonsai.Time_source.t option[@sexp.opaque] [@equal.ignore])
      ; connecting_since : Time_ns.Alternate_sexp.t option
      }
    [@@deriving sexp, equal]
  end

  module Action = struct
    type nonrec t =
      | Set of State.t
      | Activate of (Bonsai.Time_source.t[@sexp.opaque])
      | Where_to_connect_changed
    [@@deriving sexp_of]
  end

  module Result = struct
    type t =
      { state : State.t
      ; connecting_since : Time_ns.Alternate_sexp.t option
      }
    [@@deriving sexp_of]
  end

  let state' ~where_to_connect (local_ graph) =
    let dispatcher = dispatcher ~where_to_connect graph in
    let model, inject =
      Bonsai.state_machine_with_input
        ~sexp_of_model:[%sexp_of: Model.t]
        ~equal:[%equal: Model.t]
        ~sexp_of_action:[%sexp_of: Action.t]
        dispatcher
        ~default_model:{ state = Initial; clock = None; connecting_since = None }
        ~apply_action:(fun context dispatcher model action ->
          let writeback a =
            Bonsai.Apply_action_context.schedule_event
              context
              (Bonsai.Apply_action_context.inject context (Set a))
          in
          let state = model.state in
          let new_state =
            match action, dispatcher with
            | Where_to_connect_changed, _ ->
              (* If we return to the original [where_to_connect], we should behave as if
                 we were still in the initial state. *)
              Model.Initial
            | Activate _, Inactive ->
              (* The activate message got to us, but we became inactive in the interim *)
              state
            | Activate _, Active dispatch ->
              (match state with
               | Initial | State (Disconnected _ | Failed_to_connect _) ->
                 Bonsai.Apply_action_context.schedule_event context (dispatch writeback);
                 State Connecting
               | State (Connecting | Connected) ->
                 (* We got activated, but we're still listening to the previous connection. *)
                 state)
            | Set new_state, Active dispatch ->
              (match new_state with
               | Failed_to_connect _ | Disconnected _ ->
                 (* we failed, but we're still active, so try to reconnect *)
                 Bonsai.Apply_action_context.schedule_event context (dispatch writeback)
               | Connected | Connecting -> ());
              State new_state
            | Set new_state, Inactive -> State new_state
          in
          let clock =
            match action with
            | Activate clock -> Some clock
            | Set _ | Where_to_connect_changed -> model.clock
          in
          let connecting_since =
            let now () = Option.map ~f:Bonsai.Time_source.now clock in
            match state with
            | State Connected ->
              (match new_state with
               | State Connected -> model.connecting_since
               | Initial | State (Connecting | Disconnected _ | Failed_to_connect _) ->
                 now ())
            | Initial -> now ()
            | State _ -> model.connecting_since
          in
          { state = new_state; clock; connecting_since })
        graph
    in
    let () =
      let clock = Bonsai.Incr.with_clock ~f:Ui_incr.return graph in
      let on_activate =
        let%arr inject and clock in
        inject (Activate clock)
      in
      Bonsai.Edge.lifecycle ~on_activate graph
    in
    let%arr { Model.state; connecting_since; _ } = model
    and inject in
    let state =
      match state with
      | State status -> status
      | Initial -> Connecting
    in
    let connecting_since =
      match state with
      | Connected -> None
      | Connecting | Disconnected _ | Failed_to_connect _ -> connecting_since
    in
    { Result.state; connecting_since }, inject
  ;;

  module Where_to_connect_comparator = struct
    include Where_to_connect
    include Comparator.Make (Where_to_connect)
  end

  let state ~where_to_connect graph =
    let%sub result, inject =
      Bonsai.scope_model
        (module Where_to_connect_comparator)
        ~on:where_to_connect
        ~for_:(fun (local_ graph) -> state' ~where_to_connect graph)
        graph
    in
    (* We want connection status to reset immediately, without needing to wait a frame
       for an [on_change] to take effect. We do so with:
       * A [scope_model], which swaps out our state immediately.
       * An on_change, which instructs the old, now-inactive state machine to reset itself
         back to "initial" state. *)
    let () =
      let where_to_connect_with_injector = Bonsai.both where_to_connect inject in
      Bonsai.Edge.on_change'
        ~equal:(fun (a, _) (b, _) -> Where_to_connect.equal a b)
        where_to_connect_with_injector
        ~callback:
          (Bonsai.return (fun prev _new ->
             match prev with
             | None -> Effect.Ignore
             | Some (_, prev_inject) -> prev_inject Action.Where_to_connect_changed))
        graph
    in
    result
  ;;

  let on_change ~where_to_connect ~callback graph =
    let%sub { state; _ } = state ~where_to_connect graph in
    Bonsai.Edge.on_change state ~equal:State.equal ~callback graph
  ;;

  include Result
end

module Rpc = Our_rpc
