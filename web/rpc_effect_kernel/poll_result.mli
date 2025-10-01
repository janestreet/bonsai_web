open! Core
open Bonsai.For_open

(** The various rpc polling functions in [Rpc_effect] take a [Poll_result.Output_type.t]
    and return the corresponding output type. Some output types are the modules below;
    others are specific parts of a poll result, such as the last OK response.

    [Pending_or_error.t], [Response_state.t], and [Response_state_with_details.t] all
    represent the state as a variant. However, a [Pending] state (from [Pending_or_error])
    occurs both during the initial query and whenever the query changes, while a
    [No_response_yet] state (from [Response_state]) only occurs during the initial query.
    Semantically, you can think of [Pending_or_error] as tracking responses from the
    currently active query, and [Response_state] as tracking responses in general.

    If you want something particular (e.g. last response) and you don't care as much about
    the state, try using a specific [Output_type] (e.g. [Last_ok_response]) instead of
    matching.

    Can use like this to get a single output:

    {[
      let response =
        Rpc_effect.Rpc.poll
          ...
          ~output_type:Response_state_with_details
          ...
      in
      ...
    ]}

    Or this to get multiple outputs:

    {[
      let%sub [ last_ok_response; fetching_status ] =
        Rpc_effect.Rpc.poll
          ...
          ~output_type:[ Last_ok_response; Fetching_status ]
          ...
      in
      ...
    ]} *)

(** An abstract poll result. You can get more specific information out of it by calling
    [get_output]. *)
type ('query, 'response) t [@@deriving sexp_of]

module Fetching_status : sig
  type 'query t =
    | Not_fetching
    | Fetching of
        { query : 'query
        ; query_changed : bool
        (** True if the inflight query is either the first query being sent to the server
            or not equal to the most-recently-responded-to query. *)
        ; query_changed_since_last_ok : bool
        (** True if either there has been no OK response yet or the inflight query is not
            equal to the query corresponding to the last OK response. *)
        }
  [@@deriving sexp, equal, bin_io, diff]
end

(** [Pending]: The current query is in flight (or hasn't been sent yet) and has not
    returned a response.

    [Ok response]: The most recent response for the current query is [response].

    [Error error]: The most recent response for the current query is [error]. *)
module Pending_or_error = Pending_or_error

(** [No_response_yet]: There is no response yet (for any query).

    [Ok response]: The most recent response is [response].

    [Error { error; last_ok_response }]: The most recent response is [error], while the
    most recent ok response is [last_ok_response]. *)
module Response_state : sig
  type 'response t =
    | No_response_yet
    | Ok of 'response
    | Error of
        { error : Error.t
        ; last_ok_response : 'response option
        }
  [@@deriving sexp, equal, bin_io, diff]

  val map : 'a t -> f:('a -> 'b) -> 'b t
end

module Response_state_with_details : sig
  type ('query, 'response) ok_response =
    { response : 'response
    ; got_response_at : Time_ns.t
    ; query : 'query
    }
  [@@deriving sexp, equal, bin_io, diff]

  type ('query, 'response) t =
    | No_response_yet
    | Ok of ('query, 'response) ok_response
    | Error of
        { error : Error.t
        ; got_error_at : Time_ns.t
        ; query : 'query
        ; last_ok_response : ('query, 'response) ok_response option
        }
  [@@deriving sexp, equal, bin_io, diff]

  val map : ('query, 'a) t -> f:('a -> 'b) -> ('query, 'b) t
end

module Legacy_record : sig
  (** [last_ok_response] contains the most recent query/response pair that completed
      successfully, even if the RPC has returned errors since then.

      [last_error] contains the most recent query that produced an error, alongside the
      error that was returned. Unlike [last_ok_response], this field is set to [None] as
      soon a response completes sucessfully.

      [inflight_query] is [Some] when an a query has been dispatched, but has not
      completed yet.

      [refresh] can be used to manually redispatch the rpc. *)
  type ('query, 'response) t =
    { last_ok_response : ('query * 'response) option
    ; last_error : ('query * Error.t) option
    ; inflight_query : 'query option
    ; refresh : unit Effect.t
    }
  [@@deriving sexp_of]
end

module Raw_representation : sig
  (** The raw internal representation of [Poll_result.t]. Currently like
      [Legacy_record.t], but with some extra timestamp information + [equal_query]. *)
  type ('query, 'response) t =
    { last_ok_response : ('query * 'response * Time_ns.t) option
    ; last_error : ('query * Error.t * Time_ns.t) option
    ; inflight_query : ('query * Time_ns.t) option
    ; refresh : unit Effect.t
    ; equal_query : 'query -> 'query -> bool
    }
  [@@deriving sexp_of]
end

module Heterogeneous_list : sig
  type 'a t =
    | [] : unit t
    | ( :: ) : 'a * 'rest t -> ('a * 'rest) t
end

module Output_type : sig
  type ('query, 'response) poll_result := ('query, 'response) t

  (** We use the 'query and 'response type parameters to ensure type equality between
      query/response types used elsewhere as arguments and query/response types used in
      the output.

      There's a good chance you're looking for [Last_ok_response] (if you only want the
      response), [Response_state] (if you want to handle errors), or [Fetching_status] (if
      you care about the inflight query).

      Many of these have [with_details] variants, which supply a corresponding query and
      timestamp. *)
  type ('query, 'response, 'output) t =
    (* Reasonably full representations *)
    | Abstract : ('query, 'response, ('query, 'response) poll_result) t
    | Pending_or_error : ('query, 'response, 'response Pending_or_error.t) t
    | Response_state : ('query, 'response, 'response Response_state.t) t
    | Response_state_with_details :
        ('query, 'response, ('query, 'response) Response_state_with_details.t) t
    | Legacy_record : ('query, 'response, ('query, 'response) Legacy_record.t) t
    | Raw_representation : ('query, 'response, ('query, 'response) Raw_representation.t) t
    (* Assorted parts *)
    | Fetching_status : ('query, 'response, 'query Fetching_status.t) t
    (** Whether the server is currently processing a query. Contains both the inflight
        query itself and information about how the query has changed compared to queries
        for existing responses. *)
    | Fetching_status_with_details :
        ('query, 'response, ('query * Time_ns.t) Fetching_status.t) t
    (** Like [Fetching_status], but with a timestamp for when the query was sent. *)
    | Last_ok_response : ('query, 'response, 'response option) t
    (** The most recent [Ok] response. This could be the current response if we are [Ok],
        or a previous response if we are erroring. *)
    | Last_ok_response_with_details :
        ('query, 'response, ('query * 'response * Time_ns.t) option) t
    (** Like [Last_ok_response], but includes the corresponding query and timestamp
        attached to the response. *)
    | Error : ('query, 'response, Error.t option) t
    (** The current error, if we are in an error state. *)
    | Error_with_details : ('query, 'response, ('query * Error.t * Time_ns.t) option) t
    (** Like [Error], but includes the corresponding query and timestamp attached to the
        error. *)
    | Last_query : ('query, 'response, 'query option) t
    (** The query associated with the most recent response. *)
    | Refresh_effect : ('query, 'response, unit Effect.t) t
    (** [Refresh_effect] can be used to manually redispatch the RPC. *)
    (* Modifiers *)
    | Join_or_error : ('query, 'response, 'a) t -> ('query, 'response Or_error.t, 'a) t
    (** [Join_or_error] joins the [Or_error.t] in the response with the [Ok] and [Error]
        cases of the overall poll result state. Note that if the RPC successfully responds
        with an error, the [last_ok_response] will be [None].

        If [Pending_or_error] would've output a ['result Or_error.t Pending_or_error.t],
        [Join_or_error Pending_or_error] outputs a ['result Pending_or_error.t]. You can
        similarly use this to, say, get a ['result option] with
        [Join_or_error Last_ok_response] instead of a ['result Or_error.t option] with
        [Last_ok_response]. *)
    (* Support for multiple outputs *)
    | [] : ('query, 'response, unit Heterogeneous_list.t) t
    | ( :: ) :
        ('query, 'response, 'a) t * ('query, 'response, 'b Heterogeneous_list.t) t
        -> ('query, 'response, ('a * 'b) Heterogeneous_list.t) t
end

val get_output
  :  ('query, 'response) t
  -> output_type:('query, 'response, 'output) Output_type.t
  -> 'output

val empty : ('query, 'response) t

module Private : sig
  val create : ('query, 'response) Raw_representation.t -> ('query, 'response) t
end

module For_testing : sig
  val create
    :  ?last_ok_response:'query * 'response * Time_ns.t
    -> ?last_error:'query * Error.t * Time_ns.t
    -> ?inflight_query:'query * Time_ns.t
    -> ?equal_query:('query -> 'query -> bool)
    -> unit
    -> ('query, 'response) t
end
