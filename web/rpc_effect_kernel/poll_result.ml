open! Core
open Bonsai.For_open
module Time_ns = Time_ns.Alternate_sexp

type ('query, 'response) t =
  { last_ok_response : ('query * 'response * Time_ns.t) option [@sexp.option]
  ; last_error : ('query * Error.t * Time_ns.t) option [@sexp.option]
  ; inflight_query : ('query * Time_ns.t) option [@sexp.option]
  ; refresh : (unit Effect.t[@sexp.opaque]) [@sexp_drop_if Fn.const true]
  ; equal_query : ('query -> 'query -> bool[@sexp.opaque]) [@sexp_drop_if Fn.const true]
  }
[@@deriving sexp_of]

module Fetching_status = struct
  type 'query t =
    | Not_fetching
    | Fetching of
        { query : 'query
        ; query_changed : bool
        ; query_changed_since_last_ok : bool
        }
  [@@deriving sexp, equal, bin_io, diff]
end

module Pending_or_error = Pending_or_error

(* Copied from [Pending_or_error]'s RPC helpers to ensure the semantics are the same. *)
let pending_or_error_of_abstract
  { last_ok_response; last_error; inflight_query; equal_query; _ }
  =
  let last_response =
    match last_error with
    | Some (failed_query, error, _) -> Some (failed_query, Error error)
    | None ->
      (match last_ok_response with
       | Some (ok_query, value, _) -> Some (ok_query, Ok value)
       | None -> None)
  in
  match inflight_query, last_response with
  | _, None ->
    (* no response yet: we are still loading *)
    Pending_or_error.Pending
  | Some (inflight_query, _), Some (query, response) when equal_query inflight_query query
    ->
    (* re-polling for the same query: our latest response is good *)
    Pending_or_error.of_or_error response
  | Some _, Some _ ->
    (* polling for a new query: ignore our latest response *)
    Pending_or_error.Pending
  | None, Some (_, response) ->
    (* not re-polling: our latest response is good *)
    Pending_or_error.of_or_error response
;;

module Response_state = struct
  type 'response t =
    | No_response_yet
    | Ok of 'response
    | Error of
        { error : Error.t
        ; last_ok_response : 'response option
        }
  [@@deriving sexp, equal, bin_io, diff]

  let map x ~f =
    match x with
    | No_response_yet -> No_response_yet
    | Ok response -> Ok (f response)
    | Error { error; last_ok_response } ->
      Error { error; last_ok_response = Option.map last_ok_response ~f }
  ;;

  let of_abstract = function
    | { last_ok_response = None; last_error = None; _ } -> No_response_yet
    | { last_error = Some (_, error, _); last_ok_response; _ } ->
      Error { error; last_ok_response = Option.map last_ok_response ~f:Tuple3.get2 }
    | { last_ok_response = Some (_, response, _); _ } -> Ok response
  ;;
end

module Response_state_with_details = struct
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

  let map_ok_response { response; got_response_at; query } ~f =
    { response = f response; got_response_at; query }
  ;;

  let map x ~f =
    match x with
    | No_response_yet -> No_response_yet
    | Ok response -> Ok (map_ok_response response ~f)
    | Error { error; got_error_at; query; last_ok_response } ->
      Error
        { error
        ; got_error_at
        ; query
        ; last_ok_response = Option.map last_ok_response ~f:(map_ok_response ~f)
        }
  ;;

  let response_to_ok_response = function
    | query, response, got_response_at -> { response; got_response_at; query }
  ;;

  let of_abstract = function
    | { last_ok_response = None; last_error = None; _ } -> No_response_yet
    | { last_error = Some (query, error, got_error_at); last_ok_response; _ } ->
      Error
        { error
        ; got_error_at
        ; query
        ; last_ok_response = Option.map last_ok_response ~f:response_to_ok_response
        }
    | { last_ok_response = Some response; _ } -> Ok (response_to_ok_response response)
  ;;
end

module Legacy_record = struct
  type ('query, 'response) poll_result = ('query, 'response) t

  type ('query, 'response) t =
    { last_ok_response : ('query * 'response) option
    ; last_error : ('query * Error.t) option
    ; inflight_query : 'query option
    ; refresh : (unit Effect.t[@sexp.opaque])
    }
  [@@deriving sexp_of]

  let of_abstract
    ({ last_ok_response; last_error; inflight_query; refresh; _ } : _ poll_result)
    =
    { last_ok_response = Option.map last_ok_response ~f:(fun (q, r, _) -> q, r)
    ; last_error = Option.map last_error ~f:(fun (q, e, _) -> q, e)
    ; inflight_query = Option.map inflight_query ~f:fst
    ; refresh
    }
  ;;
end

module Raw_representation = struct
  type nonrec ('query, 'response) t = ('query, 'response) t =
    { last_ok_response : ('query * 'response * Time_ns.t) option
    ; last_error : ('query * Error.t * Time_ns.t) option
    ; inflight_query : ('query * Time_ns.t) option
    ; refresh : unit Effect.t
    ; equal_query : 'query -> 'query -> bool
    }

  let sexp_of_t = sexp_of_t
  let of_abstract = Fn.id
end

module Heterogeneous_list = struct
  type 'a t =
    | [] : unit t
    | ( :: ) : 'a * 'rest t -> ('a * 'rest) t
end

module Output_type = struct
  type ('query, 'response, 'output) t =
    (* Reasonably full representations *)
    | Abstract : ('query, 'response, ('query, 'response) Raw_representation.t) t
    | Pending_or_error : ('query, 'response, 'response Pending_or_error.t) t
    | Response_state : ('query, 'response, 'response Response_state.t) t
    | Response_state_with_details :
        ('query, 'response, ('query, 'response) Response_state_with_details.t) t
    | Legacy_record : ('query, 'response, ('query, 'response) Legacy_record.t) t
    | Raw_representation : ('query, 'response, ('query, 'response) Raw_representation.t) t
    (* Assorted parts *)
    | Fetching_status : ('query, 'response, 'query Fetching_status.t) t
    | Fetching_status_with_details :
        ('query, 'response, ('query * Time_ns.t) Fetching_status.t) t
    | Last_ok_response : ('query, 'response, 'response option) t
    | Last_ok_response_with_details :
        ('query, 'response, ('query * 'response * Time_ns.t) option) t
    | Error : ('query, 'response, Error.t option) t
    | Error_with_details : ('query, 'response, ('query * Error.t * Time_ns.t) option) t
    | Last_query : ('query, 'response, 'query option) t
    | Refresh_effect : ('query, 'response, unit Effect.t) t
    (* Modifiers *)
    | Join_or_error : ('query, 'response, 'a) t -> ('query, 'response Or_error.t, 'a) t
    (* Support for multiple outputs *)
    | [] : ('query, 'response, unit Heterogeneous_list.t) t
    | ( :: ) :
        ('query, 'response, 'a) t * ('query, 'response, 'b Heterogeneous_list.t) t
        -> ('query, 'response, ('a * 'b) Heterogeneous_list.t) t
end

let get_output
  (type query response output)
  (t : (query, response) t)
  ~(output_type : (query, response, output) Output_type.t)
  : output
  =
  let rec loop
    : type query response output.
      (query, response) t -> (query, response, output) Output_type.t -> output
    =
    fun t -> function
    (* Reasonably full representations *)
    | Abstract -> t
    | Pending_or_error -> pending_or_error_of_abstract t
    | Response_state -> Response_state.of_abstract t
    | Response_state_with_details -> Response_state_with_details.of_abstract t
    | Legacy_record -> Legacy_record.of_abstract t
    | Raw_representation -> Raw_representation.of_abstract t
    (* Assorted parts *)
    | Fetching_status ->
      (match loop t Fetching_status_with_details with
       | Fetching ({ query = query, _; _ } as f) -> Fetching { f with query }
       | Not_fetching -> Not_fetching)
    | Fetching_status_with_details ->
      (match t.inflight_query with
       | Some ((new_query, _) as detailed_query) ->
         let query_changed =
           (let%map.Option last_query = loop t Last_query in
            not (t.equal_query new_query last_query))
           |> Option.value ~default:true
         in
         let query_changed_since_last_ok =
           (let%map.Option last_ok_query, _, _ = t.last_ok_response in
            not (t.equal_query new_query last_ok_query))
           |> Option.value ~default:true
         in
         Fetching { query = detailed_query; query_changed; query_changed_since_last_ok }
       | None -> Not_fetching)
    | Last_ok_response -> Option.map t.last_ok_response ~f:Tuple3.get2
    | Last_ok_response_with_details -> t.last_ok_response
    | Error -> Option.map t.last_error ~f:Tuple3.get2
    | Error_with_details -> t.last_error
    | Last_query ->
      (match t with
       | { last_error = Some (query, _, _); _ }
       | { last_ok_response = Some (query, _, _); _ } -> Some query
       | { last_ok_response = None; last_error = None; _ } -> None)
    | Refresh_effect -> t.refresh
    (* Modifiers *)
    | Join_or_error output_type ->
      let last_ok_response, last_error =
        match t with
        | { last_ok_response = Some (query, Ok response, timestamp); last_error; _ } ->
          Some (query, response, timestamp), last_error
        | { last_ok_response = Some (_, Error _, _)
          ; last_error = Some (query, error, timestamp)
          ; _
          }
        | { last_ok_response = Some (query, Error error, timestamp)
          ; last_error = None
          ; _
          } -> None, Some (query, error, timestamp)
        | { last_ok_response = None; last_error; _ } -> None, last_error
      in
      loop { t with last_ok_response; last_error } output_type
    (* Support for multiple outputs *)
    | [] -> []
    | output_type :: rest ->
      let a = loop t output_type in
      let b = loop t rest in
      a :: b
  in
  loop t output_type
;;

let empty =
  { last_ok_response = None
  ; last_error = None
  ; inflight_query = None
  ; refresh = Effect.Ignore
  ; equal_query = (fun _ _ -> false)
  }
;;

module Private = struct
  let create = Fn.id
end

module For_testing = struct
  let create
    ?last_ok_response
    ?last_error
    ?inflight_query
    ?(equal_query = fun _ _ -> false)
    ()
    =
    { last_ok_response
    ; last_error
    ; inflight_query
    ; refresh = Effect.print_s [%message "refreshed!"]
    ; equal_query
    }
  ;;
end
