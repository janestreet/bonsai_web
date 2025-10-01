open! Core

let escaping = String.substr_replace_all ~pattern:"\'" ~with_:""

type t = string

let make' ~(here : [%call_pos]) ~name bag_inst =
  if am_running_test
  then
    [%sexp
      { name : (string option[@sexp.option])
      ; here : Source_code_position.t
      ; bag_inst : (Sexp.t option[@sexp.option])
      }]
    |> Sexp.to_string_mach
    |> escaping
  else ""
;;

let make ~(here : [%call_pos]) ?name () = make' ~here ~name None

module For_bonsai_web = struct
  let attr_name = "data-bonsai-test-selector"
  let to_selector_data = Fn.id

  let css_selector t =
    match t with
    | "" -> ""
    | selector_data -> [%string "[%{attr_name}='%{selector_data}']"]
  ;;

  let our_filter_printed_attributes ~key ~data:_ =
    if String.equal key attr_name then false else true
  ;;

  let display = Fn.id
  let filter_printed_attributes = our_filter_printed_attributes

  let filter_printed_attributes_with_test_selector_filtering ~filter_printed_attributes =
    match filter_printed_attributes with
    | None -> our_filter_printed_attributes
    | Some filter_printed_attributes ->
      fun ~key ~data ->
        filter_printed_attributes ~key ~data && our_filter_printed_attributes ~key ~data
  ;;
end

module Keyed = struct
  type test_selector = t

  type 'a t =
    { selectors : test_selector Sexp.Map.t ref
    ; here : Source_code_position.t
    ; to_sexp : 'a -> Sexp.t
    ; name : string option
    }

  module type Of_sexpable = sig
    type t [@@deriving sexp_of]
  end

  let create ~(here : [%call_pos]) ?name (type a) (module M : Of_sexpable with type t = a)
    =
    { selectors = ref Sexp.Map.empty; here; to_sexp = M.sexp_of_t; name }
  ;;

  let get t a =
    if am_running_test
    then (
      let { selectors; here; to_sexp; name } = t in
      let sexp = to_sexp a in
      selectors
      := Map.update !selectors sexp ~f:(function
           | Some v -> v
           | None -> make' ~here ~name (Some sexp));
      Map.find_exn !selectors sexp)
    else ""
  ;;
end
