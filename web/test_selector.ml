open! Core
open! Import
include Bonsai_web_test_selector

let attr_name = "data-bonsai-test-selector"

let attr t =
  let selector_data = For_bonsai_web.to_selector_data t in
  match selector_data with
  | "" -> Vdom.Attr.empty
  | selector_data -> Vdom.Attr.create attr_name selector_data
;;

let attr_of_opt = function
  | Some t -> attr t
  | None -> Vdom.Attr.empty
;;
