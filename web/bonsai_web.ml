open! Core
open! Js_of_ocaml

(* [Bonsai.For_open] provides an [Effect] module, but we want to export the [Effect]
   module from this library.  This [open struct] allows us to rename [Effect] to
   [Bonsai_web_effect] and restore it after opening [Bonsai.For_open], without exposing
   [Bonsai_web_effect] to users of [Bonsai_web]. *)
open struct
  module Bonsai_web_effect = Effect
end

include Bonsai.For_open
include Util
module Effect = Bonsai_web_effect
module Vdom = Import.Vdom
include Vdom.Html_syntax
module Start_for_arrow_deprecated = Start_via_incr_dom.For_arrow_deprecated
module Driver = Driver
module Test_selector = Test_selector

module Bonsai = struct
  include Bonsai.Cont
  module Test_selector = Test_selector
end

module Incr = Import.Incr
module View = Bonsai_web_ui_view
module To_incr_dom = To_incr_dom
module Persistent_var = Persistent_var
module Rpc_effect = Rpc_effect

module Start = struct
  let start
    ?(use_new_experimental_implementation = false)
    ?custom_connector
    ?bind_to_element_with_id
    ?simulate_body_focus_on_root_element
    ?time_source
    ?optimize
    app
    =
    if use_new_experimental_implementation
    then
      Start_experimental.start
        ?custom_connector
        ?bind_to_element_with_id
        ?simulate_body_focus_on_root_element
        ?time_source
        ?optimize
        app
    else
      Start_via_incr_dom.start
        ?custom_connector
        ?bind_to_element_with_id
        ?simulate_body_focus_on_root_element
        ?time_source
        ?optimize
        app
  ;;

  module Handle = struct
    type ('extra, 'incoming) t =
      | Via_incr_dom of ('extra, 'incoming) Start_via_incr_dom.Handle.t
      | Experimental of ('extra, 'incoming) Start_experimental.Handle.t

    let stop = function
      | Via_incr_dom handle -> Start_via_incr_dom.Handle.stop handle
      | Experimental handle -> Start_experimental.Handle.stop handle
    ;;

    let started = function
      | Via_incr_dom handle -> Start_via_incr_dom.Handle.started handle
      | Experimental handle -> Start_experimental.Handle.started handle
    ;;

    let schedule = function
      | Via_incr_dom handle -> Start_via_incr_dom.Handle.schedule handle
      | Experimental handle -> Start_experimental.Handle.schedule handle
    ;;

    let extra = function
      | Via_incr_dom handle -> Start_via_incr_dom.Handle.extra handle
      | Experimental handle -> Start_experimental.Handle.extra handle
    ;;

    let last_extra = function
      | Via_incr_dom handle -> Start_via_incr_dom.Handle.last_extra handle
      | Experimental handle -> Start_experimental.Handle.last_extra handle
    ;;
  end

  module Result_spec = Start_via_incr_dom.Result_spec

  let start_and_get_handle
    ?(use_new_experimental_implementation = false)
    result_spec
    ?optimize
    ?custom_connector
    ?simulate_body_focus_on_root_element
    ?time_source
    ~bind_to_element_with_id
    app
    =
    if use_new_experimental_implementation
    then
      Start_experimental.start_and_get_handle
        result_spec
        ?optimize
        ?custom_connector
        ?simulate_body_focus_on_root_element
        ?time_source
        ~bind_to_element_with_id
        app
      |> Handle.Experimental
    else
      Start_via_incr_dom.start_and_get_handle
        result_spec
        ?optimize
        ?custom_connector
        ?simulate_body_focus_on_root_element
        ?time_source
        ~bind_to_element_with_id
        app
      |> Handle.Via_incr_dom
  ;;
end
