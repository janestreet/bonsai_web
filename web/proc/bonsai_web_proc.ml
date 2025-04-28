open! Core
open! Js_of_ocaml
module Start = Bonsai_web.Start

module Bonsai = struct
  include Bonsai_proc
  module Test_selector = Bonsai_web.Bonsai.Test_selector
end

module Incr = Ui_incr
module Vdom = Virtual_dom.Vdom
module View = Bonsai_web_ui_view
module To_incr_dom = Bonsai_web.To_incr_dom
module Persistent_var = Bonsai_web.Persistent_var
module Rpc_effect = Bonsai_web.Rpc_effect
module Effect = Bonsai_web.Effect
module Value = Bonsai_proc.Value
module Computation = Bonsai_proc.Computation
module Private = Bonsai_proc.Private
module Var = Bonsai_proc.Var

let am_running_how
  : [ `Browser
    | `Browser_test
    | `Browser_benchmark
    | `Node
    | `Node_benchmark
    | `Node_test
    | `Node_jsdom_test
    ]
  =
  Am_running_how_js.am_running_how
;;

include Vdom.Html_syntax
module Test_selector = Bonsai_web.Bonsai.Test_selector
