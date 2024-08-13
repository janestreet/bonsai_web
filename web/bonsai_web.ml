open! Core
open! Js_of_ocaml

module Arrow_deprecated = struct
  module Effect = Effect
  module Vdom = Import.Vdom
  module Start = Start.Arrow_deprecated
  module Bonsai = Bonsai.Arrow_deprecated
end

module Proc = struct
  module Start = Start.Proc

  module Bonsai = struct
    include Import.Bonsai.Proc
    module Arrow_deprecated = Import.Bonsai.Arrow_deprecated
  end

  module Incr = Import.Incr
  module Vdom = Import.Vdom
  module View = Bonsai_web_ui_view
  module To_incr_dom = To_incr_dom
  module Persistent_var = Persistent_var
  module Rpc_effect = Rpc_effect
  module Effect = Effect
  module Value = Bonsai.Value
  module Computation = Bonsai.Computation
  module Private = Import.Bonsai.Private
  module Var = Bonsai.Var

  module type Model = Bonsai.Model

  include Util
  include Vdom.Html_syntax
end

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
module Start = Start.Proc

module Bonsai = struct
  include Bonsai.Cont

  module Arrow_deprecated = struct
    include Arrow_deprecated
    include Bonsai
  end
end

module Incr = Import.Incr
module View = Bonsai_web_ui_view
module To_incr_dom = To_incr_dom
module Persistent_var = Persistent_var
module Rpc_effect = Rpc_effect
