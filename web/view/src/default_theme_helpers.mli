open! Core
open Virtual_dom

val intent_colors : Constants.Intent.t option -> Constants.t -> Vdom.Attr.t

val intent_border
  :  ?width:Css_gen.Length.t
  -> Constants.Intent.t option
  -> Constants.t
  -> Vdom.Attr.t
