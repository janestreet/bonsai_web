open! Core
open! Import

module Element_type : sig
  type t =
    | Tooltip
    | Popover
end

val arrow
  :  element_type:Element_type.t
  -> intent:Constants.Intent.t option
  -> Constants.t
  -> Vdom.Node.t

val default_styles : intent:Constants.Intent.t option -> Constants.t -> Vdom.Attr.t
val default_modal_styles : Constants.t -> Vdom.Attr.t

val default_tooltip_styles
  :  intent:Constants.Intent.t option
  -> Constants.t
  -> Vdom.Attr.t

val default_tooltip_anchor_styles : Vdom.Attr.t
