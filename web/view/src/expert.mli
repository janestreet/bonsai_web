open! Core
open! Import

include module type of struct
  include Underlying_intf
end

val default_theme : Theme.t
val override_theme : Theme.t -> f:((module S) -> (module S)) -> Theme.t
val set_dark_class_on_html : Vdom.Attr.t
