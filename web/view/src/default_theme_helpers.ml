open! Core
open Virtual_dom

(* Helper functions used for the default theme. They live here so that components can use
   them. *)

let intent_colors intent (constants : Constants.t) =
  let { Constants.Fg_bg.foreground; background } =
    match intent with
    | None -> constants.extreme
    | Some intent -> Constants.Intent.lookup constants.intent intent
  in
  [%css
    {|
      background-color: %{background#Css_gen.Color};
      color: %{foreground#Css_gen.Color};
    |}]
;;

let intent_border ?(width = `Px 1) intent (constants : Constants.t) =
  match intent with
  | Some _ -> Vdom.Attr.empty
  | None ->
    let color = constants.extreme_primary_border in
    [%css {|border: %{width#Css_gen.Length} solid %{color#Css_gen.Color};|}]
;;
