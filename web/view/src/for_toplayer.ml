open! Core
open! Import

module Element_type = struct
  type t =
    | Tooltip
    | Popover
end

let arrow ~element_type ~intent (constants : Constants.t) =
  (* [arrow_len] is the distance from the center of the square to one of the points. The
     length / width of the square is therefore [(2 * arrow_len) / sqrt(2)], i.e.
     [arrow_len * sqrt(2)]. *)
  let arrow_len =
    match element_type with
    | Element_type.Tooltip ->
      (match constants.toplayer.tooltips_have_arrows with
       | `Yes_with_length_px len -> len
       | `No ->
         (* Should not happen; this functionw won't be called if the tooltip doesn't have
            an arrow. *)
         0.)
    | Popover -> constants.toplayer.popover_with_arrow_default_arrow_length_px
  in
  let side_len = `Px_float (arrow_len *. sqrt 2.) in
  Vdom.Node.div
    ~attrs:
      ([ [%css
           {|
             height: %{side_len#Css_gen.Length};
             width: %{side_len#Css_gen.Length};
             transform: rotate(45deg);
             border-bottom: none;
             border-right: none;
           |}]
       ]
       @ [ Default_theme_helpers.intent_colors intent constants
         ; Default_theme_helpers.intent_border intent constants
         ])
    []
;;

let default_styles ~intent constants =
  let module Style =
    [%css
    stylesheet
      {|
        @layer bonsai_web_ui_view.toplayer_defaults {
          .toplayer {
            padding: 0.2em 0.3em;
            border-radius: 2px;
          }
        }
      |}]
  in
  Vdom.Attr.many
    [ Style.toplayer
    ; Default_theme_helpers.intent_colors intent constants
    ; Default_theme_helpers.intent_border intent constants
    ]
;;

let default_tooltip_styles ~intent constants =
  let module Style =
    [%css
    stylesheet
      {|
        @layer bonsai_web_ui_view.tooltip_defaults {
          .tooltip {
            max-width: 300px;
          }
        }
      |}]
  in
  Vdom.Attr.combine (default_styles ~intent constants) Style.tooltip
;;

let default_tooltip_anchor_styles =
  let module Style =
    [%css
    stylesheet
      {|
        @layer bonsai_web_ui_view.tooltip_defaults {
          .anchor {
            text-decoration: underline;
            text-decoration-style: dotted;
            text-underline-offset: 0.15em;
          }
        }
      |}]
  in
  Style.anchor
;;

(* We must not use [blur] on the backdrop, as that has caused many performance issues with
   massively increased GPU load. *)
let default_modal_styles constants =
  let module Modal =
    [%css
    stylesheet
      {|
        @layer bonsai_web_ui_view.modal_defaults {
          .modal {
            padding: 0.4em 0.6em;
            margin: auto;

            &::backdrop {
              backdrop-filter: brightness(90%) grayscale(90%);
            }
          }
        }
      |}]
  in
  Vdom.Attr.combine (default_styles ~intent:None constants) Modal.modal
;;
