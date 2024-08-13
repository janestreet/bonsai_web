open! Core
open! Import
module Style = Button_style

let make constants ~attrs ~disabled ~intent ~tooltip ~on_click content =
  let additional_classes =
    match (intent : Intent.t option) with
    | None ->
      let subtle_stripe_color = constants.table.body_row_even.background in
      Vdom.Attr.many
        [ Style.subtle
        ; Style.Variables.set
            ~disabled_stripes:(Css_gen.Color.to_string_css subtle_stripe_color)
            ()
        ]
    | Some Info -> Style.primary
    | Some Success -> Style.safe
    | Some Error -> Style.danger
    | Some Warning -> Style.warn
  in
  let maybe_title =
    match tooltip with
    | None -> Vdom.Attr.empty
    | Some s -> Vdom.Attr.title s
  in
  let on_click_attr = Vdom.Attr.on_click (fun _ -> on_click) in
  let disabled_attr = if disabled then Vdom.Attr.disabled else Vdom.Attr.empty in
  let colors =
    match intent with
    | None -> constants.extreme
    | Some intent -> Intent.lookup constants.intent intent
  in
  let fg_and_bg =
    let { Fg_bg.foreground; background } = colors in
    Vdom.Attr.style Css_gen.(background_color background @> color foreground)
  in
  let vars =
    Style.Variables.set
      ~extreme_primary_border:
        (Css_gen.Color.to_string_css constants.View.Constants.extreme_primary_border)
      ~btn_bg:(Css_gen.Color.to_string_css colors.background)
      ()
  in
  let attr =
    Vdom.Attr.many
      ([ Style.btn
       ; additional_classes
       ; vars
       ; on_click_attr
       ; disabled_attr
       ; fg_and_bg
       ; maybe_title
       ]
       @ attrs)
  in
  Vdom.Node.button ~attrs:[ attr ] content
;;

let vertical_group = Style.btn_group_vrt
let horizontal_group = Style.btn_group_hz
let small = Style.small
let thinking = Style.thinking
let pressed = Style.pressed
