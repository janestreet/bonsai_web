open! Core
open! Import

module Style = struct
  type t =
    | Light
    | Dark
end

module Contrast = struct
  type t = Standard
end

module Version = struct
  type t =
    | V1
    | Bleeding
end

let c ~fg ~bg = { Fg_bg.foreground = fg; background = bg }

let toplayer_constants =
  { Toplayer.tooltips_have_arrows = `No
  ; tooltip_offset_px = 0.
  ; tooltip_show_delay = Time_ns.Span.of_ms 0.
  ; tooltip_hide_grace_period = Time_ns.Span.of_ms 0.
  ; hoverable_tooltip_hide_grace_period = Time_ns.Span.of_ms 100.
  ; popover_default_offset_px = 0.
  ; popover_with_arrow_default_offset_px = 12.
  ; popover_with_arrow_default_arrow_length_px = 8.
  }
;;

let dark_mode_constants =
  let primary = c ~fg:(`Hex "#d1d2d3") ~bg:Top_level_styles.dark_bg in
  let extreme = c ~fg:(`Name "#e2e3e4") ~bg:(`Hex "#14181c") in
  let table_even = c ~bg:(`Hex "#21242a") ~fg:primary.foreground in
  let extreme_primary_border = `Hex "#313943" in
  let header = c ~bg:(`Hex "#0b0e11") ~fg:primary.foreground in
  let info = c ~fg:(`Name "black") ~bg:(`Hex "#1BA1F2") in
  { primary
  ; extreme
  ; extreme_primary_border
  ; intent =
      { info
      ; success = c ~fg:(`Name "black") ~bg:(`Hex "#57c961")
      ; warning = c ~fg:(`Name "black") ~bg:(`Hex "#ffbe01")
      ; error = c ~fg:(`Name "white") ~bg:(`Hex "#f2581b")
      }
  ; table =
      { body_row_even = table_even
      ; body_row_odd = primary
      ; body_row_focused = c ~fg:primary.foreground ~bg:(`Hex "#4b3038")
      ; body_cell_focused = c ~fg:primary.foreground ~bg:(`Hex "#4b3038")
      ; header_row = header
      ; header_header_border = extreme_primary_border
      ; header_body_border = extreme_primary_border
      ; body_body_border = extreme_primary_border
      ; body_row_focused_border = `Hex "#774856"
      }
  ; form =
      { error_message = { foreground = `Name "black"; background = `Name "pink" }
      ; error_toggle_text = `Hex "#f54646"
      ; error_border = `Name "red"
      ; tooltip_message = { foreground = `Name "black"; background = `Name "azure" }
      ; tooltip_border = `Name "darkblue"
      ; tooltip_toggle_text = `Name "blue"
      }
  ; toplayer = toplayer_constants
  ; small_font_size = `Em_float 0.8
  ; large_font_size = `Em_float 1.2
  ; is_dark = true
  }
;;

let light_mode_constants =
  let primary = c ~fg:(`Hex "#101c28") ~bg:Top_level_styles.light_bg in
  let extreme = c ~fg:(`Name "#101c28") ~bg:(`Hex "#f0eeec") in
  let table_even = c ~bg:(`Hex "#d9d0c4") ~fg:(`Name "#070e16") in
  let extreme_primary_border = `Hex "#313943" in
  let header = c ~bg:(`Hex "#312516") ~fg:(`Hex "#f7dec5") in
  let header_header_border = `Hex "#775a34" in
  { primary
  ; extreme
  ; extreme_primary_border
  ; intent = dark_mode_constants.intent
  ; small_font_size = dark_mode_constants.small_font_size
  ; large_font_size = dark_mode_constants.large_font_size
  ; table =
      { body_row_even = table_even
      ; body_row_odd = primary
      ; body_row_focused = c ~fg:primary.foreground ~bg:(`Hex "#7D648A")
      ; body_cell_focused = c ~fg:primary.foreground ~bg:(`Hex "#7D648A")
      ; header_row = header
      ; header_header_border
      ; header_body_border = header_header_border
      ; body_body_border = extreme_primary_border
      ; body_row_focused_border = extreme_primary_border
      }
  ; is_dark = false
  ; form =
      { error_message = { foreground = `Name "black"; background = `Name "pink" }
      ; error_toggle_text = `Hex "#f54646"
      ; error_border = `Name "red"
      ; tooltip_message = { foreground = `Name "black"; background = `Name "azure" }
      ; tooltip_border = `Name "darkblue"
      ; tooltip_toggle_text = `Name "blue"
      }
  ; toplayer = toplayer_constants
  }
;;

let app_attr ~color ~is_dark ~set_min_height_to_100vh =
  Vdom.Attr.many
    [ App.Variables.set () ~bg:(color |> Color.to_string_css)
    ; (if is_dark then Top_level_styles.dark else Top_level_styles.light)
    ; (if set_min_height_to_100vh then App.set_min_height_to_100vh else Vdom.Attr.empty)
    ; (if is_dark then View.Expert.set_dark_class_on_html else Vdom.Attr.empty)
    ]
;;

let v1 ~constants ~codemirror_theme ~is_dark ~name ~version_name ~set_min_height_to_100vh =
  View.Expert.override_theme View.Expert.default_theme ~f:(fun (module M) ->
    (module struct
      class c =
        object (self)
          inherit M.c
          method! theme_name = [%string "%{name} %{version_name}"]
          method! constants = constants

          method! app_attr =
            lazy
              (app_attr
                 ~is_dark
                 ~color:self#constants.primary.background
                 ~set_min_height_to_100vh)

          method! devbar = Devbar.make self#constants ~is_dark
          method! tabs = Tabs.make
          method! button = Buttons.make self#constants
          method! use_intent_fg_or_bg_for_highlighting = `Bg
          method! codemirror_theme = Some codemirror_theme
          method! card = Cards.make self#constants
        end
    end))
;;

let theme
  ?(contrast = Contrast.Standard)
  ?(style = Style.Dark)
  ?set_min_height_to_100vh
  ~version
  ()
  =
  let is_dark, constants, name =
    match style with
    | Light -> false, light_mode_constants, "kado (light)"
    | Dark -> true, dark_mode_constants, "kado"
  in
  let codemirror_theme : View.Expert.For_codemirror.Theme.t =
    match style with
    | Light -> Solarized_light
    | Dark -> Nord
  in
  let Standard = contrast in
  let set_min_height_to_100vh = Option.is_some set_min_height_to_100vh in
  match version with
  | Version.V1 ->
    v1
      ~constants
      ~codemirror_theme
      ~is_dark
      ~name
      ~version_name:"v1"
      ~set_min_height_to_100vh
  | Bleeding ->
    v1
      ~constants
      ~codemirror_theme
      ~is_dark
      ~name
      ~version_name:"v1"
      ~set_min_height_to_100vh
;;

module Unstable = struct
  module Buttons = Buttons
  module Input = Input
end
