open! Core
open! Import
module Constants = Constants
module Theme = Theme
include Underlying_intf

type t = (module S)

let make_theme (m : t) : Theme.t =
  (module struct
    include (val m)

    let singleton = new c
  end)
;;

let override_theme ((module M) : Theme.t) ~(f : t -> t) : Theme.t =
  (module struct
    include (val f (module M))

    let singleton = new c
  end)
;;

module Set_dark_class_on_html = Vdom.Attr.Hooks.Make (struct
    open Js_of_ocaml
    module State = Unit

    module Input = struct
      type t = unit [@@deriving sexp_of]

      let combine () () = ()
    end

    let init () _elem =
      Dom_html.document##.documentElement##.classList##add
        (Js.string Bonsai_web_ui_view_tailwind_interop.bonsai_dark_class_for_tailwind)
    ;;

    let on_mount = `Do_nothing
    let update ~old_input:() ~new_input:() () _elem = ()

    let destroy () () _elem =
      Dom_html.document##.documentElement##.classList##remove
        (Js.string Bonsai_web_ui_view_tailwind_interop.bonsai_dark_class_for_tailwind)
    ;;
  end)

let set_dark_class_on_html =
  Vdom.Attr.create_hook
    "bonsai_web_ui_view.set_dark_class_on_html"
    (Set_dark_class_on_html.create ())
;;

let root_styles =
  lazy
    (Inline_css.Private.Dynamic.attr
       {|
@layer bonsai_web_ui_view.app {
  :root {
    font-family: sans-serif;
  }

  :root *,
  :root *::before,
  :root *::after {
    box-sizing: border-box;
  }
}
|})
;;

let default_theme =
  make_theme
    (module struct
      class c =
        object (self : #Underlying_intf.C.t)
          method theme_name = "default theme"

          method app_attr =
            lazy
              (Vdom.Attr.combine
                 (force root_styles)
                 (if self#constants.is_dark
                  then set_dark_class_on_html
                  else Vdom.Attr.empty))

          method constants =
            let primary =
              { Constants.Fg_bg.foreground = `Name "black"; background = `Name "white" }
            in
            let header =
              { Constants.Fg_bg.foreground = primary.background
              ; background = primary.foreground
              }
            in
            let table_row_even =
              { Constants.Fg_bg.foreground = `Name "black"; background = `Name "#e6e6e6" }
            in
            let info =
              { Constants.Fg_bg.background = `Hex "#e0f7ff"
              ; foreground = `Name "#0a90bf"
              }
            in
            let extreme_primary_border = `Name "grey" in
            { Constants.primary
            ; extreme = primary
            ; extreme_primary_border
            ; intent =
                { info
                ; success = { background = `Hex "#ecffe0"; foreground = `Name "#348203" }
                ; warning = { background = `Hex "#ffeb3b"; foreground = `Name "#6b6001" }
                ; error = { background = `Name "#ff2522"; foreground = `Name "#630100" }
                }
            ; table =
                { body_row_even = table_row_even
                ; body_row_odd = primary
                ; body_row_focused =
                    { foreground = primary.foreground; background = info.background }
                ; body_cell_focused =
                    { foreground = primary.foreground; background = info.background }
                ; header_row = header
                ; header_header_border = extreme_primary_border
                ; header_body_border = extreme_primary_border
                ; body_body_border = extreme_primary_border
                ; body_row_focused_border = info.foreground
                }
            ; form =
                { error_message =
                    { foreground = `Name "black"; background = `Name "pink" }
                ; error_toggle_text = `Hex "#f54646"
                ; error_border = `Name "red"
                ; tooltip_message =
                    { foreground = `Name "black"; background = `Name "azure" }
                ; tooltip_border = `Name "darkblue"
                ; tooltip_toggle_text = `Name "blue"
                }
            ; toplayer =
                { tooltips_have_arrows = `No
                ; tooltip_offset_px = 0.
                ; tooltip_show_delay = Time_ns.Span.of_ms 0.
                ; tooltip_hide_grace_period = Time_ns.Span.of_ms 0.
                ; hoverable_tooltip_hide_grace_period = Time_ns.Span.of_ms 150.
                ; popover_default_offset_px = 0.
                ; popover_with_arrow_default_offset_px = 12.
                ; popover_with_arrow_default_arrow_length_px = 8.
                }
            ; small_font_size = `Em_float 0.8
            ; large_font_size = `Em_float 1.2
            ; is_dark = false
            }

          method humanize_sexp sexp =
            match sexp with
            | Sexp.Atom text ->
              String.map text ~f:(function
                | '_' -> ' '
                | o -> o)
            | _ -> Sexp.to_string_hum sexp

          method button ~attrs ~disabled ~intent ~tooltip ~on_click content =
            let on_click_attr = Vdom.Attr.on_click (fun _ -> on_click) in
            let disabled_attr =
              if disabled then Vdom.Attr.disabled else Vdom.Attr.empty
            in
            let maybe_colors =
              match intent with
              | None -> Vdom.Attr.empty
              | Some intent ->
                let maybe_transparent =
                  if disabled then Css_gen.opacity 0.3 else Css_gen.empty
                in
                let { Constants.Fg_bg.foreground; background } =
                  Constants.Intent.lookup self#constants.intent intent
                in
                Vdom.Attr.style
                  Css_gen.(
                    background_color background @> color foreground @> maybe_transparent)
            in
            let maybe_title =
              match tooltip with
              | None -> Vdom.Attr.empty
              | Some s -> Vdom.Attr.title s
            in
            let attrs =
              attrs @ [ on_click_attr; disabled_attr; maybe_colors; maybe_title ]
            in
            Vdom.Node.button ~attrs content

          method badge ~attrs ~intent ~on_dismiss content =
            let attrs =
              attrs
              @ [ Default_theme_helpers.intent_colors intent self#constants
                ; Default_theme_helpers.intent_border intent self#constants
                  (* The large, constant border-radius is a hack to make the badge border
                     round without turning it into an ellipse (which using 50% would do). *)
                ; [%css
                    {|
                      border-radius: 100px;

                      padding: 4px 8px;
                      line-height: normal;
                      display: inline-flex;
                      white-space: nowrap;
                      align-items: center;
                    |}]
                ]
            in
            let dismiss_button =
              match on_dismiss with
              | None -> Vdom.Node.none_deprecated [@alert "-deprecated"]
              | Some on_dismiss ->
                Vdom.Node.span
                  ~attrs:
                    [ Vdom.Attr.on_click (fun _ -> on_dismiss)
                    ; [%css
                        {|
                          cursor: pointer;
                          margin-left: 6px;
                          line-height: 0;
                          opacity: 60%;
                          &::after {
                            content: "X";
                            font-size: 70%;
                          }
                        |}]
                    ]
                  []
            in
            Vdom.Node.span ~attrs (content @ [ dismiss_button ])

          method tabs
            : type a.  attrs:Vdom.Attr.t list
                      -> per_tab_attrs:(a -> is_active:bool -> Vdom.Attr.t list)
                      -> on_change:(from:a -> to_:a -> unit Effect.t)
                      -> equal:(a -> a -> bool)
                      -> active:a
                      -> (a * Vdom.Node.t) list
                      -> Vdom.Node.t =
            fun ~attrs ~per_tab_attrs ~on_change ~equal ~active tabs ->
              let color = self#constants.primary.foreground in
              let all_attr =
                Vdom.Attr.style (Css_gen.create ~field:"cursor" ~value:"pointer")
              in
              let active_attr =
                Vdom.Attr.style
                  (Css_gen.border_bottom ~width:(`Px 3) ~color ~style:`Solid ())
              in
              let inactive_attr i =
                Vdom.Attr.many
                  [ Vdom.Attr.style
                      (Css_gen.concat
                         [ Css_gen.border_bottom ~width:(`Px 1) ~color ~style:`Solid ()
                         ; Css_gen.opacity 0.6
                         ])
                  ; Vdom.Attr.on_click (fun _ -> on_change ~from:active ~to_:i)
                  ]
              in
              List.map tabs ~f:(fun (i, tab) ->
                let is_active = equal active i in
                Vdom.Node.div
                  ~attrs:
                    [ (if is_active then active_attr else inactive_attr i)
                    ; all_attr
                    ; Vdom.Attr.many (per_tab_attrs i ~is_active)
                    ]
                  [ tab ])
              |> Layout.hbox ~attrs ~gap:(`Em_float 0.5)

          method devbar ~attrs ~count ~intent text =
            let intent = Option.value intent ~default:Constants.Intent.Error in
            let even_attr, odd_attr =
              let both_style =
                Css_gen.(padding_left (`Em_float 0.5) @> padding_right (`Em_float 0.5))
              in
              let { Constants.Fg_bg.foreground = normal_fg; background = normal_bg } =
                self#constants.primary
              in
              let { Constants.Fg_bg.foreground = intent_fg; background = intent_bg } =
                Constants.Intent.lookup self#constants.intent intent
              in
              let even_attr =
                Vdom.Attr.style
                  Css_gen.(color normal_bg @> background_color normal_fg @> both_style)
              in
              let odd_attr =
                Vdom.Attr.style
                  Css_gen.(color intent_fg @> background_color intent_bg @> both_style)
              in
              even_attr, odd_attr
            in
            let main_attr =
              Vdom.Attr.style
                Css_gen.(
                  max_width (`Percent Percent.one_hundred_percent) @> overflow `Hidden)
            in
            Layout.hbox
              ~attrs:(attrs @ [ main_attr ])
              (List.init count ~f:(fun i ->
                 Vdom.Node.span
                   ~attrs:[ (if i % 2 = 0 then even_attr else odd_attr) ]
                   [ Vdom.Node.text text ]))

          method tooltip_attr
            ~tooltip_attrs
            ~intent
            ~position
            ~alignment
            ~hoverable_inside
            content =
            let toplayer_constants = self#constants.toplayer in
            let arrow =
              match toplayer_constants.tooltips_have_arrows with
              | `Yes_with_length_px _ -> Some (self#toplayer_tooltip_arrow ~intent)
              | `No -> None
            in
            Byo_toplayer_private_vdom.tooltip
              ~tooltip_attrs:([ self#toplayer_tooltip_styles ~intent ] @ tooltip_attrs)
              ~position:
                (match position with
                 | Auto -> Auto
                 | Top -> Top
                 | Bottom -> Bottom
                 | Left -> Left
                 | Right -> Right)
              ~alignment:
                (match alignment with
                 | Start -> Start
                 | Center -> Center
                 | End -> End)
              ~offset:
                { main_axis = toplayer_constants.tooltip_offset_px; cross_axis = 0. }
              ~hoverable_inside
              ~show_delay:toplayer_constants.tooltip_show_delay
              ~hide_grace_period:
                (match hoverable_inside with
                 | true -> toplayer_constants.hoverable_tooltip_hide_grace_period
                 | false -> toplayer_constants.tooltip_hide_grace_period)
              ?arrow
              (Vdom.Node.div content)
            |> Vdom.Attr.combine self#toplayer_tooltip_anchor_styles

          method tooltip = Tooltip.make self#constants
          method use_intent_fg_or_bg_for_highlighting : [ `Fg | `Bg ] = `Fg

          method themed_text ~attrs ~intent ~style ~size text =
            let maybe_colors =
              match intent with
              | None -> Vdom.Attr.empty
              | Some intent ->
                let highlight_color =
                  let { Constants.Fg_bg.foreground; background } =
                    Constants.Intent.lookup self#constants.intent intent
                  in
                  match self#use_intent_fg_or_bg_for_highlighting with
                  | `Fg -> foreground
                  | `Bg -> background
                in
                let intense_intents =
                  match intent with
                  | Warning | Error ->
                    Css_gen.(font_weight `Bold @> color highlight_color)
                  | _ -> Css_gen.empty
                in
                Vdom.Attr.style
                  Css_gen.(
                    text_decoration
                      ()
                      ~style:`Dashed
                      ~color:highlight_color
                      ~line:[ `Underline ]
                    @> create ~field:"text-underline-offset" ~value:"4px"
                    @> intense_intents)
            in
            let maybe_style =
              match (style : Constants.Font_style.t option) with
              | None | Some Regular -> Vdom.Attr.empty
              | Some Bold -> Vdom.Attr.style (Css_gen.font_weight `Bold)
              | Some Italic -> Vdom.Attr.style (Css_gen.font_style `Italic)
              | Some Underlined ->
                Vdom.Attr.style (Css_gen.text_decoration ~line:[ `Underline ] ())
            in
            let maybe_size =
              match (size : Constants.Font_size.t option) with
              | None | Some Regular -> Vdom.Attr.empty
              | Some Small ->
                Vdom.Attr.style (Css_gen.font_size self#constants.small_font_size)
              | Some Large ->
                Vdom.Attr.style (Css_gen.font_size self#constants.large_font_size)
            in
            Vdom.Node.span
              ~attrs:(attrs @ [ maybe_colors; maybe_size; maybe_style ])
              [ Vdom.Node.text text ]

          method codemirror_theme : For_codemirror.Theme.t option = None

          method prt_styling =
            Bonsai_web_ui_partial_render_table_styling.create
              { colors =
                  { page_bg = self#constants.primary.background
                  ; page_fg = self#constants.primary.foreground
                  ; header_bg = self#constants.table.header_row.background
                  ; header_fg = self#constants.table.header_row.foreground
                  ; header_cell_focused_bg = self#constants.table.header_row.background
                  ; header_cell_focused_fg = self#constants.table.header_row.foreground
                  ; row_even_bg = self#constants.table.body_row_even.background
                  ; row_even_fg = self#constants.table.body_row_even.foreground
                  ; row_odd_bg = self#constants.table.body_row_odd.background
                  ; row_odd_fg = self#constants.table.body_row_odd.foreground
                  ; cell_focused_bg = self#constants.table.body_cell_focused.background
                  ; cell_focused_fg = self#constants.table.body_cell_focused.foreground
                  ; cell_focused_outline =
                      Some self#constants.table.body_row_focused_border
                  ; row_of_focused_cell_fg = None
                  ; row_of_focused_cell_bg = None
                  ; row_focused_bg = self#constants.table.body_row_focused.background
                  ; row_focused_fg = self#constants.table.body_row_focused.foreground
                  ; row_focused_border = self#constants.table.body_row_focused_border
                  ; header_header_border = self#constants.table.header_header_border
                  ; body_body_border = self#constants.table.body_body_border
                  ; header_body_border = self#constants.table.header_body_border
                  }
              ; lengths =
                  Bonsai_web_ui_partial_render_table_styling.Params.Lengths.default
              ; fonts = Bonsai_web_ui_partial_render_table_styling.Params.Fonts.default
              }

          method changelog_styling = For_changelog.default self#constants

          (* toplayer *)
          method toplayer_tooltip_arrow ~intent =
            For_toplayer.arrow ~element_type:Tooltip ~intent self#constants

          method toplayer_popover_arrow =
            For_toplayer.arrow ~element_type:Popover ~intent:None self#constants

          method toplayer_tooltip_styles ~intent =
            For_toplayer.default_tooltip_styles ~intent self#constants

          method toplayer_tooltip_anchor_styles =
            For_toplayer.default_tooltip_anchor_styles

          method toplayer_popover_styles =
            For_toplayer.default_styles ~intent:None self#constants

          method toplayer_modal_styles = For_toplayer.default_modal_styles self#constants

          (* tables *)
          method table = Table.table_attr self#constants
          method table_header = Table.table_header_attr
          method table_header_row = Table.table_header_row
          method table_header_cell = Table.table_header_cell
          method table_body = Table.table_body_attr
          method table_body_row = Table.table_body_row
          method table_body_cell = Table.table_body_cell
          method table_body_cell_empty = Table.table_body_cell_empty

          (* misc forms *)
          method form_view_error = Form.view_error

          method form_view_error_details =
            Form.view_error_details (self :> Underlying_intf.C.t)

          method form_view_tooltip = Form.view_tooltip (self :> Underlying_intf.C.t)
          method form_remove_item = Form.render_remove_item (self :> Underlying_intf.C.t)
          method form_append_item = Form.render_append_item (self :> Underlying_intf.C.t)

          (* form_constructors *)
          method form_empty = Form.empty
          method form_collapsible = Form.collapsible (self :> Underlying_intf.C.t)
          method form_raw = Form.raw (self :> Underlying_intf.C.t)
          method form_record = Form.record (self :> Underlying_intf.C.t)
          method form_variant = Form.variant (self :> Underlying_intf.C.t)
          method form_tuple = Form.tuple (self :> Underlying_intf.C.t)
          method form_option = Form.option (self :> Underlying_intf.C.t)
          method form_list = Form.list (self :> Underlying_intf.C.t)
          method form_view = Form.view (self :> Underlying_intf.C.t)
          method form_toplevel_combine = Form.toplevel_combine

          (* forms *)
          method form_to_vdom = Form.to_vdom (self :> Underlying_intf.C.t)

          (* cards *)
          method card
            ~container_attrs
            ~title_attrs
            ~content_attrs
            ~intent
            ~on_click
            ~title
            ~title_kind
            ~content =
            let open Constants in
            let module Style = Card_style in
            let constants = self#constants in
            let vars =
              let intent_fg =
                Option.value_map
                  intent
                  ~default:constants.primary.foreground
                  ~f:(fun intent -> (Intent.lookup constants.intent intent).foreground)
                |> Css_gen.Color.to_string_css
              in
              let intent_bg =
                Option.value_map
                  intent
                  ~default:constants.primary.background
                  ~f:(fun intent -> (Intent.lookup constants.intent intent).background)
                |> Css_gen.Color.to_string_css
              in
              let extreme_fg =
                constants.extreme.foreground |> Css_gen.Color.to_string_css
              in
              let extreme_bg =
                constants.extreme.background |> Css_gen.Color.to_string_css
              in
              Card_style.Variables.set_all ~extreme_bg ~extreme_fg ~intent_bg ~intent_fg
            in
            let title =
              match title with
              | [] -> Vdom.Node.none_deprecated [@alert "-deprecated"]
              | _ ->
                let create_title ~f ~extra_attr =
                  f
                    ~attrs:
                      [ Style.title_bar
                      ; Style.title_text
                      ; Vdom.Attr.many title_attrs
                      ; extra_attr
                      ]
                    title
                in
                let title =
                  match title_kind with
                  | Card_title_kind.Prominent ->
                    create_title
                      ~f:(fun ~attrs x -> Layout.hbox ~attrs x)
                      ~extra_attr:Vdom.Attr.empty
                  | Discreet ->
                    create_title
                      ~f:(fun ~attrs x -> Vdom.Node.legend ~attrs x)
                      ~extra_attr:Style.card_legend
                in
                title
            in
            let create_card ~f ~extra_container_attr =
              f
                ~attrs:
                  [ Vdom.Attr.many container_attrs
                  ; Vdom.Attr.on_click (fun _ -> on_click)
                  ; vars
                  ; extra_container_attr
                  ]
                [ title
                ; Layout.vbox
                    ~attrs:[ Vdom.Attr.many content_attrs; Style.content_common ]
                    content
                ]
            in
            match title_kind with
            | Card_title_kind.Prominent ->
              create_card
                ~f:(fun ~attrs x -> Layout.vbox ~attrs x)
                ~extra_container_attr:Style.container
            | Discreet ->
              create_card
                ~f:(fun ~attrs x -> Vdom.Node.fieldset ~attrs x)
                ~extra_container_attr:Style.fieldset_container

          method textbox
            ?attrs
            ?placeholder
            ?key
            ~allow_updates_when_focused
            ~disabled
            ~value
            ~set_value
            () =
            Vdom_input_widgets.Entry.text
              ~allow_updates_when_focused
              ?extra_attrs:attrs
              ?placeholder
              ?key
              ~disabled
              ~value:(Some value)
              ~on_input:(fun s -> set_value (Option.value ~default:"" s))
              ()

          method password
            ?attrs
            ?placeholder
            ?key
            ~allow_updates_when_focused
            ~disabled
            ~value
            ~set_value
            () =
            Vdom_input_widgets.Entry.password
              ?extra_attrs:attrs
              ?placeholder
              ?key
              ~allow_updates_when_focused
              ~disabled
              ~value:(Some value)
              ~on_input:(fun s -> set_value (Option.value ~default:"" s))
              ()

          method textarea
            ?attrs
            ?placeholder
            ?key
            ~allow_updates_when_focused
            ~disabled
            ~value
            ~set_value
            () =
            Vdom_input_widgets.Entry.text_area
              ~allow_updates_when_focused
              ?extra_attrs:attrs
              ?placeholder
              ?key
              ~disabled
              ~value
              ~on_input:set_value
              ()

          method number
            ?(attrs = [])
            ?placeholder
            ?min
            ?max
            ?key
            ~allow_updates_when_focused
            ~disabled
            ~step
            ~value
            ~set_value
            () =
            let min = Option.value_map min ~default:Vdom.Attr.empty ~f:Vdom.Attr.min in
            let max = Option.value_map max ~default:Vdom.Attr.empty ~f:Vdom.Attr.max in
            Vdom_input_widgets.Entry.number
              ~allow_updates_when_focused
              ~extra_attrs:(attrs @ [ min; max ])
              ?placeholder
              ?key
              ~disabled
              ~value
              ~on_input:set_value
              ~step
              (module Vdom_input_widgets.Decimal)

          method range
            ?(attrs = [])
            ?min
            ?max
            ?key
            ~allow_updates_when_focused
            ~disabled
            ~step
            ~value
            ~set_value
            () =
            let min = Option.value_map min ~default:Vdom.Attr.empty ~f:Vdom.Attr.min in
            let max = Option.value_map max ~default:Vdom.Attr.empty ~f:Vdom.Attr.max in
            Vdom_input_widgets.Entry.range
              ?key
              ~extra_attrs:(attrs @ [ min; max ])
              ~allow_updates_when_focused
              ~disabled
              ~value:(Some value)
              ~on_input:(function
                | None -> Effect.print_s [%message "BUG: no value"]
                | Some value -> set_value value)
              ~step
              (module Vdom_input_widgets.Decimal)
        end
    end)
;;
