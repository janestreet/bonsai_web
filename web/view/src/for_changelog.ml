open! Core
open! Import

module Entry_type = struct
  type t =
    | Feature
    | Bug_fix
    | Update
  [@@deriving sexp]
end

type t =
  { entry : Vdom.Attr.t
  ; notification : Vdom.Attr.t
  ; header : Vdom.Attr.t
  ; footer : Vdom.Attr.t
  ; chip_color : Entry_type.t -> Css_gen.Color.t
  ; close_x_button : Vdom.Node.t
  }

let default (constants : Constants.t) =
  let module Styles =
    [%css
    stylesheet
      {|
        @layer bonsai_web_ui_view.changelog_defaults {
          .entry {
            font-size: 1.2em;
            padding-bottom: 10px;
            font-weight: normal;
            line-height: 1.4em;
          }

          .notification {
            background-color: #4585e7;
            color: white;
            padding: 6px 10px;
            border-radius: 4px;
            z-index: 100;
            font-size: 12px;
            cursor: pointer;
          }

          .notification:hover {
            background-color: #1f4298;
          }

          .header {
            padding-bottom: 8px;
            margin: 0;
            border-bottom: 1px solid rgb(221, 221, 221);
            color: %{constants.primary.foreground#Css_gen.Color};
            font-size: %{constants.large_font_size#Css_gen.Length};
          }

          .footer {
            color: #aaa;
            padding-top: 8px;
            border-top: 1px solid #ddd;
          }
        }
      |}]
  in
  let chip_color (entry_type : Entry_type.t) =
    match entry_type with
    | Feature -> `Hex "#4585E7"
    | Bug_fix -> `Hex "#CFA026"
    | Update -> `Name "darkorange"
  in
  { entry = Styles.entry
  ; notification = Styles.notification
  ; header = Styles.header
  ; footer = Styles.footer
  ; chip_color
  ; close_x_button = Vdom.Node.none
  }
;;
