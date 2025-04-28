open! Core
open! Import

module Fg_bg = struct
  type t =
    { foreground : Color.t
    ; background : Color.t
    }
  [@@deriving fields ~getters]
end

module Intent = struct
  type t =
    | Info
    | Success
    | Warning
    | Error
  [@@deriving sexp, equal, compare, enumerate]

  type colors =
    { info : Fg_bg.t
    ; success : Fg_bg.t
    ; warning : Fg_bg.t
    ; error : Fg_bg.t
    }

  let lookup { info; success; warning; error } = function
    | Info -> info
    | Success -> success
    | Warning -> warning
    | Error -> error
  ;;
end

module Font_style = struct
  type t =
    | Regular
    | Bold
    | Italic
    | Underlined
  [@@deriving sexp, equal, compare, enumerate]
end

module Font_size = struct
  type t =
    | Small
    | Regular
    | Large
  [@@deriving sexp, equal, compare, enumerate]
end

module Table = struct
  type t =
    { body_row_even : Fg_bg.t
    ; body_row_odd : Fg_bg.t
    ; body_row_focused : Fg_bg.t
    ; body_cell_focused : Fg_bg.t
    ; header_row : Fg_bg.t
    ; header_header_border : Color.t
    ; header_body_border : Color.t
    ; body_body_border : Color.t
    ; body_row_focused_border : Color.t
    }
end

module Form = struct
  type t =
    { error_message : Fg_bg.t
    ; error_toggle_text : Color.t
    ; error_border : Color.t
    ; tooltip_message : Fg_bg.t
    ; tooltip_toggle_text : Color.t
    ; tooltip_border : Color.t
    }
end

module Toplayer = struct
  (** [hoverable_tooltip_hide_grace_period] should be sufficiently long for the user to
      move their cursor from the anchor to the tooltip.

      If [tooltips_have_arrows], [tooltip_offset_px] should be big enough that the arrow
      doesn't overlap with the anchor. The same applies to
      [popover_with_arrow_default_offset_px]. *)
  type t =
    { tooltips_have_arrows : [ `No | `Yes_with_length_px of float ]
    ; tooltip_offset_px : float
    ; tooltip_show_delay : Time_ns.Span.t
    ; tooltip_hide_grace_period : Time_ns.Span.t
    ; hoverable_tooltip_hide_grace_period : Time_ns.Span.t
    ; popover_default_offset_px : float
    ; popover_with_arrow_default_offset_px : float
    ; popover_with_arrow_default_arrow_length_px : float
    }
end

type t =
  { primary : Fg_bg.t
  ; extreme : Fg_bg.t
  ; extreme_primary_border : Color.t
  ; intent : Intent.colors
  ; table : Table.t
  ; form : Form.t
  ; toplayer : Toplayer.t
  ; small_font_size : Css_gen.Length.t
  ; large_font_size : Css_gen.Length.t
  ; is_dark : bool
  }

module Card_title_kind = struct
  type t =
    | Prominent
    | Discreet
end
