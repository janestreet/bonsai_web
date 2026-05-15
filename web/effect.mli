open! Core
open! Async_kernel
open! Import
open Bonsai.For_open
include module type of Virtual_dom.Vdom.Effect
include module type of Ui_effect_of_deferred

module Focus : sig
  type nonrec t =
    { attr : Vdom.Attr.t
    ; focus : unit t
    ; blur : unit t
    }

  (** [on_effect] returns a [Vdom.Attr.t] and two [unit Effect.t]s that focus/blur the
      [Vdom.Node.t] containing the [Vdom.Attr.t]. The attr should not be used on more than
      one [Vdom.Node.t], as only the first element will be focused/blurred when the effect
      runs.

      If [prevent_scroll] is true, the browser will not scroll the element into view after
      focusing it. The default behavior is to scroll into view.

      **JSDom**: When [name_for_testing] is provided, the focus and blur effects will
      print in test mode. They will be a no-op otherwise. *)
  val on_effect
    :  ?name_for_testing:string
    -> ?prevent_scroll:bool
    -> unit
    -> Bonsai.graph
    -> t Bonsai.t

  (** [on_activate] will focus the element that the returned attr is attached to when this
      computation is activated. See [Bonsai.Edge] for more details on the component
      lifecycle.

      If [prevent_scroll] is true, the browser will not scroll the element into view after
      focusing it. The default behavior is to scroll into view.

      **JSDom**: When [name_for_testing] is provided, the focus will print, otherwise it's
      a no-op. *)
  val on_activate
    :  ?name_for_testing:string
    -> ?prevent_scroll:bool
    -> unit
    -> Bonsai.graph
    -> Vdom.Attr.t Bonsai.t
end

(** [reload_page] will cause a page reload if running normally in a browser.

    **JSDom**: When running in JSDom, [reload_page] will just print that a reload would
    normally occur. *)
val reload_page : unit Effect.t

(** [alert] calls [window.alert] which causes an alert box to pop up with your provided
    message. We don't recommend using this API, since it has a rather poor user
    experience.

    Consider using Skyline's Alert Dialog component instead.

    **JSDom**: When running in JSDom, [alert] will throw an exception. *)
val alert : string -> unit Effect.t

(** [set_document_title] will set the title of the page to the provided string

    **JSDom**: When running in JSDom, [set_document_title] will just print that the title
    is being set. *)
val set_document_title : string -> unit Effect.t

(** [on_change_set_document_title] will cause the title of the page to be set to the
    provided [string Bonsai.t] and updated whenever its value changes

    **JSDom**: When running in JSDom, [on_change_set_document_title] will just print that
    the title is being set. *)
val on_change_set_document_title : string Bonsai.t -> Bonsai.graph -> unit
