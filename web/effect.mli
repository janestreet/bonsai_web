open! Core
open! Async_kernel
open! Import
open Bonsai.For_open
include module type of Virtual_dom.Vdom.Effect
include module type of Ui_effect_of_deferred
module Js := Js_of_ocaml.Js

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
    -> local_ Bonsai.graph
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
    -> local_ Bonsai.graph
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
val on_change_set_document_title : string Bonsai.t -> local_ Bonsai.graph -> unit

(** [of_js_promise] takes a JavaScript promise and converts it into a
    ['a Or_error.t Effect.t].

    **Important**: This effect will only be run once even if scheduled multiple times.
    Unlike other effects, which execute every time they are scheduled, this effect will
    resolve to the exact same in-memory value/reference every time it is bound/mapped.

    [on_exn]'s argument is (typically) a JavaScript error object. *)
val of_js_promise
  :  on_exn:([ `Exn of Js.error Js.t | `Unknown of Js.Unsafe.any ] -> 'a Core.Or_error.t)
  -> 'a Browser_js_types.js_promise Js.t
  -> 'a Core.Or_error.t Effect.t

(** [of_js_promise_exn] takes a JavaScript promise and converts it into an [Effect.t].

    **Important**: This effect will only be run once even if scheduled multiple times.
    Unlike other effects, which execute every time they are scheduled, this effect will
    resolve to the exact same in-memory value/reference every time it is bound/mapped.

    Note that if the promise raises an exception, it will not be handled here and will not
    be caught by a try-catch block. *)
val of_js_promise_exn : 'a. 'a Browser_js_types.js_promise Js.t -> 'a Effect.t

(** [to_js_promise] immediately runs the given effect in a JavaScript promise. All
    exceptions must be handled directly with promises *)
val to_js_promise : 'a. 'a Effect.t -> 'a Browser_js_types.js_promise Js.t
