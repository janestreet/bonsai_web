open! Core
open! Async_kernel
open! Import
open Js_of_ocaml
include Virtual_dom.Vdom.Effect
include Ui_effect_of_deferred

module Focus = struct
  type nonrec t =
    { attr : Vdom.Attr.t
    ; focus : unit t
    ; blur : unit t
    }

  (** The Js_of_ocaml type Dom_html.element doesn't have the correct options for their
      `focus` method. Cast to this in order to work around this bug. *)
  type focusable =
    < focus : < preventScroll : bool Js.t Js.readonly_prop > Js.t -> unit Js.meth >

  let as_focusable : Dom_html.element Js.t -> focusable Js.t = Js.Unsafe.coerce
  let attr_name path = [%string "data-focus-handle-%{path}"]

  let on_effect =
    let control_focus ~on_element path =
      let element =
        Dom_html.document##querySelector (Js.string [%string "[%{attr_name path}]"])
      in
      match Js.Opt.to_option element with
      | None -> ()
      | Some element -> on_element element
    in
    let focus_effect ~prevent_scroll =
      of_sync_fun
        (control_focus ~on_element:(fun element ->
           Dom_html.CoerceTo.element element
           |> Js.Opt.to_option
           |> Option.map ~f:as_focusable
           |> Option.iter ~f:(fun element ->
             element##focus
               (object%js
                  val preventScroll = Js.bool prevent_scroll
               end))))
    in
    let blur_effect =
      of_sync_fun (control_focus ~on_element:(fun element -> element##blur))
    in
    let open Bonsai.Let_syntax in
    fun ?name_for_testing ?(prevent_scroll = false) () ->
      match Am_running_how_js.(am_in_browser_like_api am_running_how) with
      | false ->
        let print_effect_focus, print_effect_blur =
          Option.value_map
            name_for_testing
            ~f:(fun name_for_testing ->
              ( print_s [%message "focus effect for" name_for_testing]
              , print_s [%message "blur effect for" name_for_testing] ))
            ~default:(Ignore, Ignore)
        in
        fun (local_ _graph) ->
          Bonsai.return
            { attr = Vdom.Attr.empty
            ; focus = print_effect_focus
            ; blur = print_effect_blur
            }
      | true ->
        fun (local_ graph) ->
          let path = Bonsai.path_id graph in
          let%arr path in
          let attr = Vdom.Attr.create (attr_name path) "" in
          { attr; focus = focus_effect ~prevent_scroll path; blur = blur_effect path }
  ;;

  let on_activate ?name_for_testing ?(prevent_scroll = false) () (local_ graph) =
    let open Bonsai.Let_syntax in
    let%sub { attr; focus; blur = _ } =
      on_effect ?name_for_testing ~prevent_scroll () graph
    in
    let () = Bonsai.Edge.lifecycle ~on_activate:focus graph in
    attr
  ;;
end

let reload_page =
  of_thunk (fun () ->
    match Util.am_running_how with
    | `Browser -> Dom_html.window##.location##reload
    | `Node_test
    | `Node_jsdom_test
    | `Node
    | `Node_benchmark
    | `Browser_test
    | `Browser_benchmark -> Core.print_s [%message "Reloading page skipped in test"])
;;

let alert =
  of_sync_fun (fun s -> Js_of_ocaml.Dom_html.window##alert (Js_of_ocaml.Js.string s))
;;

let set_document_title =
  of_sync_fun (fun title ->
    match Am_running_how_js.(am_in_browser_like_api am_running_how) with
    | false -> Core.print_s [%message "set document title" (title : string)]
    | true -> Js_of_ocaml.Dom_html.document##.title := Js_of_ocaml.Js.string title)
;;

let on_change_set_document_title title =
  Bonsai.Edge.on_change
    ~trigger:`After_display
    ~equal:String.equal
    title
    ~callback:(Bonsai.return set_document_title)
;;

let is_error : 'a Js.t -> bool =
  fun x ->
  let is_error =
    Js.Unsafe.js_expr
      {js|
      (function (x) {
        return Error.isError(x);
      })|js}
  in
  Js.Unsafe.fun_call is_error [| Js.Unsafe.inject x |] |> Js.to_bool
;;

let promise_then
  : type a b.
    a Browser_js_types.js_promise Js.t
    -> (* NB: this [a -> b] map signature is not always correct since [Promise.then] will
          sometimes bind instead of map. However, it's correct for the types used in this
          file. *)
       on_resolved:(a -> b)
    -> on_rejected:([ `Exn of Js.error Js.t | `Unknown of Js.Unsafe.any ] -> b)
    -> b Browser_js_types.js_promise Js.t
  =
  fun promise ~on_resolved ~on_rejected ->
  let on_resolved = Js.wrap_callback on_resolved in
  let on_rejected =
    Js.wrap_callback (fun maybe_error ->
      let checked_value =
        match is_error maybe_error with
        | true ->
          let error : Js.error Js.t = Js.Unsafe.coerce maybe_error in
          `Exn error
        | false -> `Unknown (Js.Unsafe.inject maybe_error)
      in
      on_rejected checked_value)
  in
  Js.Unsafe.meth_call
    promise
    "then"
    [| Js.Unsafe.inject on_resolved; Js.Unsafe.inject on_rejected |]
;;

let of_js_promise_exn : type a. a Browser_js_types.js_promise Js.t -> a t =
  fun promise ->
  Ui_effect.Expert.of_fun ~f:(fun ~callback ~on_exn ->
    let (_promise : unit Browser_js_types.js_promise Js.t) =
      promise_then
        promise
        ~on_resolved:(fun value -> callback value)
        ~on_rejected:(function
          | `Exn error -> on_exn (Js_error.Exn (Js_error.of_error error))
          | `Unknown _value ->
            Exn.create_s
              [%message "Exception thrown by Promise was not a JavaScript error"]
            |> on_exn)
    in
    ())
;;

let of_js_promise
  ~(on_exn : [ `Exn of Js.error Js.t | `Unknown of Js.Unsafe.any ] -> 'a Core.Or_error.t)
  (promise : 'a Browser_js_types.js_promise Js.t)
  =
  Ui_effect.Expert.of_fun ~f:(fun ~callback ~on_exn:on_exn_effect ->
    let (_promise : unit Browser_js_types.js_promise Js.t) =
      promise_then
        promise
        ~on_resolved:(fun value -> callback (Core.Or_error.return value))
        ~on_rejected:(fun error ->
          try
            let value = on_exn error in
            callback value
          with
          | exn -> on_exn_effect exn)
    in
    ())
;;

let to_js_promise : type a. a t -> a Browser_js_types.js_promise Js.t =
  let create_js_promise
    : type a.
      f:(on_resolved:(a -> unit) -> on_rejected:(exn -> unit) -> unit)
      -> a Browser_js_types.js_promise Js.t
    =
    fun ~f ->
    let f =
      Js.wrap_callback (fun res rej ->
        (* The values passed into this callback are the raw js promise resolve and reject
           methods, which must be called as js functions *)
        let on_resolved value = Js.Unsafe.fun_call res [| Js.Unsafe.inject value |] in
        let on_rejected exn = Js.Unsafe.fun_call rej [| Js.Unsafe.inject exn |] in
        f ~on_resolved ~on_rejected)
    in
    let create_promise =
      Js.Unsafe.js_expr
        {js|
      (function (f) {
        return new Promise ((res, rej) => {
          f (res, rej);
        });
      })
    |js}
    in
    Js.Unsafe.fun_call create_promise [| Js.Unsafe.inject f |]
  in
  fun effect ->
    let f ~on_resolved ~on_rejected =
      Ui_effect.Expert.eval
        ~on_exn:(fun exn -> Js_error.attach_js_backtrace exn ~force:false |> on_rejected)
        ~f:on_resolved
        effect
    in
    create_js_promise ~f
;;
