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

  let on_effect =
    let control_focus ~on_element path =
      let element =
        Dom_html.document##querySelector
          (Js.string [%string "[data-focus-handle=%{path}]"])
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
      match Am_running_how_js.am_in_browser_like_api with
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
          let attr = Vdom.Attr.create "data-focus-handle" path in
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
    match Am_running_how_js.am_in_browser_like_api with
    | false -> Core.print_s [%message "set document title" (title : string)]
    | true -> Js_of_ocaml.Dom_html.document##.title := Js_of_ocaml.Js.string title)
;;

let on_change_set_document_title title =
  Bonsai.Edge.on_change
    ~equal:String.equal
    title
    ~callback:(Bonsai.return set_document_title)
;;
