open! Core
open Js_of_ocaml

module Session_storage_var = struct
  type 'a t =
    { var : 'a Ui_incr.Var.t
    ; get : unit -> 'a
    ; set : 'a -> unit
    ; on_set : (unit -> unit) list ref
    }

  let getter_setter () =
    let open Option.Let_syntax in
    let%map storage = Dom_html.window##.sessionStorage |> Js.Optdef.to_option in
    let set key value = storage##setItem (Js.string key) (Js.string value) in
    let get key =
      storage##getItem (Js.string key) |> Js.Opt.to_option |> Option.map ~f:Js.to_string
    in
    get, set
  ;;

  let event_listening_available () =
    Js.Optdef.test (Js.Unsafe.coerce Dom_html.window)##.addEventListener
    || Js.Optdef.test (Js.Unsafe.coerce Dom_html.window)##.attachEvent
  ;;

  let on_custom_storage_event ~unique_id ~callback =
    if event_listening_available ()
    then (
      let on_storage e =
        let e
          : < detail : < key : Js.js_string Js.t Js.readonly_prop > Js.t Js.readonly_prop >
              Js.t
          =
          Js.Unsafe.coerce e
        in
        if Js.strict_equals e##.detail##.key (Js.string unique_id) then callback ();
        Js._true (* not preventDefault *)
      in
      let listener_id =
        Dom_html.addEventListener
          Dom_html.window
          (Dom_events.Typ.make Bonsai_introspection_protocol.storage_event_name)
          (Dom.handler on_storage)
          Js._false (* useCapture *)
      in
      ignore listener_id)
  ;;

  let create (type a) (module M : Sexpable with type t = a) ~unique_id ~default =
    let getter_setter = getter_setter () in
    let get, set =
      match getter_setter with
      | None -> (fun () -> default), fun _ -> ()
      | Some (getter, setter) ->
        ( (fun () ->
            match getter unique_id with
            | None -> default
            | Some sexp ->
              (match Or_error.try_with (fun () -> M.t_of_sexp (Sexp.of_string sexp)) with
               | Ok a -> a
               | Error e ->
                 eprint_s
                   [%message
                     "WARNING: Could not deserialize var in session storage"
                       (unique_id : string)
                       (e : Error.t)];
                 default))
        , fun v -> setter unique_id (M.sexp_of_t v |> Sexp.to_string) )
    in
    let var = Ui_incr.Var.create (get ()) in
    let get =
      match getter_setter with
      | None -> fun () -> Ui_incr.Var.value var
      | Some _ -> get
    in
    let on_set = ref [] in
    let set v =
      Ui_incr.Var.set var v;
      set v;
      List.iter !on_set ~f:(fun f -> f ())
    in
    on_custom_storage_event ~unique_id ~callback:(fun () ->
      Ui_incr.Var.set var (get ());
      List.iter !on_set ~f:(fun f -> f ()));
    { var; get; set; on_set }
  ;;

  let get { get; _ } = get ()
  let set { set; _ } v = set v
  let add_on_set_listener { on_set; _ } f = on_set := f :: !on_set
  let to_incr { var; _ } = Ui_incr.Var.watch var
end
