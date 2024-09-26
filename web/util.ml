open! Core
open! Js_of_ocaml

let am_running_how
  : [ `Browser
    | `Browser_test
    | `Browser_benchmark
    | `Node
    | `Node_benchmark
    | `Node_test
    | `Node_jsdom_test
    ]
  =
  Am_running_how_js.am_running_how
;;

let am_within_disabled_fieldset (event : #Dom_html.event Js.t) =
  match am_running_how with
  | `Node_test | `Node_jsdom_test | `Node_benchmark | `Node -> false
  | `Browser | `Browser_test | `Browser_benchmark ->
    let (event : < composedPath : 'a Js.js_array Js.t Js.meth ; Dom_html.event > Js.t) =
      Js.Unsafe.coerce event
    in
    Js.to_array event##composedPath
    |> Array.exists ~f:(fun element ->
      let tag_name = Js.Optdef.to_option element##.tagName in
      let disabled = Js.Optdef.to_option element##.disabled in
      match Option.both tag_name disabled with
      | None -> false
      | Some (tag_name, disabled) ->
        String.equal (Js.to_string tag_name) "FIELDSET" && Js.to_bool disabled)
;;

module For_bonsai_internal = struct
  let set_stack_overflow_exception_check () =
    let get_test_truncated_trace =
      match am_running_how with
      | `Browser | `Node -> Fn.id
      | `Node_test
      | `Node_jsdom_test
      | `Node_benchmark
      | `Browser_test
      | `Browser_benchmark ->
        fun stack_trace ->
          let first_line =
            Core.String.split_lines stack_trace |> List.hd |> Option.value ~default:""
          in
          sprintf
            {|%s
<truncated stack to preserve determinism between fast-build and fast-exe>|}
            first_line
    in
    Bonsai.Private.set_perform_on_exception (fun exn ->
      match exn with
      | Stack_overflow ->
        let stack_trace =
          Js_of_ocaml.Js.Js_error.of_exn exn
          |> Option.bind ~f:Js_of_ocaml.Js.Js_error.stack
          |> Option.value_map
               ~default:"<no stack trace found>"
               ~f:get_test_truncated_trace
        in
        eprintf
          {|Stack overflow inside of a bonsai computation is not supported! In a future release your app might crash.
%s|}
          stack_trace
      | _ -> ())
  ;;
end
