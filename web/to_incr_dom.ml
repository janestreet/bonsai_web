open! Core
open! Async_kernel
open! Import
open Incr.Let_syntax
include To_incr_dom_intf
module Bonsai_action = Bonsai.Private.Action

let () = Lazy.force For_profiling.run_top_level_side_effects

module State = struct
  type t = { mutable last_lifecycle : Bonsai.Private.Lifecycle.Collection.t }

  let create () = { last_lifecycle = Bonsai.Private.Lifecycle.Collection.empty }
end

let convert_generic
  (type input action_input model action extra)
  ~computation
  ~fresh
  ~recursive_scopes
  ~time_source
  (info : (model, action, action_input, _, unit) Bonsai.Private.Computation.info)
  : (module S with type Input.t = input and type Extra.t = extra)
  =
  let equal_model = info.model.equal in
  let sexp_of_model = info.model.sexp_of in
  let default_model = info.model.default in
  (module struct
    module Input = struct
      type t = input
    end

    module Model = struct
      type t = model [@@deriving equal, sexp_of]

      let default = default_model
    end

    module Action = struct
      type t = action Bonsai_action.t

      let sexp_of_t = Bonsai_action.Type_id.to_sexp info.action
    end

    module Extra = struct
      type t = extra
    end

    module State = State

    type t = (Action.t, Model.t, State.t, Extra.t) Incr_dom.Component.with_extra

    let create_generic
      ~fresh
      ~input
      ~(model : model Incr.t)
      ~(inject : action Bonsai_action.t -> unit Ui_effect.t)
      ~apply_action
      run
      =
      let environment =
        Bonsai.Private.Environment.(empty |> add_exn ~key:fresh ~data:input)
      in
      let snapshot, () =
        run
          ~environment
          ~fix_envs:Bonsai.Private.Environment.Recursive.empty
          ~path:Bonsai.Private.Path.empty
          ~model
          ~inject
        |> Bonsai.Private.Trampoline.run
      in
      let%map view, extra = Bonsai.Private.Snapshot.result snapshot
      and input =
        Bonsai.Private.Input.to_incremental (Bonsai.Private.Snapshot.input snapshot)
      and lifecycle = Bonsai.Private.Snapshot.lifecycle_or_empty snapshot ~here:[%here]
      and model in
      let schedule_event = Vdom.Effect.Expert.handle_non_dom_event_exn in
      let apply_action action _state ~schedule_action:_ =
        apply_action ~inject ~schedule_event (Some input) model action
      in
      let on_display state ~schedule_action:_ =
        let diff =
          Bonsai.Private.Lifecycle.Collection.diff state.State.last_lifecycle lifecycle
        in
        state.State.last_lifecycle <- lifecycle;
        Vdom.Effect.Expert.handle_non_dom_event_exn diff;
        Bonsai.Time_source.Private.trigger_after_display
          Incr_dom.Start_app.Private.time_source;
        For_profiling.log_all_computation_watcher_nodes_in_javascript_console ()
      in
      Incr_dom.Component.create_with_extra ~on_display ~extra ~apply_action model view
    ;;

    let create ~input ~old_model:_ ~model ~inject =
      Bonsai.Private.Instrumentation.create_computation_with_instrumentation
        For_profiling.default_instrumentation_for_incr_dom_start_app
        ~computation
        ~time_source
        ~recursive_scopes
        ~f:(create_generic ~fresh ~input ~model ~inject ~apply_action:info.apply_action)
        info
    ;;
  end)
;;

let default_custom_connector _connector =
  raise_s
    [%message
      "The component passed to [Bonsai_web.to_incr_dom] used a custom connector, but \
       none was provided when the app was started. To fix this, use the \
       [~custom_connector] argument when calling [Bonsai_web.Start.start]"]
;;

let convert_with_extra
  ?(custom_connector = default_custom_connector)
  ?(optimize = false)
  component
  =
  let fresh = Type_equal.Id.create ~name:"" sexp_of_opaque in
  let var = Bonsai.Private.(Value.named App_input fresh |> conceal_value) in
  let maybe_optimize = if optimize then Bonsai.Private.pre_process else Fn.id in
  let recursive_scopes = Bonsai.Private.Computation.Recursive_scopes.empty in
  let () = () in
  let component input graph =
    Rpc_effect.Private.with_connector
      (function
        | Self { on_conn_failure } ->
          Rpc_effect.Private.self_connector ~on_conn_failure ()
        | Url { url; on_conn_failure } ->
          Rpc_effect.Private.url_connector ~on_conn_failure url
        | Custom custom -> custom_connector custom)
      (fun graph -> component input graph)
      graph
  in
  let computation = component var |> Bonsai.Private.top_level_handle |> maybe_optimize in
  let time_source = Incr_dom.Start_app.Private.time_source in
  let (T info) = Bonsai.Private.gather ~recursive_scopes ~time_source computation in
  convert_generic ~computation ~fresh ~time_source ~recursive_scopes info
;;

let convert ?custom_connector ?optimize component =
  convert_with_extra ?custom_connector ?optimize (fun input (local_ graph) ->
    Bonsai.map (component input graph) ~f:(fun r -> r, ()))
;;
