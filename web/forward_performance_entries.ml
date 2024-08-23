open! Core
open Bonsai.Private

type 'result t = { instrumented_computation : 'result Bonsai.Private.Computation.t }

let instrument component =
  let component =
    Bonsai.Private.Graph_info.iter_graph_updates
      component
      ~on_update:For_profiling.set_latest_graph_info
  in
  let instrumented_computation =
    Instrumentation.instrument_computation
      component
      ~start_timer:(fun s -> Javascript_profiling.Manual.mark (s ^ "before"))
      ~stop_timer:(fun s ->
        let before = s ^ "before" in
        let after = s ^ "after" in
        Javascript_profiling.Manual.mark after;
        Javascript_profiling.Manual.measure ~name:s ~start:before ~end_:after)
  in
  { instrumented_computation }
;;
