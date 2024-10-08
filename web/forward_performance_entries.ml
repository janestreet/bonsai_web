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
      ~start_timer:(fun s -> s, Javascript_profiling.Timer.start ())
      ~stop_timer:(fun (s, timer) ->
        let measurement = Javascript_profiling.Timer.stop timer in
        Javascript_profiling.measure s measurement)
  in
  { instrumented_computation }
;;
