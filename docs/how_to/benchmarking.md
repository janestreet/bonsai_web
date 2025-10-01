# Benchmarking

The `bonsai_bench` library which provides tools for benchmarking Bonsai
programs.

It currently runs benchmarks as javascript / wasm executables, on Node.
It's pretty good, although it's not quite as good as benchmarking in a
real browser: - We don't benchmark the diffing / patching of vdom - We
can't benchmark layout, style recalculations, etc - We don't run the
actual Bonsai runtime; rather, use `Bonsai_driver`

## Writing Benchmarks

```{=html}
```
You can create a benchmark for the [startup
time](./bonsai_runtime.md#startup) of some
`local_ graph -> 'a Bonsai.t`:

```{=html}
<!-- $MDX file=../../examples/bonsai_guide_code/bench/benchmarking_examples.ml,part=app_startup -->
```
``` ocaml
let app_startup_bench : Bonsai_bench.t =
  Bonsai_bench.create_for_startup ~name:"app startup" my_app
;;
```

It's likely you'll want to test a bunch of different values for some
parameter:

```{=html}
<!-- $MDX file=../../examples/bonsai_guide_code/bench/benchmarking_examples.ml,part=list_of_things_startup -->
```
``` ocaml
let list_of_things_bench : Bonsai_bench.t list =
  let create ~size =
    Bonsai_bench.create_for_startup
      ~name:[%string "List of %{size#Int} things"]
      (list_of_things ~size:(Bonsai.return size))
  in
  [ create ~size:1
  ; create ~size:5
  ; create ~size:10
  ; create ~size:100
  ; create ~size:1_000
  ]
;;
```

You can also benchmark how long it takes Bonsai to recompute in response
to some interactions, e.g. updating an input, or injecting an action:

```{=html}
<!-- $MDX file=../../examples/bonsai_guide_code/bench/benchmarking_examples.ml,part=state_interaction_bench -->
```
``` ocaml
open Bonsai_bench

let state_bench : Bonsai_bench.t =
  Bonsai_bench.create
    ~name:"Bonsai.state"
    ~component:(fun (local_ graph) ->
      let state, set_state =
        Bonsai.state 0 ~sexp_of_model:[%sexp_of: Int.t] ~equal:[%equal: Int.t] graph
      in
      let%arr state and set_state in
      state, set_state)
    ~get_inject:(fun (_, inject) -> inject)
    Interaction.(many_with_recomputes [ inject 1; reset_model ])
;;
```

You can run your `Bonsai_bench.t`s via `Bonsai_bench.run_via_command`:

```{=html}
<!-- $MDX file=../../examples/bonsai_guide_code/bench/benchmarking_examples.ml,part=running_benchmarks -->
```
``` ocaml
let () =
  Bonsai_bench.run_via_command
    ([ app_startup_bench ] @ list_of_things_bench @ [ state_bench ])
;;
```

Note that you should only use one \[run_via_command\] or
\[run_sets_via_command\] per file.

There are more examples in the [`example/`
directory](https://github.com/janestreet/bonsai_bench/tree/master/example).

## Bonsai_bench Profiler

You can also dig deeper into what's happening in a particular benchmark
with `Bonsai_bench.profile`.

```{=html}
<!-- $MDX file=../../examples/bonsai_guide_code/bench/benchmarking_examples.ml,part=profile -->
```
``` ocaml
let profile =
  Bonsai_bench.profile ~name:"Profiling Benchmarks" [ app_startup_bench; state_bench ]
;;

let () = Bonsai_bench.run_sets_via_command [ profile ]
```

```{=html}
<aside>
```
Because \[profile\] runs on an instrumented computation, the total
running time of the test may be higher. Furthermore, because \[profile\]
only runs the computation once, timing may vary between runs. It is
useful for drilling into slow benchmarks, but \[benchmark\] should be
the source of truth for timing interactions.
```{=html}
</aside>
```
## Comparison Benchmarking

`Bonsai_bench` also offers `compare_startup` and `compare_interactions`,
which will run a set of benchmarks across multiple implementations of
some Bonsai computation.

Let's say we want to compare the following implementations of "(foo a) +
(bar b)":

```{=html}
<!-- $MDX file=../../examples/bonsai_guide_code/bench/benchmarking_examples.ml,part=functions_to_compare -->
```
``` ocaml
let f1 a b =
  let%arr a and b in
  foo a + bar b
;;

let f2 a b =
  let foo_a =
    let%arr a in
    foo a
  in
  let bar_b =
    let%arr b in
    bar b
  in
  let%arr foo_a and bar_b in
  foo_a + bar_b
;;
```

To compare the startup times, we need to wrap our computations so that
they take a single input, give each a name, and provide some initial
input values to compare against:

```{=html}
<!-- $MDX file=../../examples/bonsai_guide_code/bench/benchmarking_examples.ml,part=startup_comparison -->
```
``` ocaml
let computations =
  let wrap f a_b (local_ _graph) =
    let%sub a, b = a_b in
    f a b
  in
  [ "f1", wrap f1; "f2", wrap f2 ]
;;

let startup_inputs = [ "same", (2, 2); "different", (1, 2) ]

let startup_set : Bonsai_bench.Benchmark_set.t =
  Bonsai_bench.compare_startup ~name:"Startup: f1 vs f2" ~computations startup_inputs
;;
```

And to compare recomputation time in response to some interactions, we
take the same wrapped and named computations, and specify some
`Bonsai_bench.Scenario.t`s:

```{=html}
<!-- $MDX file=../../examples/bonsai_guide_code/bench/benchmarking_examples.ml,part=interaction_comparison -->
```
``` ocaml
let scenarios =
  let update_inputs ~a_update_numerator ~a_update_denominator input =
    List.init 100 ~f:(fun i ->
      Bonsai_bench.Interaction.update_input
        input
        ~f:
          (if i mod a_update_denominator < a_update_numerator
           then Tuple2.map_fst ~f:(fun x -> x + 1)
           else Tuple2.map_snd ~f:(fun x -> x + 1)))
    |> Bonsai_bench.Interaction.many_with_recomputes
  in
  [ { Bonsai_bench.Scenario.test_name = "only a changes"
    ; initial = 2, 2
    ; interaction = update_inputs ~a_update_numerator:1 ~a_update_denominator:1
    }
  ; { Bonsai_bench.Scenario.test_name = "mostly a changes"
    ; initial = 2, 2
    ; interaction = update_inputs ~a_update_numerator:9 ~a_update_denominator:10
    }
  ; { Bonsai_bench.Scenario.test_name = "a and b change equally"
    ; initial = 2, 2
    ; interaction = update_inputs ~a_update_numerator:1 ~a_update_denominator:2
    }
  ; { Bonsai_bench.Scenario.test_name = "mostly b changes"
    ; initial = 2, 2
    ; interaction = update_inputs ~a_update_numerator:1 ~a_update_denominator:10
    }
  ; { Bonsai_bench.Scenario.test_name = "only b changes"
    ; initial = 2, 2
    ; interaction = update_inputs ~a_update_numerator:0 ~a_update_denominator:1
    }
  ]
;;

let interaction_set : Bonsai_bench.Benchmark_set.t =
  Bonsai_bench.compare_interactions
    ~name:"Interactions: f1 vs f2"
    ~get_inject:(fun _ _ -> Effect.Ignore)
    ~computations
    scenarios
;;
```

We can then run `Benchmark_set.t`s via
`Bonsai_bench.run_sets_via_command`:

```{=html}
<!-- $MDX file=../../examples/bonsai_guide_code/bench/benchmarking_examples.ml,part=running_sets -->
```
``` ocaml
let () =
  Bonsai_bench.run_sets_via_command
    [ startup_set
    ; interaction_set
    ; Bonsai_bench.set ~name:"list of things" list_of_things_bench
    ]
;;
```

It'll display a table, where the rows are initial inputs / scenarios,
and the columns are the various computation implementations.

For example, if `foo` is MUCH slower than `bar`, our results might look
like:

Startup:

  x           f1            f2
  ----------- ------------- ------------
  same        19.071291ms   18.64735ms
  different   18.740442ms   19.11809ms

Interaction:

  x                        f1             f2
  ------------------------ -------------- --------------
  only a changes           1.810346842s   1.825642143s
  mostly a changes         1.781062269s   1.68082669s
  a and b change equally   1.793728596s   935.796706ms
  mostly b changes         1.822570964s   202.889371ms
  only b changes           1.842941556s   434.993us

## Running a benchmark

To run the benchmark, pass it to `Bonsai_bench.bench`, a thin wrapper
around `Core_bench_js.bench` that handles necessary cleanup between
tests:

```{=html}
<!-- $MDX skip -->
```
``` ocaml
let () =
  let quota = Core_bench_js.Quota.Span (Time.Span.of_sec 1.0) in
  Bonsai_bench.bench ~run_config:(Core_bench_js.Run_config.create () ~quota) [ state ]
```

Build the `javascript-executables` target for the directory with
`BUILD_PROFILE=fast-exe`. Then, from a terminal, run:
`node path/to/executable/name.bc.js` to see the results.

```{=html}
<!--
  --expose-gc       : allows you to run a garbage collection using [gc()] at any point
  --gc-global       : will perform a stop-the-world global garbage collection
  --gc-interval <n> : will gc after <n> allocations.  Setting this very high might let
                      us prevent garbage collection during test runs.
  --always-compact  : force compaction during gc
  --random-seed <n> : seed to use for randomness
-->
```
