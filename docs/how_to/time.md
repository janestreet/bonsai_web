# Time

`Bonsai.Clock` contains some utils for dealing with time. You should use
this over any other time sources, because it is designed for the
browser, and can be controlled in tests.

## Accessing Time

We can access the current time as a `Time_ns.t Bonsai.t`:

```{=html}
<!-- $MDX file=../../examples/bonsai_guide_code/lib/time_examples.ml,part=clock_approx_now -->
```
``` ocaml
let approx_current_time (local_ graph) =
  let%arr now = Bonsai.Clock.approx_now ~tick_every:(Time_ns.Span.of_sec 1.) graph in
  Vdom.Node.text (Time_ns.to_string_utc now)
;;
```

```{=html}
<iframe data-external="1" src="https://bonsai:8535#clock_approx_now">
```
```{=html}
</iframe>
```
There's also a `Bonsai.Clock.now`, which will update once every frame:

```{=html}
<!-- $MDX file=../../examples/bonsai_guide_code/lib/time_examples.ml,part=clock_now -->
```
``` ocaml
let current_time (local_ graph) =
  let%arr now = Bonsai.Clock.now graph in
  Vdom.Node.text (Time_ns.to_string_utc now)
;;
```

```{=html}
<iframe data-external="1" src="https://bonsai:8535#clock_now">
```
```{=html}
</iframe>
```
You should be very careful with `Clock.now`, because any computations
depending on it will have to recompute, and possibly re-patch the DOM,
every frame.

If you just want to display how long has passed since some time, you can
also use \[vdom_time_ago\], which is implemented entirely in vdom, and
won't impact your Bonsai computations:

```{=html}
<!-- $MDX file=../../examples/bonsai_guide_code/lib/time_examples.ml,part=vdom_time_ago -->
```
``` ocaml
let vdom_time_ago (local_ graph) =
  let time_since, set_time_since = Bonsai.state (Time_ns.now ()) graph in
  let%arr time_since and set_time_since in
  Vdom.Node.div
    [ Vdom.Node.button
        ~attrs:[ Vdom.Attr.on_click (fun _ -> set_time_since (Time_ns.now ())) ]
        [ Vdom.Node.text "Update base time" ]
    ; Vdom_time_ago.view time_since
    ]
;;
```

```{=html}
<iframe data-external="1" src="https://bonsai:8535#vdom_time_ago">
```
```{=html}
</iframe>
```
If we want to access the current time in effects without it [becoming
stale](./effects_and_stale_values.mdx), we can use
`Bonsai.Clock.get_current_time`:

```{=html}
<!-- $MDX file=../../examples/bonsai_guide_code/lib/time_examples.ml,part=current_time_effect -->
```
``` ocaml
let measure_time (local_ graph) =
  let%arr get_time = Bonsai.Clock.get_current_time graph in
  Vdom.Node.button
    ~attrs:
      [ Vdom.Attr.on_click (fun _ ->
          let%bind.Effect start = get_time in
          let%bind.Effect () = long_effect in
          let%bind.Effect end_ = get_time in
          let diff = Time_ns.diff end_ start |> Time_ns.Span.to_string_hum in
          Effect.alert [%string "that took: %{diff}"])
      ]
    [ Vdom.Node.text "Click to measure a long effect." ]
;;
```

```{=html}
<iframe data-external="1" src="https://bonsai:8535#current_time">
```
```{=html}
</iframe>
```
## Delaying Effects

`Bonsai.Clock.sleep` can be used to delay an effect (like `setTimeout`):

```{=html}
<!-- $MDX file=../../examples/bonsai_guide_code/lib/time_examples.ml,part=clock_sleep -->
```
``` ocaml
let clock_sleep_demo (local_ graph) =
  let%arr sleep = Bonsai.Clock.sleep graph in
  Vdom.Node.button
    ~attrs:
      [ Vdom.Attr.on_click (fun _ ->
          let%bind.Effect () = sleep (Time_ns.Span.of_sec 2.) in
          Effect.alert "... 2 seconds later...")
      ]
    [ Vdom.Node.text "delayed alert" ]
;;
```

```{=html}
<iframe data-external="1" src="https://bonsai:8535#clock_sleep">
```
```{=html}
</iframe>
```
## Scheduling Effects

And `Bonsai.Clock.every` runs an effect repeatedly:

```{=html}
<!-- $MDX file=../../examples/bonsai_guide_code/lib/time_examples.ml,part=clock_every -->
```
``` ocaml
let clock_every_demo (local_ graph) =
  let count, set_count = Bonsai.state 0 graph in
  Bonsai.Clock.every
    ~when_to_start_next_effect:`Every_multiple_of_period_blocking
    ~trigger_on_activate:false
    (Time_ns.Span.of_sec 1.0)
    (let%arr count and set_count in
     set_count (count + 1))
    graph;
  let%arr count in
  Vdom.Node.text [%string "Seconds since you opened the page: %{count#Int}"]
;;
```

```{=html}
<iframe data-external="1" src="https://bonsai:8535#clock_every">
```
```{=html}
</iframe>
```
