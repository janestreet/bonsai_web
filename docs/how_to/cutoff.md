# Incremental Cutoffs

An important property of incremental computations is that they are only
re-run when their inputs change. This provides a cutoff mechanism.

For example, if `a` changes from `2` to `4`, `mod_two_a` is recomputed,
but won't change, so we don't need to re-run
`expensive_combine mod_two_a mod_three_b`.

```{=html}
<!-- $MDX file=../../examples/bonsai_guide_code/cutoff_examples.ml,part=mod_cutoff -->
```
``` ocaml
let computation (a : int Bonsai.t) (b : int Bonsai.t) : int Bonsai.t =
  let mod_two_a =
    let%arr a in
    a mod 2
  in
  let mod_three_b =
    let%arr b in
    b mod 3
  in
  let%arr mod_two_a and mod_three_b in
  expensive_combine mod_two_a mod_three_b
;;
```

By default, whether a `Bonsai.t` "has changed" is determined by
`phys_equal`. In practice, this usually suffices! But sometimes, you
might mint values that are equal, but not `phys_equal`. In this case,
you can use `Bonsai.cutoff` to provide a custom equality function:

```{=html}
<!-- $MDX file=../../examples/bonsai_guide_code/cutoff_examples.ml,part=custom_cutoff -->
```
``` ocaml
let computation (a : int Bonsai.t) (b : int Bonsai.t) : int Bonsai.t =
  let secret_a =
    (let%arr a in
     derive_secret a)
    |> Bonsai.cutoff ~equal:[%equal: Secret.t]
  in
  let secret_b =
    (let%arr b in
     derive_secret b)
    |> Bonsai.cutoff ~equal:[%equal: Secret.t]
  in
  let%arr secret_a and secret_b in
  expensive_combine secret_a secret_b
;;
```

We can use custom cutoffs to implement some cool behavior, e.g. track
the last time that a value changed:

```{=html}
<!-- $MDX file=../../examples/bonsai_guide_code/cutoff_examples.ml,part=last_modified_time -->
```
``` ocaml
let with_last_modified_time
  ~equal
  input
  (* Although [Bonsai.Clock.Expert.now] is generally discouraged, the cutoff only pays
     attention to [input], so [now] shouldn't cause re-firing of this computation's
     transitive dependencies. *)
  (local_ graph)
  =
  let now = Bonsai.Clock.Expert.now graph in
  let result = Bonsai.both input now in
  let%sub result, time =
    Bonsai.Incr.value_cutoff result ~equal:(fun (a, _) (b, _) -> equal a b) graph
  in
  result, time
;;
```
