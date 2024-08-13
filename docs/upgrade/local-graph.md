Bonsai's new local\_ graph API is finally here! We recommend looking
through the[new mli
file](https://github.com/janestreet/bonsai/blob/master/src/bonsai.mli)
to see how types have changed.

## Moving from `Value`/`Computation` to `local_ Bonsai.graph` and `Bonsai.t`

You can convert your codebase to Bonsai's new API one file at a time!
The process is fairly mechanical and straightforward:

1.  Replace any `open! Bonsai_web` with `open! Bonsai_web.Cont`
2.  Rename any `'a Value.t` to `'a Bonsai.t`
3.  Rename any `'a Computation.t` to
    `local_ Bonsai.graph -> 'a Bonsai.t`
4.  For the most part, `let%sub` is no longer needed. Instead,
    instantiate Bonsai components by passing `graph` to them.
5.  `let%arr`s don't need to be modified at all. They work like fancy
    `let%map`s. See below for more discussion on this topic.
6.  Components can now return tuples / records of `Bonsai.t`s! If you
    were previously returning `('a * 'b) Bonsai.t`, consider returning
    `(a Bonsai.t * 'b Bonsai.t)` instead. This is because we no longer
    need `let%sub`, so there's no longer anything enforcing that our
    components must return a single `Computation.t`.

Any Bonsai component that uses Bonsai primitives, either directly or
transitively, will need a `local_ graph` parameter. This is fine and
expected.

If anything breaks or you're unsure of something, please reach out!

## let%map vs let%arr

You might have noticed that we have removed all of the `let%sub`s but
still have `let%arr`s. Previously `let%map`s were always considered
harmful, and in the `local_ graph` that's no longer the case! `let%map`
and `let%arr` have the same type and can be used interchangebly in the
simple case of `let%map_or_arr foo = foo in`. This is incredibly nice
for newcomers who are familiar with `let%map` from the various common
monads which support it. That said, `let%arr` still has some fancy
support for incremental computation built in!

`let%arr` has performance benefits over `let%map` when you are pattern
matching: `let%arr { a; _ } = r` will only trigger incremental updates
when `a` changes, while `let%map` will trigger when any field in `r`
changes. As a guideline: if anything on the left side of your `let%arr`s
is a pattern with ignored parts (e.g. `let%arr a, _ = ...`;
`let%arr {a; _} = ...`), keep them as `let%arr`s for now.

## Patterns

We continue to recommend splitting non-trivial stateful components up
into modular pieces when possible. This should be even more flexible
with the `local_ graph` API.

It's also often good practice to split the part of your component that
generates vdom view into a separate, pure OCaml function.

More pattterns to come soon!

## API Changes

Alongside the new `local_ graph` API, we'd like to do some housekeeping
on the APIs Bonsai provides, and how they are organized. Some of these
are new features that are now possible because of `local_ graph`, others
are just cleanup we've been meaning to do for a while.

### Changed

-   `Bonsai.yoink` -\> `Bonsai.peek`: still uses the same api (with
    graph changes) - just a new name! Peek is a better representation of
    what is happening and is more inline with what this operation is
    called on other data structures (stacks, Deferred.t, Mvar.t, etc).
-   Most `Bonsai.Value.t` combinators are now available directly in the
    `Bonsai` module for use with `Bonsai.t`s.

### Removed

Many combinators for dealing with `Bonsai.Value.t` and
`Bonsai.Computation` are no longer needed, and are not included in the
new API. If you think we've forgotten something useful, please reach
out!

### Known Issues

#### Top-level `match%sub`

Top-level `match%sub`'s raise an exception on app startup. For example:

**Raises**:

`ocaml skip let component =   match%sub ... with   | ... -> ...`

**OK**:

`ocaml skip let component (local_ graph) =   match%sub ... with   | ... -> ...`

**OK**: (if only called from inside a Bonsai context)

`ocaml skip let component () =   match%map ... with   | ... -> ...`

**OK**:

`ocaml skip let component =   match%map ... with   | ... -> ...`
