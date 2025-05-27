# Bonsai's Runtime

```{=html}
```
```{=html}
```
A Bonsai web app is really just a giant file of JavaScript that
[js_of_ocaml](./javascript_interop.md) compiles from your OCaml
bytecode. When you open your web app in the browser, that JavaScript
runs sequentially. Your web app starts because in your `main.ml`, you
wrote:

```{=html}
<!-- $MDX file=../../examples/bonsai_guide_code/bonsai_runtime_examples.ml,part=start -->
```
``` ocaml
let () = Bonsai_web.Start.start My_app.component
```

What does `Bonsai_web.Start.start` do?

## Startup Phase

The first thing Bonsai does is run your
`local_ graph -> Vdom.Node.t Bonsai.t` function. As it does so, it
updates `graph` to create a static computation graph for your Bonsai
program. This includes all the [state](../guide/04-state.md),
[lifecycles](./lifecycles.md), and other \[Bonsai.\*\] APIs that take a
`graph`. It also includes all the `let%arr`s and `match%sub`s in your
Bonsai program: Bonsai's internals can get a `graph` implicitly.

The result is an internal type to Bonsai called `Computation.t`. You
don't need to worry about [how exactly it
works](../advanced/how_bonsai_works.md), but it allows Bonsai to do some
pre-processing / optimization.

After that pre-processing step, Bonsai creates a single Incremental Var
that stores your app's state, and a `Queue.t` for [pending
actions](#action-application). Then, it "compiles" the `Computation.t`
into a `Vdom.Node.t Incremental.t`, into which it threads accessors for
all the various leaves of state.

Finally, Bonsai runs an Incremental stabilization to compute an initial
`Vdom.Node.t`, converts that into a DOM node using `virtual_dom`, and
attaches that into the DOM.

It also sets up a [listener that transfers
focus](./focus.md#bonsai-focus-stealer) from the `body` element to your
app root.

## Runtime Loop

At this point, Bonsai has started. It is now in the "runtime" phase,
where it will:

-   Update states
-   Recompute the view
-   Update the DOM
-   Run lifecycles
-   Schedule the next frame

forever. The rest of this doc will go into more detail about this
runtime loop.

### Clock Flush

The first thing we do every frame is update our [internal
clock](./time.md) to the "current time" as [reported by the
browser](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date/now),
then run any `Effect.t`s that were scheduled via the clock for a time
that has passed.

### Action Application

"Action Application" is when Bonsai updates all of its states and state
machines. To understand actions, we need to clarify how `Effect.t`s run.

When an `Effect.t` runs due to a [lifecycle event](./lifecycles.md), a
[DOM event listener](../guide/01-virtual_dom.md#event-handlers), an
[on_change](./edge_triggered_effects.md), or anything else that
ultimately calls `Effect.Expert.handle` (do not use this outside of
[hooks/widgets](./low_level_vdom.md)!):

-   If the effect is an `Effect.Ignore`, it completes immediately
-   If the effect is a synchronous function (e.g. `Effect.of_sync_fun`,
    `Effect.of_thunk`), it runs the function, and then completes
-   If the effect is a deferred function, it runs the function, and
    completes when the `Deferred.t` gets filled
-   If the effect is an `Effect.Many`, it runs all effects within it,
    then completes without waiting for them to complete
-   If the effect is an `Effect.bind`, it waits for the `Effect.t` being
    binded on to complete, then runs the `~f`, and waits for that
    `Effect.t` to complete.

If an effect is injecting an action into a `Bonsai.state_machine` (which
includes setting a `Bonsai.state`), it will put the action onto a global
`Queue.t`, then complete whenever the action is eventually applied.

The runtime loop's *action application* phase iterates through the
action queue. For each action, it:

-   If the next action is a `state_machine1`, runs incremental
    stabilization, so that `state_machine1` receives an up-to-date input
-   Applies the action, completing the effect that queued the action
-   Updates the Bonsai state var to reflect the new state

It does so until the action queue is empty, so that that multiple
sequentially composed `Effect.t`s that each apply actions will all run
within the next frame.

### Stabilization

After action application, Bonsai does another incremental stabilization
to compute the latest view.

### Diff + Patch

Virtual_dom diffs the new view against the previous one, and patches the
DOM.

Immediately after, any [hook `on_mount`s](./low_level_vdom.md#hooks)
that use `` `Schedule_immediately_after_this_dom_patch_completes `` will
run.

### Lifecycles

After diffing / patching the DOM, Bonsai will run any lifecycle events
scheduled for that frame. As a result, [lifecycle
events](./lifecycles.md) (which include
[on_change](./edge_triggered_effects.md)) will only run once per frame.

Note that we do not stabilize or apply actions after diffing + patching
or lifecycle scheduling. This means that any state updates scheduled by
lifecycle events will only be applied in the next frame. This is one
reason why it's preferable to avoid needing to [synchronize
states](./organizing_state.md).

### Scheduling the Next Frame

Finally, Bonsai schedules the next frame using the browser's
[`requestAnimationFrame`
API](https://developer.mozilla.org/en-US/docs/Web/API/Window/requestAnimationFrame).
Because `requestAnimationFrame` might not run while the tab is
backgrounded, we will also schedule the next frame via `setTimeout` for
1000ms, and use whichever happens first.
