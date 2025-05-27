# JavaScript Interop

For the most part, you should be able to write web apps with pure OCaml.
But in some [lower-level use cases](./low_level_vdom.md), you might need
to interact with runtime JavaScript APIs.

This article will be most useful for maintainers of low-level tools /
components.

## Be Very Careful!

You should treat:

-   Custom javascript files included via the build system
-   Any uses of `Js.Unsafe.*`
-   Any hand-written types for bindings

With particular scrutiny, as much of it cannot be checked by the
compiler. Additionally, since most tests run in
[Node.js](https://nodejs.org/en) (possibly with
[JSDom](./testing.md#jsdom)), you should thoroughly manually test these
kinds of changes.

```{=html}
```
## `Js_of_ocaml` and `ppx_js`

[`js_of_ocaml`](https://github.com/ocsigen/js_of_ocaml) is a compiler
from OCaml bytecode to Javacript or WebAssembly. If we mark an
executable as being "js_of_ocaml" in the build system, we can compile it
to JavaScript or WebAssembly.

The `Js_of_ocaml` library provides OCaml types and APIs for working with
JS/Wasm values. At it's core is `'a Js.t`, which represents a JavaScript
value of type `'a`. The `'a` is a phantom object type, describing the
properties and methods available on the JavaScript value.

The [runtime
representation](https://ocsigen.org/js_of_ocaml/latest/manual/library)
of OCaml values differs depending on whether they are compiled to
[JavaScript](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Data_structures)
or
[WebAssembly](https://developer.mozilla.org/en-US/docs/WebAssembly/Guides/Understanding_the_text_format).
In either case, you should consider this to be an implementation detail,
and NEVER interact with `Js.t` values as if they were normal OCaml
primitives.

You can convert primitives with the `Js_of_ocaml.Js` module. For
example, `Js.string` and `Js.float` convert OCaml strings / floats to
JavaScript strings / floats, and `Js.to_string` and `Js.to_float`
convert back.

```{=html}
<aside>
```
If some external API takes an arbitrary type, passing in + pulling out
OCaml values is fine, as long as they will only ever be used from OCaml
code, and will only be stored by the external system.
```{=html}
</aside>
```
In JavaScript, almost everything is an
[object](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Object);
i.e. values have various properties and methods.

To interact with `'a Js.t`s beyond just converting them to/from OCaml
with `Js.*` functions, you'll need to [use
`ppx_js`](https://ocsigen.org/js_of_ocaml/latest/manual/ppx), which
allows safely getting/setting properties, calling methods, and creating
objects, as long as the `'a` types are sound.

### Number Representation

JavaScript is a 32bit platform, where the OCaml `int` type is 32 bits.
WebAssembly is a 32bit platform, where the OCaml `int` type is 31 bits.
If you anticipate needing large ints, consider using `Int63` or `Int64`.

### Performance

Converting between OCaml and JavaScript values can have a cost,
especially for strings. For the vast majority of cases, this does not
matter. But it's good to remember, especially if you're wrangling a lot
of data, or dealing with large strings / lists.

## Browser Bindings

The `Js_of_ocaml.Dom_html` and `Js_of_ocaml.Dom` modules provide types
and helper functions for working with a subset of DOM APIs. These are
hand-written, incomplete, and occasionally incorrect.

If you need to interact with some property / method not exposed by these
types, you'll need to hand-write bindings. When doing this, be **very
explicit** about types:

```{=html}
<!-- $MDX file=../../examples/bonsai_guide_code/javascript_interop_examples.ml,part=custom_types_and_coerce -->
```
``` ocaml
open Js_of_ocaml

module Window_dimensions = struct
  type window =
    < innerHeight : Js.number Js.t Js.readonly_prop
    ; innerWidth : Js.number Js.t Js.readonly_prop >

  type t =
    { height : int
    ; width : int
    }

  let number_to_int (x : Js.number Js.t) : int = Js.float_of_number x |> Int.of_float

  let get =
    (* [Js.Unsafe.coerce] is pretty much [Obj.magic]! Be very explicit about annotating
       types when using it. *)
    let window : window Js.t = Js.Unsafe.coerce Dom_html.window in
    { height = number_to_int window##.innerHeight
    ; width = number_to_int window##.innerWidth
    }
  ;;
end
```

In contrast, if you used `Js.Unsafe.get`, it'd be easy to forget to
convert the `Js.number Js.t`s to `int`s.

We are currently working on generating bindings to DOM types and APIs
from [WebIDL](https://developer.mozilla.org/en-US/docs/Glossary/WebIDL),
which should hopefully eliminate the need to use `Js.Unsafe.*` for
dealing with the browser.

## Writing Bindings

Aside from browser APIs, you might also want to interact with some
JavaScript library you've pulled into your web app.

First, you'll need to figure out how those functions/values are exposed.
Typically, the external library you pull in will need to stick them on
the global object.

Then, you'll need to write bindings for accessing exposed APIs from the
global object. This will look similar to how we wrote custom bindings
for properties of `window` above, except you'll coerce
`Js.Unsafe.global` instead of `Dom_html.window`.

Finally, you'll need a set of types for interacting with the exposed
APIs. Be very careful while defining these! This is what the compiler
will use to ensure to typecheck your code.

`Js_of_ocaml` provides some [documentation on writing bindings for
libraries](https://ocsigen.org/js_of_ocaml/latest/manual/bindings).

A few things to keep in mind:

-   Anything you get by interacting directly with a `Js.t` via `ppx_js`
    should also be a `Js.t`
-   Avoid creating OCaml variables pointing to JavaScript variables in
    the toplevel, since almost everything in JavaScript is mutable, and
    might change during runtime
-   Never use `Obj.magic`. `Js.Unsafe.coerce` is also very unsafe, but
    it will at least enforce that you are coercing between `Js.t`s.
