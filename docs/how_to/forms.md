# Forms

Bonsai has an entire library dedicated to building and combining forms
called `Bonsai_web_ui_form`.

```{=html}
```
There are two submodules within `Bonsai_web_ui_form`:

1.  `Bonsai_web_ui_form.With_manual_view` is the newer, recommended way
    to build forms and gives you full control over the composition of
    the views of your form.
2.  `Bonsai_web_ui_form.With_automatic_view` is a legacy implementation
    and composes the views of your form automatically, but in a highly
    opinionated and not-so-customizable way.

Both modules share an underlying type, which makes it easy to convert
back and forth, to assist in [migration of legacy
forms](#migrating-from-legacy-forms) to the new recommendation. For the
rest of this doc, we'll focus on manual view forms and this module alias
will be in effect:

```{=html}
<!-- $MDX skip -->
```
``` ocaml
module Form = Bonsai_web_ui_form.With_manual_view
```

# Form.t

The primary type in the forms library is `('a, 'view) Form.t`:

```{=html}
<!-- $MDX skip -->
```
``` ocaml
type ('a, 'view) Form.t =
  { value : 'a Or_error.t
  ; view : 'view
  ; set : 'a -> unit Ui_effect.t
  }
```

A value of type `('a, 'view) Form.t` represents the state of a form at
one particular instant in time, where the form in question can edit
values of type `'a` and whose view is represented by a `'view`.

Because of the inherently stateful nature of form UIs, form building
functions take `(local_ graph)`. For example, a Vdom-based textbox form
element that produces strings has type:

```{=html}
<!-- $MDX skip -->
```
``` ocaml
val Form.Elements.Textbox.string
  :  ?placeholder:string Bonsai.t
  -> ?extra_attrs:Vdom.Attr.t list Value.t
  -> unit
  -> local_ Bonsai.graph
  -> (string, Vdom.Node.t) Form.t Bonsai.t
```

And the type for a checkbox that produces bools has this type:

```{=html}
<!-- $MDX skip -->
```
``` ocaml
val Form.Elements.Checkbox.bool
  :  ?extra_attrs:Vdom.Attr.t list Value.t
  -> default:bool
  -> unit
  -> local_ Bonsai.graph
  -> (bool, Vdom.Node.t) Form.t Bonsai.t
```

As we saw above, a form has three pieces. Let's go into more depth on
what each of them are useful for.

## Form.value

The `value` of a `('a, 'view) Form.t` is a `'a Or_error.t` representing
the current output of the form as filled in by the user. For a simple
textbox, `'a` might be `string`. Bonsai Forms provides facilities for
building larger, more complex forms out of smaller pieces, so the `'a`
could be any OCaml type.

In the following example, the value of a textbox is extracted and
printed as a sexp:

```{=html}
<!-- $MDX file=../../examples/bonsai_guide_code/form_examples.ml,part=form_textbox_value -->
```
``` ocaml
let textbox_value (local_ graph) =
  let textbox =
    Form.Elements.Textbox.string ~allow_updates_when_focused:`Always () graph
  in
  let%arr textbox in
  let view = Form.view textbox in
  let value = Form.value textbox in
  Vdom.Node.div
    [ View.hbox ~gap:(`Px 5) [ Vdom.Node.text "my textbox"; view ]
    ; Vdom.Node.sexp_for_debugging ([%sexp_of: string Or_error.t] value)
    ]
;;
```

```{=html}
<iframe data-external="1" src="https://bonsai:8535#form_textbox_value">
```
```{=html}
</iframe>
```
Forms returning their values inside of an `Or_error.t` may be surprising
at first, but in practice, many kinds of forms are fallible, either
because validation has failed, or because a large form is only partially
filled out. By making the assumption that *all* forms are fallible, the
rest of the API is simpler.

## Form.view

The `view` of a form is an arbitrary value representing how the form
should look. Leaf input elements generally have `Vdom.Node.t` as their
`view`, while a form that produces a tuple might have a `view` with a
tuple of `Vdom.Node.t`. Eventually, you'll use the `view` to construct
the `Vdom.Node.t` you display in your app.

Forms provides `Form.map_view` for modifying the view of a form. For
example, we can use it to add a label to a textbox:

```{=html}
<!-- $MDX file=../../examples/bonsai_guide_code/form_examples.ml,part=textbox_with_label -->
```
``` ocaml
let labelled_textbox (label : string Bonsai.t) (local_ graph)
  : (string, Vdom.Node.t) Form.t Bonsai.t
  =
  let textbox =
    Form.Elements.Textbox.string ~allow_updates_when_focused:`Always () graph
  in
  let%arr textbox and label in
  Form.map_view textbox ~f:(fun view ->
    View.hbox ~gap:(`Px 5) [ Vdom.Node.text label; view ])
;;
```

```{=html}
<iframe data-external="1" src="https://bonsai:8535#textbox_with_label">
```
```{=html}
</iframe>
```
Some other common uses of `Form.map_view` include:

### Displaying an error to the user

It's good practice to let users know when the form is in a bad state.
Suppose you write the following function for displaying errors to a
user:

```{=html}
<!-- $MDX skip -->
```
``` ocaml
(* Render a warning sign and show the error text to the user *)
val display_error : Error.t -> Vdom.Node.t
```

It's easy to incorporate this function into the forms code! Suppose we
want to augment an `int` textbox with its error message:

```{=html}
<!-- $MDX file=../../examples/bonsai_guide_code/form_examples.ml,part=textbox_with_error -->
```
``` ocaml
let display_error error =
  View.text ~attrs:[ {%css|color: red;|} ] [%string "⚠  %{Error.to_string_hum error}"]
;;

let int_with_error_display (local_ graph) =
  let textbox = Form.Elements.Textbox.int ~allow_updates_when_focused:`Always () graph in
  let%arr textbox in
  Form.map_view textbox ~f:(fun view ->
    let error_display =
      match Form.value textbox with
      | Ok _ -> Vdom.Node.none
      | Error error -> display_error error
    in
    View.hbox ~gap:(`Px 5) [ view; error_display ])
;;
```

```{=html}
<iframe data-external="1" src="https://bonsai:8535#textbox_with_error">
```
```{=html}
</iframe>
```
### Submit Controls

Many forms are explicitly submitted by the user by clicking a button, as
opposed to automatic submissions whenever values change. We can write a
general function which adds a submit button underneath a form:
`<!-- $MDX file=../../examples/bonsai_guide_code/form_examples.ml,part=with_submit_button -->`{=html}

``` ocaml
let with_submit_button (form : ('a, 'view) Form.t) ~(on_submit : 'a -> unit Effect.t) =
  let submit_button ~extra_attrs =
    Vdom.Node.button
      ~attrs:({%css|width: max-content;|} :: extra_attrs)
      [ Vdom.Node.text "Submit!" ]
  in
  Form.map_view form ~f:(fun view ->
    let submit_button =
      match Form.value form with
      | Ok value ->
        submit_button ~extra_attrs:[ Vdom.Attr.on_click (fun _ -> on_submit value) ]
      | Error _ -> submit_button ~extra_attrs:[ Vdom.Attr.disabled ]
    in
    View.vbox ~gap:(`Px 5) [ view; submit_button ])
;;
```

Now, look how easy it is to add a submit button to our textbox with
error messages above:
`<!-- $MDX file=../../examples/bonsai_guide_code/form_examples.ml,part=textbox_with_submit -->`{=html}

``` ocaml
let textbox_with_submit (local_ graph) =
  let textbox = int_with_error_display graph in
  let%arr textbox in
  with_submit_button textbox ~on_submit:(fun value -> Effect.alert (Int.to_string value))
;;
```

```{=html}
<iframe data-external="1" src="https://bonsai:8535#textbox_with_submit">
```
```{=html}
</iframe>
```
## Form.set

Setting the contents of a form is a rarer requirement. Most forms are
read-only (the user is the only one filling it out), but sometimes, a
form should be modified by the program, perhaps to initialize the form
in a specific state.

```{=html}
<!-- $MDX file=../../examples/bonsai_guide_code/form_examples.ml,part=form_set -->
```
``` ocaml
let form_set (local_ graph) =
  let textbox =
    Form.Elements.Textbox.string ~allow_updates_when_focused:`Always () graph
  in
  let%arr textbox in
  Vdom.Node.div
    [ View.hbox ~gap:(`Px 5) [ Vdom.Node.text "my textbox"; Form.view textbox ]
    ; Vdom.Node.button
        ~attrs:[ Vdom.Attr.on_click (fun _ -> Form.set textbox "hello world") ]
        [ Vdom.Node.text "click me" ]
    ]
;;
```

```{=html}
<iframe data-external="1" src="https://bonsai:8535#form_set">
```
```{=html}
</iframe>
```
# Projection

Notably missing in the Forms API is a "map" function. In its place is
`Form.project`, which has this type signature:

```{=html}
<!-- $MDX skip -->
```
``` ocaml
val project
  : ?extend_view_with_error:('view -> Error.t -> 'view)
  -> ('a, 'view) t
  -> parse_exn:('a -> 'b)
  -> unparse:('b -> 'a)
  -> ('b, 'view) t
```

`project` is a way to move from a form producing values of type `'a` to
a form producing values of type `'b`, but it requires two "mapping"
functions, `parse_exn`, which moves from `'a` to `'b` as you'd expect,
but the other, `unparse`, goes in the opposite direction!

`unparse` is required because `Form.set` needs to be able to accept
values of type `'b` and turn them back into `'a`s to be set into the
form being projected.

In practice, `project` is used to build forms for types that can be
parsed from other types. For example, `Form.Elements.Textbox.int` is
implemented like so:

```{=html}
<!-- $MDX file=../../examples/bonsai_guide_code/form_examples.ml,part=int_textbox -->
```
``` ocaml
let int (local_ graph) : (int, Vdom.Node.t) Form.t Bonsai.t =
  let form = Form.Elements.Textbox.string ~allow_updates_when_focused:`Always () graph in
  let%arr form in
  Form.project form ~parse_exn:Int.of_string ~unparse:Int.to_string
;;
```

```{=html}
<iframe data-external="1" src="https://bonsai:8535#int_textbox">
```
```{=html}
</iframe>
```
You'll notice that non-integers are reported as an error. `Form.project`
captures the exception thrown by `Int.of_string` and the `Form.value`
returned by the `project`ed form is an `Error`. The optional
`extend_view_with_error` method gives you an easy way to incorporate
this error into the form's view.

## Validation is just projection!

You may have observed that `Form.project` can also be used for
validation. If we take `'b = 'a` in the type signature for
`Form.project` and provide the identity function for `unparse`, then the
`parse_exn` function can signal an invalid or valid state by raising or
not raising, respectively.

Because this is a common pattern, Bonsai Forms provides the following
function, built on top of `Form.project` to help reduce boilerplate:

```{=html}
<!-- $MDX skip -->
```
``` ocaml
val validate : 'a t -> f:('a -> unit Or_error.t) -> 'a t
```

# Combinators

Most forms contain many input elements, and Bonsai Forms comes with a
set of combinators for combining smaller subforms into a larger form.

## Form.both

The simplest combinator is `Form.both`:

```{=html}
<!-- $MDX skip -->
```
``` ocaml
val both
  :  ('a, 'view_a) Form.t
  -> ('b, 'view_b) Form.t
  -> ('a * 'b, 'view_a * 'view_b) Form.t
```

We can use `Form.both` to combine many individual forms into a single
one, and then manually construct a vdom view:

```{=html}
<!-- $MDX file=../../examples/bonsai_guide_code/form_examples.ml,part=form_both -->
```
``` ocaml
let big_tuple_form (local_ graph)
  : ((int * float) * (string * bool), Vdom.Node.t) Form.t Bonsai.t
  =
  let int_and_float =
    let%arr int = Form.Elements.Textbox.int ~allow_updates_when_focused:`Always () graph
    and float =
      Form.Elements.Textbox.float ~allow_updates_when_focused:`Always () graph
    in
    Form.both int float
  in
  let string_and_bool =
    let%arr string =
      Form.Elements.Textbox.string ~allow_updates_when_focused:`Always () graph
    and bool = Form.Elements.Checkbox.bool ~default:false () graph in
    Form.both string bool
  in
  let%arr int_and_float and string_and_bool in
  Form.both int_and_float string_and_bool
  |> Form.map_view ~f:(fun ((int_view, float_view), (string_view, bool_view)) ->
    View.vbox
      [ View.hbox
          ~gap:(`Px 5)
          [ Vdom.Node.text "an int: "; int_view; Vdom.Node.text "a float: "; float_view ]
      ; View.hbox
          ~gap:(`Px 5)
          [ Vdom.Node.text "a string: "
          ; string_view
          ; Vdom.Node.text "a bool: "
          ; bool_view
          ]
      ])
;;
```

```{=html}
<iframe data-external="1" src="https://bonsai:8535#form_both">
```
```{=html}
</iframe>
```
## Form.Typed module

### Records

For this example, we'll build a form for the following type:

```{=html}
<!-- $MDX file=../../examples/bonsai_guide_code/form_examples.ml,part=record_form_type -->
```
``` ocaml
type t =
  { some_string : string
  ; an_int : int
  ; on_or_off : bool
  }
[@@deriving typed_fields, sexp_of]
```

Building a form that produces values of this type requires the use of
[`ppx_typed_fields`](https://github.com/janestreet/ppx_typed_fields),
which you'll need to add to your jbuild. Deriving `typed_fields` will
make a module named `Typed_field` containing a type with a constructor
representing each field in the record it was derived on.

```{=html}
<!-- $MDX file=../../examples/bonsai_guide_code/form_examples.ml,part=record_form -->
```
``` ocaml
module Record = struct
  type t =
    { some_string : string
    ; an_int : int
    ; on_or_off : bool
    }
  [@@deriving typed_fields, sexp_of]

  let form : local_ Bonsai.graph -> (t, Vdom.Node.t) Form.t Bonsai.t =
    Form.Typed.Record.make
      (module struct
        (* Reimport the module that typed_fields just derived *)
        module Typed_field = Typed_field

        (* The type of the view for each field's subform *)
        type field_view = Vdom.Node.t

        (* The type of the view for the entire form *)
        type resulting_view = Vdom.Node.t

        (* This type definition is boilerplate and will become unneccessary once OCaml
           gets polymorphic parameters. *)
        type form_of_field_fn =
          { f : 'a. 'a Typed_field.t @ local -> ('a, field_view) Form.t Bonsai.t }

        (* Provide a form computation for each field in the record *)
        let form_for_field
          : type a.
            a Typed_field.t -> local_ Bonsai.graph -> (a, field_view) Form.t Bonsai.t
          =
          fun typed_field (local_ graph) ->
          match typed_field with
          | Some_string ->
            Form.Elements.Textbox.string ~allow_updates_when_focused:`Always () graph
          | An_int ->
            Form.Elements.Number.int
              ~default:0
              ~step:1
              ~allow_updates_when_focused:`Always
              ()
              graph
          | On_or_off -> Form.Elements.Checkbox.bool ~default:false () graph
        ;;

        (* Combine the views of each subform into the view for the resulting form *)
        let finalize_view { f } (local_ _graph) : resulting_view Bonsai.t =
          let%arr some_string_form = f Some_string
          and an_int_form = f An_int
          and on_or_off_form = f On_or_off in
          View.vbox
            [ View.hbox [ View.text "Some string:"; Form.view some_string_form ]
            ; View.hbox [ View.text "An int:"; Form.view an_int_form ]
            ; View.hbox [ View.text "On or off:"; Form.view on_or_off_form ]
            ]
        ;;
      end)
  ;;
end
```

### Variants

We can also do the same for variants with `[@@deriving typed_variants]`:

```{=html}
<!-- $MDX file=../../examples/bonsai_guide_code/form_examples.ml,part=variant_form -->
```
``` ocaml
module Variant = struct
  type t =
    | A
    | B of int
    | C of string
  [@@deriving typed_variants, sexp_of]

  let form : local_ Bonsai.graph -> (t, Vdom.Node.t) Form.t Bonsai.t =
    Form.Typed.Variant.make
      (module struct
        (* Reimport the module that typed_variants just derived *)
        module Typed_variant = Typed_variant

        (* The type of the view for the clause picker subform *)
        type picker_view = Vdom.Node.t

        (* The type of the view for each clause's subform *)
        type variant_view = Vdom.Node.t

        (* The type of the view for the resulting form *)
        type resulting_view = Vdom.Node.t

        (* Provide a form computation for selecting which clause *)
        let form_for_picker =
          Form.Elements.Dropdown.list
            (module Typed_variant.Packed)
            (Bonsai.return Typed_variant.Packed.all)
            ~equal:Typed_variant.Packed.equal
        ;;

        (* Provide a form computation for constructing the clause's arguments *)
        let form_for_variant
          : type a.
            a Typed_variant.t -> local_ Bonsai.graph -> (a, variant_view) Form.t Bonsai.t
          =
          fun typed_variant (local_ graph) ->
          match typed_variant with
          | A ->
            Form.return () |> Form.map_view ~f:(fun () -> Vdom.Node.none) |> Bonsai.return
          | B -> Form.Elements.Textbox.int ~allow_updates_when_focused:`Always () graph
          | C -> Form.Elements.Textbox.string ~allow_updates_when_focused:`Always () graph
        ;;

        (* Combine the views of the picker and corresponding subform into the view for the
           resulting form *)
        let finalize_view picker clause_form (local_ _graph) =
          let clause_view =
            match%arr clause_form with
            | Ok (_which_clause, clause_form) -> Form.view clause_form
            | Error err -> Vdom.Node.sexp_for_debugging [%message (err : Error.t)]
          in
          let%arr clause_view and picker in
          View.vbox [ View.hbox [ View.text "Pick a constructor:"; picker ]; clause_view ]
        ;;
      end)
  ;;
end
```

## Putting it all together

Combining the record and variant forms from before using `Form.both`, we
can create one final form and print the results:

```{=html}
<!-- $MDX file=../../examples/bonsai_guide_code/form_examples.ml,part=record_and_variant_form -->
```
``` ocaml
let view_record_and_variant_form : local_ Bonsai.graph -> Vdom.Node.t Bonsai.t =
  fun graph ->
  let form =
    let%arr record_form = Record.form graph
    and variant_form = Variant.form graph in
    Form.both record_form variant_form
  in
  let%arr form in
  let record_view, variant_view = Form.view form in
  View.vbox
    [ Vdom.Node.h4 [ View.text "Record" ]
    ; record_view
    ; Vdom.Node.hr ()
    ; Vdom.Node.h4 [ View.text "Variant" ]
    ; variant_view
    ; Vdom.Node.hr ()
    ; Vdom.Node.sexp_for_debugging
        ([%sexp_of: (Record.t * Variant.t) Or_error.t] (Form.value form))
    ]
;;
```

```{=html}
<iframe data-external="1" src="https://bonsai:8535#record_and_variant_form">
```
```{=html}
</iframe>
```
# Migrating from legacy forms

`Bonsai_web_ui_form.With_automatic_view` used to be the default kind of
forms. However, they were extremely hard to customize. Thankfully,
conversion between the two is easy and can be done with the following
functions:

```{=html}
<!-- $MDX skip -->
```
``` ocaml
val to_manual_form
  :  'a Form.With_automatic_view.t
  -> ('a, Vdom.Node.t) Form.With_manual_view.t

val to_manual_form'
  :  'a Form.With_automatic_view.t Bonsai.t
  -> local_ Bonsai.graph
  -> ('a, Vdom.Node.t) Form.With_manual_view.t

val of_manual_form
  :  ('a, Vdom.Node.t) Form.With_manual_view.t Bonsai.t
  -> local_ Bonsai.graph_
  -> 'a Form.With_automatic_view.t Bonsai.t
```

These conversion methods make it easy to start replacing legacy forms
from the bottom-up, the top-down, or a combination of both!
