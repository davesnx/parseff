---
title: Combinators
description: Composition operators for building complex parsers
---

<!-- This file is generated from doc/combinators.mld. Do not edit directly. -->

# Combinators

Combinators compose parsers for alternation, lookahead, error labeling, backtracking control, and bounded recursion.

## Alternation

### `or_`

[`Parseff.or_`](./Parseff.md#val-or_) tries the first parser; if it fails (and did not commit), rewinds and tries the second.

```ocaml
val or_ : (unit -> 'a) -> (unit -> 'a) -> 'a
```

```ocaml
let bool_parser () =
  Parseff.or_
    (fun () ->
      Parseff.literal "true";
      true)
    (fun () ->
      Parseff.literal "false";
      false)
```

When both branches fail at the same position (such as `"maybe"` at offset 0), Parseff merges their expectations into a sorted list: `expected "false" or "true", found 'maybe'`. When they fail at different positions, the furthest failure wins: on `"tr"`, the first branch reaches offset 2 and reports `expected "ue", found end of input`.

### `one_of` and `one_of_labeled`

[`Parseff.one_of`](./Parseff.md#val-one_of) tries a list of parsers in order, merging expectations with the same rules as `or_`. [`Parseff.one_of_labeled`](./Parseff.md#val-one_of_labeled) attaches a label to each branch (wrapping each branch in [`Parseff.expect`](./Parseff.md#val-expect)) so failures at the start position report the labels instead of inner primitive expectations.

```ocaml
val one_of : (unit -> 'a) list -> 'a
val one_of_labeled : (string * (unit -> 'a)) list -> 'a
```

```ocaml
let keyword_parser () =
  Parseff.one_of
    [
      (fun () -> Parseff.literal "let");
      (fun () -> Parseff.literal "const");
      (fun () -> Parseff.literal "var");
    ]

let scalar () =
  Parseff.one_of_labeled
    [
      ("number", fun () -> `Num (Parseff.integer ()));
      ("boolean", fun () -> `Bool (bool_parser ()));
    ]
```

If a branch of `one_of_labeled` fails past its starting position, its inner error is preserved rather than replaced by the branch label.

### `optional`

[`Parseff.optional`](./Parseff.md#val-optional) returns `Some v` if the parser succeeds, or `None` without consuming input if it fails.

```ocaml
val optional : (unit -> 'a) -> 'a option
```

```ocaml
let optional_sign () =
  Parseff.optional (fun () ->
      Parseff.or_ (fun () -> Parseff.char '-') (fun () -> Parseff.char '+'))
```

## Lookahead

### `look_ahead`

[`Parseff.look_ahead`](./Parseff.md#val-look_ahead) runs a parser and rewinds the cursor on both success and failure.

```ocaml
val look_ahead : (unit -> 'a) -> 'a
```

### `peek_with` and `advance_to`

[`Parseff.peek_with`](./Parseff.md#val-peek_with) behaves like [`Parseff.look_ahead`](./Parseff.md#val-look_ahead) and also returns the byte offset where the parser stopped. [`Parseff.advance_to`](./Parseff.md#val-advance_to) moves to that offset without re-parsing the bytes.

```ocaml
val peek_with : (unit -> 'a) -> 'a * int
val advance_to : int -> unit
```

```ocaml
let keyword_or_name () =
  let word, stop = Parseff.peek_with name in
  if is_keyword word then Parseff.fail "expected a name"
  else (
    Parseff.advance_to stop;
    word)
```

### `not_followed_by`

[`Parseff.not_followed_by`](./Parseff.md#val-not_followed_by) succeeds when the inner parser fails and fails at its starting position with `expected <label>` when it succeeds, consuming no input. Failures inside the inner parser are discarded.

```ocaml
val not_followed_by : ?label:string -> (unit -> 'a) -> unit
```

```ocaml
let less_than () =
  Parseff.literal "<";
  Parseff.not_followed_by ~label:"'<' alone" (fun () -> Parseff.char '=')
```

## Error labeling

### `expect`

[`Parseff.expect`](./Parseff.md#val-expect) replaces `Expected` failures at the parser's starting position with `expected <label>`. Failures strictly past the starting position (and [`Parseff.fail`](./Parseff.md#val-fail)/[`Parseff.error`](./Parseff.md#val-error)) are left unchanged.

```ocaml
val expect : string -> (unit -> 'a) -> 'a
```

```ocaml
let dot () = Parseff.expect "a dot separator" (fun () -> Parseff.char '.')
```

## Naming constructs and controlling backtracking

### `context`

[`Parseff.context`](./Parseff.md#val-context) attaches a construct name to failures strictly past its starting position, reported outermost first in `error.contexts` and printed as `* while parsing <name>` by [`Parseff.render_error`](./Parseff.md#val-render_error). Pass `~delimited:true` for constructs with an opening token (such as arrays or blocks) so multi-line error reports draw a bracket from the opener to the failure.

```ocaml
val context : ?delimited:bool -> ?start:int -> string -> (unit -> 'a) -> 'a
```

```ocaml
let array_parser value () =
  Parseff.context ~delimited:true "array" (fun () ->
      let _ = Parseff.char '[' in
      let xs =
        Parseff.sep_by value (fun () ->
            Parseff.skip_whitespace ();
            Parseff.char ',')
      in
      Parseff.skip_whitespace ();
      let _ = Parseff.char ']' in
      xs)
```

Name grammar constructs (statements, objects, declarations), not single tokens. If two failures tie at the furthest position, the first construct stack is kept.

### `commit`

[`Parseff.commit`](./Parseff.md#val-commit) seals the nearest enclosing backtracking frame (`or_`, `one_of`, `many`, `sep_by`, `optional`, `fold_left`, or `fold_right`), prevents it from backtracking, forgets failures recorded at or beyond the current position, and releases buffered streaming input held by that frame.

```ocaml
val commit : unit -> unit
```

### `cut`

[`Parseff.cut`](./Parseff.md#val-cut) seals every backtracking frame opened since the innermost enclosing [`Parseff.context`](./Parseff.md#val-context) (or since the start of the parse if none). If that `context` later fails, frames outside it are sealed as well so enclosing `or_`, `many`, or `sep_by` cannot swallow the error.

```ocaml
val cut : unit -> unit
```

```ocaml
let statement () =
  Parseff.context "statement" (fun () ->
      Parseff.or_
        (fun () ->
          Parseff.literal "let";
          Parseff.cut ();
          Parseff.skip_whitespace ();
          let _ =
            Parseff.take_while ~at_least:1 ~label:"name" (fun c ->
                c >= 'a' && c <= 'z')
          in
          Parseff.skip_whitespace ();
          let _ = Parseff.char '=' in
          Parseff.skip_whitespace ();
          let _ =
            Parseff.take_while ~at_least:1 ~label:"expression" (fun c ->
                c >= '0' && c <= '9')
          in
          Parseff.char ';')
        (fun () ->
          let _ =
            Parseff.take_while ~at_least:1 (fun c -> c >= '0' && c <= '9')
          in
          Parseff.char ';'))
```

### `commit` or `cut`

Use `commit` when an alternative in `or_`/`one_of` is chosen. Use `cut` when an entire construct is decided and any later failure inside it should become a parse error rather than ending an enclosing loop.

|  | `commit ()` | `cut ()` |
| --- | --- | --- |
| Frames sealed | Nearest backtracking frame (even if outside the innermost `context`). | Every backtracking frame since the innermost `context` (or start of parse). |
| When enclosing `context` fails | Outer frames backtrack as usual, so an enclosing `many` can stop. | Outer frames are sealed too; the failure becomes the parse error. |
| Prior failures at/past cursor | Forgotten, so errors report only what the committed branch expected. | Kept. |
| Streaming memory | Releases input pinned by the nearest frame only. | Releases input pinned by all sealed frames. |
| Inside `look_ahead`, `peek_with`, or `capture` | Does not cross the boundary. With `capture ~release:true`, moves the rewind point and stops. | Does not cross the boundary. With `capture ~release:true`, moves the rewind point and continues outward. |

```ocaml
let decided_statement decide () =
  Parseff.context "statement" (fun () ->
      Parseff.or_
        (fun () ->
          Parseff.literal "let ";
          decide ();
          let _ =
            Parseff.take_while ~at_least:1 ~label:"name" (fun c ->
                c >= 'a' && c <= 'z')
          in
          Parseff.literal " = ";
          let _ =
            Parseff.take_while ~at_least:1 ~label:"expression" (fun c ->
                c >= '0' && c <= '9')
          in
          Parseff.char ';')
        (fun () ->
          let _ =
            Parseff.take_while ~at_least:1 (fun c -> c >= '0' && c <= '9')
          in
          Parseff.char ';'))

let read_statements decide input =
  Parseff.parse ~consume:`Partial input (fun () ->
      let xs =
        Parseff.many (fun () ->
            Parseff.skip_whitespace ();
            decided_statement decide ())
      in
      (List.length xs, Parseff.position ()))
  |> Result.map_error Parseff.error_message
```

```ocaml
# read_statements Parseff.commit "1; let x = ;";;
- : (int * int, string) result = Ok (1, 2)
# read_statements Parseff.cut "1; let x = ;";;
- : (int * int, string) result = Error "expected expression, found ';'"
```

With `commit`, only `or_` is sealed, so `many` rewinds to offset 2 and returns the first statement. With `cut`, `many` is sealed too, so the broken `let` fails the parse. (With the default ``~consume:`All``, both fail because furthest failure still reports the missing expression.)

### Tracing

Pass `~trace` to [`Parseff.parse`](./Parseff.md#val-parse), [`Parseff.Source.parse`](./Parseff-Source.md#val-parse), or [`Parseff.Push.parse`](./Parseff-Push.md#val-parse) to observe [`Parseff.trace_event`](./Parseff.md#type-trace_event)s from `context` and recovery combinators. [`Parseff.print_trace`](./Parseff.md#val-print_trace) prints an indented tree:

```ocaml
let number () =
  Parseff.context "number" (fun () ->
      Parseff.take_while ~at_least:1 (fun c -> c >= '0' && c <= '9'))

let list_parser () =
  Parseff.context ~delimited:true "list" (fun () ->
      let _ = Parseff.char '[' in
      let xs = Parseff.sep_by number (fun () -> Parseff.char ',') in
      let _ = Parseff.char ']' in
      xs)

let _ =
  Parseff.parse ~trace:(Parseff.print_trace Format.std_formatter) "[1,x]"
    list_parser
```

```
list @0
  number @1
  number ok @1..2
  number @3
  number failed @3 (from @3)
list failed @2 (from @0)
```

A `failed` line prints the parser position where the construct failed, not the furthest error offset (here `list` failed at `@2`, while the error is at offset 3).

## Recursion

### `rec_`

[`Parseff.rec_`](./Parseff.md#val-rec_) tracks nesting depth against `~max_depth` (default `128`), failing with `Depth_limit_exceeded { limit }` instead of overflowing the stack. Wrap the entry point of a recursive grammar rule (you do not need `rec_` on every mutually recursive helper).

```ocaml
val rec_ : (unit -> 'a) -> 'a
```

```ocaml
let rec json () =
  Parseff.rec_ (fun () ->
      Parseff.skip_whitespace ();
      Parseff.one_of
        [ array_parser; object_parser; null_parser; bool_parser; number_parser; string_parser ])
```
