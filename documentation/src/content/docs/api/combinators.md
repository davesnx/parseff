---
title: Combinators
description: Composition operators for building complex parsers
---

Combinators compose simple parsers into more complex ones. They handle alternation, error labeling, lookahead, and recursion.

## Alternation

### `or_`

```ocaml
val or_ : (unit -> 'a) -> (unit -> 'a) -> unit -> 'a
```

Tries the left parser. If it fails, backtracks (resets the cursor) and tries the right parser.

```ocaml
let bool_parser () =
  Parseff.or_
    (fun () -> let _ = Parseff.consume "true" in true)
    (fun () -> let _ = Parseff.consume "false" in false)
    ()
(* "true"  -> Ok true  *)
(* "false" -> Ok false *)
(* "maybe" -> Error { pos = 0; error = `Expected "false" } *)
```

On `"maybe"`, the left branch fails at position 0 (expected `"true"`, got `"m"`), backtracks, and the right branch also fails (expected `"false"`, got `"m"`). The error from the last branch attempted is reported.

`or_` is ideal for two alternatives. When you have more, use [`one_of`](#one_of) — it takes a list and avoids nested `or_` calls:

```ocaml
(* With nested or_ — works but gets awkward with many alternatives *)
let keyword () =
  Parseff.or_
    (fun () -> Parseff.consume "let")
    (fun () ->
      Parseff.or_
        (fun () -> Parseff.consume "const")
        (fun () -> Parseff.consume "var")
        ())
    ()

(* With one_of — cleaner, easier to extend *)
let keyword () =
  Parseff.one_of
    [ (fun () -> Parseff.consume "let")
    ; (fun () -> Parseff.consume "const")
    ; (fun () -> Parseff.consume "var")
    ] ()
```

---

### `one_of`

```ocaml
val one_of : (unit -> 'a) list -> unit -> 'a
```

Tries each parser in order until one succeeds. Equivalent to chaining `or_`, but cleaner for more than two alternatives.

```ocaml
let json_value () =
  Parseff.one_of
    [
      null_parser;
      bool_parser;
      number_parser;
      string_parser;
      array_parser;
      object_parser;
    ]
    ()
```

If all parsers fail, `one_of` fails with the error from the last parser attempted.

---

### `one_of_labeled`

```ocaml
val one_of_labeled : (string * (unit -> 'a)) list -> unit -> 'a
```

Like `one_of`, but each parser has a label. On failure, the error message reports all labels:

```ocaml
let literal () =
  Parseff.one_of_labeled
    [
      ("number", fun () -> Number (Parseff.digit ()));
      ("string", string_parser);
      ("boolean", bool_parser);
    ]
    ()
(* On failure: "expected one of: number, string, boolean" *)
```

This gives users a clear picture of what was expected at a given position, without exposing internal parser details.

---

### `optional`

```ocaml
val optional : (unit -> 'a) -> unit -> 'a option
```

Tries the parser. Returns `Some result` on success, `None` on failure (without consuming input).

```ocaml
let signed_number () =
  let sign = Parseff.optional (fun () -> Parseff.char '-') () in
  let n = number () in
  match sign with
  | Some _ -> -n
  | None -> n
(* "42"  -> 42  *)
(* "-42" -> -42 *)
```

---

## Lookahead

### `look_ahead`

```ocaml
val look_ahead : (unit -> 'a) -> 'a
```

Runs a parser without consuming input. The cursor stays where it was before the call. Fails if the parser fails.

```ocaml
let peek_open_paren () =
  let _ = Parseff.look_ahead (fun () -> Parseff.char '(') in
  (* cursor hasn't moved — '(' is still the next character *)
  parse_parenthesized_expr ()
```

Useful for context-sensitive decisions: peek at what's coming, then choose which parser to run.

---

## Error labeling

### `expect`

```ocaml
val expect : string -> (unit -> 'a) -> 'a
```

Runs a parser. If it fails, replaces the error message with the given description. Reads naturally: "expect a digit", "expect a closing bracket".

```ocaml
let dot () =
  Parseff.expect "a dot separator" (fun () -> Parseff.char '.')

let digit_val () =
  Parseff.expect "a digit (0-9)" Parseff.digit
```

Without `expect`, a failed `char '.'` reports `expected '.'`. With `expect`, it reports `expected a dot separator`. The difference matters when users see these messages.

Use `expect` at the boundaries of your parser — the points where a user-facing description makes more sense than a raw token name:

```ocaml
let ip_address () =
  let a = number () in
  let _ = Parseff.expect "a dot separator" (fun () -> Parseff.char '.') in
  let b = number () in
  let _ = Parseff.expect "a dot separator" (fun () -> Parseff.char '.') in
  let c = number () in
  let _ = Parseff.expect "a dot separator" (fun () -> Parseff.char '.') in
  let d = number () in
  Parseff.end_of_input ();
  (a, b, c, d)
```

---

## Recursion

### `rec_`

```ocaml
val rec_ : (unit -> 'a) -> 'a
```

Marks a recursive entry point for depth tracking. Wrap the body of recursive parsers with `rec_` so that `parse ~max_depth` can fail cleanly instead of overflowing the stack.

```ocaml
let rec json () =
  Parseff.rec_ (fun () ->
    Parseff.skip_whitespace ();
    Parseff.one_of
      [
        array_parser;
        object_parser;
        null_parser;
        bool_parser;
        number_parser;
        string_parser;
      ]
      ()
  )

and array_parser () =
  let _ = Parseff.consume "[" in
  (* ... calls json () recursively ... *)
  let _ = Parseff.consume "]" in
  Array elements
```

The `~max_depth` parameter on `parse` controls the limit (default: 128):

```ocaml
(* Reject inputs nested deeper than 64 levels *)
Parseff.parse ~max_depth:64 input json
```

When the limit is exceeded, parsing fails with `"maximum nesting depth N exceeded"` rather than a stack overflow.

:::tip
Only wrap the top-level recursive entry point with `rec_`. You don't need it on every mutually recursive function — just the one that represents "entering a new nesting level".
:::


