---
title: Error Handling
description: Building better error messages in Parseff
---

Parseff gives you several ways to control error messages, from simple string errors to typed domain errors with polymorphic variants. This guide shows you how to use each one and when to reach for which.

## Basic errors with `fail`

The simplest way to report an error:

```ocaml
let byte () =
  let n = int_of_string (Parseff.take_while1 (fun c -> c >= '0' && c <= '9') ~label:"digit") in
  if n >= 0 && n <= 255 then n
  else Parseff.fail "number must be between 0 and 255"
```

```ocaml
match Parseff.parse "300" byte with
| Error { pos; error = `Expected expected } ->
    (* pos = 0, expected = "number must be between 0 and 255" *)
| Ok n -> Printf.printf "Got: %d\n" n
```

`fail` produces an `Expected` error with your string message. It's good for quick validation where a human-readable message is enough.

---

## Better messages with `expect`

When a parser fails, the default error message comes from the combinator itself: `expected '.'`, `expected digit`. These are accurate but not always helpful to users who don't know the grammar.

`expect` wraps a parser and replaces its error message with a description you provide:

```ocaml
let colon () =
  Parseff.expect "a colon separator" (fun () -> Parseff.char ':')

let key_value () =
  let key = Parseff.take_while1 (fun c -> c <> ':' && c <> '\n') ~label:"key" in
  let _ = colon () in
  let value = Parseff.take_while1 (fun c -> c <> '\n') ~label:"value" in
  (key, value)
```

Now instead of `expected ':'` at position 4, the user sees `expected a colon separator`, which makes more sense if they don't know the parser internals. See the [IP Address example](/parseff/examples/ip-address) for a complete walkthrough using `expect`.

---

## Labeled alternation with `one_of_labeled`

When a parser tries multiple alternatives and all fail, the default error message only reports the last branch. `one_of_labeled` reports all of them:

```ocaml
let literal () =
  Parseff.one_of_labeled
    [
      ("number", fun () -> Number (Parseff.digit ()));
      ("string", string_parser);
      ("boolean", bool_parser);
    ]
    ()
```

On failure: `expected one of: number, string, boolean`. The user sees every option that was available at that position.

---

## Adding context with `or_`

You can wrap parsers with `or_` to replace an error with a contextual message:

```ocaml
let context name parser () =
  Parseff.or_
    (fun () -> parser ())
    (fun () -> Parseff.fail (Printf.sprintf "in %s: parse failed" name))
    ()
```

On failure, `or_` backtracks and the second branch produces a message like `in port number: parse failed`.

:::caution[This loses the original error]
The `context` pattern replaces the original error entirely. If a number was out of range, the user sees `in port number: parse failed` instead of `out of range`. You get location context but lose the specific reason.

For most cases, `expect` (which replaces the message on the same failure) or typed errors with `error` (which preserve structure) are better choices. Use this pattern only when you need a coarse "which section failed" signal and don't care about the specific cause.
:::

---

## Custom error types

Use `error` with polymorphic variants for rich, structured errors:

```ocaml
let validated_byte () =
  let s = Parseff.take_while1 (fun c -> c >= '0' && c <= '9') ~label:"digit" in
  let n = int_of_string s in
  if n > 255 then Parseff.error (`Out_of_range (n, 255))
  else n
```

The error flows through the result type, so callers can pattern match on it:

```ocaml
match Parseff.parse "300" validated_byte with
| Ok n -> Printf.printf "Got: %d\n" n
| Error { pos; error = `Out_of_range (value, max) } ->
    Printf.printf "Error at %d: %d exceeds maximum %d\n" pos value max
| Error { pos; error = `Expected expected } ->
    Printf.printf "Parse error at %d: %s\n" pos expected
```

---

## Polymorphic variants

For quick typed errors without defining a separate type:

```ocaml
let number () =
  let s = Parseff.take_while1 (fun c -> c >= '0' && c <= '9') ~label:"digit" in
  let n = int_of_string s in
  if n > 255 then Parseff.error (`Too_large n)
  else if n < 0 then Parseff.error (`Negative n)
  else n

match Parseff.parse "300" number with
| Error { error = `Too_large n; pos } ->
    Printf.printf "Number %d too large at position %d\n" n pos
| Error { error = `Negative n; pos } ->
    Printf.printf "Negative number %d at position %d\n" n pos
| Error { error = `Expected msg; _ } ->
    Printf.printf "Parse error: %s\n" msg
| Ok n -> Printf.printf "Got %d\n" n
```

Polymorphic variants are a good middle ground. You get typed errors without the boilerplate of defining a separate error type. They work well for errors that callers need to handle differently (out of range vs. missing input vs. invalid format).

---

## Complete example: expression parser with errors

A parser for parenthesized arithmetic like `(10 / 0)` and `(1 + 2)`, with typed errors for invalid operators and division by zero:

```ocaml
let number () =
  let digits = Parseff.take_while1 (fun c -> c >= '0' && c <= '9') ~label:"digit" in
  int_of_string digits

let operator () =
  Parseff.skip_whitespace ();
  let op = Parseff.any_char () in
  Parseff.skip_whitespace ();
  match op with
  | '+' | '-' | '*' | '/' -> op
  | c -> Parseff.error (`Invalid_operator (String.make 1 c))

let rec expr () =
  Parseff.skip_whitespace ();
  let c = Parseff.look_ahead Parseff.any_char in
  match c with
  | '(' -> paren_expr ()
  | '0'..'9' -> number ()
  | _ -> Parseff.fail "expected number or '('"

and paren_expr () =
  let _ = Parseff.expect "opening parenthesis" (fun () -> Parseff.char '(') in
  Parseff.skip_whitespace ();
  let left = expr () in
  let op = operator () in
  let right = expr () in
  Parseff.skip_whitespace ();
  let _ = Parseff.expect "closing parenthesis" (fun () -> Parseff.char ')') in
  match op with
  | '+' -> left + right
  | '-' -> left - right
  | '*' -> left * right
  | '/' ->
      if right = 0 then Parseff.error `Divide_by_zero
      else left / right
  | _ -> assert false

let parse_expr input =
  match Parseff.parse input expr with
  | Ok result -> Printf.printf "Result: %d\n" result
  | Error { pos; error = `Expected expected } ->
      Printf.printf "Parse error at %d: %s\n" pos expected
  | Error { pos; error = `Invalid_operator op } ->
      Printf.printf "Invalid operator '%s' at %d\n" op pos
  | Error { pos; error = `Divide_by_zero } ->
      Printf.printf "Division by zero at %d\n" pos

let () =
  parse_expr "(1 + 2)";     (* Result: 3 *)
  parse_expr "(10 / 0)";    (* Division by zero at 5 *)
  parse_expr "(1 + )";      (* Parse error at 5: expected number or '(' *)
```

The key technique: use `expect` for better messages on structural elements (parentheses, operators) and `error` with polymorphic variants for domain validation (division by zero, invalid operators). The exhaustive `match` on the result ensures every error case is handled.
