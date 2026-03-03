---
title: Error handling
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

## Custom errors with `error`

While `fail` produces string-based errors, `error` lets you define structured error types using polymorphic variants. This gives you compile-time exhaustiveness checking and the ability to carry data with each error.

```ocaml
let number () =
  let s = Parseff.take_while1 (fun c -> c >= '0' && c <= '9') ~label:"digit" in
  let n = int_of_string s in
  if n > 255 then Parseff.error (`Too_large n)
  else if n < 0 then Parseff.error (`Negative n)
  else n
```

`error` has the signature `val error : 'e -> 'a`. It accepts any value and never returns (like `fail`, it aborts the current parser). The value you pass becomes the `error` field in the result.

`Parseff.parse` returns a result with an open row type: custom variants appear alongside the built-in `` `Expected `` variant from `fail` and combinator failures. The compiler warns you if you forget to handle one:

```ocaml
match Parseff.parse "300" number with
| Error { error = `Too_large n; pos } ->
    Printf.printf "Number %d too large at position %d\n" n pos
| Error { error = `Negative n; pos } ->
    Printf.printf "Negative number %d at position %d\n" n pos
| Error { error = `Expected msg; _ } ->
    Printf.printf "Parse error: %s\n" msg
| Ok n -> Printf.printf "Got %d\n" n
```

When errors compose across parsers, the result type is the union of all variants. If parser A can produce `` `Too_large `` and parser B can produce `` `Invalid_format ``, calling both means the result type includes both. The compiler enforces exhaustive matching on all of them.

---

## Putting it together

Use `expect` for better messages on structural elements (parentheses, delimiters) and `error` with polymorphic variants for domain validation (out of range, division by zero). The compiler enforces exhaustive matching on the result, so every error case is handled.

For a complete example combining these techniques, see the [Expression Parser](/parseff/examples/expression-parser) walkthrough.
