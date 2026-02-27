---
title: Quick Start
description: Get started with Parseff
---

## Installation

```bash
opam install parseff
```

Or from source:

```bash
git clone https://github.com/davesnx/parseff.git
cd parseff
opam install . --deps-only
dune build
dune install
```

## A Minimal Parser

A parser is a function `unit -> 'a` that calls Parseff combinators. Run it with `Parseff.parse`:

```ocaml
let greeting () =
  let hello = Parseff.consume "hello" in
  hello

let () =
  match Parseff.parse "hello" greeting with
  | Ok (result) -> print_endline result  (* "hello" *)
  | Error { pos; error = `Expected msg } ->
      Printf.printf "Error at %d: %s\n" pos msg
```

`consume` matches an exact string and returns it. If the input doesn't match, parsing fails with an error.

## Matching Characters

`satisfy` matches a single character by predicate. `char` matches an exact character:

```ocaml
let digit () =
  Parseff.satisfy (fun c -> c >= '0' && c <= '9') ~label:"digit"

let dot () = Parseff.char '.'
```

`take_while` scans characters as long as a predicate holds:

```ocaml
let digits () =
  Parseff.take_while1 (fun c -> c >= '0' && c <= '9') "digits"
```

## Sequencing

Parsers compose through ordinary let-bindings. Each call advances the input cursor:

```ocaml
let key_value () =
  let key = Parseff.take_while1 (fun c -> c <> '=') "key" in
  let _ = Parseff.char '=' in
  let value = Parseff.take_while1 (fun c -> c <> '\n') "value" in
  (key, value)

let () =
  match Parseff.parse "name=parseff" key_value with
  | Ok ((k, v)) -> Printf.printf "%s -> %s\n" k v
  | Error { pos; error = `Expected msg } ->
      Printf.printf "Error at %d: %s\n" pos msg
```

## Alternation

`or_` tries the left parser first. If it fails, it backtracks and tries the right:

```ocaml
let bool_value () =
  Parseff.or_
    (fun () -> let _ = Parseff.consume "true" in true)
    (fun () -> let _ = Parseff.consume "false" in false)
    ()
```

`one_of` generalizes this to a list of alternatives:

```ocaml
let keyword () =
  Parseff.one_of
    [ (fun () -> Parseff.consume "if")
    ; (fun () -> Parseff.consume "else")
    ; (fun () -> Parseff.consume "while")
    ]
    ()
```

## Repetition

`many` applies a parser zero or more times. `many1` requires at least one match:

```ocaml
let number () =
  let digits = Parseff.many1 Parseff.digit () in
  List.fold_left (fun acc d -> (acc * 10) + d) 0 digits
```

`sep_by` parses a list of elements separated by a delimiter:

```ocaml
let csv_ints () =
  Parseff.sep_by
    (fun () ->
      Parseff.skip_whitespace ();
      let n = number () in
      Parseff.skip_whitespace ();
      n)
    (fun () -> Parseff.char ',')
    ()
```

## Validation and Errors

Use `fail` for string errors or `error` for typed errors:

```ocaml
let byte () =
  let n = number () in
  if n >= 0 && n <= 255 then n
  else Parseff.error (`Out_of_range n)
```

The error type flows through to the result:

```ocaml
match Parseff.parse "300" byte with
| Ok (n) -> Printf.printf "Got %d\n" n
| Error { error = `Out_of_range n; _ } ->
    Printf.printf "%d is out of range\n" n
| Error { error = `Expected msg; _ } ->
    Printf.printf "Parse error: %s\n" msg
```

## Complete Example: IP Address Parser

Putting it all together:

```ocaml
let number () =
  let digits = Parseff.many1 Parseff.digit () in
  let n = List.fold_left (fun acc d -> (acc * 10) + d) 0 digits in
  if n >= 0 && n <= 255 then n
  else Parseff.error (`Out_of_range n)

let ip_address () =
  let a = number () in
  let _ = Parseff.char '.' in
  let b = number () in
  let _ = Parseff.char '.' in
  let c = number () in
  let _ = Parseff.char '.' in
  let d = number () in
  Parseff.end_of_input ();
  (a, b, c, d)

let () =
  match Parseff.parse "192.168.1.1" ip_address with
  | Ok ((a, b, c, d)) ->
      Printf.printf "Parsed: %d.%d.%d.%d\n" a b c d
  | Error { pos; error = `Out_of_range n } ->
      Printf.printf "Error at %d: %d out of range (0-255)\n" pos n
  | Error { pos; error = `Expected msg } ->
      Printf.printf "Error at %d: %s\n" pos msg
```

## Next Steps

- [API Reference](/parseff/api/overview) — all combinators and their signatures
- [Error Handling](/parseff/guides/errors) — custom error types and labeling
- [Performance](/parseff/performance) — benchmarks and optimization techniques
