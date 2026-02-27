---
title: Quick Start
description: Install Parseff and write your first parser
---

## Installation

### With opam

```bash
opam install parseff
```

### With dune package management

Add Parseff to your `dune-project`:

```lisp
(package
 (name my_project)
 (depends
  (ocaml (>= 5.3))
  (parseff (>= 0.1))))
```

Then lock and build:

```bash
dune pkg lock
dune build
```

### From source

```bash
git clone https://github.com/davesnx/parseff.git
cd parseff
opam install . --deps-only
dune build
dune install
```

### Add to your dune file

```lisp
(executable
 (name my_parser)
 (libraries parseff))
```

## We recommend not opening Parseff

We recommend always writing `Parseff.consume`, `Parseff.char`, `Parseff.many`, etc. — fully qualified.

When you open the module, names like `char`, `error`, `position`, and `or_` become unqualified. In a file that also defines its own helper functions and types, it quickly becomes unclear which names come from Parseff and which are yours. The `Parseff.` prefix acts as documentation: every combinator call is visually distinct from your application logic.

Parseff also defines its own `result` type (with a `pos` field on errors), which shadows `Stdlib.result`. Opening Parseff would silently break any code that uses the standard `Result` module.

All examples in this documentation use the fully qualified form.

## Example: an IPv4 address parser

A Parseff parser is a function `unit -> 'a` that calls combinators to consume input. Here's a complete parser that validates IPv4 addresses:

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
  | Ok (a, b, c, d) ->
      Printf.printf "Parsed: %d.%d.%d.%d\n" a b c d
  | Error { pos; error = `Out_of_range n } ->
      Printf.printf "Error at %d: %d out of range (0-255)\n" pos n
  | Error { pos; error = `Expected msg } ->
      Printf.printf "Error at %d: %s\n" pos msg
  | Error { pos; error = `Unexpected_end_of_input } ->
      Printf.printf "Error at %d: unexpected end of input\n" pos
```

A few things to notice:

- **Sequencing is just `let` bindings.** Each call advances the cursor through the input.
- **Validation is normal OCaml.** Parse the digits, check the range, raise a typed error if invalid.
- **Errors are typed.** `Out_of_range` is a polymorphic variant you define. Parseff adds `Expected` and `Unexpected_end_of_input` for parse failures.

## Learn more

- **[Your First Parser](/parseff/guides/first-parser)** -- a hands-on tutorial that walks through every concept from matching characters to building a complete parser
- **[API Reference](/parseff/api/overview)** -- all combinators and their signatures
- **[Parsing an IP Address](/parseff/examples/ip-address)** -- a detailed breakdown of the parser above with error handling variations
- **[A JSON Parser](/parseff/examples/json-parser)** -- recursive descent, mutual recursion, and depth limiting
- **[Error Handling](/parseff/guides/errors)** -- custom error types, `expect`, and labeled alternation
- **[Comparison with Angstrom](/parseff/guides/comparison)** -- benchmarks, API comparison, and migration guide
