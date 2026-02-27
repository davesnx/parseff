---
title: Quick Start
description: Parseff is a direct-style parser combinator library for OCaml 5. Write parsers as plain functions with typed errors, streaming input, and zero-copy APIs.
---

Parseff is a direct-style parser combinator library for OCaml 5. Write parsers as plain functions with typed errors, streaming input, and zero-copy APIs.

- **Direct-style**: parsers are plain `unit -> 'a` functions composed with `let` bindings
- **Typed errors**: domain-specific errors via polymorphic variants
- **Streaming input**: same parser code works with strings, channels, and custom sources
- **Performance**: 1.6x to 4.3x faster than Angstrom on JSON benchmarks, with 3x less memory
- **Zero-copy APIs**: span-based operations for low-allocation parsing on hot paths

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

## Example: an IPv4 address parser

Here's a complete parser that validates [IPv4 addresses](/parseff/examples/ip-address):

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

- **[Your First Parser](/parseff/guides/first-parser)**: a hands-on tutorial that walks through every concept from matching characters to building a complete parser
- **[API Reference](/parseff/api/overview)**: all combinators and their signatures
- **[Parsing an IP Address](/parseff/examples/ip-address)**: a detailed breakdown of the parser above with error handling variations
- **[A JSON Parser](/parseff/examples/json-parser)**: recursive descent, mutual recursion, and depth limiting
- **[Error Handling](/parseff/guides/errors)**: custom error types, `expect`, and labeled alternation
- **[Comparison with Angstrom](/parseff/guides/comparison)**: benchmarks, API comparison, and migration guide
