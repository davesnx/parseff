---
title: Quick start
description: Parseff is a direct-style parser combinator library for OCaml 5 where parsers are plain functions (unit -> 'a), errors are typed via polymorphic variants, and algebraic effects handle control flow, backtracking, and streaming input. Designed for performance with zero-copy span APIs and fused operations.
---

Parseff is a direct-style parser combinator library for OCaml 5 where:
- parsers are plain `unit -> 'a` functions composed with `let` bindings
- any parser can raise domain-specific errors with polymorphic variants
- the same parser code works with strings, channels, streams and custom sources
- it's 1.6x to 4.3x faster than Angstrom on JSON benchmarks, with 3x less memory
- also comes with span-based operations for low-allocation parsing on hot paths

## Installation

### with opam

```bash
opam install parseff -y
```

### with dune package management

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
make init
make build
dune install
```

### Add to your dune file

```lisp
(executable
 (name my_parser)
 (libraries parseff))
```

## Example

Here's a small parser that validates IPv4 addresses `192.168.1.1` or `0.0.0.0`. A detailed explanation of the [IPv4 addresses parser](/parseff/examples/ip-address):

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
```

## API organization

| Section | Combinators |
|---------|------------|
| [Core](/parseff/api/primitives) | `consume`, `char`, `satisfy`, `take_while`, `take_while1`, `skip_while`, `match_regex`, `fail`, `error`, `end_of_input` |
| [Combinators](/parseff/api/combinators) | `or_`, `one_of`, `one_of_labeled`, `optional`, `look_ahead`, `expect`, `rec_` |
| [Repetition and separation](/parseff/api/repetition) | `many`, `many1`, `count`, `sep_by`, `sep_by1`, `between`, `end_by`, `end_by1`, `chainl`, `chainl1`, `chainr`, `chainr1` |
| [Convenience](/parseff/api/convenience) | `digit`, `letter`, `alphanum`, `any_char`, `is_whitespace`, `whitespace`, `whitespace1`, `skip_whitespace` |
| [Errors](/parseff/api/errors) | `fail`, `error`, `expect` |
| [Diagnostics](/parseff/api/diagnostics) | `warn`, `warn_at`, `parse_until_end`, `parse_source_until_end` |
| [Zero-copy and fused operations](/parseff/api/zero-copy) | `take_while_span`, `sep_by_take_span`, `sep_by_take`, `fused_sep_take`, `skip_while_then_char` |
| [Streaming](/parseff/api/streaming) | `Source.of_string`, `Source.of_channel`, `Source.of_function`, `parse_source`, `parse_source_until_end` |
