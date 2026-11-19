---
title: Quick start
description: Direct-style parser combinator library for OCaml 5 with automatic backtracking, streaming, error recovery, and zero-copy slices.
---

# Quick start

Parseff is a direct-style parser combinator library for OCaml 5. Parsers are plain functions (`unit -> 'a`); the runtime handles backtracking, streaming input, and error reporting with source excerpts.

## Features

- **Direct-style parsers**: compose plain `unit -> 'a` functions without monadic (`>>=`, `>>|`, `*>`) or binding (`let*`, `let+`, `and+`) operators.
- [**Structured errors and source excerpts**](/parseff/api/errors/): furthest-failure reporting with merged expectations, typed custom errors, construct contexts, and multi-error [recovery](/parseff/guides/error-recovery/).
- **Automatic backtracking**: [`Parseff.or_`](/parseff/api/combinators/#or_) and `Parseff.one_of`, with `commit` and `cut` to seal branches.
- [**Pull and push streaming**](/parseff/api/streaming/): `Source.parse`, `Source.parse_each`, and `Push` with bounded backtracking.
- **Domain-safe**: parse state lives per domain so independent parses run in parallel across OCaml 5 domains.
- [**Zero-copy slices and fused scanners**](/parseff/api/zero-copy/): `Slice.t`, `Charset`, and `sep_by_take` for low-allocation hot paths.
- **Minimal dependencies**: depends only on `uucp` for Unicode character properties.

## Performance

In the benchmark suite, Parseff's generic parsers are up to ~1.5x faster than Angstrom's baselines, and its fused zero-copy paths are ~3x faster. Against Angstrom's optimized JSON parser, Parseff's optimized path is ~1.5x faster while reducing minor allocations from ~11.1 GB to ~1.3 GB.

See the [comparison with Angstrom](/parseff/guides/comparison/), [`bench/bench_json.ml`](https://github.com/davesnx/parseff/blob/main/bench/bench_json.ml), and [`bench/`](https://github.com/davesnx/parseff/tree/main/bench).

## Installation

### opam

```bash
opam install parseff -y
```

### Dune package management

Add `parseff` to your `dune-project`:

```lisp
(package
 (name my_project)
 (depends
  (ocaml (>= 5.3))
  parseff))
```

Lock and build:

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

An IPv4 address parser validating octets in `0`–`255` (see the [IP address guide](/parseff/guides/ip-address/)):

```ocaml
type Parseff.custom_error += Out_of_range of int

let octet () =
  let digits = Parseff.many ~at_least:1 Parseff.digit in
  let value d = Char.code d - Char.code '0' in
  let n = List.fold_left (fun acc d -> (acc * 10) + value d) 0 digits in
  if n >= 0 && n <= 255 then n
  else Parseff.error (Out_of_range n)

let ip_address () =
  let a = octet () in
  ignore (Parseff.char '.');
  let b = octet () in
  ignore (Parseff.char '.');
  let c = octet () in
  ignore (Parseff.char '.');
  let d = octet () in
  (a, b, c, d)

let () =
  match Parseff.parse "192.168.1.1" ip_address with
  | Ok (a, b, c, d) ->
      Printf.printf "Parsed: %d.%d.%d.%d\n" a b c d
  | Error { kind = Custom (Out_of_range n); range; _ } ->
      Printf.printf "Error at %d: %d out of range (0-255)\n" range.start n
  | Error e ->
      prerr_endline (Parseff.render_error ~name:"address" e)
```

## API organization

| Section | Key APIs |
|---------|----------|
| [Primitives](/parseff/api/primitives/) | `literal`, `char`, `satisfy`, `peek_char`, `Charset`, `take_while`, `skip_while`, `take_until`, `skip_until`, `end_of_input`, `position`, `with_range`, `location`, `recognize` |
| [Combinators](/parseff/api/combinators/) | `or_`, `one_of`, `one_of_labeled`, `optional`, `look_ahead`, `peek_with`, `skip_to`, `not_followed_by`, `expect`, `context`, `commit`, `cut`, `rec_`, `parse ~trace` |
| [Repetition and separation](/parseff/api/repetition/) | `many`, `count`, `sep_by`, `end_by`, `between`, `many_iter`, `sep_by_iter`, `fold_left`, `fold_right`, `expression`, `precedence` |
| [Convenience](/parseff/api/convenience/) | `digit`, `letter`, `alphanum`, `any_char`, `integer`, `number`, `hex_integer`, `whitespace`, `skip_whitespace`, `lexeme`, `keyword`, `identifier`, `take`, `within`, `take_iter`, `take_while_iter` |
| [Errors](/parseff/api/errors/) | `fail`, `error`, `expect`, `context`, `cut`, `render_error`, `error_message` |
| [Error recovery](/parseff/guides/error-recovery/) | `capture`, `recover`, `require`, `list`, `skip_nested` |
| [Diagnostics](/parseff/api/diagnostics/) | `warn`, `Diagnostics`, `Ref` |
| [Zero-copy API](/parseff/api/zero-copy/) | `Slice.take_while`, `Slice.take`, `Slice.recognize`, `Slice.sep_by_take`, `sep_by_take` |
| [Streaming](/parseff/api/streaming/) | `Source.of_*`, `Source.parse`, `Source.parse_each`, `Push.parse`, `Push.feed`, `Push.finish`, `Push.parse_each` |
| [Binary parsing](/parseff/api/binary/) | `any_uint8`, `any_int8`, `any_varint`, `BE.*`, `LE.*` |
| [UTF-8 parsing](/parseff/api/utf8/) | `Utf8.satisfy`, `Utf8.char`, `Utf8.any_char`, `Utf8.take_while`, `Utf8.letter`, `Utf8.whitespace`, `Utf8.Slice.take_while` |
