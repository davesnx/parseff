---
title: API Overview
description: Parseff API reference — types, runner, and organization
---

## Parser type

A parser in Parseff is a function `unit -> 'a`. Parsers perform algebraic effects to consume input, backtrack, and report errors. The effect handler installed by `parse` manages all state.

```ocaml
let my_parser () =
  let x = Parseff.consume "hello" in
  let _ = Parseff.consume " " in
  let y = Parseff.consume "world" in
  (x, y)
```

## Running parsers

### `parse`

```ocaml
val parse :
  ?max_depth:int ->
  string ->
  (unit -> 'a) ->
  ('a, [> `Expected of string | `Unexpected_end_of_input]) result
```

Runs a parser on an input string. Returns `Ok value` on success, or `Error { pos; error }` on failure.

The `~max_depth` parameter limits recursion depth for parsers that use `rec_` (default: 128).

```ocaml
match Parseff.parse "hello world" my_parser with
| Ok (x, y) -> Printf.printf "Matched: %s %s\n" x y
| Error { pos; error = `Expected msg } ->
    Printf.printf "Error at %d: %s\n" pos msg
| Error _ -> print_endline "Parse error"
```

Custom errors raised via `error` are also returned through the result type:

```ocaml
let byte () =
  let s = Parseff.take_while1 (fun c -> c >= '0' && c <= '9') ~label:"digit" in
  let n = int_of_string s in
  if n > 255 then Parseff.error (`Out_of_range n) else n

match Parseff.parse "300" byte with
| Error { error = `Out_of_range n; _ } ->
    Printf.printf "%d exceeds 255\n" n
| _ -> ()
```

### `parse_source`

For streaming input from files, channels, or custom byte sources. See the [Streaming API reference](/parseff/api/streaming).

## Result type

```ocaml
type ('a, 'e) result =
  | Ok of 'a
  | Error of { pos : int; error : 'e }
```

- `Ok value` -- the parsed value
- `Error { pos; error }` -- position where parsing failed and the error value

## Position

```ocaml
val position : unit -> int
```

Returns the current parser offset in bytes from the start of the input:

```ocaml
let with_pos p () =
  let start = Parseff.position () in
  let value = p () in
  let end_ = Parseff.position () in
  (value, start, end_)
```

## Span type

Zero-copy string slices for high-performance parsing:

```ocaml
type span = { buf : string; off : int; len : int }

val span_to_string : span -> string
```

A `span` references a slice of the original input string without allocating a new string. Call `span_to_string` when you need the actual string value.

```ocaml
let fast_digits () =
  let digits = Parseff.take_while_span (fun c -> c >= '0' && c <= '9') in
  (* No allocation yet — span points into the input buffer *)
  let s = Parseff.span_to_string digits in
  (* Now a string is allocated *)
  s
```

## API organization

| Section | Combinators |
|---------|------------|
| [Core](/parseff/api/primitives) | `consume`, `char`, `satisfy`, `take_while`, `take_while1`, `skip_while`, `match_regex`, `fail`, `error`, `end_of_input` |
| [Combinators](/parseff/api/combinators) | `or_`, `one_of`, `one_of_labeled`, `optional`, `look_ahead`, `expect`, `rec_` |
| [Repetition & Separation](/parseff/api/repetition) | `many`, `many1`, `count`, `sep_by`, `sep_by1`, `between`, `end_by`, `end_by1`, `chainl`, `chainl1`, `chainr`, `chainr1` |
| [Convenience](/parseff/api/convenience) | `digit`, `letter`, `alphanum`, `any_char`, `is_whitespace`, `whitespace`, `whitespace1`, `skip_whitespace` |
| [Zero-Copy & Fused Ops](/parseff/api/zero-copy) | `take_while_span`, `sep_by_take_span`, `sep_by_take`, `fused_sep_take`, `skip_while_then_char` |
| [Streaming](/parseff/api/streaming) | `Source.of_string`, `Source.of_channel`, `Source.of_function`, `parse_source` |
