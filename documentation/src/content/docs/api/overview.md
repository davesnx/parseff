---
title: API Overview
description: Parseff API reference — types, runner, and organization
---

## Parser Type

A parser in Parseff is a function `unit -> 'a`. Parsers perform algebraic effects to consume input, backtrack, and report errors. The effect handler installed by `parse` manages all state.

```ocaml
let my_parser () =
  let x = consume "hello" in
  let _ = consume " " in
  let y = consume "world" in
  (x, y)
```

## Running Parsers

### `parse`

```ocaml
val parse :
  string -> (unit -> 'a) ->
  ('a, [> `Expected of string | `Unexpected_end_of_input]) result
```

Runs a parser on an input string. Returns `Ok value` on success, or `Error { pos; error }` on failure.

```ocaml
match parse "hello world" my_parser with
| Ok result -> Printf.printf "Matched: %s %s\n" (fst result) (snd result)
| Error { pos; error = `Expected msg } ->
    Printf.printf "Error at %d: %s\n" pos msg
```

Custom errors raised via `error` are also returned through the result type:

```ocaml
let byte () =
  let n = parse_int () in
  if n > 255 then error (`Out_of_range n) else n

match parse "300" byte with
| Error { error = `Out_of_range n; _ } ->
    Printf.printf "%d exceeds 255\n" n
| _ -> ()
```

## Result Type

```ocaml
type ('a, 'e) result =
  | Ok of 'a
  | Error of { pos: int; error: 'e }
```

- `Ok value` — the parsed value
- `Error { pos; error }` — the position where parsing failed and the error value

If you need the current input offset inside a parser, call `position ()`:

```ocaml
let with_pos p () =
  let value = p () in
  let pos = position () in
  (value, pos)
```

## Span Type

Zero-copy string slices for high-performance parsing:

```ocaml
type span = { buf: string; off: int; len: int }

val span_to_string : span -> string
```

A `span` references a slice of the original input string without allocating a new string. Call `span_to_string` only when you need the actual string value.

```ocaml
let digits = take_while_span (fun c -> c >= '0' && c <= '9') in
(* No allocation yet — span points into the input buffer *)
let s = span_to_string digits in
(* Now a string is allocated *)
```

## API Organization

- **[Core Primitives](/parseff/api/primitives)** — `consume`, `char`, `satisfy`, `take_while`, `match_re`, `fail`, `error`, `end_of_input`
- **[Combinators](/parseff/api/combinators)** — `or_`, `look_ahead`, `expect`, `one_of`, `one_of_labeled`, `optional`
- **[Repetition](/parseff/api/repetition)** — `many`, `many1`, `sep_by`, `sep_by1`, `count`
- **[Convenience](/parseff/api/convenience)** — `digit`, `letter`, `alphanum`, `whitespace`, `skip_whitespace`, `any_char`
- **[Zero-Copy API](/parseff/api/zero-copy)** — `take_while_span`, `sep_by_take_span`, `fused_sep_take`, `skip_while_then_char`

## Complete Example

```ocaml
let integer () =
  let sign = Parseff.optional (fun () -> Parseff.char '-') () in
  let digits = Parseff.take_while1 (fun c -> c >= '0' && c <= '9') "digit" in
  let n = int_of_string digits in
  match sign with Some _ -> -n | None -> n

let json_array () =
  let _ = Parseff.char '[' in
  Parseff.skip_whitespace ();
  let values = Parseff.sep_by
    (fun () ->
      Parseff.skip_whitespace ();
      let n = integer () in
      Parseff.skip_whitespace ();
      n)
    (fun () -> Parseff.char ',')
    ()
  in
  Parseff.skip_whitespace ();
  let _ = Parseff.char ']' in
  Parseff.end_of_input ();
  values

let () =
  match Parseff.parse "[1, -2, 3]" json_array with
  | Ok (xs) ->
      Printf.printf "Sum: %d\n" (List.fold_left (+) 0 xs)
  | Error { pos; error = `Expected msg } ->
      Printf.printf "Parse error at %d: %s\n" pos msg
```
