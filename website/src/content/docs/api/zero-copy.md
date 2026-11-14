---
title: Zero-copy API
description: High-performance span-based parsing operations
---

<!-- This file is generated from doc/zero-copy.mld. Do not edit directly. -->


# Zero-copy API

The zero-copy API allows you to parse without allocating intermediate strings, significantly improving performance and reducing memory usage.


## Span type

```ocaml
type span
```
A `Parseff.span` is a zero-copy slice of the input string. It refers to the input instead of copying it, so no string is allocated until you call `Parseff.span_to_string`. The type is abstract: read a span with `Parseff.span_length` and `Parseff.span_get`.

For parsers run with `Parseff.parse_source`, spans may be materialized into owned strings internally so the streaming runtime can reclaim older buffer prefixes. In-memory `Parseff.parse` still keeps the zero-copy behavior.

```ocaml
val span_to_string : span -> string
val span_length : span -> int
val span_get : span -> int -> char
```
`Parseff.span_get` counts the index from the start of the span. It raises `Invalid_argument` if the index is not in `0 .. span_length span - 1`.


## Span-based operations


### `take_while_span`

`Parseff.take_while_span` is like `Parseff.take_while` but returns a zero-copy `Parseff.span` instead of allocating a string. No memory allocation until you call `Parseff.span_to_string`. Under `Parseff.parse_source`, the returned span may be backed by an owned copy instead of the live streaming buffer.

```ocaml
val take_while_span : (char -> bool) -> span
```
```ocaml
(* no allocation until you need it *)
let digits_span =
  Parseff.take_while_span (fun c -> c >= '0' && c <= '9')
let n = int_of_span digits_span
```
Real-world usage:

```ocaml
(* Parse integers efficiently *)
let int_of_span span =
  let rec loop i acc =
    if i >= Parseff.span_length span then acc
    else
      let c = Parseff.span_get span i in
      loop (i + 1) (acc * 10 + (Char.code c - Char.code '0'))
  in
  loop 0 0

let fast_integer () =
  let digits =
    Parseff.take_while_span (fun c -> c >= '0' && c <= '9')
  in
  int_of_span digits
```

### `take_span`

`Parseff.take_span` reads exactly `n` bytes like `Parseff.take` and returns a `Parseff.span`. Use it for a length-prefixed payload that you pass on without changing it.

```ocaml
val take_span : int -> span
```
```ocaml
let payload () =
  let len = Parseff.BE.any_uint16 () in
  Parseff.take_span len

let () =
  match Parseff.parse "\x00\x03abc" payload with
  | Ok span -> assert (Parseff.span_to_string span = "abc")
  | Error e -> prerr_endline (Parseff.render_error ~name:"payload" e)
```

### `recognize_span`

`Parseff.recognize_span` returns the input text that a parser consumed, as a `Parseff.span`. See `Parseff.recognize`.

```ocaml
val recognize_span : (unit -> 'a) -> span
```

### `sep_by_take_span`

`Parseff.sep_by_take_span` is like `Parseff.sep_by_take` but returns zero-copy `Parseff.span`s. No `String.sub` allocations per element in the in-memory runner. Under `Parseff.parse_source`, spans may be copied so the runtime can discard older chunks safely.

```ocaml
val sep_by_take_span : ws:(char -> bool) -> sep:char -> (char -> bool) -> span list
```
```ocaml
let int_of_span span =
  let rec loop i acc =
    if i >= Parseff.span_length span then acc
    else
      let c = Parseff.span_get span i in
      loop (i + 1) (acc * 10 + (Char.code c - Char.code '0'))
  in
  loop 0 0

(* Parse CSV line with zero-copy *)
let csv_line () =
  let spans =
    Parseff.sep_by_take_span ~ws:Parseff.is_whitespace ~sep:','
      (fun c -> c <> ',' && c <> '\n')
  in
  (* Only allocate strings when needed *)
  List.map Parseff.span_to_string spans

(* or process spans directly, avoiding string allocation entirely *)
let csv_to_ints () =
  let spans =
    Parseff.sep_by_take_span ~ws:Parseff.is_whitespace ~sep:','
      (fun c -> c >= '0' && c <= '9')
  in
  List.map int_of_span spans
```

## Fused operations


### `sep_by_take`

`Parseff.sep_by_take` parses zero or more separated values in one pass. `~ws` selects the characters to skip around each `~sep`; the last predicate selects the characters of each value. Returns a list of matched strings with no intermediate parser round-trips.

```ocaml
val sep_by_take : ws:(char -> bool) -> sep:char -> (char -> bool) -> string list
```
```ocaml
(* Parse array values efficiently *)
let array_values () =
  let _ = Parseff.char '[' in
  let values =
    Parseff.sep_by_take ~ws:Parseff.is_whitespace ~sep:','
      (fun c -> c >= '0' && c <= '9')
  in
  let _ = Parseff.char ']' in
  List.map int_of_string values

(* Matches "[1, 2, 3]" -> [1; 2; 3] *)
```

## Performance comparison

String allocation:

```ocaml
(* allocates intermediate strings *)
let parse_numbers () =
  Parseff.sep_by
    (fun () ->
      Parseff.skip_whitespace ();
      let s =
        Parseff.take_while ~at_least:1 (fun c -> c >= '0' && c <= '9') ~label:"digit"
      in
      int_of_string s)
    (fun () -> Parseff.char ',')
    ()

(* no allocations until conversion *)
let parse_numbers_fast () =
  let spans =
    Parseff.sep_by_take_span ~ws:Parseff.is_whitespace ~sep:','
      (fun c -> c >= '0' && c <= '9')
  in
  List.map int_of_span spans
```

## Complete example: high-performance JSON array

```ocaml
let int_of_span span =
  let rec loop i acc =
    if i >= Parseff.span_length span then acc
    else
      let c = Parseff.span_get span i in
      loop (i + 1) (acc * 10 + (Char.code c - Char.code '0'))
  in
  loop 0 0

let json_array () =
  let _ = Parseff.char '[' in
  let spans =
    Parseff.sep_by_take_span ~ws:Parseff.is_whitespace ~sep:','
      (fun c -> c >= '0' && c <= '9')
  in
  let _ = Parseff.char ']' in
  List.map int_of_span spans

let () =
  match Parseff.parse "[1, 2, 3, 4, 5]" json_array with
  | Ok nums ->
      Printf.printf "Sum: %d\n" (List.fold_left ( + ) 0 nums)
  | Error e -> prerr_endline (Parseff.render_error ~name:"array" e)
```
The zero-copy version is roughly 2\.5x faster than the standard API and uses 3x less memory.


## When to use zero-copy

**Use zero-copy when:**

- Parsing large inputs
- Performance is critical
- You can process spans directly (e.g., custom number parsing)
- Parsing hot paths (inner loops)
**Skip zero-copy when:**

- Readability is more important than performance
- You need to store parsed strings long-term (just use the regular API)
- The performance difference doesn't matter for your use case