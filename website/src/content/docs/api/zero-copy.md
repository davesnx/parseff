---
title: Zero-copy API
description: High-performance span-based parsing operations
---

<!-- This file is generated from doc/zero-copy.mld. Do not edit directly. -->


# Zero-copy API

The zero-copy API allows you to parse without allocating intermediate strings, significantly improving performance and reducing memory usage.


## Slice type

```ocaml
module Slice : sig type t end
```
A `Parseff.Slice.t` is a zero-copy slice of the input string. It refers to the input instead of copying it, so no string is allocated until you call `Parseff.Slice.to_string`. The type is abstract: read a slice with `Parseff.Slice.length` and `Parseff.Slice.get`.

For parsers run with `Parseff.parse_source`, slices may be materialized into owned strings internally so the streaming runtime can reclaim older buffer prefixes. In-memory `Parseff.parse` still keeps the zero-copy behavior.

```ocaml
val Slice.to_string : Slice.t -> string
val Slice.length : Slice.t -> int
val Slice.get : Slice.t -> int -> char
```
`Parseff.Slice.get` counts the index from the start of the slice. It raises `Invalid_argument` if the index is not in `0 .. Slice.length s - 1`.


## Slice operations


### `Slice.take_while`

`Parseff.Slice.take_while` is like `Parseff.take_while` but returns a zero-copy `Parseff.Slice.t` instead of allocating a string. No memory allocation until you call `Parseff.Slice.to_string`. Under `Parseff.parse_source`, the returned slice may be backed by an owned copy instead of the live streaming buffer.

```ocaml
val Slice.take_while : (char -> bool) -> Slice.t
```
```ocaml
(* no allocation until you need it *)
let digits_span =
  Parseff.Slice.take_while (fun c -> c >= '0' && c <= '9')
let n = int_of_span digits_span
```
Real-world usage:

```ocaml
(* Parse integers efficiently *)
let int_of_span span =
  let rec loop i acc =
    if i >= Parseff.Slice.length span then acc
    else
      let c = Parseff.Slice.get span i in
      loop (i + 1) (acc * 10 + (Char.code c - Char.code '0'))
  in
  loop 0 0

let fast_integer () =
  let digits =
    Parseff.Slice.take_while (fun c -> c >= '0' && c <= '9')
  in
  int_of_span digits
```

### `Slice.take`

`Parseff.Slice.take` reads exactly `n` bytes like `Parseff.take` and returns a `Parseff.Slice.t`. Use it for a length-prefixed payload that you pass on without changing it.

```ocaml
val Slice.take : int -> Slice.t
```
```ocaml
let payload () =
  let len = Parseff.BE.any_uint16 () in
  Parseff.Slice.take len

let () =
  match Parseff.parse "\x00\x03abc" payload with
  | Ok span -> assert (Parseff.Slice.to_string span = "abc")
  | Error e -> prerr_endline (Parseff.render_error ~name:"payload" e)
```

### `Slice.recognize`

`Parseff.Slice.recognize` returns the input text that a parser consumed, as a `Parseff.Slice.t`. See `Parseff.recognize`.

```ocaml
val Slice.recognize : (unit -> 'a) -> Slice.t
```

### `Slice.sep_by_take`

`Parseff.Slice.sep_by_take` is like `Parseff.sep_by_take` but returns zero-copy `Parseff.Slice.t`s. No `String.sub` allocations per element in the in-memory runner. Under `Parseff.parse_source`, slices may be copied so the runtime can discard older chunks safely.

```ocaml
val Slice.sep_by_take : ws:(char -> bool) -> sep:char -> (char -> bool) -> Slice.t list
```
```ocaml
let int_of_span span =
  let rec loop i acc =
    if i >= Parseff.Slice.length span then acc
    else
      let c = Parseff.Slice.get span i in
      loop (i + 1) (acc * 10 + (Char.code c - Char.code '0'))
  in
  loop 0 0

(* Parse CSV line with zero-copy *)
let csv_line () =
  let spans =
    Parseff.Slice.sep_by_take ~ws:Parseff.is_whitespace ~sep:','
      (fun c -> c <> ',' && c <> '\n')
  in
  (* Only allocate strings when needed *)
  List.map Parseff.Slice.to_string spans

(* or process slices directly, avoiding string allocation entirely *)
let csv_to_ints () =
  let spans =
    Parseff.Slice.sep_by_take ~ws:Parseff.is_whitespace ~sep:','
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

(* no allocations until conversion *)
let parse_numbers_fast () =
  let spans =
    Parseff.Slice.sep_by_take ~ws:Parseff.is_whitespace ~sep:','
      (fun c -> c >= '0' && c <= '9')
  in
  List.map int_of_span spans
```

## Complete example: high-performance JSON array

```ocaml
let int_of_span span =
  let rec loop i acc =
    if i >= Parseff.Slice.length span then acc
    else
      let c = Parseff.Slice.get span i in
      loop (i + 1) (acc * 10 + (Char.code c - Char.code '0'))
  in
  loop 0 0

let json_array () =
  let _ = Parseff.char '[' in
  let spans =
    Parseff.Slice.sep_by_take ~ws:Parseff.is_whitespace ~sep:','
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
- You can process slices directly (e.g., custom number parsing)
- Parsing hot paths (inner loops)
**Skip zero-copy when:**

- Readability is more important than performance
- You need to store parsed strings long-term (just use the regular API)
- The performance difference doesn't matter for your use case