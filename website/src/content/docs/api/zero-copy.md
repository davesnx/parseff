---
title: Zero-copy API
description: Zero-copy slices and fused scanners
---

<!-- This file is generated from doc/zero-copy.mld. Do not edit directly. -->

# Zero-copy API

`Parseff.Slice` and `Parseff.sep_by_take` avoid intermediate string allocations when scanning hot paths or converting bytes directly into values.

## `Slice`

A `Parseff.Slice.t` borrows a byte range from the input string without copying under `Parseff.parse`. Under `Parseff.Source.parse` (or inside `Parseff.within`), slices may be materialized into owned copies so the streaming buffer can reclaim older prefixes.

```ocaml
module Slice : sig
  type t
  val to_string : t -> string
  val length : t -> int
  val get : t -> int -> char
  val take_while : ?set:Charset.t -> (char -> bool) -> t
  val take : int -> t
  val recognize : (unit -> 'a) -> t
  val sep_by_take :
    ws:(char -> bool) -> sep:char -> (char -> bool) -> t list
end
```

`Parseff.Slice.get` indexes relative to the start of the slice and raises `Invalid_argument` outside `0 .. Slice.length s - 1`.

### `Slice.take_while`

Zero-copy counterpart of `Parseff.take_while`, with the same `~at_least`, `~label`, and `~set`.

```ocaml
let int_of_slice slice =
  let rec loop i acc =
    if i >= Parseff.Slice.length slice then acc
    else
      let c = Parseff.Slice.get slice i in
      loop (i + 1) ((acc * 10) + (Char.code c - Char.code '0'))
  in
  loop 0 0

let fast_uint () =
  int_of_slice (Parseff.Slice.take_while (fun c -> c >= '0' && c <= '9'))
```

### `Slice.take`

Reads exactly `n` bytes like `Parseff.take` and returns a `Parseff.Slice.t`.

```ocaml
let payload () =
  let len = Parseff.BE.any_uint16 () in
  Parseff.Slice.take len

let () =
  match Parseff.parse "\x00\x03abc" payload with
  | Ok slice -> assert (Parseff.Slice.to_string slice = "abc")
  | Error e -> prerr_endline (Parseff.render_error ~name:"payload" e)
```

### `Slice.recognize`

Runs a parser, discards its return value, and returns the consumed bytes as a `Parseff.Slice.t`. See `Parseff.recognize`.

### `Slice.sep_by_take`

Fused separated-list scanner returning `Parseff.Slice.t` elements. `~ws` skips surrounding bytes around `~sep`, and the final predicate matches each element's bytes.

```ocaml
let csv_to_ints () =
  Parseff.Slice.sep_by_take ~ws:Parseff.is_whitespace ~sep:',' (fun c ->
      c >= '0' && c <= '9')
  |> List.map int_of_slice

let json_int_array () =
  let _ = Parseff.char '[' in
  let nums = csv_to_ints () in
  let _ = Parseff.char ']' in
  nums

let () =
  match Parseff.parse "[1, 2, 3, 4, 5]" json_int_array with
  | Ok nums -> assert (List.fold_left ( + ) 0 nums = 15)
  | Error e -> prerr_endline (Parseff.render_error ~name:"array" e)
```

## Fused string scanner: `sep_by_take`

`Parseff.sep_by_take` runs the same fused loop as `Parseff.Slice.sep_by_take` and returns `string list` directly without intermediate combinator frames.

```ocaml
val sep_by_take :
  ws:(char -> bool) -> sep:char -> (char -> bool) -> string list
```

```ocaml
let array_values () =
  let _ = Parseff.char '[' in
  let values =
    Parseff.sep_by_take ~ws:Parseff.is_whitespace ~sep:',' (fun c ->
        c >= '0' && c <= '9')
  in
  let _ = Parseff.char ']' in
  List.map int_of_string values
```
