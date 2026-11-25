---
title: Primitives
description: Basic parsing operations in Parseff
---

<!-- This file is generated from doc/primitives.mld. Do not edit directly. -->

# Primitives

Primitives read bytes directly from the input.

## String and character matching

### `literal`

[`Parseff.literal`](./Parseff.md#val-literal) matches an exact string and returns `()`. On a mismatch, it fails at the first differing byte expecting the rest of the string: `literal "ab"` on `"ax"` fails at offset 1 with `expected "b"`. Mismatches inside a multi-byte UTF-8 character are reported at the start of that character. Pass `~case_sensitive:false` for ASCII case-insensitive matching (`A`–`Z` and `a`–`z` only).

```ocaml
val literal : ?case_sensitive:bool -> string -> unit
```

```ocaml
let greeting () =
  Parseff.literal ~case_sensitive:false "hello ";
  Parseff.literal "world"
```

In streaming, `literal` checks buffered bytes first and pulls more input only while the prefix matches, so a wrong byte fails immediately without waiting for the rest of the literal.

### `char`

[`Parseff.char`](./Parseff.md#val-char) matches a single byte and returns it.

```ocaml
val char : char -> char
```

```ocaml
let pair () =
  let _ = Parseff.char '(' in
  let a = Parseff.digit () in
  let _ = Parseff.char ',' in
  let b = Parseff.digit () in
  let _ = Parseff.char ')' in
  (a, b)
```

### `satisfy`

[`Parseff.satisfy`](./Parseff.md#val-satisfy) matches one byte satisfying a predicate. `~label` names the expectation in error messages (default: `"matching character"`).

```ocaml
val satisfy : ?label:string -> (char -> bool) -> char
```

```ocaml
let vowel () =
  Parseff.satisfy ~label:"vowel" (fun c -> String.contains "aeiouAEIOU" c)

let uppercase () =
  Parseff.satisfy ~label:"uppercase letter" (fun c -> c >= 'A' && c <= 'Z')
```

### `peek_char`

[`Parseff.peek_char`](./Parseff.md#val-peek_char) returns `Some c` for the next byte without consuming it, or `None` at end of input. Use it for cheap single-byte dispatch instead of trying alternatives with [`Parseff.or_`](./Parseff.md#val-or_).

```ocaml
val peek_char : unit -> char option
```

```ocaml
let value_start () =
  match Parseff.peek_char () with
  | Some '[' -> `Array
  | Some '{' -> `Object
  | Some '"' -> `String
  | _ -> Parseff.fail "expected value"
```

## Character scanning

### `take_while` and `skip_while`

[`Parseff.take_while`](./Parseff.md#val-take_while) consumes bytes while a predicate holds and returns them as a string. [`Parseff.skip_while`](./Parseff.md#val-skip_while) does the same without allocating.

```ocaml
val take_while :
  ?at_least:int -> ?at_most:int -> ?label:string -> ?set:Charset.t ->
  (char -> bool) -> string

val skip_while :
  ?at_least:int -> ?at_most:int -> ?label:string -> ?set:Charset.t ->
  (char -> bool) -> unit
```

- `~at_least` requires at least that many matching bytes, failing with `~label` (default: `"at least n characters"`) if fewer match.
- `~at_most` caps the byte count, failing with `Limit_exceeded { limit = at_most }` if another matching byte follows. The error covers those `at_most + 1` bytes.
- [`Parseff.skip_while`](./Parseff.md#val-skip_while) and [`Parseff.Slice.take_while`](./Parseff-Slice.md#val-take_while) take `~at_least`, `~at_most` and `~label` too. On a stream, `skip_while` does not keep the bytes it skips, so `at_most` can exceed `~backtrack_window`.
- `~set` replaces per-byte predicate calls with a precomputed [`Parseff.Charset`](./Parseff-Charset.md) lookup table.

```ocaml
let digits () =
  Parseff.take_while ~at_least:1 ~label:"digit" (fun c -> c >= '0' && c <= '9')

let csv_field () =
  Parseff.skip_while (fun c -> c = ' ');
  let value = Parseff.take_while (fun c -> c <> ',' && c <> '\n') in
  Parseff.skip_while (fun c -> c = ' ');
  value
```

### `Charset`

[`Parseff.Charset`](./Parseff-Charset.md) builds a 256-byte lookup table once outside your parser so [`Parseff.take_while`](./Parseff.md#val-take_while), [`Parseff.skip_while`](./Parseff.md#val-skip_while), [`Parseff.Slice.take_while`](./Parseff-Slice.md#val-take_while), and [`Parseff.Iter.take_while`](./Parseff-Iter.md#val-take_while) avoid calling an OCaml predicate closure per byte.

```ocaml
module Charset : sig
  type t
  val of_predicate : (char -> bool) -> t
  val mem : t -> char -> bool
end
```

```ocaml
let is_digit c = c >= '0' && c <= '9'
let digit_set = Parseff.Charset.of_predicate is_digit

let fast_digits () =
  Parseff.take_while ~at_least:1 ~label:"digit" ~set:digit_set is_digit
```

### `take_until` and `skip_until`

[`Parseff.take_until`](./Parseff.md#val-take_until) reads all bytes before the first occurrence of a terminator string without consuming the terminator. [`Parseff.skip_until`](./Parseff.md#val-skip_until) skips them without allocating. Both fail at end of input with `expected "<terminator>"` (or `~label`) if the terminator never appears. `~at_most` caps the bytes before the terminator, failing with `Limit_exceeded { limit = at_most }` if the terminator does not start within `at_most + 1` bytes, even when input ends there.

```ocaml
val take_until : ?at_most:int -> ?label:string -> string -> string
val skip_until : ?at_most:int -> ?label:string -> string -> unit
```

```ocaml
let block_comment () =
  Parseff.literal "/*";
  let body = Parseff.take_until "*/" in
  Parseff.literal "*/";
  body
```

In streaming, `skip_until` retains only the last `String.length terminator - 1` bytes, so it can skip past text longer than `~backtrack_window`; `take_until` must hold its entire result in the window.

## Input bounds and positions

### `end_of_input`

[`Parseff.end_of_input`](./Parseff.md#val-end_of_input) succeeds only when no input remains. [`Parseff.parse`](./Parseff.md#val-parse) and [`Parseff.Source.parse`](./Parseff-Source.md#val-parse) enforce this by default unless ``~consume:`Partial`` is passed, so top-level parsers rarely need it.

```ocaml
val end_of_input : unit -> unit
```

### `position`, `with_range`, and `location`

- [`Parseff.position`](./Parseff.md#val-position) returns the current byte offset from the start of the input. Under [`Parseff.Source.parse`](./Parseff-Source.md#val-parse) it counts from where the parse starts in the source; add [`Parseff.Source.offset`](./Parseff-Source.md#val-offset), read before the parse, to get an absolute offset.
- [`Parseff.with_range`](./Parseff.md#val-with_range) runs a parser and returns its result alongside the [`Parseff.range`](./Parseff.md#type-range) of bytes it consumed.
- [`Parseff.location`](./Parseff.md#val-location) returns a 1-based [`Parseff.location`](./Parseff.md#type-location) record (`byte_col` counts bytes, not Unicode code points). The line index is built lazily on first call. Outside a parser, use [`Parseff.location_of_position`](./Parseff.md#val-location_of_position).

```ocaml
type range = { start : int; stop : int }
type location = private { offset : int; line : int; byte_col : int }

val position : unit -> int
val with_range : (unit -> 'a) -> 'a * range
val location : unit -> location
val location_of_position : string -> int -> location
```

```ocaml
let octet () =
  let n, range =
    Parseff.with_range (fun () ->
        int_of_string
          (Parseff.take_while ~at_least:1 ~label:"digit" (fun c ->
               c >= '0' && c <= '9')))
  in
  if n > 255 then
    Parseff.fail
      ~notes:[ { Parseff.at = Some range; text = "octets are 0 to 255" } ]
      "octet out of range";
  n

let report_error input = function
  | Error e ->
      let loc = Parseff.location_of_position input e.Parseff.range.start in
      Printf.printf "line %d, byte %d: %s\n" loc.line loc.byte_col
        (Parseff.error_message e)
  | Ok _ -> ()
```

### `recognize` and `Slice.recognize`

[`Parseff.recognize`](./Parseff.md#val-recognize) runs a parser, discards its result, and returns the consumed input bytes as a string. [`Parseff.Slice.recognize`](./Parseff-Slice.md#val-recognize) returns a zero-copy [`Parseff.Slice.t`](./Parseff-Slice.md#type-t).

```ocaml
val recognize : (unit -> 'a) -> string
val Slice.recognize : (unit -> 'a) -> Slice.t
```

```ocaml
let ident () =
  Parseff.recognize (fun () ->
      let _ = Parseff.letter () in
      let _ =
        Parseff.many (fun () ->
            Parseff.or_ Parseff.alphanum (fun () -> Parseff.char '_'))
      in
      ())

let () =
  match Parseff.parse ~consume:`Partial "foo_1 = 2" ident with
  | Ok name -> assert (name = "foo_1")
  | Error _ -> assert false
```

### Failure primitives

For [`Parseff.fail`](./Parseff.md#val-fail), [`Parseff.error`](./Parseff.md#val-error), and [`Parseff.expect`](./Parseff.md#val-expect), see [`errors`](./errors.md).
