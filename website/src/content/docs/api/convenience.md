---
title: Convenience combinators
description: Common parsing patterns made easy
---

<!-- This file is generated from doc/convenience.mld. Do not edit directly. -->

# Convenience combinators

Ready-to-use parsers for ASCII character classes, numbers, whitespace, tokens, fixed-length slices, and framed payloads.

## Character classes

```ocaml
val digit : unit -> char
val letter : unit -> char
val alphanum : unit -> char
val any_char : unit -> char
```

- [`Parseff.digit`](./Parseff.md#val-digit): ASCII decimal digit (`'0'`..`'9'`). Use [`Parseff.integer`](./Parseff.md#val-integer) to parse an integer value directly.
- [`Parseff.letter`](./Parseff.md#val-letter): ASCII letter (`'a'`..`'z'` or `'A'`..`'Z'`).
- [`Parseff.alphanum`](./Parseff.md#val-alphanum): ASCII letter or digit.
- [`Parseff.any_char`](./Parseff.md#val-any_char): any single byte; fails only at end of input.

```ocaml
let username () =
  let first = Parseff.letter () in
  let rest = Parseff.many Parseff.alphanum in
  String.make 1 first ^ String.of_seq (List.to_seq rest)
```

## Numbers

All number parsers stop at the first byte that does not belong to the number and work across chunk boundaries in [`Parseff.Source.parse`](./Parseff-Source.md#val-parse).

```ocaml
val integer : unit -> int
val number : unit -> float
val hex_integer : unit -> int
```

- [`Parseff.integer`](./Parseff.md#val-integer): optional `-` followed by ASCII digits. Fails without a digit or on `int` overflow.
- [`Parseff.number`](./Parseff.md#val-number): JSON-grammar decimal float (optional `-`, digits, optional fraction only when a digit follows `'.'`, optional exponent only when a digit or sign+digit follows `'e'`/`'E'`). Fails without a digit or on float overflow. `"-0"` returns `-0.` (whereas `integer` returns `0`).
- [`Parseff.hex_integer`](./Parseff.md#val-hex_integer): unsigned ASCII hex digits (no sign, no `0x` prefix). Fails without a digit or above `max_int`.

```ocaml
let point () =
  let x = Parseff.integer () in
  let _ = Parseff.char ',' in
  let y = Parseff.integer () in
  (x, y)

let color () =
  let _ = Parseff.char '#' in
  Parseff.hex_integer ()
```

### Custom number formats

`integer`, `number`, and `hex_integer` do not accept leading `+`, `.5`, digit separators (`1_000`), `inf`, or `nan`, and stop before non-matching suffixes: `number` on `"5."` returns `5.` and stops at `'.'`, and on `"2em"` returns `2.` and stops at `'e'`. Build custom formats with [`Parseff.take_while`](./Parseff.md#val-take_while) and [`Parseff.recognize`](./Parseff.md#val-recognize):

```ocaml
let is_digit c = c >= '0' && c <= '9'
let is_letter c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')

let quantity () =
  let text =
    Parseff.recognize (fun () ->
        let _ = Parseff.take_while ~at_least:1 ~label:"digit" is_digit in
        Parseff.optional (fun () ->
            let _ = Parseff.char '.' in
            Parseff.take_while ~at_least:1 ~label:"digit" is_digit))
  in
  let unit_ = Parseff.take_while ~at_least:1 ~label:"unit" is_letter in
  (float_of_string text, unit_)
```

```ocaml
# Parseff.parse "2em" quantity;;
- : (float * string, Parseff.error) result = Ok (2., "em")
# Parseff.parse "1.5eV" quantity;;
- : (float * string, Parseff.error) result = Ok (1.5, "eV")
```

## Whitespace

```ocaml
val is_whitespace : char -> bool
val whitespace : ?at_least:int -> unit -> string
val skip_whitespace : unit -> unit
```

- [`Parseff.is_whitespace`](./Parseff.md#val-is_whitespace) returns `true` for space, tab, newline, and carriage return.
- [`Parseff.whitespace`](./Parseff.md#val-whitespace) reads zero or more ASCII whitespace bytes as a string (pass `~at_least:1` to require at least one).
- [`Parseff.skip_whitespace`](./Parseff.md#val-skip_whitespace) skips ASCII whitespace without allocating. Prefer it whenever you do not need the matched string.

```ocaml
let words () =
  Parseff.sep_by ~at_least:1
    (fun () ->
      Parseff.take_while ~at_least:1 ~label:"word" (fun c ->
          not (Parseff.is_whitespace c)))
    (fun () -> Parseff.whitespace ~at_least:1 ())
```

## Tokens

### `lexeme`

[`Parseff.lexeme`](./Parseff.md#val-lexeme) runs a parser, then `~skip` (default: [`Parseff.skip_whitespace`](./Parseff.md#val-skip_whitespace)), and returns the parser's result.

```ocaml
val lexeme : ?skip:(unit -> unit) -> (unit -> 'a) -> 'a
```

```ocaml
let skip_space_and_comments () =
  Parseff.skip_whitespace ();
  while Parseff.peek_char () = Some '#' do
    Parseff.skip_while (fun c -> c <> '\n');
    Parseff.skip_whitespace ()
  done

let token p = Parseff.lexeme ~skip:skip_space_and_comments p

let pair () =
  let a = token Parseff.digit in
  let _ = token (fun () -> Parseff.char ',') in
  let b = token Parseff.digit in
  (a, b)
```

### `keyword`, `is_ident_char`, and `identifier`

- [`Parseff.is_ident_char`](./Parseff.md#val-is_ident_char): `true` for ASCII letters, digits, and `'_'`.
- [`Parseff.keyword`](./Parseff.md#val-keyword): matches a literal only when the following byte does not satisfy `~is_ident` (default [`Parseff.is_ident_char`](./Parseff.md#val-is_ident_char)), so `keyword "let"` rejects `"letter"` at offset 0. Raises `Invalid_argument` on `""`.
- [`Parseff.identifier`](./Parseff.md#val-identifier): matches one `start` byte followed by zero or more `inner` bytes. `~reserved` rejects keywords as ordinary `Expected` failures at offset 0 so alternatives in [`Parseff.or_`](./Parseff.md#val-or_) still work.

```ocaml
val is_ident_char : char -> bool
val keyword : ?is_ident:(char -> bool) -> string -> unit
val identifier :
  ?label:string ->
  ?reserved:(string -> bool) ->
  (char -> bool) ->
  (char -> bool) ->
  string
```

```ocaml
let is_start c =
  (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c = '_'

let reserved = function
  | "let" | "in" | "if" | "then" | "else" -> true
  | _ -> false

let name () = Parseff.identifier ~reserved is_start Parseff.is_ident_char
```

In streaming, `keyword` needs `String.length s + 1` bytes of `~backtrack_window`, and `identifier` needs the word length plus one byte.

## Fixed-length reads and framed input

```ocaml
val take : int -> string
val within : int -> (unit -> 'a) -> 'a
```

- [`Parseff.take`](./Parseff.md#val-take) reads exactly `n` bytes as a string (fails with an `Expected` error if fewer than `n` bytes remain or `n < 0`).
- [`Parseff.within`](./Parseff.md#val-within) runs a parser bounded to the next `n` bytes, presenting EOF at the frame boundary without copying the payload. Fails if bytes remain unread inside the frame or the input ends before `n` bytes.
- To stream a payload larger than `~backtrack_window` to a callback, see [`Iter`](./streaming.md#iter).

```ocaml
let frame () =
  let len = Parseff.BE.any_uint16 () in
  Parseff.within len (fun () ->
      let tag = Parseff.any_uint8 () in
      let body = Parseff.take_while (fun _ -> true) in
      (tag, body))
```
