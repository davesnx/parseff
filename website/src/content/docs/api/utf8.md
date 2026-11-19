---
title: UTF-8 parsing
description: Parse Unicode code points from UTF-8 input
---

<!-- This file is generated from doc/utf8.mld. Do not edit directly. -->


# UTF-8 Parsing

The `Parseff.Utf8` module provides primitives that operate on Unicode code points (`Uchar.t`) instead of byte sequences (`string`). The input is still an OCaml `string`, but characters are decoded as UTF-8 sequences.

Primitives operate at the code point level. A base character followed by a combining accent is two separate code points. `Parseff.Utf8.satisfy` returns them individually. For grapheme-level parsing, compose the code-point primitives with a grapheme segmentation library like [uuseg](https://erratique.ch/software/uuseg).


## Mixing with byte-level primitives

UTF-8 and byte-level primitives can be freely mixed in the same parser. Use byte-level `Parseff.literal` or `Parseff.satisfy` for ASCII structural tokens, and `Parseff.Utf8` for multilingual text content:

```ocaml
let field () =
  let key = Parseff.take_while ~at_least:1
    (fun c -> c >= 'a' && c <= 'z')
    ~label:"key"
  in
  Parseff.literal ":";
  Parseff.Utf8.skip_whitespace ();
  let value = Parseff.Utf8.take_while ~at_least:1
    ~label:"value"
    (fun u -> Uchar.to_int u <> 0x0A)
  in
  (key, value)
```

### `satisfy`

`Parseff.Utf8.satisfy` decodes the next UTF-8 code point and tests it against a predicate. Advances by 1--4 bytes depending on the encoding. The `~label` is used in error messages; without it, the error says `expected matching character`.

```ocaml
val satisfy : ?label:string -> (Uchar.t -> bool) -> Uchar.t
```
```ocaml
(* Match any CJK Unified Ideograph *)
let cjk_char () =
  Parseff.Utf8.satisfy ~label:"CJK character" (fun u ->
      let i = Uchar.to_int u in
      i >= 0x4E00 && i <= 0x9FFF)
```

### `char`

`Parseff.Utf8.char` matches an exact Unicode code point.

```ocaml
val char : Uchar.t -> Uchar.t
```
```ocaml
let lambda () = Parseff.Utf8.char (Uchar.of_int 0x03BB)  (* λ *)
let arrow () = Parseff.Utf8.char (Uchar.of_int 0x2192)   (* → *)
```

### `take_while`

`Parseff.Utf8.take_while` consumes code points while the predicate holds. Returns the matched UTF-8 bytes as a string. The optional `~at_least` parameter counts *code points*, not bytes — users think in characters when using Unicode primitives, so the count matches that mental model.

```ocaml
val take_while : ?at_least:int -> ?label:string -> (Uchar.t -> bool) -> string
```
```ocaml
(* Parse a Unicode word *)
let word () =
  Parseff.Utf8.take_while
    Uucp.Alpha.is_alphabetic
    ~at_least:1
    ~label:"letter"

(* Parses "hello", "café", "東京", "Москва", etc. *)
```

### `skip_while`

`Parseff.Utf8.skip_while` advances past code points without building a string. More efficient than `Parseff.Utf8.take_while` when you don't need the result.

```ocaml
val skip_while : (Uchar.t -> bool) -> unit
```

### `Slice.Utf8.take_while`

`Parseff.Slice.Utf8.take_while` returns a zero-copy `Parseff.Slice.t` instead of allocating a new string. Use `Parseff.Slice.to_string` to materialize when needed.

```ocaml
val Slice.Utf8.take_while : (Uchar.t -> bool) -> Slice.t
```

## Convenience combinators

These are built on top of the structural primitives using Unicode character properties from the [uucp](https://erratique.ch/software/uucp) library.


### `letter`

`Parseff.Utf8.letter` matches any Unicode alphabetic character using `Uucp.Alpha.is_alphabetic`. This covers Latin, Greek, Cyrillic, CJK, Arabic, Devanagari, and all other Unicode scripts.

```ocaml
let l = Parseff.Utf8.letter ()
(* Matches: 'a', 'é', 'λ', '中', 'д', 'ع', 'अ', ... *)
```

### `alphanum`

`Parseff.Utf8.alphanum` matches a Unicode alphabetic character or an ASCII digit. Combines `Uucp.Alpha.is_alphabetic` with the ASCII digit range.


### `whitespace` and `skip_whitespace`

`Parseff.Utf8.whitespace` and `Parseff.Utf8.skip_whitespace` use the full Unicode White\_Space property (`Uucp.White.is_white_space`). This includes ASCII whitespace plus:

- NO-BREAK SPACE (U+00A0)
- EN SPACE (U+2002), EM SPACE (U+2003)
- IDEOGRAPHIC SPACE (U+3000)
- and others
The `~at_least` parameter on `Parseff.Utf8.whitespace` counts code points, not bytes.

```ocaml
(* Skip any Unicode whitespace before a value *)
Parseff.Utf8.skip_whitespace ();
let value = Parseff.Utf8.take_while ~at_least:1
  Uucp.Alpha.is_alphabetic
  ~label:"word"
```

### `is_whitespace`

`Parseff.Utf8.is_whitespace` exposes the Unicode whitespace predicate for use with `Parseff.Utf8.take_while` or `Parseff.Utf8.skip_while` directly.


## Invalid UTF-8

All UTF-8 primitives raise a parse error when they encounter an invalid byte sequence. This includes:

- Bare continuation bytes (0x80--0xBF)
- Invalid lead bytes (0xFE, 0xFF)
- Overlong encodings
- Truncated multi-byte sequences
The error expects `"valid UTF-8"` and the position points to the first invalid byte: `expected valid UTF-8, found '\xFF'`. In an error report the invalid byte prints as `\xFF` in the found text and in the source line.

`Parseff.Utf8.char` reports the expected character itself, for example `expected 'é', found 'e'`. A `Utf8` primitive that takes a label reports it when the predicate does not match or too few code points match. Invalid UTF-8 always reports `valid UTF-8`.


## Position tracking

Positions remain byte offsets, consistent with the rest of parseff. A single `Parseff.Utf8.satisfy` call advances the position by 1--4 bytes depending on the UTF-8 encoding of the matched code point. `Parseff.position` always returns a byte offset.


## Columns in error messages

The column in an error header counts Unicode characters (code points). `location.byte_col` in `Parseff.error` counts bytes, because it indexes the input string. On a line with non-ASCII text the two differ:

```
input.json:1:6: [error] expected ',' or ']', found '"'
1 | ["é" "b"]
  |      ^
```
Here `location.byte_col` is 7\. `location` is `None` when the line is not known. When streaming, if the start of the line is no longer buffered, the header still has the character column, because Parseff counts the characters of the line that it drops. When the error starts in the dropped part of the line, the header column is `location.byte_col`, which counts bytes, and the excerpt shows only the kept part of the line; pass `~source` to `render_error` to get the character column and the full line then. The carets under the source line count terminal cells: a wide character such as `名` takes two, and a combining mark takes none.


## Streaming support

All UTF-8 primitives work with streaming input (`Parseff.parse_source`). Multi-byte UTF-8 sequences that span chunk boundaries are handled correctly, the streaming runtime ensures enough bytes are available before decoding each code point.
