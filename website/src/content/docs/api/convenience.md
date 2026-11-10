---
title: Convenience combinators
description: Common parsing patterns made easy
---

<!-- This file is generated from doc/convenience.mld. Do not edit directly. -->


# Convenience combinators

Convenience combinators provide ready-to-use parsers for common patterns like digits, numbers, letters, and whitespace.


## Character classes


### `digit`

`Parseff.digit` parses a decimal digit (0-9) and returns its integer value.

```ocaml
val digit : unit -> int
```
```ocaml
(* Parse two-digit number *)
let two_digits () =
  let a = Parseff.digit () in
  let b = Parseff.digit () in
  a * 10 + b

(* Matches "42" -> 42 *)
```

### `letter`

`Parseff.letter` parses an ASCII letter (a-z or A-Z).

```ocaml
val letter : unit -> char
```
```ocaml
let initial () = Parseff.letter ()

(* Matches "A" -> 'A' *)
(* Matches "z" -> 'z' *)
(* Fails on "1" *)
```

### `alphanum`

`Parseff.alphanum` parses an alphanumeric character (letter or digit).

```ocaml
val alphanum : unit -> char
```
```ocaml
let username () =
  let first = Parseff.letter () in
  let rest = Parseff.many Parseff.alphanum () in
  String.make 1 first ^ String.of_seq (List.to_seq rest)

(* Matches "user123" -> "user123" *)
(* Fails on "123user" (must start with letter) *)
```

### `any_char`

`Parseff.any_char` parses any character. Fails only at end of input.

```ocaml
val any_char : unit -> char
```
```ocaml
(* Skip past a single unknown character *)
let skip_one () =
  let _ = Parseff.any_char () in
  ()

(* Peek at the next character without consuming it *)
let peek () = Parseff.look_ahead Parseff.any_char
```

## Numbers

The number parsers read the bytes of a number directly from the input. They work across chunk boundaries in `Parseff.parse_source`. A failure is an `Expected` error. A missing number fails at its start, so `Parseff.or_` can try another branch, and its message merges with the other alternatives at that position. A `-`, `.` or exponent marker with no digit after it fails with `expected digit` after the marker: `"-x"` fails at offset 1, and `"5."` and `"1e"` fail at offset 2\. In streaming a number must be shorter than `backtrack_window` bytes, or the parse fails with `Backtrack_window_exceeded` at its start.

A number parser stops at the first byte that does not fit its syntax and leaves that byte for the next parser. On `"0x1F"` each of them reads `"0"` and leaves `"x1F"`.


### `integer`

`Parseff.integer` parses an optional `-` and one or more decimal digits. It accepts leading zeros. It does not accept a leading `+`, `_` separators, a fraction or an exponent: on `"1e5"` it returns `1` and leaves `"e5"`. A value outside `min_int .. max_int` fails with `expected integer in int range`. On success it does not allocate.

```ocaml
val integer : unit -> int
```
```ocaml
let point () =
  let x = Parseff.integer () in
  let _ = Parseff.char ',' in
  let y = Parseff.integer () in
  (x, y)

(* Matches "12,-7" -> (12, -7) *)
(* Fails on "+1,2" with "expected integer" *)
(* Fails on "99999999999999999999,0" with "expected integer in int range" *)
```

### `number`

`Parseff.number` parses a number with the JSON number grammar: an optional `-`, one or more digits, an optional fraction (`.` and digits), and an optional exponent (`e` or `E`, an optional `+` or `-`, and digits). `"-0.5"`, `"1E5"` and `"1e+5"` match. A `.` or an `e` must be followed by a digit, so `"5."` and `"1e"` fail. It does not accept a leading `+`, `.5`, `_` separators, `inf` or `nan`. A value too large for a `float` gives `infinity`.

An `e` or `E` after the number always starts an exponent. So `"2em"` and `"1.5eV"` fail with `expected digit` after the `e`. To read a number followed by a unit, see Other number formats below.

```ocaml
val number : unit -> float
```
```ocaml
let coordinates () =
  let lat = Parseff.number () in
  let _ = Parseff.char ',' in
  let lon = Parseff.number () in
  (lat, lon)

(* Matches "48.85,-2.35e0" -> (48.85, -2.35) *)
(* Fails on "48.,2" with "expected digit" *)
```

### `hex_integer`

`Parseff.hex_integer` parses one or more hexadecimal digits (`0-9`, `a-f`, `A-F`). It does not accept a sign or a `0x` prefix. A value above `max_int` fails with `expected hexadecimal integer in int range`. On success it does not allocate.

```ocaml
val hex_integer : unit -> int
```
```ocaml
let color () =
  let _ = Parseff.char '#' in
  Parseff.hex_integer ()

(* Matches "#ff8800" -> 0xff8800 *)
```

### Other number formats

The number parsers follow the JSON number grammar. They do not read a leading `+`, a bare `.5` or `5.`, digit separators such as `1_000`, a `0x` prefix, `inf` or `nan`, or locale formats such as `1,5`. They also do not read a unit that starts with `e` directly after the number, such as `2em`: the `e` starts an exponent and the parse fails.

For these formats, write your own parser with `Parseff.take_while`, `Parseff.recognize` and a conversion function. This parser reads digits with an optional fraction, converts them with `float_of_string`, and then reads the unit:

```ocaml
let is_digit c = c >= '0' && c <= '9'
let is_letter c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')

let quantity () =
  let text =
    Parseff.recognize (fun () ->
        let _ = Parseff.take_while ~at_least:1 ~label:"digit" is_digit in
        Parseff.optional
          (fun () ->
            let _ = Parseff.char '.' in
            Parseff.take_while ~at_least:1 ~label:"digit" is_digit)
          ())
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


### `is_whitespace`

`Parseff.is_whitespace` returns true for whitespace characters (space, tab, newline, carriage return). This is a predicate, not a parser.

```ocaml
val is_whitespace : char -> bool
```
```ocaml
let trim_parser () =
  Parseff.skip_while Parseff.is_whitespace;
  let value =
    Parseff.take_while ~at_least:1
      (fun c -> not (Parseff.is_whitespace c))
      ~label:"value"
  in
  Parseff.skip_while Parseff.is_whitespace;
  value

(* Matches "  hello  " -> "hello" *)
```

### `whitespace`

`Parseff.whitespace` parses zero or more whitespace characters. Returns the matched string. Always succeeds (returns empty string if no whitespace).

```ocaml
val whitespace : unit -> string
```
```ocaml
let spaced_values () =
  let a = Parseff.digit () in
  let _ = Parseff.whitespace () in
  let b = Parseff.digit () in
  (a, b)

(* Matches "1 2" -> (1, 2) *)
(* Matches "1    2" -> (1, 2) *)
(* Matches "12" -> (1, 2) *)
```

### `whitespace ~at_least:1`

`Parseff.whitespace` with `~at_least:1` parses one or more whitespace characters. Fails if no whitespace found.

```ocaml
val whitespace : ?at_least:int -> unit -> string
```
```ocaml
let words () =
  Parseff.sep_by ~at_least:1
    (fun () ->
      Parseff.take_while ~at_least:1
        (fun c -> not (Parseff.is_whitespace c))
        ~label:"word")
    (fun () -> Parseff.whitespace ~at_least:1 ())
    ()

(* Matches "hello world" -> ["hello"; "world"] *)
(* Fails on "helloworld" (no whitespace separator) *)
```

### `skip_whitespace`

`Parseff.skip_whitespace` skips zero or more whitespace characters (returns unit). More efficient than `Parseff.whitespace` when you don't need the matched string.

```ocaml
val skip_whitespace : unit -> unit
```
```ocaml
(* Parse comma-separated list with flexible spacing *)
let flexible_list () =
  let _ = Parseff.char '[' in
  Parseff.skip_whitespace ();
  let values =
    Parseff.sep_by
      (fun () ->
        Parseff.skip_whitespace ();
        let n = Parseff.digit () in
        Parseff.skip_whitespace ();
        n)
      (fun () -> Parseff.char ',')
      ()
  in
  Parseff.skip_whitespace ();
  let _ = Parseff.char ']' in
  values

(* All of these parse to [1; 2; 3]: *)
(* "[1,2,3]" *)
(* "[ 1 , 2 , 3 ]" *)
(* "[  1  ,  2  ,  3  ]" *)
```
**Tip:** Always use `skip_whitespace` instead of `whitespace` when you don't need the matched string:

```ocaml
(* allocates a string you don't need *)
let _ = Parseff.whitespace () in
parse_value ()

(* skips without allocating *)
Parseff.skip_whitespace ();
parse_value ()
```

## Tokens


### `lexeme`

`Parseff.lexeme` runs a parser, then skips what follows the token, and returns the result of the parser. By default it skips whitespace with `Parseff.skip_whitespace`. Pass `~skip` when the grammar also has comments.

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

(* Matches "1 , 2" -> (1, 2) *)
(* Matches "1 # first\n, 2" -> (1, 2) *)
```

### `keyword`

`Parseff.keyword` matches a literal only when the next byte cannot continue an identifier. Use it for reserved words, so that `"let"` does not match the start of `"letter"`. By default ASCII letters, digits and `_` continue an identifier. Pass `~is_ident` to change that set. `~is_ident` receives one byte, not a UTF-8 character. At the end of the input the keyword matches. An empty keyword raises `Invalid_argument`.

In streaming, `keyword` reads the byte after the keyword, so it needs a backtrack window of one byte more than the keyword.

```ocaml
val keyword : ?is_ident:(char -> bool) -> string -> unit
```
```ocaml
let binding () =
  Parseff.keyword "let";
  Parseff.skip_whitespace ();
  Parseff.take_while ~at_least:1 (fun c -> c >= 'a' && c <= 'z') ~label:"name"

(* Matches "let x" -> "x" *)
(* Fails on "letter x" at offset 0, expecting "let" *)
```
A failure is reported at the start of the keyword and expects the keyword, so it merges with the other alternatives at that position. The comment shows the message of `parse`. In streaming, the `found` text holds only the bytes already read.

```ocaml
let statement () =
  Parseff.or_
    (fun () -> Parseff.keyword "let")
    (fun () -> Parseff.keyword "in")
    ()

(* parse fails on "inx": expected "in" or "let", found 'inx' *)
```