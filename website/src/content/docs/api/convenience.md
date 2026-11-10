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

The number parsers read the bytes of a number directly from the input. They work across chunk boundaries in `Parseff.parse_source`. A failure is an `Expected` error at the start of the number, so `Parseff.or_` can try another branch, and its message merges with the other alternatives at that position. In streaming a number must be shorter than `backtrack_window` bytes, or the parse fails with `Backtrack_window_exceeded` at its start.


### `int`

`Parseff.int` parses an optional `-` and one or more decimal digits. It accepts leading zeros. It does not accept `+` or `_` separators. A value outside `min_int .. max_int` fails with `expected integer in int range`. On success it does not allocate.

```ocaml
val int : unit -> int
```
```ocaml
let point () =
  let x = Parseff.int () in
  let _ = Parseff.char ',' in
  let y = Parseff.int () in
  (x, y)

(* Matches "12,-7" -> (12, -7) *)
(* Fails on "+1,2" with "expected integer" *)
(* Fails on "99999999999999999999,0" with "expected integer in int range" *)
```

### `float`

`Parseff.float` parses an optional `-`, one or more digits, an optional fraction, and an optional exponent. A `.` or an `e` must be followed by a digit. It does not accept `.5`, `inf` or `nan`. A value too large for a `float` gives `infinity`.

```ocaml
val float : unit -> float
```
```ocaml
let measurement () =
  let value = Parseff.float () in
  let unit_ = Parseff.take_while ~at_least:1 (fun c -> c >= 'a' && c <= 'z') in
  (value, unit_)

(* Matches "2.5kg" -> (2.5, "kg") *)
(* Matches "6.02e23mol" -> (6.02e23, "mol") *)
(* Fails on "1.kg" with "expected digit" *)
```

### `hex_int`

`Parseff.hex_int` parses one or more hexadecimal digits (`0-9`, `a-f`, `A-F`). It does not accept a sign or a `0x` prefix. A value above `max_int` fails with `expected hexadecimal integer in int range`. On success it does not allocate.

```ocaml
val hex_int : unit -> int
```
```ocaml
let color () =
  let _ = Parseff.char '#' in
  Parseff.hex_int ()

(* Matches "#ff8800" -> 0xff8800 *)
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