---
title: Repetition and separation
description: Looping constructs for parsing repeated and separated patterns
---

<!-- This file is generated from doc/repetition.mld. Do not edit directly. -->


# Repetition and separation

These combinators handle patterns that repeat: lists, separated values, delimited blocks, and operator chains.


## Basic repetition


### `many`

`Parseff.many` applies a parser zero or more times. Returns a list of results. Always succeeds (returns `[]` if the parser fails immediately). Pass `~at_least` to require a minimum number of matches.

```ocaml
val many : ?at_least:int -> (unit -> 'a) -> unit -> 'a list
```
```ocaml
let digits () = Parseff.many Parseff.digit ()
(* "123"  -> [1; 2; 3]  *)
(* ""     -> []         *)
(* "abc"  -> []         *)
```

Pass `~at_least:1` to require at least one match. Fails if the parser doesn't succeed at least once.

```ocaml
let digits1 () = Parseff.many ~at_least:1 Parseff.digit ()
(* "123" -> [1; 2; 3]  *)
(* ""    -> Error      *)
(* "abc" -> Error      *)
```

### `count`

`Parseff.count` applies a parser exactly `n` times. Fails if the parser doesn't match `n` times.

```ocaml
val count : int -> (unit -> 'a) -> unit -> 'a list
```
```ocaml
let three_digits () = Parseff.count 3 Parseff.digit ()
(* "123"  -> [1; 2; 3]  *)
(* "12"   -> Error      *)
```
Useful for fixed-width formats:

```ocaml
let hex_digit () =
  Parseff.satisfy
    (fun c ->
      (c >= '0' && c <= '9')
      || (c >= 'a' && c <= 'f')
      || (c >= 'A' && c <= 'F'))
    ~label:"hex digit"

(* Parse #RRGGBB color *)
let hex_color () =
  let _ = Parseff.char '#' in
  let r = Parseff.count 2 hex_digit () in
  let g = Parseff.count 2 hex_digit () in
  let b = Parseff.count 2 hex_digit () in
  (r, g, b)
(* "#ff00aa" -> (['f';'f'], ['0';'0'], ['a';'a']) *)
```

## Separated lists


### `sep_by`

`Parseff.sep_by` parses zero or more elements separated by a separator. The separator's return value is discarded. Always succeeds. Pass `~at_least` to require a minimum number of elements.

```ocaml
val sep_by : ?at_least:int -> (unit -> 'a) -> (unit -> 'b) -> unit -> 'a list
```
```ocaml
let csv_line () =
  Parseff.sep_by
    (fun () -> Parseff.take_while (fun c -> c <> ',' && c <> '\n'))
    (fun () -> Parseff.char ',')
    ()
(* "a,b,c" -> ["a"; "b"; "c"] *)
(* ""      -> [""]             *)
```

Pass `~at_least:1` to `sep_by` to require at least one element.

```ocaml
let csv_line1 () =
  Parseff.sep_by ~at_least:1
    (fun () ->
      Parseff.take_while ~at_least:1
        (fun c -> c <> ',' && c <> '\n')
        ~label:"value")
    (fun () -> Parseff.char ',')
    ()
(* "a,b,c" -> ["a"; "b"; "c"] *)
(* "a"     -> ["a"]           *)
(* ""      -> Error            *)
```

## Delimiters and terminators


### `between`

`Parseff.between` parses an opening delimiter, then the body, then a closing delimiter. Returns the body's value.

```ocaml
val between : (unit -> 'a) -> (unit -> 'b) -> (unit -> 'c) -> unit -> 'c
```
```ocaml
let parens p =
  Parseff.between
    (fun () -> Parseff.char '(')
    (fun () -> Parseff.char ')')
    p

let parenthesized_digit () =
  parens
    (fun () ->
      Parseff.skip_whitespace ();
      let n = Parseff.digit () in
      Parseff.skip_whitespace ();
      n)
    ()
(* "(42)"   -> 42 *)
(* "( 42 )" -> Error (only parses one digit) *)
```
Works well for bracketed structures:

```ocaml
let braces p =
  Parseff.between
    (fun () -> Parseff.char '{')
    (fun () -> Parseff.char '}')
    p

let brackets p =
  Parseff.between
    (fun () -> Parseff.char '[')
    (fun () -> Parseff.char ']')
    p
```

### `end_by`

`Parseff.end_by` parses zero or more elements, each followed by a separator. Unlike `Parseff.sep_by`, the separator comes **after** each element (including the last). Pass `~at_least` to require a minimum number of elements.

```ocaml
val end_by : ?at_least:int -> (unit -> 'a) -> (unit -> 'b) -> unit -> 'a list
```
```ocaml
(* Parse semicolon-terminated statements *)
let statements () =
  Parseff.end_by
    (fun () ->
      Parseff.take_while ~at_least:1
        (fun c -> c <> ';' && c <> '\n')
        ~label:"statement")
    (fun () -> Parseff.char ';')
    ()
(* "a;b;c;" -> ["a"; "b"; "c"] *)
(* ""       -> []               *)
```

Pass `~at_least:1` to `end_by` to require at least one element.

## Operator chains

These combinators parse sequences of values joined by operators, handling associativity. They're the standard tool for expression parsing with operator precedence.


### `chainl`

`Parseff.chainl` parses one or more values separated by an operator, combining them **left-associatively**. The operator parser returns a function that combines two values. Pass `~default` to return a fallback value when zero elements match.

```ocaml
val chainl : (unit -> 'a) -> (unit -> 'a -> 'a -> 'a) -> ?default:'a -> unit -> 'a
```
```ocaml
(* Parse "1-2-3" as ((1-2)-3) = -4 *)
let subtraction () =
  Parseff.chainl
    (fun () -> Parseff.digit ())
    (fun () ->
      let _ = Parseff.char '-' in
      fun a b -> a - b)
    ()
(* "1-2-3" -> -4  (left-associative: (1-2)-3) *)
```

### `chainr`

`Parseff.chainr` is like `Parseff.chainl` but combines **right-associatively**. Pass `~default` to return a fallback value when zero elements match.

```ocaml
val chainr : (unit -> 'a) -> (unit -> 'a -> 'a -> 'a) -> ?default:'a -> unit -> 'a
```
```ocaml
(* Parse "2^3^2" as 2^(3^2) = 512 *)
let power () =
  Parseff.chainr
    (fun () -> Parseff.digit ())
    (fun () ->
      let _ = Parseff.char '^' in
      fun a b -> int_of_float (float_of_int a ** float_of_int b))
    ()
(* "2^3^2" -> 512  (right-associative: 2^(3^2)) *)
```

#### Using `~default`

Pass `~default` to `chainl` or `chainr` to return a fallback value when zero elements match.

```ocaml
let maybe_subtract () =
  Parseff.chainl
    (fun () -> Parseff.digit ())
    (fun () ->
      let _ = Parseff.char '-' in
      fun a b -> a - b)
    ~default:0
    ()
(* "1-2" -> -1 *)
(* ""    -> 0  *)
```

## Complete example: JSON array

```ocaml
let integer () =
  let sign = Parseff.optional (fun () -> Parseff.char '-') () in
  let digits =
    Parseff.take_while ~at_least:1 (fun c -> c >= '0' && c <= '9') ~label:"digit"
  in
  let n = int_of_string digits in
  match sign with Some _ -> -n | None -> n

let json_array () =
  let _ = Parseff.char '[' in
  Parseff.skip_whitespace ();
  let values =
    Parseff.sep_by
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
  | Ok nums ->
      Printf.printf "Sum: %d\n" (List.fold_left ( + ) 0 nums)
  | Error { pos; error = `Expected msg } ->
      Printf.printf "Error at %d: %s\n" pos msg
  | Error _ -> print_endline "Parse error"
```