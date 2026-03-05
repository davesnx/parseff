---
title: Repetition and separation
description: Looping constructs for parsing repeated and separated patterns
---

<!-- This file is generated from doc/repetition.mld. Do not edit directly. -->

# Repetition and Separation

These combinators handle patterns that repeat: lists, separated values, delimited blocks, and operator chains.


## Basic Repetition


### `many`

```ocaml
val many : (unit -> 'a) -> unit -> 'a list
```
`Parseff.many` applies a parser zero or more times. Returns a list of results. Always succeeds (returns `[]` if the parser fails immediately).

```ocaml
let digits () = Parseff.many Parseff.digit ()
(* "123"  -> [1; 2; 3] *)
(* ""     -> []         *)
(* "abc"  -> []         *)
```

### `many1`

```ocaml
val many1 : (unit -> 'a) -> unit -> 'a list
```
`Parseff.many1` is like `Parseff.many` but requires at least one match. Fails if the parser doesn't succeed at least once.

```ocaml
let digits1 () = Parseff.many1 Parseff.digit ()
(* "123" -> [1; 2; 3] *)
(* ""    -> Error      *)
(* "abc" -> Error      *)
```

### `count`

```ocaml
val count : int -> (unit -> 'a) -> unit -> 'a list
```
`Parseff.count` applies a parser exactly `n` times. Fails if the parser doesn't match `n` times.

```ocaml
let three_digits () = Parseff.count 3 Parseff.digit ()
(* "123"  -> [1; 2; 3] *)
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

## Separated Lists


### `sep_by`

```ocaml
val sep_by : (unit -> 'a) -> (unit -> 'b) -> unit -> 'a list
```
`Parseff.sep_by` parses zero or more elements separated by a separator. The separator's return value is discarded. Always succeeds.

```ocaml
let csv_line () =
  Parseff.sep_by
    (fun () -> Parseff.take_while (fun c -> c <> ',' && c <> '\n'))
    (fun () -> Parseff.char ',')
    ()
(* "a,b,c" -> ["a"; "b"; "c"] *)
(* ""      -> [""]             *)
```

### `sep_by1`

```ocaml
val sep_by1 : (unit -> 'a) -> (unit -> 'b) -> unit -> 'a list
```
`Parseff.sep_by1` is like `Parseff.sep_by` but requires at least one element.

```ocaml
let csv_line1 () =
  Parseff.sep_by1
    (fun () ->
      Parseff.take_while1
        (fun c -> c <> ',' && c <> '\n')
        ~label:"value")
    (fun () -> Parseff.char ',')
    ()
(* "a,b,c" -> ["a"; "b"; "c"] *)
(* "a"     -> ["a"]           *)
(* ""      -> Error            *)
```

## Delimiters and Terminators


### `between`

```ocaml
val between : (unit -> 'a) -> (unit -> 'b) -> (unit -> 'c) -> unit -> 'c
```
`Parseff.between` parses an opening delimiter, then the body, then a closing delimiter. Returns the body's value.

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

```ocaml
val end_by : (unit -> 'a) -> (unit -> 'b) -> unit -> 'a list
```
`Parseff.end_by` parses zero or more elements, each followed by a separator. Unlike `Parseff.sep_by`, the separator comes **after** each element (including the last).

```ocaml
(* Parse semicolon-terminated statements *)
let statements () =
  Parseff.end_by
    (fun () ->
      Parseff.take_while1
        (fun c -> c <> ';' && c <> '\n')
        ~label:"statement")
    (fun () -> Parseff.char ';')
    ()
(* "a;b;c;" -> ["a"; "b"; "c"] *)
(* ""       -> []               *)
```

### `end_by1`

```ocaml
val end_by1 : (unit -> 'a) -> (unit -> 'b) -> unit -> 'a list
```
`Parseff.end_by1` is like `Parseff.end_by` but requires at least one element.


## Operator Chains

These combinators parse sequences of values joined by operators, handling associativity. They're the standard tool for expression parsing with operator precedence.


### `chainl1`

```ocaml
val chainl1 : (unit -> 'a) -> (unit -> 'a -> 'a -> 'a) -> unit -> 'a
```
`Parseff.chainl1` parses one or more values separated by an operator, combining them **left-associatively**. The operator parser returns a function that combines two values.

```ocaml
(* Parse "1-2-3" as ((1-2)-3) = -4 *)
let subtraction () =
  Parseff.chainl1
    (fun () -> Parseff.digit ())
    (fun () ->
      let _ = Parseff.char '-' in
      fun a b -> a - b)
    ()
(* "1-2-3" -> -4  (left-associative: (1-2)-3) *)
```

### `chainr1`

```ocaml
val chainr1 : (unit -> 'a) -> (unit -> 'a -> 'a -> 'a) -> unit -> 'a
```
`Parseff.chainr1` is like `Parseff.chainl1` but combines **right-associatively**.

```ocaml
(* Parse "2^3^2" as 2^(3^2) = 512 *)
let power () =
  Parseff.chainr1
    (fun () -> Parseff.digit ())
    (fun () ->
      let _ = Parseff.char '^' in
      fun a b -> int_of_float (float_of_int a ** float_of_int b))
    ()
(* "2^3^2" -> 512  (right-associative: 2^(3^2)) *)
```

### `chainl`

```ocaml
val chainl : (unit -> 'a) -> (unit -> 'a -> 'a -> 'a) -> 'a -> unit -> 'a
```
`Parseff.chainl` is like `Parseff.chainl1` but takes a default value. Returns the default if zero elements match.

```ocaml
let maybe_subtract () =
  Parseff.chainl
    (fun () -> Parseff.digit ())
    (fun () ->
      let _ = Parseff.char '-' in
      fun a b -> a - b)
    0
    ()
(* "1-2" -> -1 *)
(* ""    -> 0  *)
```

### `chainr`

```ocaml
val chainr : (unit -> 'a) -> (unit -> 'a -> 'a -> 'a) -> 'a -> unit -> 'a
```
`Parseff.chainr` is like `Parseff.chainr1` but with a default value for zero matches.


## Complete Example: JSON Array

```ocaml
let integer () =
  let sign = Parseff.optional (fun () -> Parseff.char '-') () in
  let digits =
    Parseff.take_while1 (fun c -> c >= '0' && c <= '9') ~label:"digit"
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