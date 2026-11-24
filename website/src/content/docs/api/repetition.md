---
title: Repetition and separation
description: Looping constructs for parsing repeated and separated patterns
---

<!-- This file is generated from doc/repetition.mld. Do not edit directly. -->

# Repetition and separation

Combinators for repeated items, separated lists, delimited blocks, and operator-precedence expressions. To pass each element to a callback instead of building a list, see [`Iter`](./streaming.md#iter).

## Basic repetition

### `many`

`Parseff.many` runs a parser repeatedly until it fails or an iteration consumes no input (dropping that zero-width result), returning the matched list.

```ocaml
val many : ?at_least:int -> ?at_most:int -> (unit -> 'a) -> 'a list
```

- `~at_least` requires at least that many matches.
- `~at_most` caps the count (unstable: may change in any minor release), failing with `Limit_exceeded { limit = at_most }` if another match consumes input.
```ocaml
let digits () = Parseff.many Parseff.digit
let digits1 () = Parseff.many ~at_least:1 Parseff.digit
```

### `count`

`Parseff.count` runs a parser exactly `n` times, failing if any repetition fails. It raises `Invalid_argument` if `n < 0`, so check a count decoded from the input first.

```ocaml
val count : int -> (unit -> 'a) -> 'a list
```

```ocaml
let hex_digit () =
  Parseff.satisfy ~label:"hex digit" (fun c ->
      (c >= '0' && c <= '9')
      || (c >= 'a' && c <= 'f')
      || (c >= 'A' && c <= 'F'))

let hex_color () =
  let _ = Parseff.char '#' in
  let r = Parseff.count 2 hex_digit in
  let g = Parseff.count 2 hex_digit in
  let b = Parseff.count 2 hex_digit in
  (r, g, b)
```

### Errors at the end of a loop

The failure that stops a repetition loop is still recorded for furthest-failure reporting. On `"12x"`, `many digit` stops at `'x'` and a following `end_of_input` fails at the same offset, producing `expected digit or end of input`. If a loop stops partway through an element (such as `"ab,ab,ax"` for `sep_by (fun () -> literal "ab") (fun () -> char ',')`), the deeper failure (`expected "b"` at offset 7\) wins over `end_of_input` at offset 5\.

## Separated and delimited lists

### `sep_by` and `end_by`

`Parseff.sep_by` parses zero or more elements separated by a separator (discarding separator results). `Parseff.end_by` expects the separator *after* every element, including the last. Pass `~at_least:1` to require at least one element; both stop on zero-width steps. An element that matches empty input also matches after the last separator, so rows separated by line ends give an extra empty row after a final line end, unless the row fails at end of input with `Parseff.not_followed_by` `Parseff.end_of_input`.

```ocaml
val sep_by : ?at_least:int -> (unit -> 'a) -> (unit -> 'b) -> 'a list
val end_by : ?at_least:int -> (unit -> 'a) -> (unit -> 'b) -> 'a list
```

```ocaml
let csv_line () =
  Parseff.sep_by ~at_least:1
    (fun () ->
      Parseff.take_while ~at_least:1 ~label:"value" (fun c ->
          c <> ',' && c <> '\n'))
    (fun () -> Parseff.char ',')

let statements () =
  Parseff.end_by
    (fun () ->
      Parseff.take_while ~at_least:1 ~label:"statement" (fun c ->
          c <> ';' && c <> '\n'))
    (fun () -> Parseff.char ';')
```

### `between`

`Parseff.between` parses an opening delimiter, a body, and a closing delimiter, returning the body's result.

```ocaml
val between : (unit -> 'a) -> (unit -> 'b) -> (unit -> 'c) -> 'c
```

```ocaml
let parens p =
  Parseff.between (fun () -> Parseff.char '(') (fun () -> Parseff.char ')') p

let brackets p =
  Parseff.between (fun () -> Parseff.char '[') (fun () -> Parseff.char ']') p
```

## Operator chains and expressions

### `fold_left` and `fold_right`

`Parseff.fold_left` and `Parseff.fold_right` parse one or more elements separated by an operator parser returning a combine function, folding left- or right-associatively. Pass `~otherwise` to return a default when the first element fails (later failures after `commit`/`cut` still fail). Both stop if an operator and element together consume no input.

```ocaml
val fold_left : ?otherwise:'a -> (unit -> 'a) -> (unit -> 'a -> 'a -> 'a) -> 'a
val fold_right : ?otherwise:'a -> (unit -> 'a) -> (unit -> 'a -> 'a -> 'a) -> 'a
```

```ocaml
let subtraction () =
  Parseff.fold_left ~otherwise:0
    (fun () -> Char.code (Parseff.digit ()) - Char.code '0')
    (fun () ->
      let _ = Parseff.char '-' in
      ( - ))

let power () =
  Parseff.fold_right
    (fun () -> Char.code (Parseff.digit ()) - Char.code '0')
    (fun () ->
      let _ = Parseff.char '^' in
      fun a b -> int_of_float (float_of_int a ** float_of_int b))
```

### `expression` and `precedence`

`Parseff.expression` parses expressions from a precedence table ordered from lowest to highest precedence, supporting `Parseff.infix_left`, `Parseff.infix_right`, `Parseff.prefix`, and `Parseff.postfix`. Use `expression` when operators are parsed by token parsers that may share prefixes, play multiple roles, or should appear in `Expected` errors.

`Parseff.precedence` uses a single `~operator` lookup function per operand returning `Some (prec, op)` or `None`. It supports infix and postfix operators (parse prefix operators in `operand` with `precedence ~min` inside `Parseff.rec_`) and is faster for grammars with many precedence levels that can dispatch from `Parseff.peek_char` or a lexer token.

```ocaml
val expression :
  operand:(unit -> 'a) ->
  ('a, [< `Infix | `Prefix | `Postfix ]) operator list list ->
  'a

val precedence :
  ?min:int ->
  operator:(unit -> (int * ('a, [< `Infix | `Postfix ]) operator) option) ->
  (unit -> 'a) ->
  'a
```

See the [Expressions with precedence](../guides/expression-parser.md) guide for complete examples.

## Complete example: JSON integer array

```ocaml
let json_array () =
  let _ = Parseff.char '[' in
  Parseff.skip_whitespace ();
  let values =
    Parseff.sep_by
      (fun () ->
        Parseff.skip_whitespace ();
        let n = Parseff.integer () in
        Parseff.skip_whitespace ();
        n)
      (fun () -> Parseff.char ',')
  in
  Parseff.skip_whitespace ();
  let _ = Parseff.char ']' in
  values

let () =
  match Parseff.parse "[1, -2, 3]" json_array with
  | Ok nums -> Printf.printf "Sum: %d\n" (List.fold_left ( + ) 0 nums)
  | Error e -> prerr_endline (Parseff.render_error ~name:"array" e)
```
