---
title: "Parsing an IP Address"
description: Build a validated IPv4 address parser step by step
---

This walkthrough builds an IPv4 address parser from scratch. It covers digit matching, multi-character numbers, range validation, error reporting, and ensuring complete input consumption.

Source: [`test/test_ip.ml`](https://github.com/davesnx/parseff/blob/main/test/test_ip.ml) and [`examples/better_errors.ml`](https://github.com/davesnx/parseff/blob/main/examples/better_errors.ml)

## What we're building

An IPv4 address looks like `192.168.1.1`: four numbers from 0 to 255, separated by dots. Simple enough to parse by hand, but it's a perfect first example because it combines several core concepts:

- Parsing individual digits and assembling multi-digit numbers
- Matching literal separators
- Validating parsed values against domain rules
- Reporting errors at the right position

## The complete parser

Here's the full code. We'll break it down section by section below.

```ocaml
let number () =
  let digits = Parseff.many1 Parseff.digit () in
  let n = List.fold_left (fun acc d -> (acc * 10) + d) 0 digits in
  if n >= 0 && n <= 255 then n
  else Parseff.fail (Printf.sprintf "number out of range: %d" n)

let ip_address () =
  let a = number () in
  let _ = Parseff.char '.' in
  let b = number () in
  let _ = Parseff.char '.' in
  let c = number () in
  let _ = Parseff.char '.' in
  let d = number () in
  Parseff.end_of_input ();
  (a, b, c, d)
```

## Parsing a single number

```ocaml
let number () =
  let digits = Parseff.many1 Parseff.digit () in
  let n = List.fold_left (fun acc d -> (acc * 10) + d) 0 digits in
  if n >= 0 && n <= 255 then n
  else Parseff.fail (Printf.sprintf "number out of range: %d" n)
```

`Parseff.digit` parses a single decimal digit and returns its integer value (0-9). `many1` applies it one or more times, producing a list like `[1; 9; 2]`.

The `fold_left` converts the digit list to an integer: `1*100 + 9*10 + 2 = 192`.

Then we validate: if the number isn't in the 0-255 range, we call `Parseff.fail` with a descriptive message. This is the parsing-then-validation pattern: parse the structure first, then check the semantics.

## Assembling the address

```ocaml
let ip_address () =
  let a = number () in
  let _ = Parseff.char '.' in
  let b = number () in
  let _ = Parseff.char '.' in
  let c = number () in
  let _ = Parseff.char '.' in
  let d = number () in
  Parseff.end_of_input ();
  (a, b, c, d)
```

This reads exactly like the format it parses: number, dot, number, dot, number, dot, number. Each `let` binding advances the cursor through the input.

`char '.'` matches a single dot character. We bind it to `_` because we don't need the return value; we just need it to be there. Use `char` for single characters and `consume` for multi-character strings.

`end_of_input ()` at the end ensures there's no trailing data. Without it, `"1.2.3.4 extra stuff"` would parse successfully as `(1, 2, 3, 4)` and silently ignore the rest.

## Running it

```ocaml
match Parseff.parse "192.168.1.1" ip_address with
| Ok (a, b, c, d) ->
    Printf.printf "Parsed: %d.%d.%d.%d\n" a b c d
| Error { pos; error = `Expected msg } ->
    Printf.printf "Error at %d: %s\n" pos msg
| Error _ -> print_endline "Parse error"
```

On valid input, you get an `Ok` with the four octets as a tuple. On invalid input, the error tells you exactly where parsing failed.

## Error cases

Let's look at what happens with bad input:

**Out of range**: `"1.2.3.256"` parses `1`, `.`, `2`, `.`, `3`, `.`, then reads `256`. The `number` parser validates and fails with `"number out of range: 256"` at position 6 (the start of `256`).

**Incomplete**: `"1.2.3"` parses `1`, `.`, `2`, `.`, `3`, then tries to match `'.'` and fails because the input is exhausted.

**Trailing data**: `"1.2.3.4 extra"` parses all four octets, then `end_of_input` fails because there are characters remaining.

## Improving error messages with `expect`

The basic parser's error messages are functional but terse. `expect` lets you replace them with something more human-readable:

```ocaml
let digit_val () =
  Parseff.expect "a digit (0-9)" Parseff.digit

let number_0_255 () =
  let digits = Parseff.many1 digit_val () in
  let n = List.fold_left (fun acc d -> (acc * 10) + d) 0 digits in
  if n >= 0 && n <= 255 then n
  else Parseff.fail (Printf.sprintf "number %d is out of range (must be 0-255)" n)

let ip_address () =
  let a = number_0_255 () in
  let _ = Parseff.expect "a dot separator" (fun () -> Parseff.char '.') in
  let b = number_0_255 () in
  let _ = Parseff.expect "a dot separator" (fun () -> Parseff.char '.') in
  let c = number_0_255 () in
  let _ = Parseff.expect "a dot separator" (fun () -> Parseff.char '.') in
  let d = number_0_255 () in
  Parseff.end_of_input ();
  (a, b, c, d)
```

Now on `"192.168.1."`, instead of `expected digit`, the error says `expected a digit (0-9)`. On `"192.168.1"`, instead of `expected '.'`, it says `expected a dot separator`.

## Using typed errors

For programmatic error handling, polymorphic variants give you structured errors you can pattern match on:

```ocaml
let number_0_255_typed () =
  let digits = Parseff.many1 digit_val () in
  let n = List.fold_left (fun acc d -> (acc * 10) + d) 0 digits in
  if n >= 0 && n <= 255 then n
  else Parseff.error (`Out_of_range n)
```

Now callers can distinguish parsing failures from validation failures:

```ocaml
match Parseff.parse "192.168.1.300" ip_address with
| Ok (a, b, c, d) ->
    Printf.printf "%d.%d.%d.%d\n" a b c d
| Error { error = `Out_of_range n; _ } ->
    Printf.printf "Octet %d is not in 0-255\n" n
| Error { error = `Expected msg; _ } ->
    Printf.printf "Parse error: %s\n" msg
| Error _ -> print_endline "Unknown error"
```

`fail` is fine for errors shown directly to users. `error` is better when code needs to react differently to different failure modes.
