---
title: "Parsing an IP address"
description: Build a validated IPv4 address parser step by step
---

# Guide: Parsing an IP address

Build a validated IPv4 address parser step by step.

Source: [`test/test_ip.ml`](https://github.com/davesnx/parseff/blob/main/test/test_ip.ml) and [`examples/better_errors/main.ml`](https://github.com/davesnx/parseff/blob/main/examples/better_errors/main.ml).

An IPv4 address (`192.168.1.1`) consists of four decimal octets in `0`–`255` separated by `'.'`.

## The parser

```ocaml
let octet () =
  let digits = Parseff.many ~at_least:1 Parseff.digit in
  let value d = Char.code d - Char.code '0' in
  let n = List.fold_left (fun acc d -> (acc * 10) + value d) 0 digits in
  if n >= 0 && n <= 255 then n
  else Parseff.fail (Printf.sprintf "number out of range: %d" n)

let ip_address () =
  let a = octet () in
  let _ = Parseff.char '.' in
  let b = octet () in
  let _ = Parseff.char '.' in
  let c = octet () in
  let _ = Parseff.char '.' in
  let d = octet () in
  (a, b, c, d)
```

`octet` parses digits and validates the range afterward (unlike `Parseff.integer`, it rejects a leading `-`). `Parseff.parse` requires full consumption by default, so trailing input fails:

```ocaml
match Parseff.parse "192.168.1.1" ip_address with
| Ok (a, b, c, d) -> Printf.printf "Parsed: %d.%d.%d.%d\n" a b c d
| Error e -> prerr_endline (Parseff.render_error ~name:"address" e)
```

## Error behavior

- **Out of range** (`"1.2.3.256"`): fails with `"number out of range: 256"` at offset 9 (pass `~from` to `Parseff.fail` to underline the whole octet).
- **Incomplete** (`"1.2.3"`): fails expecting `'.'`, found end of input.
- **Trailing data** (`"1.2.3.4 extra"`): fails at offset 7 with `expected digit or end of input` because the final `many` stopped at the space and furthest-failure reporting merges both expectations at offset 7.

## Clearer errors with `expect`

Wrap primitives with `Parseff.expect` to replace raw token names with domain wording:

```ocaml
let digit_val () = Parseff.expect "a digit (0-9)" Parseff.digit
let dot () = Parseff.expect "a dot separator" (fun () -> Parseff.char '.')

let octet () =
  let start = Parseff.position () in
  let digits = Parseff.many ~at_least:1 digit_val in
  let value d = Char.code d - Char.code '0' in
  let n = List.fold_left (fun acc d -> (acc * 10) + value d) 0 digits in
  if n >= 0 && n <= 255 then n
  else Parseff.fail ~from:start (Printf.sprintf "octet %d out of range (0-255)" n)

let ip_address () =
  let a = octet () in
  let _ = dot () in
  let b = octet () in
  let _ = dot () in
  let c = octet () in
  let _ = dot () in
  let d = octet () in
  (a, b, c, d)
```

On `"192.168.1.x"`, this reports `expected a digit (0-9)`. On `"192.168.1x"`, it merges both possibilities at offset 9: `expected a digit (0-9) or a dot separator`.

## Typed errors

Extend `Parseff.custom_error` when callers need to handle validation failures programmatically:

```ocaml
type Parseff.custom_error += Out_of_range of int

let octet_typed () =
  let start = Parseff.position () in
  let digits = Parseff.many ~at_least:1 digit_val in
  let value d = Char.code d - Char.code '0' in
  let n = List.fold_left (fun acc d -> (acc * 10) + value d) 0 digits in
  if n >= 0 && n <= 255 then n
  else Parseff.error ~from:start (Out_of_range n)
```

```ocaml
match Parseff.parse "192.168.1.300" ip_address with
| Ok (a, b, c, d) -> Printf.printf "%d.%d.%d.%d\n" a b c d
| Error { kind = Custom (Out_of_range n); _ } ->
    Printf.printf "Octet %d is not in 0-255\n" n
| Error e -> prerr_endline (Parseff.render_error e)
```
