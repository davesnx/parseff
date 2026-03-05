---
title: Error handling
description: Building better error messages in Parseff
---

<!-- This file is generated from doc/errors.mld. Do not edit directly. -->

# Error Handling

Parseff exposes three main APIs for error handling:

- `Parseff.fail`: fail with a string message
- `Parseff.error`: fail with a typed custom value
- `Parseff.expect`: relabel failures from another parser

## `fail`

```ocaml
val fail : string -> 'a
```
Abort parsing with a message. The message is returned as `Error { error = `Expected msg; ... }`.

```ocaml
let byte () =
  let n =
    int_of_string
      (Parseff.take_while1
         (fun c -> c >= '0' && c <= '9')
         ~label:"digit")
  in
  if n >= 0 && n <= 255 then n
  else Parseff.fail "number must be between 0 and 255"

let () =
  match Parseff.parse "300" byte with
  | Error { pos; error = `Expected msg } ->
      (* pos = 0, msg = "number must be between 0 and 255" *)
      Printf.printf "Error at %d: %s\n" pos msg
  | Ok n -> Printf.printf "Got: %d\n" n
  | Error _ -> ()
```
Use this when a human-readable string is enough.


## `error`

```ocaml
val error : 'e -> 'a
```
Abort parsing with a typed custom error value.

```ocaml
let number () =
  let s =
    Parseff.take_while1 (fun c -> c >= '0' && c <= '9') ~label:"digit"
  in
  let n = int_of_string s in
  if n > 255 then Parseff.error (`Too_large n)
  else if n < 0 then Parseff.error (`Negative n)
  else n

let () =
  match Parseff.parse "300" number with
  | Error { error = `Too_large n; pos } ->
      Printf.printf "Number %d too large at position %d\n" n pos
  | Error { error = `Negative n; pos } ->
      Printf.printf "Negative number %d at position %d\n" n pos
  | Error { error = `Expected msg; _ } ->
      Printf.printf "Parse error: %s\n" msg
  | Error _ -> Printf.printf "Other error\n"
  | Ok n -> Printf.printf "Got %d\n" n
```
Use this when callers need to pattern match on specific failure cases.

Polymorphic variants work great with `error` for quick error types:

```ocaml
let number_quick () =
  let s =
    Parseff.take_while1 (fun c -> c >= '0' && c <= '9') ~label:"digit"
  in
  let n = int_of_string s in
  if n > 255 then Parseff.error `Too_large
  else if n < 0 then Parseff.error `Negative
  else n
```

## `expect`

```ocaml
val expect : string -> (unit -> 'a) -> 'a
```
Run a parser and replace its failure message with a clearer description.

```ocaml
let dot () =
  Parseff.expect "a dot separator" (fun () -> Parseff.char '.')

let number () =
  let s =
    Parseff.take_while1 (fun c -> c >= '0' && c <= '9') ~label:"digit"
  in
  int_of_string s

let ip_address () =
  let a = number () in
  let _ = dot () in
  let b = number () in
  let _ = dot () in
  let c = number () in
  let _ = dot () in
  let d = number () in
  Parseff.end_of_input ();
  (a, b, c, d)
```
Without `expect`, a failed `char '.'` reports `expected '.'`. With `expect`, it reports `expected a dot separator`.
