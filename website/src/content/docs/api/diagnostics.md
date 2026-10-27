---
title: Diagnostics
description: Non-fatal diagnostics and full-consumption runners
---

<!-- This file is generated from doc/diagnostics.mld. Do not edit directly. -->


# Diagnostics

Use diagnostics when you want to keep parsing and report multiple validation problems instead of stopping at the first one.


## Types

```ocaml
module Diagnostics : sig
  type 'd entry = { pos : int; value : 'd }
  type 'd t
  val create : unit -> 'd t
  val to_list : 'd t -> 'd entry list
  val length : 'd t -> int
  val is_empty : 'd t -> bool
  val clear : 'd t -> unit
end
```
A `Parseff.Diagnostics.t` collects the diagnostics of one parse. Its type parameter is the type of the values given to `Parseff.warn`, and `Parseff.Diagnostics.to_list` returns the same type. The two cannot differ.


## Emitting diagnostics

```ocaml
val warn : 'd Diagnostics.t -> 'd -> unit
```
`Parseff.warn` records a non-fatal diagnostic at the current parser position.

```ocaml
val warn_at : 'd Diagnostics.t -> pos:int -> 'd -> unit
```
`Parseff.warn_at` records a non-fatal diagnostic at an explicit position.


## Collecting diagnostics

Create a collector and pass it to the runner with `~diagnostics`. The runner empties it first. The parser must warn into the same collector. `warn` into a collector that the runner did not get does nothing.

The result of the parse is the usual `Parseff.Error.t` result. Read the diagnostics from the collector after the parse. They are there for `Ok` and for `Error`.

```ocaml
val parse :
  ?max_depth:int ->
  ?consume:[ `All | `Prefix ] ->
  ?diagnostics:'d Diagnostics.t ->
  string ->
  (unit -> 'a) ->
  ('a, Error.t) Stdlib.result
```
```ocaml
Parseff.parse ~consume:`All input parser
```
This requires that all input is consumed. It is the same as calling `Parseff.end_of_input` once more at the end of your parser. It does not change how explicit `Parseff.end_of_input` calls behave inside your parser. Without `~consume`, the parse may stop before the end of the input. `Parseff.parse_source` takes the same arguments.


## Example

```ocaml
type validation = [ `Octet_out_of_range of int ]

let number_lenient diagnostics () =
  let start = Parseff.position () in
  let digits = Parseff.many ~at_least:1 Parseff.digit () in
  let n = List.fold_left (fun acc d -> (acc * 10) + d) 0 digits in
  if n > 255 then
    Parseff.warn_at diagnostics ~pos:start (`Octet_out_of_range n);
  n

let ip_address_lenient diagnostics () =
  let a = number_lenient diagnostics () in
  let _ = Parseff.char '.' in
  let b = number_lenient diagnostics () in
  let _ = Parseff.char '.' in
  let c = number_lenient diagnostics () in
  let _ = Parseff.char '.' in
  let d = number_lenient diagnostics () in
  (a, b, c, d)

let () =
  let diagnostics : validation Parseff.Diagnostics.t =
    Parseff.Diagnostics.create ()
  in
  let outcome =
    Parseff.parse ~consume:`All ~diagnostics "999.2.3.256"
      (ip_address_lenient diagnostics)
  in
  (match outcome with
  | Ok (a, b, c, d) -> Printf.printf "Parsed: %d.%d.%d.%d\n" a b c d
  | Error error ->
      Printf.printf "Parse error at %d: %s\n" error.Parseff.Error.span.start
        (Parseff.Error.message error));
  List.iter
    (fun ({ pos; value } : validation Parseff.Diagnostics.entry) ->
      match value with
      | `Octet_out_of_range n ->
          Printf.printf "  at %d: octet %d out of range (0-255)\n" pos n)
    (Parseff.Diagnostics.to_list diagnostics)
```

## Backtracking semantics

Diagnostics are transactional with parser control flow:

- `Parseff.or_`: diagnostics from failed branches are rolled back
- `Parseff.many`: diagnostics from the final failing attempt are rolled back
- `Parseff.look_ahead`: diagnostics are rolled back
This prevents diagnostics from leaking out of speculative branches.
