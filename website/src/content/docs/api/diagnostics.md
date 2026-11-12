---
title: Diagnostics
description: Non-fatal diagnostics and full-consumption runners
---

<!-- This file is generated from doc/diagnostics.mld. Do not edit directly. -->


# Diagnostics

Use diagnostics when you want to keep parsing and report multiple validation problems instead of stopping at the first one.

To collect parse errors and continue after them, see the [Error recovery](https://davesnx.github.io/parseff/guides/error-recovery/) guide.


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
val warn : ?pos:int -> 'd Diagnostics.t -> 'd -> unit
```
`Parseff.warn` records a non-fatal diagnostic at the current parser position, or at the byte offset `~pos` when it is given.


## Collecting diagnostics

Create a collector and pass it to the runner with `~diagnostics`. The runner empties it first. The parser must warn into the same collector. `warn` into a collector that the runner did not get does nothing.

The result of the parse is the usual `Parseff.error` result. Read the diagnostics from the collector after the parse. They are there for `Ok` and for `Error`.

```ocaml
val parse :
  ?max_depth:int ->
  ?consume:[ `All | `Partial ] ->
  ?diagnostics:'d Diagnostics.t ->
  string ->
  (unit -> 'a) ->
  ('a, error) Stdlib.result
```
```ocaml
Parseff.parse input parser
```
By default the parse requires that all input is consumed. It is the same as calling `Parseff.end_of_input` once more at the end of your parser. It does not change how explicit `Parseff.end_of_input` calls behave inside your parser. Pass `` ~consume:`Partial `` to let the parse stop before the end of the input. `Parseff.parse_source` takes the same arguments.


## Example

```ocaml
type validation = [ `Octet_out_of_range of int ]

let number_lenient diagnostics () =
  let start = Parseff.position () in
  let digits = Parseff.many ~at_least:1 Parseff.digit () in
  let n = List.fold_left (fun acc d -> (acc * 10) + d) 0 digits in
  if n > 255 then
    Parseff.warn ~pos:start diagnostics (`Octet_out_of_range n);
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
    Parseff.parse ~diagnostics "999.2.3.256"
      (ip_address_lenient diagnostics)
  in
  (match outcome with
  | Ok (a, b, c, d) -> Printf.printf "Parsed: %d.%d.%d.%d\n" a b c d
  | Error error ->
      Printf.printf "Parse error at %d: %s\n" error.range.start
        (Parseff.error_message error));
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
- `Parseff.look_ahead` and `Parseff.peek_with`: diagnostics are rolled back
This prevents diagnostics from leaking out of speculative branches.


## Ref

A `Parseff.Ref.t` is a `ref` whose changes are undone when the parser backtracks, in the same cases as diagnostics. Use it for state that the parser builds as it reads, such as a symbol table. Only the value in the cell is restored, so store immutable values.

```ocaml
module Ref : sig
  type 'a t
  val create : 'a -> 'a t
  val get : 'a t -> 'a
  val set : 'a t -> 'a -> unit
end
```
In this example both branches of `statement` call `declare`, and the first branch fails on `;`. The plain `ref` keeps the name of the failed branch. The `Parseff.Ref.t` does not.

```ocaml
let plain = ref []
let names = Parseff.Ref.create []

let declare () =
  let name =
    Parseff.take_while ~at_least:1 (fun c -> c >= 'a' && c <= 'z')
  in
  plain := name :: !plain;
  Parseff.Ref.set names (name :: Parseff.Ref.get names)

let statement () =
  Parseff.or_
    (fun () ->
      declare ();
      let _ = Parseff.char '=' in
      `Assign)
    (fun () ->
      declare ();
      let _ = Parseff.char ';' in
      `Declare)
    ()

let () =
  plain := [];
  Parseff.Ref.set names [];
  match Parseff.parse "abc;" statement with
  | Ok `Declare ->
      assert (!plain = [ "abc"; "abc" ]);
      assert (Parseff.Ref.get names = [ "abc" ])
  | _ -> assert false
```