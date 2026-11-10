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
By default the parse requires that all input is consumed. It is the same as calling `Parseff.end_of_input` once more at the end of your parser. It does not change how explicit `Parseff.end_of_input` calls behave inside your parser. Pass `~consume:`Partial` to let the parse stop before the end of the input. `Parseff.parse_source` takes the same arguments.


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
- `Parseff.look_ahead`: diagnostics are rolled back
This prevents diagnostics from leaking out of speculative branches.


## Ref

A `Parseff.Ref.t` is a `ref` whose changes are undone when the parser backtracks.

```ocaml
module Ref : sig
  type 'a t
  val create : 'a -> 'a t
  val get : 'a t -> 'a
  val set : 'a t -> 'a -> unit
end
```
Use it for a value that the parser builds as it reads, such as a symbol table, an indentation stack or a counter. A plain `ref` is wrong for this. When a branch fails, the parser goes back and tries the next branch, but the `ref` keeps the changes of the failed branch.

In this example both branches of `statement` call `declare`. On `"abc;"`, the first branch reads `abc` and fails on `;`, then the second branch reads `abc` again. The `ref` holds the name twice. The `Parseff.Ref.t` holds it once, because the parser undid the `set` of the failed branch.

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

### When a set is undone

A `Parseff.Ref.set` is undone when its branch backtracks. The rule is the same as for diagnostics:

- the failed branch of `Parseff.or_` and `Parseff.one_of`;
- a failed `Parseff.optional`;
- the last iteration of `Parseff.many`, `Parseff.sep_by` and `Parseff.fold_left`, the one that fails or consumes nothing;
- `Parseff.look_ahead`, also when it succeeds.
A `set` is kept when its branch succeeds. After `Parseff.commit` or `Parseff.cut` seals a branch, a failure no longer undoes the `set`s in that branch. An outer branch that backtracks still undoes them.

An `Parseff.error` is a failure like a failed `Parseff.char`, so a branch that backtracks on it undoes its `set`s. A `Parseff.fail` or an exception of your own escapes `Parseff.or_`, `Parseff.optional`, `Parseff.many`, `Parseff.sep_by` and `Parseff.fold_left`, and skips the undo. The cell keeps the value it had when the exception was raised, also when a `try ... with` in the parser catches the exception. `Parseff.look_ahead` restores the value in every case. Diagnostics behave the same way.

Only the value in the cell is restored. A mutable value inside the cell, such as a `Hashtbl.t` or an array, keeps its changes. Store immutable values, such as a `Map` or a list.


### Lifetime

Create a cell anywhere, inside or outside the parser. A parse does not reset the cell. The value at the end of a parse stays in it, also when the parse fails, so set the start value before each parse. Outside a parse, `Parseff.Ref.get` and `Parseff.Ref.set` work as for a `ref`.


### Cost

A `set` inside a branch that can backtrack allocates 6 words to keep the old value. The old value stays in memory while an open branch can undo it. A `set` in each iteration of a `Parseff.many` at the top of the parser keeps one old value. A grammar inside one `Parseff.or_` keeps every old value until that `Parseff.or_` ends. A parser that never calls `set` allocates nothing more.


### Errors

`Parseff.Ref.set` raises `Invalid_argument` when another parse that is still running set the same cell. This happens when:

- two parses run interleaved on one domain, for example through effects in a `Parseff.Source.of_function` reader;
- you call `set` outside a parse while such a reader waits, on a cell that the waiting parse set;
- you call `Parseff.parse` inside a parser, and both parses set the cell.
Give each parse its own cell. A cell is not safe to share between domains.
