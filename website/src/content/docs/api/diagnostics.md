---
title: Diagnostics
description: Warnings, diagnostic collectors, and backtracking Ref state
---

<!-- This file is generated from doc/diagnostics.mld. Do not edit directly. -->


# Diagnostics

Diagnostics let a parser record warnings and recovered errors without stopping at the first problem.

To recover from syntax errors and continue parsing, see the [Error recovery](../guides/error-recovery.md) guide.


## Types

```ocaml
type severity = [ `Error | `Warning ]

module Diagnostics : sig
  type entry = { pos : int; value : error }
  type t
  val create : unit -> t
  val to_list : t -> entry list
  val length : t -> int
  val is_empty : t -> bool
  val clear : t -> unit
end
```
A `Parseff.Diagnostics.t` collects the diagnostics of one parse. Each entry carries a `Parseff.error` and its start offset (`pos = value.range.start`):

- `` `Warning ``: emitted by `Parseff.warn`.
- `` `Error ``: recorded by `Parseff.recover`, `Parseff.require`, or `Parseff.list` before continuing.

## Emitting and collecting diagnostics

```ocaml
val warn :
  ?from:int -> ?upto:int -> ?notes:note list -> ?help:string ->
  custom_error -> unit

val parse :
  ?max_depth:int ->
  ?diagnostics:Diagnostics.t ->
  ?trace:(trace_event -> unit) ->
  ?consume:[ `All | `Partial ] ->
  string ->
  (unit -> 'a) ->
  ('a, error) Stdlib.result
```
`Parseff.warn` records a `Parseff.custom_error` with severity `` `Warning `` and continues parsing. `~from`, `~upto`, `~notes`, and `~help` work as in `Parseff.fail`.

Pass a `Parseff.Diagnostics.t` collector to `Parseff.parse`, `Parseff.Source.parse`, or `Parseff.Push.parse` via `~diagnostics`. The runner clears the collector before running; entries are available after the parse whether it returns `Ok` or `Error`. Without `~diagnostics`, `warn` is a no-op and `recover`/`require`/`list` do not recover.


## Example

```ocaml
type Parseff.custom_error += Octet_out_of_range of int

let number_lenient () =
  let start = Parseff.position () in
  let digits = Parseff.many ~at_least:1 Parseff.digit in
  let value d = Char.code d - Char.code '0' in
  let n = List.fold_left (fun acc d -> (acc * 10) + value d) 0 digits in
  if n > 255 then Parseff.warn ~from:start (Octet_out_of_range n);
  n

let ip_address_lenient () =
  let a = number_lenient () in
  let _ = Parseff.char '.' in
  let b = number_lenient () in
  let _ = Parseff.char '.' in
  let c = number_lenient () in
  let _ = Parseff.char '.' in
  let d = number_lenient () in
  (a, b, c, d)

let custom = function
  | Octet_out_of_range n -> Printf.sprintf "octet %d out of range (0-255)" n
  | _ -> "unknown"

let report input =
  let diagnostics = Parseff.Diagnostics.create () in
  let outcome = Parseff.parse ~diagnostics input ip_address_lenient in
  (match outcome with
  | Ok (a, b, c, d) -> Printf.printf "Parsed: %d.%d.%d.%d\n" a b c d
  | Error error ->
      Printf.printf "Parse error at %d: %s\n" error.range.start
        (Parseff.error_message error));
  List.iter
    (fun ({ value; _ } : Parseff.Diagnostics.entry) ->
      print_endline
        (Parseff.render_error ~name:"ip" ~source:input ~custom value))
    (Parseff.Diagnostics.to_list diagnostics)
```
```ocaml
# report "999.2.3.256";;
Parsed: 999.2.3.256
ip:1:1: [warning] octet 999 out of range (0-255)
1 | 999.2.3.256
  | ^^^
ip:1:9: [warning] octet 256 out of range (0-255)
1 | 999.2.3.256
  |         ^^^
- : unit = ()
```
To inspect specific entries programmatically, match on `severity` and `kind`:

```ocaml
let octets_out_of_range diagnostics =
  List.filter_map
    (fun ({ value; _ } : Parseff.Diagnostics.entry) ->
      match value with
      | { severity = `Warning; kind = Custom (Octet_out_of_range n); _ } ->
          Some n
      | _ -> None)
    (Parseff.Diagnostics.to_list diagnostics)
```

## Backtracking semantics

Diagnostics are transactional with backtracking:

- `Parseff.or_` / `Parseff.one_of`: rolled back on failed branches.
- `Parseff.many` / `Parseff.sep_by`: rolled back on the final failing iteration.
- `Parseff.look_ahead` / `Parseff.peek_with`: always rolled back.
- `Parseff.capture` / `Parseff.recover`: rolled back when the inner parser fails (`recover` records the inner parser's error instead; pass `~keep` to retain earlier diagnostics, see the [Error recovery](../guides/error-recovery.md) guide).

## Backtracking state (`Ref`)

`Parseff.Ref.t` is a mutable cell whose writes roll back on backtracking under the same rules as diagnostics (for symbol tables, indentation stacks, or similar parser state). Store immutable values, as only the cell binding is restored.

```ocaml
module Ref : sig
  type 'a t
  val create : 'a -> 'a t
  val get : 'a t -> 'a
  val set : 'a t -> 'a -> unit
end
```
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

let () =
  plain := [];
  Parseff.Ref.set names [];
  match Parseff.parse "abc;" statement with
  | Ok `Declare ->
      assert (!plain = [ "abc"; "abc" ]);
      assert (Parseff.Ref.get names = [ "abc" ])
  | _ -> assert false
```