---
title: Diagnostics
description: Non-fatal diagnostics and full-consumption runners
---

Use diagnostics when you want to keep parsing and report multiple validation
problems instead of stopping at the first one.

## Types

```ocaml
type 'd diagnostic = { pos : int; diagnostic : 'd }

type ('e, 'd) error_with_diagnostics = {
  pos : int;
  error : 'e;
  diagnostics : 'd diagnostic list;
}

type ('a, 'e, 'd) result_with_diagnostics =
  ('a * 'd diagnostic list, ('e, 'd) error_with_diagnostics) result
```

## Emitting diagnostics

### `warn`

Records a non-fatal diagnostic at the current parser position.

```ocaml
val warn : 'd -> unit
```

### `warn_at`

Records a non-fatal diagnostic at an explicit position.

```ocaml
val warn_at : pos:int -> 'd -> unit
```

## Runners

### `parse_until_end`

Runs a parser on a string, enforces full input consumption implicitly, and
returns both:

- `Ok (value, diagnostics)` on success
- `Error { pos; error; diagnostics }` on failure

Semantically, this is equivalent to running your parser and then calling
`end_of_input` once more at the end. It does not change how explicit
`Parseff.end_of_input ()` calls behave inside your parser.

```ocaml
val parse_until_end :
  ?max_depth:int ->
  string ->
  (unit -> 'a) ->
  ('a, [> `Expected of string | `Unexpected_end_of_input ], 'd)
  result_with_diagnostics
```

### `parse_source_until_end`

Streaming equivalent of `parse_until_end` for `Source.t` inputs.

In plain terms: `parse_source_until_end` always does one extra
`Parseff.end_of_input ()` check after your parser returns.

If your parser already calls `Parseff.end_of_input ()` itself, that call keeps
the same behavior as before. The runner just adds a final safety check.

```ocaml
val parse_source_until_end :
  ?max_depth:int ->
  Parseff.Source.t ->
  (unit -> 'a) ->
  ('a, [> `Expected of string | `Unexpected_end_of_input ], 'd)
  result_with_diagnostics
```


## Example

```ocaml
type validation = [ `Octet_out_of_range of int ]

let number_lenient () =
  let start = Parseff.position () in
  let digits = Parseff.many1 Parseff.digit () in
  let n = List.fold_left (fun acc d -> (acc * 10) + d) 0 digits in
  if n > 255 then Parseff.warn_at ~pos:start (`Octet_out_of_range n);
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

let () =
  let outcome = Parseff.parse_until_end "999.2.3.256" ip_address_lenient in
  match outcome with
  | Ok ((a, b, c, d), diagnostics) ->
      Printf.printf "Parsed: %d.%d.%d.%d\n" a b c d;
      List.iter
        (fun ({ pos; diagnostic } : validation Parseff.diagnostic) ->
          match diagnostic with
          | `Octet_out_of_range n ->
              Printf.printf "  at %d: octet %d out of range (0-255)\n" pos n)
        diagnostics
  | Error { pos; error = `Expected msg; diagnostics } ->
      Printf.printf "Parse error at %d: %s\n" pos msg;
      List.iter
        (fun ({ pos; diagnostic } : validation Parseff.diagnostic) ->
          match diagnostic with
          | `Octet_out_of_range n ->
              Printf.printf "  noted at %d: octet %d out of range\n" pos n)
        diagnostics
  | Error _ ->
      Printf.printf "Parse failed\n"
```

## Backtracking semantics

Diagnostics are transactional with parser control flow:

- `or_`: diagnostics from failed branches are rolled back
- `many`: diagnostics from the final failing attempt are rolled back
- `look_ahead`: diagnostics are rolled back

This prevents diagnostics from leaking out of speculative branches.
