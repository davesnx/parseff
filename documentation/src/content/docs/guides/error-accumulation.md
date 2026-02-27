---
title: Error Accumulation
description: Tracking multiple errors across a parse
---

By default, Parseff stops at the first error and reports it. Sometimes you want to collect all errors — for example, to show users every problem in their input at once instead of making them fix issues one at a time.

This is an advanced technique that requires some manual bookkeeping.

## The problem

A typical Parseff parser fails on the first error:

```ocaml
(* Parses "1.2.3.256" — stops at the first out-of-range octet *)
match Parseff.parse "1.2.3.256" ip_address with
| Error { pos; error = `Expected msg } ->
    Printf.printf "Error at %d: %s\n" pos msg
    (* Only reports the first error *)
```

If the input has multiple problems (`"999.2.3.256"`), the user only sees the first one.

## Tracking errors with mutable state

One approach: use a mutable error list alongside parsing. Instead of failing immediately on validation errors, record them and continue:

```ocaml
type error_entry = { pos: int; msg: string }

let errors : error_entry list ref = ref []

let record_error pos msg =
  errors := { pos; msg } :: !errors

let number_lenient () =
  let start = Parseff.position () in
  let digits = Parseff.many1 Parseff.digit () in
  let n = List.fold_left (fun acc d -> (acc * 10) + d) 0 digits in
  if n > 255 then (
    record_error start (Printf.sprintf "octet %d out of range (0-255)" n);
    n  (* return the value anyway — keep parsing *)
  ) else n
```

This parser records the error but doesn't stop. The caller can check the error list after parsing completes:

```ocaml
let ip_address_lenient () =
  errors := [];
  let a = number_lenient () in
  let _ = Parseff.char '.' in
  let b = number_lenient () in
  let _ = Parseff.char '.' in
  let c = number_lenient () in
  let _ = Parseff.char '.' in
  let d = number_lenient () in
  Parseff.end_of_input ();
  (a, b, c, d)

let () =
  match Parseff.parse "999.2.3.256" ip_address_lenient with
  | Ok (a, b, c, d) ->
      if !errors = [] then
        Printf.printf "Valid: %d.%d.%d.%d\n" a b c d
      else (
        Printf.printf "Parsed with errors:\n";
        List.iter (fun e ->
          Printf.printf "  at %d: %s\n" e.pos e.msg
        ) (List.rev !errors)
      )
  | Error { pos; error = `Expected msg } ->
      Printf.printf "Parse error at %d: %s\n" pos msg
```

Output:

```
Parsed with errors:
  at 0: octet 999 out of range (0-255)
  at 8: octet 256 out of range (0-255)
```

## Furthest error tracking

Another useful pattern: track which error occurred at the furthest position in the input. This is often the most informative error, since it represents the point where the parser made the most progress before failing:

```ocaml
type error_context = {
  mutable errors: (int * string) list;
  mutable furthest: int;
}

let error_ctx = { errors = []; furthest = 0 }

let track_error pos msg =
  error_ctx.errors <- (pos, msg) :: error_ctx.errors;
  if pos > error_ctx.furthest then error_ctx.furthest <- pos

let best_error () =
  List.find_opt
    (fun (pos, _) -> pos = error_ctx.furthest)
    error_ctx.errors
```

## Limitations

Error accumulation in Parseff is a userland pattern, not a built-in feature. This means:

- **Structural errors still stop parsing.** If `char '.'` fails because the dot is missing, parsing stops. You can only continue past validation errors (where parsing succeeded but the value is invalid).
- **Mutable state requires care.** Reset the error list before each parse. If you're parsing concurrently, use per-parse state instead of a global ref.
- **Backtracking complicates things.** If `or_` backtracks, errors recorded in the failed branch are still in the list. You may need to filter errors by position relative to the final parse result.
