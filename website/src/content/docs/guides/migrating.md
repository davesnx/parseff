---
title: Migrating
description: Update code written for an older Parseff release
---

# Migrating

How to update code written for an older Parseff release. Each section covers one release step; apply them in order when you skip a release.

## From 0\.5 to 0\.6

| 0\.5 | 0\.6 |
| --- | --- |
| `parse_source` | `Parseff.Source.parse` |
| `many p ()`, `or_ a b ()` and the other combinators that take parsers | `many p`, `or_ a b`; where a parser is expected, `fun () -> many p` |
| `let item = optional p` | `let item () = optional p` |
| `fail msg`, final for `or_`, `many` and `optional` | `cut (); fail msg` keeps it final; `fail msg` alone lets them try the next option |
| `warn d x` with a value `x` of your type | Declare `type Parseff.custom_error += My_warning of ...` and call `warn (My_warning x)`; it records into the collector given to the runner with `~diagnostics` |
| `warn_at d ~pos x` | `warn ~from:pos (My_warning x)`: the range goes from `pos` to the current position; add `~upto:pos` for an empty range at `pos` |
| `'d Diagnostics.t`, `'d Diagnostics.entry` | `Diagnostics.t`, `Diagnostics.entry` |
| `entry.value` as your value | `entry.value` is an `error`: match `` { severity = `Warning; kind = Custom (My_warning x); _ } `` |
| `fold_left element op ~otherwise:default ()` | `fold_left ~otherwise:default element op`; `default` applies only when the first `element` fails. To fall back on any failure, use `or_ (fun () -> fold_left element op) (fun () -> default)`. The same holds for `fold_right` |
| `span`, `span_to_string s` | `Slice.t`, `Slice.to_string s` |
| `s.len` | `Slice.length s` |
| `s.buf.[s.off + i]` | `Slice.get s i` |
| `take_while_span`, `Utf8.take_while_span` | `Slice.take_while`, `Utf8.Slice.take_while` |
| `digit ()` as an `int` | `Char.code (digit ()) - Char.code '0'` |
| `satisfy is_vowel ~label:"vowel"` | `satisfy ~label:"vowel" is_vowel`; the same for `Utf8.satisfy` |
| `Error { kind = Failure msg; _ }` | `Error { kind = Failed msg; _ }` |
| An exhaustive match on `error_kind` | Add a case for `Limit_exceeded { limit }` |
| `({ offset = 0; line = 1; col = 1 } : location)` | No replacement: take the `location` of an error |
| `{ line; col; offset }` on a `location` | `{ line; byte_col; _ }` |
| `e1 = e2` on two errors | `e1.range = e2.range && error_message e1 = error_message e2` |
| `skip_while_then_char p c` | `skip_while p; let _ = char c in ...` |
| `Utf8.skip_while_then_char p u` | `Utf8.skip_while p; let _ = Utf8.char u in ...` |
| `fused_sep_take ws c q` | `skip_while ws; let _ = char c in skip_while ws; take_while ~at_least:1 ~label:"value" q` |
| `sep_by_take ws c q` | `sep_by_take ~ws ~sep:c q` |
| `sep_by_take_span ws c q` | `Slice.sep_by_take ~ws ~sep:c q` |
| `match_regex (Re.compile r)` | `take_while p` for a character class, `recognize` with combinators for a longer pattern, or `Re` on the text of `take_while p` or `recognize p` |
| `consume s` | `literal s` |
| `let x = consume s in` | `literal s; let x = s in` |
Code that compiles unchanged can still behave differently:

- Invalid arguments raise `Invalid_argument` instead of being clamped: a negative `~at_least` or `~at_most`, an `~at_most` below `~at_least`, a `~max_depth` below 1, a negative `~from`, `~upto` or note range, a negative `count`, and a position outside the input in `location_of_position`.
- Going past `~at_most` fails with `Limit_exceeded`, and the failure is fatal like `cut (); fail`: an enclosing `or_`, `optional` or repetition does not try another option.
- A second `Parseff.Source.parse` on the same source starts after the input the first one consumed, instead of losing the bytes it read ahead. `Parseff.Source.offset` gives that position.
- `Parseff.Diagnostics.to_list` returns entries sorted by position.
- `skip_nested` closes a pair whose opening and closing bytes are the same, such as `('"', '"')`, when that byte matches the innermost open bracket.

## From 0\.4 to 0\.5

The common case is one pattern. Replace the match on the built-in variants by a match on the error value:

```ocaml
(* 0.4 *)
match Parseff.parse input parser with
| Ok v -> v
| Error { pos; error = `Expected msg } ->
    Printf.eprintf "%d: %s\n" pos msg;
    exit 1
| Error { pos; error = `Unexpected_end_of_input } ->
    Printf.eprintf "%d: end of input\n" pos;
    exit 1
| Error _ -> exit 1

(* 0.5 *)
match Parseff.parse input parser with
| Ok v -> v
| Error e ->
    prerr_endline (Parseff.render_error ~name:"input" e);
    exit 1
```

| 0\.4 | 0\.5 |
| --- | --- |
| `parse_until_end s p` | `parse s p` |
| `parse_source_until_end src p` | `parse_source src p` |
| `parse s p` | `` parse ~consume:`Partial s p `` |
| `parse_source src p` | `` parse_source ~consume:`Partial src p `` |
| `Ok (v, ds)` and `Error { error; diagnostics }` | `Ok v` and `Error e`; read the diagnostics from the collector |
| `warn x` | `let d = Diagnostics.create ()`, `parse ~diagnostics:d`, `warn d x` |
| `warn_at ~pos x` | `warn_at d ~pos x` |
| `'d Parseff.diagnostic = { pos; diagnostic }` | `'d Parseff.Diagnostics.entry = { pos; value }` |
| `` error (`Too_large n) `` | `type Parseff.custom_error += Too_large of int`, then `error (Too_large n)` |
| `` Error { pos; error = `Too_large n } `` | `Error { kind = Custom (Too_large n); _ }`, with a final `_` case |
| `('a, 'e) Parseff.result` | `('a, Parseff.error) result` |
- `Error { pos; error }` becomes `Error e`. The position is `e.range.start` and the message is `Parseff.error_message e`.
- `` `Failure msg `` becomes `Failure msg`. Invalid UTF-8, a failed `match_regex` without a label and a mismatched `BE.int16` are `Expected` items (`valid UTF-8`, `text matching the pattern`, `int16be 0xFFFE`), so they merge with the alternatives expected at the same position.
- `` `Expected msg `` becomes `Expected { items; found; more }`. `Parseff.error_message e` gives `expected A, B, or C, found X` instead of `expected A or expected B`, sorted and without duplicates.
- `` `Unexpected_end_of_input `` becomes `Expected { found = End; _ }` and keeps what was expected.
- `` `Depth_limit_exceeded msg `` becomes `Depth_limit_exceeded { limit }`, and `` `Backtrack_window_exceeded info `` becomes `Backtrack_window_exceeded { limit; oldest }`.
- `Parseff.Ok` and `Parseff.Error` are `Stdlib.Ok` and `Stdlib.Error`. Code that writes `Ok v` and `Error e` keeps compiling.
- Tests that compare error strings change: messages name what was expected at the furthest failure, and `consume` fails at the first differing byte.
