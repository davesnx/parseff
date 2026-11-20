---
title: Making parsers fast
description: Techniques for writing high-performance parsers with Parseff
---

# Guide: Making parsers fast

Practical techniques for high-throughput, low-allocation parsers in Parseff.

## Skip instead of allocate

Use `Parseff.skip_whitespace` and `Parseff.skip_while` whenever you discard the matched bytes:

```ocaml
(* allocates and discards a string *)
let _ = Parseff.whitespace () in
parse_value ()

(* zero allocation *)
Parseff.skip_whitespace ();
parse_value ()
```

## Precompute byte sets with `Charset`

`Parseff.take_while`, `Parseff.skip_while`, `Parseff.Slice.take_while`, and `Parseff.take_while_iter` accept a precomputed `Parseff.Charset` via `~set`, replacing per-byte closure calls with a 256-byte table lookup. Build the set once outside the parser:

```ocaml
let is_ident c =
  (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9') || c = '_'

let ident_set = Parseff.Charset.of_predicate is_ident

let ident () = Parseff.take_while ~at_least:1 ~label:"identifier" ~set:ident_set is_ident
```

## Use zero-copy slices and fused scanners

`Parseff.Slice` borrows the input buffer without allocating substrings under `Parseff.parse`, and `Parseff.sep_by_take` / `Parseff.Slice.sep_by_take` scan an entire separated list in one loop instead of entering combinator frames per element:

```
Operation                        Replaces
sep_by_take ~ws ~sep pred        sep_by (take_while ~at_least:1 pred) (skip_while ws; char sep; skip_while ws)
Slice.sep_by_take ~ws ~sep pred  Same, returning Slice.t list instead of string list
```

Elements must be non-empty: on a trailing or doubled separator, `sep_by_take` stops before that separator and its leading whitespace.

```ocaml
let is_digit c = c >= '0' && c <= '9'

let int_of_slice slice =
  let rec loop i acc =
    if i >= Parseff.Slice.length slice then acc
    else
      let c = Parseff.Slice.get slice i in
      loop (i + 1) ((acc * 10) + (Char.code c - Char.code '0'))
  in
  loop 0 0

let parse_ints () =
  Parseff.Slice.sep_by_take ~ws:Parseff.is_whitespace ~sep:',' is_digit
  |> List.map int_of_slice
```

See the [Zero-copy API](https://davesnx.github.io/parseff/api/zero-copy/) reference for details.

## Dispatch with `peek_char` instead of `or_` in tight loops

Each `Parseff.or_` installs a backtracking frame. Inside hot loops, dispatch on `Parseff.peek_char` or read a token once and match on it:

```ocaml
let keyword () =
  let w =
    Parseff.take_while ~at_least:1 ~label:"keyword" (fun c ->
        c >= 'a' && c <= 'z')
  in
  match w with
  | "class" | "function" | "let" -> w
  | _ -> Parseff.fail "expected keyword"
```

Reserve `or_` and `one_of` for branches with genuinely distinct structure or outside inner loops. For expression grammars with many operator precedence levels, `Parseff.precedence` dispatches with a single lookup per operand instead of scanning a table.
