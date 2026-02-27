---
title: "Zero-Copy Performance"
description: How fused operations and spans make Parseff 4x faster than Angstrom
---

This walkthrough dissects the benchmark code that demonstrates Parseff's performance advantage. We'll look at the same JSON array parser implemented three ways — Parseff with zero-copy spans, Parseff with fair string conversion, and Angstrom — and explain what makes each one faster or slower.

Source: [`bench/bench_vs_angstrom.ml`](https://github.com/davesnx/parseff/blob/main/bench/bench_vs_angstrom.ml)

## The benchmark

All three parsers parse the same input:

```
[1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
```

The goal: parse this into a `float list`. Simple enough that the parsing overhead dominates — you're measuring the library, not the complexity of the grammar.

**Results** (100,000 iterations, 3 repeats):

| Parser | Parses/sec | vs. Angstrom | Memory |
|--------|-----------|-------------|--------|
| Parseff (zero-copy spans) | ~4,940,000 | 4.3x faster | 197 MB |
| Parseff (fair comparison) | ~1,930,000 | 1.6x faster | — |
| Angstrom | ~1,150,000 | baseline | 584 MB |

The "fair comparison" uses the same `float_of_string` conversion as Angstrom. The zero-copy version avoids that call entirely for small numbers.

## Setup

```ocaml
let json_input = {|[1, 2, 3, 4, 5, 6, 7, 8, 9, 10]|}
let is_digit_or_sign c = (c >= '0' && c <= '9') || c = '-' || c = '.'
let is_ws c = c = ' ' || c = '\t' || c = '\n' || c = '\r'
```

Character predicates are defined at module level as plain functions. These get inlined by the OCaml compiler — no closure allocation per call.

## The Parseff zero-copy parser

```ocaml
let json_array () =
  Parseff.skip_while_then_char is_ws '[';
  Parseff.skip_while is_ws;
  let spans = Parseff.sep_by_take_span is_ws ',' is_digit_or_sign in
  let elements = List.map float_of_span spans in
  Parseff.skip_while_then_char is_ws ']';
  elements
```

Four lines of actual parsing, and every one is chosen for performance. Let's go through them.

### `skip_while_then_char`

```ocaml
Parseff.skip_while_then_char is_ws '[';
```

This fuses two operations into one: skip whitespace, then match `[`. A naive version would be:

```ocaml
Parseff.skip_while is_ws;
let _ = Parseff.char '[' in
()
```

The fused version dispatches a single effect instead of two. In a tight loop parsing thousands of arrays, that difference compounds.

### `sep_by_take_span`

```ocaml
let spans = Parseff.sep_by_take_span is_ws ',' is_digit_or_sign in
```

This is the heavy hitter. A single call that:

1. Takes characters where `is_digit_or_sign` is true (the first number)
2. Skips whitespace where `is_ws` is true
3. Matches the separator `,`
4. Skips whitespace again
5. Repeats from step 1 until no more separators

It returns a `span list` — zero-copy slices into the original input buffer. No `String.sub` calls, no intermediate string allocations.

Compare to the non-fused equivalent:

```ocaml
(* This does the same thing, but slower *)
Parseff.sep_by
  (fun () ->
    Parseff.skip_whitespace ();
    let s = Parseff.take_while1 is_digit_or_sign ~label:"number" in
    Parseff.skip_whitespace ();
    s)
  (fun () -> Parseff.char ',')
  ()
```

The non-fused version dispatches multiple effects per element (skip, take, skip, char). The fused version does everything in one pass through the handler.

### `float_of_span` — the fast path

```ocaml
let[@inline always] float_of_span (s : Parseff.span) =
  if s.len = 1 then
    Float.of_int (Char.code (String.unsafe_get s.buf s.off) - Char.code '0')
  else if s.len = 2 && String.unsafe_get s.buf s.off >= '1' then
    Float.of_int
      (((Char.code (String.unsafe_get s.buf s.off) - Char.code '0') * 10)
      + (Char.code (String.unsafe_get s.buf (s.off + 1)) - Char.code '0'))
  else float_of_string (Parseff.span_to_string s)
```

For 1-digit and 2-digit numbers (which cover most of our benchmark input), we compute the float directly from the character codes — no string allocation, no `float_of_string` parsing. For anything larger, we fall back to `span_to_string` + `float_of_string`.

This is the main difference between the zero-copy and fair comparison versions:

```ocaml
(* Fair version — always calls float_of_string *)
let[@inline] float_of_span_fair (s : Parseff.span) =
  float_of_string (Parseff.span_to_string s)
```

The fair version is 1.6x faster than Angstrom. The zero-copy fast path pushes it to 4.3x.

## The Angstrom equivalent

For comparison, here's the same parser in Angstrom:

```ocaml
open Angstrom

let ws = skip_while (function ' ' | '\t' | '\n' | '\r' -> true | _ -> false)

let number =
  take_while1 (function '0' .. '9' | '-' | '.' -> true | _ -> false)
  >>| float_of_string

let json_array =
  char '[' *> ws *> sep_by (ws *> char ',' *> ws) number <* ws <* char ']'
```

This is idiomatic Angstrom. It's concise, but every operator (`*>`, `<*`, `>>=`, `>>|`) allocates closures and threads them through a CPS-based execution model.

Key differences:

| | Parseff (zero-copy) | Angstrom |
|---|---|---|
| Effect dispatches | 3 (skip+char, sep_by_take_span, skip+char) | ~30+ (one per operator) |
| String allocations per element | 0 (spans) | 1 (`take_while1`) |
| Number conversion | Direct char-code math | `float_of_string` |
| Execution model | Direct function calls | CPS with closures |

## What the spans look like

A `Parseff.span` is:

```ocaml
type span = { buf : string; off : int; len : int }
```

It points into the original input string. No copying. For the input `[1, 2, 3, 4, 5, 6, 7, 8, 9, 10]`:

| Span | `off` | `len` | Content |
|------|-------|-------|---------|
| 1st | 1 | 1 | `1` |
| 2nd | 4 | 1 | `2` |
| 3rd | 7 | 1 | `3` |
| ... | ... | ... | ... |
| 10th | 28 | 2 | `10` |

Each span is three integers — 24 bytes on a 64-bit system. Compare that to allocating a new string for each number (header + length + characters + alignment padding).

Call `Parseff.span_to_string span` when you actually need a string. Until then, the span is just a view.

## When to use zero-copy

Zero-copy spans make a significant difference when:

- You're parsing many small values (like elements in a list)
- You can process the span directly without converting to a string
- The parser is in a hot path that runs thousands or millions of times

They don't help much when:

- You always need the string anyway (you'll call `span_to_string` immediately)
- The parsed values are large (the allocation savings are proportionally smaller)
- The parser runs rarely (optimization overhead isn't justified)

## The optimization layers

The 412x improvement from Parseff's initial version (12K parses/sec) to the current one (4.94M parses/sec) came from stacking these techniques:

| Technique | Impact | What it avoids |
|-----------|--------|---------------|
| `take_while` instead of regex | 5-10x | Regex compilation and automaton overhead |
| Fused operations (`skip_while_then_char`, `sep_by_take_span`) | 2-4x | Multiple effect dispatches per operation |
| Zero-copy spans | 2-3x | `String.sub` allocations |
| Fast-path number conversion | ~2x | `float_of_string` overhead for small numbers |

These are multiplicative. Each layer removes overhead that the previous one couldn't.

## What we covered

| Concept | API | Purpose |
|---------|-----|---------|
| Fused skip + char | `skip_while_then_char` | One effect dispatch instead of two |
| Fused separated list | `sep_by_take_span` | Parse entire list in one operation |
| Zero-copy slices | `span` type | Avoid `String.sub` allocations |
| Span materialization | `span_to_string` | Convert to string only when needed |
| Inline annotation | `[@inline always]` | Eliminate function call overhead |


