---
title: Performance
description: Parseff performance characteristics and benchmarks
---

## Benchmark Results

Benchmarked on a JSON array parser (`[1, 2, 3, ..., 10]`) over 100,000 iterations using the OCaml `benchmark` library. Source: [`bench/bench_vs_angstrom.ml`](https://github.com/davesnx/parseff/blob/main/bench/bench_vs_angstrom.ml).

| | Parses/sec | vs. Angstrom | Memory |
|---|---|---|---|
| Parseff (zero-copy spans) | ~4,940,000 | 4.3x faster | 197 MB |
| Parseff (fair comparison) | ~1,930,000 | 1.6x faster | — |
| Angstrom | ~1,150,000 | baseline | 584 MB |

### Methodology

- **Zero-copy spans**: Uses `sep_by_take_span` and a custom `float_of_span` that avoids `float_of_string` for 1-2 digit integers. This represents the fastest path when you control the conversion logic.
- **Fair comparison**: Uses the same `float_of_string` call as the Angstrom parser. This isolates the parsing overhead from number conversion.
- Both parsers parse the same input and produce the same output (`float list`).

### Optimization Journey

Starting from ~12,000 parses/sec (101x slower than Angstrom), systematic optimization achieved a 412x improvement. The major gains came from:

1. Replacing regex with `take_while` character scanning
2. Fused operations that reduce effect dispatches
3. Zero-copy span APIs that avoid `String.sub` allocations
4. Handler-level batch operations (`sep_by_take`, `skip_while_then_char`)

## Why Parseff is Fast

### Direct character scanning

`take_while` scans input in a tight `while` loop with character predicates — no regex compilation, no automaton overhead:

```ocaml
let digits = take_while (fun c -> c >= '0' && c <= '9')
```

### Fused operations

Combined operations reduce the number of effect dispatches per parse:

```ocaml
skip_while_then_char is_whitespace ','
(* One effect dispatch instead of two *)

sep_by_take is_whitespace ',' is_digit
(* Parses an entire separated list in a single effect *)
```

### Zero-copy spans

Span-based APIs return `{ buf; off; len }` slices of the input string without calling `String.sub`:

```ocaml
let span = take_while_span is_digit in
(* No allocation — span points into the original input *)
```

### Minimal abstraction overhead

Parsers are plain functions. No monadic wrapping, no functor application, no intermediate closures in the happy path.

## Performance Tips

### Pre-compile regexes

```ocaml
(* Compiles once at module init *)
let number_re = Re.compile (Re.Posix.re "[0-9]+")
let number () = match_re number_re
```

Never compile regexes inside parser functions — that adds 100x+ overhead per call.

### Prefer `take_while` over regex

For simple character classes, `take_while` is 5-10x faster than `match_re`:

```ocaml
let digits () = take_while1 (fun c -> c >= '0' && c <= '9') "digit"
```

### Use `skip_whitespace` when you don't need the string

```ocaml
skip_whitespace ();
parse_value ()
```

### Use fused operations in hot paths

```ocaml
let value = fused_sep_take is_whitespace ',' is_digit
```

### Minimize `or_` in loops

Each alternation sets up a handler for backtracking. In tight loops, prefer parsing then validating:

```ocaml
let keyword () =
  let w = take_while1 is_alphanum "keyword" in
  match w with
  | "class" | "function" | "const" | "let" -> w
  | _ -> fail "expected keyword"
```

## Trade-offs

### Streaming buffer growth

When using `parse_source`, the internal buffer grows monotonically — old data is never discarded, so memory usage equals total input size. This is fine for most inputs. For multi-gigabyte streams, a future `commit` mechanism may allow discarding consumed data.

### Backtracking cost

Each `or_` saves and restores the cursor position. Deep alternation trees can be expensive, especially inside `many` loops.

### OCaml 5+ required

Algebraic effects are not available before OCaml 5.0. Parseff requires OCaml >= 5.3.

---

See the [Optimization Guide](/parseff/guides/optimization) for a full checklist and advanced techniques.
