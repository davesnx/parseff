---
title: Comparison with Angstrom
description: How Parseff compares to Angstrom in performance, API style, and trade-offs
---

# Comparison with Angstrom

How Parseff compares to Angstrom in performance, API style, and trade-offs.

[Angstrom](https://github.com/inhabitedtype/angstrom) is the most widely used parser combinator library in the OCaml ecosystem. This page compares Parseff and Angstrom side by side: performance, API style, and when to use each.

## Performance

This page focuses on the cross-library parser workloads in `bench/`: JSON, CSV, and arithmetic. As a high-level snapshot, the strongest Parseff results in those workloads are:

```
Benchmark    Best Parseff result                     What it measures
JSON         ~2.30M/s optimized                      Full JSON array parser vs Angstrom
CSV          ~3.62M/s fused                          CSV field parser vs Angstrom and MParser
Arithmetic   ~9.67M/s fused                          Expression parser/evaluator vs Angstrom and MParser
```

Those rows are useful for a high-level snapshot, but they are not directly comparable across rows because the workloads differ.

### JSON benchmark

Benchmarked on a JSON array parser (`{[1, 2, 3, ..., 10]}`) over 1,000,000 iterations (3 runs). Source: [bench/bench_json.ml](https://github.com/davesnx/parseff/blob/main/bench/bench_json.ml).

```
                        Parses/sec     vs. Angstrom (generic)   Minor allocs
Parseff (optimized)     ~2,300,000     1.9x faster              1.3 GB
Parseff (generic)       ~1,560,000     1.3x faster              800 MB
Angstrom (optimized)    ~1,550,000     1.3x faster             11.1 GB
Angstrom (generic)      ~1,190,000     baseline                 5.9 GB
```

**Optimized** uses `sep_by_take_span` plus `List.map` over zero-copy spans. Scanning avoids per-element `String.sub` allocation during parsing, while value conversion still uses the stdlib `float_of_string` via `Parseff.span_to_string`.

**Generic** uses the ordinary `char`, `skip_while`, `take_while`, and `sep_by` combinators with the same `float_of_string` conversion as `Angstrom (generic)`, so it reflects the baseline direct-style API.

All parsers produce the same output (`float list`) from the same input and require full input consumption.

Minor allocation totals are the cumulative values reported by `Benchmark.latencyN` for the full 1,000,000-parse run.

Against each library's best JSON parser, Parseff's optimized path is still ~1.5x faster, while the generic Parseff parser is roughly on par with Angstrom's optimized parser and still ~1.3x faster than Angstrom's generic baseline.

### Other cross-library benchmarks

The other cross-library workloads in `bench/` show the same pattern:

```
                       Parseff best     Parseff generic   Angstrom best   MParser
CSV                    ~3,620,000       ~2,500,000       ~2,290,000      ~1,340,000
Arithmetic             ~9,670,000       ~2,080,000       ~4,490,000      ~1,170,000
```

On CSV, Parseff's generic parser is still ~9% faster than Angstrom's best result, and the fused path is ~1.6x faster. On arithmetic, Parseff's generic parser is ~1.5x faster than Angstrom's generic baseline, and the fused evaluator is ~2.2x faster than Angstrom's optimized parser.

### Why Parseff is faster

**Direct character scanning.** `Parseff.take_while` runs a tight `while` loop with character predicates. No regex compilation, no automaton overhead.

**Fewer allocations.** Span-based APIs return `{ buf; off; len }` slices of the input string without calling `String.sub`. Angstrom's `take_while1` allocates a new string per call.

**Fused operations.** `Parseff.sep_by_take_span` parses an entire separated list in one fused operation. Angstrom's equivalent chains `sep_by`, `char`, `skip_while`, and `take_while1` through monadic operators, each creating closures.

**No monadic overhead.** Parsers are direct function calls. No CPS, no closure allocation for sequencing.

## API style

The fundamental difference: Parseff uses direct-style imperative code. Angstrom uses monadic composition.

### Sequencing

**Parseff:**

```ocaml
let key_value () =
  let key = Parseff.take_while ~at_least:1 (fun c -> c <> ':') ~label:"key" in
  let _ = Parseff.char ':' in
  Parseff.skip_whitespace ();
  let value = Parseff.take_while ~at_least:1 (fun c -> c <> '\n') ~label:"value" in
  (key, value)
```

**Angstrom:**

```ocaml
let key_value =
  take_while1 (fun c -> c <> ':') >>= fun key ->
  char ':' >>= fun _ ->
  skip_while is_ws >>= fun () ->
  take_while1 (fun c -> c <> '\n') >>= fun value ->
  return (key, value)
```

Both do the same thing. Parseff reads like sequential OCaml code. Angstrom threads results through `>>=` and `return`.

### Alternation

**Parseff:**

```ocaml
let value () =
  Parseff.one_of
    [ null_parser; bool_parser; number_parser; string_parser ]
    ()
```

**Angstrom:**

```ocaml
let value =
  null_parser <|> bool_parser <|> number_parser <|> string_parser
```

Similar readability. Angstrom's `<|>` is more concise. Parseff's `Parseff.one_of` is explicit about the list structure.

### Repetition

**Parseff:**

```ocaml
let numbers () =
  Parseff.sep_by
    (fun () ->
      Parseff.skip_whitespace ();
      let s = Parseff.take_while ~at_least:1 is_digit ~label:"digit" in
      Parseff.skip_whitespace ();
      int_of_string s)
    (fun () -> Parseff.char ',')
    ()
```

**Angstrom:**

```ocaml
let numbers =
  sep_by (ws *> char ',' <* ws)
    (take_while1 is_digit >>| int_of_string)
```

Angstrom is more concise here thanks to applicative operators (`*>`, `<*`). Parseff is more explicit: whitespace handling is visible, not hidden in operator chains.

### A complete side-by-side

Here's the same expression parser in both libraries:

**Parseff:**

```ocaml
let rec expr () =
  Parseff.fold_left
    term
    (fun () ->
      Parseff.skip_whitespace ();
      let _ = Parseff.char '+' in
      Parseff.skip_whitespace ();
      fun a b -> a + b)
    ()

and term () =
  Parseff.fold_left
    factor
    (fun () ->
      Parseff.skip_whitespace ();
      let _ = Parseff.char '*' in
      Parseff.skip_whitespace ();
      fun a b -> a * b)
    ()

and factor () =
  Parseff.or_
    (fun () ->
      let _ = Parseff.char '(' in
      let e = expr () in
      let _ = Parseff.char ')' in
      e)
    (fun () -> Parseff.digit ())
    ()
```

**Angstrom:**

```ocaml
let expr =
  fix (fun expr ->
    let factor =
      char '(' *> expr <* char ')'
      <|> (satisfy is_digit >>| fun c -> Char.code c - 48)
    in
    let term =
      chainl1 factor (ws *> char '*' <* ws >>| fun _ -> ( * ))
    in
    chainl1 term (ws *> char '+' <* ws >>| fun _ -> ( + ))
  )
```

Angstrom is denser. Parseff is more readable for people who aren't fluent in monadic/applicative operators.

## Feature comparison

| Feature | Parseff | Angstrom |
| --- | --- | --- |
| OCaml version | 5\.3+ | 4\.x+ |
| API style | Imperative (direct style) | Monadic (CPS-based) |
| Streaming | `parse_source` with `Source.t` | Buffered / Unbuffered modules |
| Backtracking | Automatic via `or_` | Automatic via `<|>` |
| Zero-copy | `span` type + fused ops | Limited via `Unsafe` / bigstring slices |
| Recursion safety | `rec_` with `~max_depth` | Manual (no built-in depth limit) |
| Custom errors | `error` with polymorphic variants | Limited (string-based) |
| Error labels | `expect`, `one_of_labeled` | `<?>` operator |
| Async support | Not built-in (wrap in Domain) | Incremental API with Partial |
| Maturity | New | Battle-tested, widely used |

## Broader comparison

| Feature | Parseff | Angstrom | MParser | Opal |
| --- | --- | --- | --- | --- |
| Imperative-style API | Yes | No | No | No |
| Monadic interface | No | Yes | Yes | Yes |
| Backtracking by default | Yes | Yes | No | No |
| Unbounded lookahead | Yes | Yes | Yes | No |
| Custom error types | Yes | No | No | No |
| Zero-copy API | Yes | Yes | No | No |
| Streaming/incremental | Yes | Yes | No | No |
| Requires OCaml 5+ | Yes | No | No | No |
**Note:** MParser and Opal require explicit backtracking (like Parsec's `try`). Angstrom and Parseff backtrack automatically on alternation. MParser and Opal don't support streaming input. Only Parseff supports custom typed errors beyond strings.
