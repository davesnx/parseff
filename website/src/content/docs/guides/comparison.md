---
title: Comparison with Angstrom
description: How Parseff compares to Angstrom in performance, API style, and trade-offs
---

# Comparison with Angstrom

How Parseff compares to [Angstrom](https://github.com/inhabitedtype/angstrom) in performance, API style, and trade-offs.

## Performance

Cross-library workloads in [`bench/`](https://github.com/davesnx/parseff/tree/main/bench):

```
Benchmark    Best Parseff result   What it measures
JSON         ~2.47M/s optimized    Full JSON array parser vs Angstrom
CSV          ~3.93M/s fused        CSV field parser vs Angstrom and MParser
Arithmetic   ~11.86M/s fused       Expression evaluator vs Angstrom and MParser
```

### JSON benchmark

JSON array parser (`[1, 2, 3, ..., 10]`) over 1,000,000 iterations (3 runs). Source: [`bench/bench_json.ml`](https://github.com/davesnx/parseff/blob/main/bench/bench_json.ml).

```
                        Parses/sec     vs. Angstrom (generic)   Minor allocs
Parseff (optimized)     ~2,470,000     2.0x faster              1.5 GB
Parseff (generic)       ~1,740,000     1.4x faster              1.1 GB
Angstrom (optimized)    ~1,720,000     1.4x faster             11.1 GB
Angstrom (generic)      ~1,260,000     baseline                 5.9 GB
```

- **Optimized**: `Parseff.Slice.sep_by_take` plus `List.map` (avoids per-element `String.sub` during scanning; still converts with `float_of_string` via `Parseff.Slice.to_string`).
- **Generic**: ordinary `char`, `skip_while`, `take_while`, and `sep_by` with the same `float_of_string` conversion as `Angstrom (generic)`.
- Minor allocations are cumulative `Benchmark.latencyN` totals across all 1,000,000 parses.

### Other cross-library benchmarks

```
            Parseff best   Parseff generic   Angstrom best   MParser
CSV         ~3,930,000     ~2,790,000        ~2,550,000      ~1,490,000
Arithmetic  ~11,860,000    ~2,960,000        ~5,360,000      ~1,510,000
```

### Why Parseff is faster

- **Direct character scanning**: `Parseff.take_while` runs a tight loop over the input buffer (optionally backed by a `Parseff.Charset` table).
- **Zero-copy slices**: `Parseff.Slice` avoids per-token `String.sub` allocations.
- **Fused scanners**: `Parseff.Slice.sep_by_take` scans a separated list in one loop without chaining monadic closures.
- **Direct-style calls**: sequencing uses plain OCaml function calls rather than CPS closures.

## API style

Parseff uses direct-style OCaml functions (`unit -> 'a`); Angstrom uses monadic and applicative combinators.

### Sequencing

**Parseff:**

```ocaml
let key_value () =
  let key = Parseff.take_while ~at_least:1 ~label:"key" (fun c -> c <> ':') in
  ignore (Parseff.char ':');
  Parseff.skip_whitespace ();
  let value =
    Parseff.take_while ~at_least:1 ~label:"value" (fun c -> c <> '\n')
  in
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

### Alternation and repetition

**Parseff:**

```ocaml
let value () =
  Parseff.one_of [ null_parser; bool_parser; number_parser; string_parser ]

let numbers () =
  Parseff.sep_by
    (fun () ->
      Parseff.skip_whitespace ();
      let s = Parseff.take_while ~at_least:1 ~label:"digit" is_digit in
      Parseff.skip_whitespace ();
      int_of_string s)
    (fun () -> Parseff.char ',')
```

**Angstrom:**

```ocaml
let value =
  null_parser <|> bool_parser <|> number_parser <|> string_parser

let numbers =
  sep_by (ws *> char ',' <* ws)
    (take_while1 is_digit >>| int_of_string)
```

### Side-by-side expression parser

**Parseff:**

```ocaml
let rec expr () =
  Parseff.fold_left term (fun () ->
      Parseff.skip_whitespace ();
      ignore (Parseff.char '+');
      Parseff.skip_whitespace ();
      ( + ))

and term () =
  Parseff.fold_left factor (fun () ->
      Parseff.skip_whitespace ();
      ignore (Parseff.char '*');
      Parseff.skip_whitespace ();
      ( * ))

and factor () =
  Parseff.or_
    (fun () ->
      ignore (Parseff.char '(');
      let e = Parseff.rec_ expr in
      ignore (Parseff.char ')');
      e)
    (fun () -> Char.code (Parseff.digit ()) - Char.code '0')
```

**Angstrom:**

```ocaml
let expr =
  fix (fun expr ->
    let factor =
      char '(' *> expr <* char ')'
      <|> (satisfy is_digit >>| fun c -> Char.code c - 48)
    in
    let term = chainl1 factor (ws *> char '*' <* ws >>| fun _ -> ( * )) in
    chainl1 term (ws *> char '+' <* ws >>| fun _ -> ( + )))
```

## Feature comparison

| Feature | Parseff | Angstrom |
| --- | --- | --- |
| OCaml version | 5\.3+ | 4\.x+ |
| API style | Direct style | Monadic (CPS) |
| Pull streaming | `Source.parse`, `Source.parse_each` | Buffered / Unbuffered |
| Push / async | `Push.parse`, `Push.parse_each`; Eio fibers via `Source` | Incremental `Partial` API |
| Backtracking | Automatic via `or_`; bounded window in streaming | Automatic via `<|>`; unbounded buffer |
| Zero-copy | `Slice.t` + fused scanners | `Unsafe` / bigstring slices |
| Recursion safety | `rec_` with `~max_depth` | Manual |
| Custom errors | Extensible `custom_error`, diagnostics, recovery | String-based |
| Error labels | `expect`, `one_of_labeled`, `context` | `<?>` |
| Maturity | New | Battle-tested, widely used |

## Broader comparison

| Feature | Parseff | Angstrom | MParser | Opal |
| --- | --- | --- | --- | --- |
| Direct-style API | Yes | No | No | No |
| Monadic interface | No | Yes | Yes | Yes |
| Backtracking by default | Yes | Yes | No | No |
| Unbounded lookahead | In-memory (bounded window in streaming) | Yes | Yes | No |
| Custom error types | Yes | No | No | No |
| Zero-copy API | Yes | Yes | No | No |
| Streaming / incremental | Yes | Yes | No | No |
| Requires OCaml 5+ | Yes | No | No | No |
MParser and Opal require explicit backtracking (like Parsec's `try`) and do not support streaming.
