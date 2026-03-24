# Autoresearch: Parseff Performance Optimization

## Objective
Maximize throughput of the Parseff OCaml parser combinator library — both the "fused" combinators (bulk operations in single effect round-trips) and "generic" combinators (standard char/or_/sep_by style). The library uses OCaml 5 algebraic effects for its imperative-style API.

## Metrics
- **Primary**: `json_fused` (parses/sec, higher is better) — JSON array parsing using fused combinators (flagship benchmark)
- **Secondary**: `json_generic`, `csv_fused`, `csv_generic`, `arith_fused`, `arith_generic` — other benchmark variants

## How to Run
`./autoresearch.sh` — builds release profile, runs bench_quick.exe, outputs `METRIC name=number` lines.

## Files in Scope
- `lib/parseff.ml` — Core library: effect types, handler helpers, `run_deep` effect handler, combinators. This is the main optimization target.
- `bench/parseff_bench.ml` — Fused benchmark parsers (json_array, csv, arithmetic)
- `bench/bench_quick.ml` — Quick benchmark runner (generic parsers defined here)

## Off Limits  
- `lib/parseff.mli` — Public API must not change (signatures, types)
- `test/` — Test files should not be modified (except updating expected error messages if internal error format changes)
- Do not change the benchmark inputs or iteration counts
- Do not add new opam dependencies

## Constraints
- All tests must pass (`dune build @runtest`)
- Public API signatures in parseff.mli must be preserved
- Must remain correct — parsers must produce the same results
- OCaml 5.4.0, no flambda

## Architecture Overview
Parseff uses OCaml 5 algebraic effects. Each combinator (`char`, `satisfy`, `take_while`, etc.) does `Effect.perform` which suspends the parser, the `run_deep` handler processes the operation on the input buffer, then resumes the parser via `Effect.Deep.continue`. 

**Key hot paths:**
- `run_deep` handler loop — the effect handler with match cases for each effect type
- `handle_consume` — matches literal strings against input
- `handle_satisfy` — checks one character against a predicate
- `handle_match_char` — matches a specific character (dedicated effect, avoids closure)
- `handle_take_while` / `handle_skip_while` — bulk character scanning
- `handle_sep_by_take_core` — fused separator+take loop
- `handle_skip_while_then_char` — fused skip+char operation
- `compose_branch_errors` — error composition during backtracking (or_)
- `Choose` effect handler in `run_deep` — backtracking with position save/restore

**State representation:**
```ocaml
type state = {
  input : string;        (* the input string *)
  mutable pos : int;     (* current position *)
  max_depth : int;
  mutable current_depth : int;
  mutable diagnostics_rev : (int * Obj.t) list;
  mutable line_index : line_index option;
}
```

**Performance characteristics:**
- Fused combinators (sep_by_take_span, skip_while_then_char) do entire loops inside handler — ~4 effect round-trips for JSON
- Generic combinators (char, sep_by, or_) do 1 effect per call — ~50+ round-trips for JSON
- Each effect round-trip costs ~50-100ns (continuation capture + handler dispatch + resume)
- Error paths use structured error_msg type (deferred formatting) — already optimized

## What's Been Tried
- **Structured error messages (O1)**: Changed Parse_error from `int * string` to `int * error_msg` with deferred formatting. Eliminates string allocation in compose_branch_errors.
- **Dedicated Match_char effect (O2)**: `char c` uses specialized Match_char effect instead of going through satisfy. Avoids String.make + closure allocation.
- **peek_char primitive (O3)**: Zero-backtracking lookahead via Peek_char effect.
- **Fixed expect (O4)**: Extracts position from caught exception instead of extra Position effect.
- **1-char consume special case (O5)**: handle_consume fast-paths single-character strings.
- **Pre-allocated error constants (O7)**: Common error messages are module-level constants.

## Ideas to Explore
- Reduce effect handler dispatch overhead (fewer match cases? function table?)
- Inline handler helpers more aggressively ([@inline always] on hot paths)
- Reduce allocations in `or_` / `Choose` handler (avoid ref allocation for last_exn?)
- Try `[@@unboxed]` on state fields or effect payloads
- Optimize `many` / `sep_by` to avoid List.rev (use a Buffer or mutable array?)
- Add more fused combinators for common generic patterns
- Optimize the `go` function in run_deep (the main loop)
- Try using `Obj.magic` tricks to avoid boxing in hot paths (carefully)
- Profile with `perf` to find actual hotspots
- Look at OCaml 5.4 effect handler internals for optimization opportunities
