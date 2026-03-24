# Autoresearch Ideas Backlog

## High Impact (requires structural changes)
- **CPS-based handler instead of result boxing**: `go` currently returns `('b, e) result`, allocating `Ok v` for every success. A CPS approach (`go p ~on_ok ~on_error`) would avoid this allocation. Tricky because `go` is polymorphically recursive.
- **Flambda2/OxCaml compiler**: Would enable `[@local]` for closures (stack allocation), better inlining, and mode polymorphism. Could be 10-20%+ improvement.
- **Hybrid effects/direct approach**: Use direct state-passing for non-backtracking operations (skip_while, take_while) and effects only for backtracking (Choose, Greedy_many). Would require API changes.

## Medium Impact (worth investigating)
- **Monomorphize the handler for common types**: Create a `parse_string_list` that knows the result type is `string list` and can avoid boxing.
- **Profile with hardware perf counters**: Need proper access to CPU counters (IPC, cache misses, branch misses) to identify remaining hotspots.
- **Try splitting parseff.ml into smaller files**: Might help compiler optimize individual files better.

## Low Impact (diminishing returns)
- **Pre-sized list accumulation**: For `sep_by_take_span`, if we could estimate element count from input length, could pre-allocate.
- **SIMD-like scanning**: Use `String.get_int32` or `String.get_int64` to scan 4/8 bytes at a time for whitespace.
- **Stack-allocated state**: OCaml doesn't support this, but `[@local]` in OxCaml would help.

## Tried and Failed
- Loop unrolling: hurts on short inputs
- Recursive scan_while: 32% slower than while+ref
- Over-inlining: causes icache pressure
- New fused effects: extra match arm cost > round-trip savings
- Array-based accumulation: worse for small lists (Array.make is expensive)
