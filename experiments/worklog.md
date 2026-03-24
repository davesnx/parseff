# Autoresearch Worklog: Parseff Compiler/Structural Optimization

## Session Info
- **Started**: 2026-03-24
- **Branch**: autoresearch/oxcaml-20260324
- **Worktree**: /home/me/parseff-oxcaml (shares opam switch with /home/me/parseff)
- **OCaml**: 5.4.0 (vanilla, no flambda)
- **Goal**: Explore compiler flags, no-effects branch, and structural optimizations

## Key Insights
(Updated as experiments accumulate)

## Next Ideas
- Phase 1: Compiler flags (-O3 combos, -inline, -rounds, -Oclassic, -O2)
- Phase 2: explore/no-effects branch benchmarks
- Phase 3: Structural experiments (Bytes, shallow handlers, individual refs, no Fun.protect)

---

## Experiment Log

### Run 1: Baseline — json_fused=4,735,745 (KEEP)
- Timestamp: 2026-03-24
- Flags: `-O3 -unbox-closures` (current lib/dune and bench/dune)
- Results: json_fused=4,735,745 | json_generic=862,467 | csv_fused=3,586,093 | csv_generic=1,522,893 | arith_fused=6,588,917 | arith_generic=685,836
- Insight: This is the baseline with standard -O3 and -unbox-closures. All subsequent experiments compare against this.
- Next: Try -unbox-closures-factor 20 and -inline 100
