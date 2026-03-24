# Autoresearch Worklog: Parseff Performance

## Session Info
- **Started**: 2026-03-24
- **Branch**: autoresearch/perf-20260324
- **OCaml**: 5.4.0 (vanilla, no flambda)
- **Goal**: Maximize Parseff throughput (fused + generic)

## Key Insights
(Updated as experiments accumulate)

## Next Ideas
- Aggressive inlining on handler helpers
- Reduce effect handler match arms
- Optimize Choose handler (or_/backtracking)
- Try Bytes vs String for input buffer
- Profile with perf to find actual bottlenecks

---

## Experiment Log
