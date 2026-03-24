# Autoresearch Worklog: Parseff Compiler/Structural Optimization

## Session Info
- **Started**: 2026-03-24
- **Branch**: autoresearch/oxcaml-20260324
- **Worktree**: /home/me/parseff-oxcaml (shares opam switch with /home/me/parseff)
- **OCaml**: 5.4.0 (vanilla, no flambda)
- **Goal**: Explore compiler flags, no-effects branch, and structural optimizations

## Key Insights

1. **Compiler flags sweet spot**: `-O3 -unbox-closures -unbox-closures-factor 20` is optimal. Additional flags (-inline, -rounds, -Oclassic, -O2, -inline-max-depth, -inline-max-unroll) all hurt or are neutral.
2. **Effects are the #1 bottleneck**: The no-effects branch is 49% faster on json_fused and 128% faster on arith_fused. Each effect round-trip costs ~50-100ns.
3. **Fun.protect is measurably expensive**: Manual try/with saves ~2% by avoiding closure allocation and exception handling overhead.
4. **[@inline always] has a sweet spot**: 11 hot handler helpers benefit from forced inlining. Adding more causes code bloat and icache pressure.
5. **scan_while: while+ref > recursion > unrolled**: For short inputs (1-10 chars), the simple while loop with a ref is fastest. Recursive version is 32% worse; unrolled is 12% worse.
6. **Effect handler arm order doesn't matter much**: OCaml 5 extensible variant matching is by pointer identity, not sequential. Reordering was within noise.

## Next Ideas
- Try shallow handlers instead of deep handlers
- Specialize the handler for common effect subsets
- Try `[@specialize]` or `[@unrolled]` on key functions
- Reduce allocations in Greedy_many (avoid List.rev?)
- Try a continuation-passing style instead of effects
- Profile with `perf` to find actual hardware bottlenecks

---

## Experiment Log

### Run 1: Baseline — json_fused=4,735,745 (KEEP)
- Flags: `-O3 -unbox-closures`
- Results: json_fused=4,735,745 | json_generic=862,467 | csv_fused=3,586,093 | arith_fused=6,588,917
- Insight: Starting point. All subsequent experiments compare against this.

### Run 2: -unbox-closures-factor 20 — json_fused=4,935,103 (KEEP)
- Delta: +4.2% json_fused, +22.1% arith_fused
- Insight: More aggressive closure unboxing helps. This is the best compiler flag found.

### Run 3: -inline 100 — json_fused=4,676,940 (DISCARD)
- Delta: -1.2% json_fused
- Insight: Excessive inlining bloats code and hurts instruction cache.

### Run 4: -rounds 3 — json_fused=4,902,201 (DISCARD)
- Delta: -0.7% vs run 2
- Insight: More optimization rounds don't help; already converged.

### Run 5: -Oclassic — json_fused=4,748,676 (DISCARD)
- Insight: Old optimization pipeline is worse than -O3 for this codebase.

### Run 6: -O2 — json_fused=4,874,007 (DISCARD)
- Insight: -O3 is better than -O2 for parseff.

### Run 7: -inline 20 — json_fused=4,915,937 (DISCARD)
- Insight: Within noise of run 2. Moderate inlining is neutral.

### Run 8: No-effects branch — json_fused=7,346,729 (INFO)
- Delta: +49% vs effects baseline
- Insight: **CRITICAL FINDING.** Effects are the dominant overhead. The no-effects branch (state-passing) is dramatically faster.

### Run 9: No-effects + unbox-closures-factor 20 — json_fused=7,307,004 (INFO)
- Insight: Compiler flag tweaks don't help no-effects version (no closures to unbox).

### Run 10: Remove Fun.protect — json_fused=5,037,910 (KEEP)
- Delta: +2.1% over run 2
- Insight: Fun.protect adds overhead via closure allocation + try/with wrapper.

### Run 11: Unrolled scan_while 2x — json_fused=4,426,737 (DISCARD)
- Delta: -12% vs run 10
- Insight: Short inputs (1-10 chars) don't benefit from loop unrolling.

### Run 12: Recursive scan_while — json_fused=3,448,752 (DISCARD)
- Delta: -32% vs run 10
- Insight: While+ref drastically outperforms tail recursion for character scanning.

### Run 13: [@inline always] hot handlers — json_fused=5,098,529 (KEEP)
- Delta: +1.2% over run 10
- Insight: Forcing inlining on 11 core handlers reduces call overhead.

### Run 14: Over-inlining 3 more handlers — json_fused=4,742,033 (DISCARD)
- Delta: -7% vs run 13
- Insight: Code bloat from inlining fused_sep_take etc. hurts icache.

### Run 15: [@inline never] compose_branch_errors — json_fused=4,736,530 (DISCARD)
- Insight: Forcing out-of-line on error path doesn't help.

### Run 16: Reorder effect arms + static unreachable_exn — json_fused=5,075,369 (KEEP)
- Insight: Neutral on performance but avoids allocation. Kept for code quality.

### Run 17: -inline-max-depth 3 — json_fused=5,156,899 (DISCARD)
- Insight: Helps json_fused slightly but catastrophically hurts arith_fused (-45%).

### Run 18: -inline-max-unroll 2 — json_fused=4,807,692 (DISCARD)
- Insight: Unrolling recursive functions doesn't help.
