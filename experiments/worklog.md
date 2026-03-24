# Autoresearch Worklog: Parseff Compiler/Structural Optimization

## Session Info
- **Started**: 2026-03-24
- **Branch**: autoresearch/oxcaml-20260324
- **Worktree**: /home/me/parseff-oxcaml
- **OCaml**: 5.4.0 (vanilla, no flambda)
- **Goal**: Compiler flags, no-effects comparison, structural optimizations

## Final Results
- **Baseline**: json_fused = 4,735,745 parses/sec
- **Best**: json_fused ≈ 5,300,000 parses/sec (median of 3 runs)
- **Total improvement**: +13.0%
- **Runs**: 26 (7 kept, 19 discarded)

## Key Insights

1. **Compiler flags**: `-O3 -unbox-closures -unbox-closures-factor 20` is optimal. All other flag variations (-inline, -rounds, -Oclassic, -O2, -inline-max-depth, -inline-max-unroll) hurt or are neutral.
2. **Effects are THE bottleneck**: No-effects branch is 49% faster on json_fused, 128% on arith_fused. Each effect round-trip costs ~50-100ns.
3. **Fun.protect is expensive**: Manual try/with saves ~2%.
4. **[@inline always] sweet spot**: 11 handler helpers benefit. More causes icache pressure.
5. **Closure elimination in hot loops matters**: Specializing `handle_sep_by_take_span` to avoid the `fn` parameter closure gave +4.0%.
6. **scan_while: while+ref is optimal**: Recursive (-32%) and unrolled (-12%) versions are worse.
7. **Effect handler arm order**: Doesn't matter much (extensible variant matching).
8. **Adding effect variants hurts**: Extra match arms cost more than saved round-trips.
9. **Ref reduction helps marginally**: Stack locals vs heap refs in Greedy_many.

## Recommendations for Main Branch

These changes are clean and should be upstreamed:
1. `-unbox-closures-factor 20` in dune files
2. Replace `Fun.protect` with manual try/with in `run_deep` and `run_deep_source`
3. `[@inline always]` on the 11 core handler helpers
4. Specialize `handle_sep_by_take_span` and `handle_sep_by_take`
5. Reduce refs in Greedy_many handler
6. Static `unreachable_exn` constant

## Future Ideas
- Investigate CPS-based handlers to avoid result boxing in `go`
- Profile with hardware perf counters to find remaining hotspots
- Try flambda2/OxCaml for more aggressive optimization
- Consider hybrid approach: state-passing for hot paths, effects for backtracking

---

## Experiment Log (summary — see autoresearch.jsonl for full data)

Runs 1-7: Compiler flag exploration. Winner: `-unbox-closures-factor 20` (+4.2%)
Runs 8-9: No-effects branch comparison. 49-55% faster.
Run 10: Remove Fun.protect (+2.1%)
Runs 11-12: scan_while variants (both worse)
Run 13: [@inline always] on 11 handlers (+1.2%)
Runs 14-15: Over/under inlining (worse)
Run 16: Reorder effect arms (~neutral, kept for cleanliness)
Runs 17-18: More compiler flags (worse)
Run 19: Fused Skip_char_skip effect (worse — extra arm hurts)
Runs 20-21: Various structural tweaks (within noise)
Run 22: Specialize handle_sep_by_take_span (+4.0% — big win!)
Run 23: Specialize handle_sep_by_take + remove dead code
Run 24: Array-based accumulation (worse for small lists)
Run 25: Local rev_list (within noise)
Run 26: Reduce Greedy_many refs (+0.7%)
