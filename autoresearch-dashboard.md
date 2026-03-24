# Autoresearch Dashboard: parseff-oxcaml

**Runs:** 31 | **Kept:** 7 | **Discarded:** 24 | **Crashed:** 0
**Baseline:** json_fused: 4,735,745 parses/sec (#1)
**Best:** json_fused: ~5,300,000 parses/sec (median, +13.0%)
**Confidence:** ~2.5x (improvement well above noise floor)

## Cumulative Improvement: +13.0% json_fused

### Winning Changes (all applied in worktree)
1. **`-unbox-closures-factor 20`** — compiler flag: +4.2%
2. **Remove `Fun.protect`** — manual try/with: +2.1%
3. **`[@inline always]` on 11 hot handlers** — forced inlining: +1.2%
4. **Reorder effect arms + static `unreachable_exn`** — micro-opt: ~0%
5. **Specialize `handle_sep_by_take_span`** — eliminate closure: +4.0%
6. **Specialize `handle_sep_by_take`** — consistency: ~0%
7. **Reduce refs in Greedy_many** — stack locals: +0.7%

### Key Discovery
No-effects branch is **49% faster** on json_fused, **128% faster** on arith_fused.
Effects are the dominant performance cost. On vanilla OCaml 5.4, there's no way to reduce this.

### Explored & Rejected
**Compiler flags:** -inline 100/20, -rounds 3, -Oclassic, -O2, -inline-max-depth 3, -inline-max-unroll 2, -linscan, -function-sections
**Code structure:** unrolled/recursive scan_while, over-inlining, [@inline never] cold, new fused effects, Array accum, local rev_list
**Informational:** -unsafe gives ~2% more but unsafe for production
