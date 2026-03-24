# Autoresearch Dashboard: parseff-oxcaml

**Runs:** 26 | **Kept:** 7 | **Discarded:** 19 | **Crashed:** 0
**Baseline:** json_fused: 4,735,745 parses/sec (#1)
**Best:** json_fused: 5,352,030 parses/sec (#26, +13.0%)
**Confidence:** ~2.5x (improvement well above noise floor)

## Cumulative Improvement: +13.0% json_fused

### Winning Changes (kept)
1. **`-unbox-closures-factor 20`** (+4.2%) — compiler flag: more aggressive closure unboxing
2. **Remove `Fun.protect`** (+2.1%) — manual try/with avoids closure+wrapper overhead
3. **`[@inline always]` on 11 hot handlers** (+1.2%) — force inlining of handler helpers
4. **Reorder effect arms + static `unreachable_exn`** (~neutral) — micro-optimization
5. **Specialize `handle_sep_by_take_span`** (+4.0%) — eliminate closure call in hot loop
6. **Specialize `handle_sep_by_take`** (~neutral) — consistency, remove dead code
7. **Reduce refs in Greedy_many** (+0.7%) — stack locals vs heap refs

### Key Discovery: No-Effects Branch
The `explore/no-effects` branch (direct state-passing) is **49% faster** on json_fused and **128% faster** on arith_fused. Effects are the dominant cost.

### Dead Ends
- Compiler flags: `-inline 100/20`, `-rounds 3`, `-Oclassic`, `-O2`, `-inline-max-depth 3`, `-inline-max-unroll 2`
- Code structure: unrolled/recursive `scan_while`, over-inlining, `[@inline never]` on cold handlers
- Allocation: Array-based accumulation, local `rev_list`
- New fused effects: extra match arm overhead > round-trip savings

| # | commit | json_fused | delta | status | description |
|---|--------|-----------|-------|--------|-------------|
| 1 | b9da14f | 4,735,745 | — | keep | baseline |
| 2 | 3c3a45c | 4,935,103 | +4.2% | keep | -unbox-closures-factor 20 |
| 3-7 | — | — | — | discard | compiler flag variants |
| 8 | no-efx | 7,346,729 | +55.1% | info | no-effects branch |
| 10 | 95fc85b | 5,037,910 | +6.4% | keep | Remove Fun.protect |
| 11-12 | — | — | — | discard | scan_while variants |
| 13 | 8623200 | 5,098,529 | +7.7% | keep | [@inline always] hot handlers |
| 14-15 | — | — | — | discard | over/under-inlining |
| 16 | abf72cd | 5,075,369 | +7.2% | keep | reorder effect arms |
| 17-21 | — | — | — | discard | flag/structural variants |
| 22 | a2df466 | 5,303,633 | +12.0% | keep | specialize sep_by_take_span |
| 23 | 39c3885 | 5,312,649 | +12.2% | keep | specialize sep_by_take |
| 24-25 | — | — | — | discard | array accum, local rev |
| 26 | 0ed14a6 | 5,352,030 | **+13.0%** | **keep** | reduce Greedy_many refs |
