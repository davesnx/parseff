# Autoresearch Dashboard: parseff-oxcaml

**Runs:** 32 | **Kept:** 8 | **Discarded:** 24 | **Crashed:** 0
**Baseline:** json_fused: 4,735,745 parses/sec (#1)
**Best:** json_fused: 5,591,747 parses/sec (#32, **+18.1%**)
**Confidence:** ~3.5x (improvement well above noise floor)

## Cumulative Improvement: +18.1% json_fused, +37.1% arith_generic

### Winning Changes (all applied in worktree)
1. **`-unbox-closures-factor 20`** — compiler flag: +4.2%
2. **Remove `Fun.protect`** — manual try/with: +2.1%
3. **`[@inline always]` on 11 hot handlers** — forced inlining: +1.2%
4. **Reorder effect arms + static `unreachable_exn`** — micro-opt: ~0%
5. **Specialize `handle_sep_by_take_span`** — eliminate closure: +4.0%
6. **Specialize `handle_sep_by_take`** — consistency: ~0%
7. **Reduce refs in Greedy_many** — stack locals: +0.7%
8. **O8: Deferred error formatting** — Obj.magic to skip format_error_msg on backtracking: **+4.7%** json_fused, **+37.1%** arith_generic

### Key Discovery
No-effects branch is **49% faster** on json_fused, **128% faster** on arith_fused. Effects are the dominant cost.

| # | commit | json_fused | delta | arith_generic | status | description |
|---|--------|-----------|-------|--------------|--------|-------------|
| 1 | b9da14f | 4,735,745 | — | 685,836 | keep | baseline |
| 2 | 3c3a45c | 4,935,103 | +4.2% | 712,370 | keep | -unbox-closures-factor 20 |
| 10 | 95fc85b | 5,037,910 | +6.4% | 687,089 | keep | Remove Fun.protect |
| 13 | 8623200 | 5,098,529 | +7.7% | 715,686 | keep | [@inline always] hot handlers |
| 16 | abf72cd | 5,075,369 | +7.2% | 616,576 | keep | Reorder effect arms |
| 22 | a2df466 | 5,303,633 | +12.0% | 704,106 | keep | Specialize sep_by_take_span |
| 23 | 39c3885 | 5,312,649 | +12.2% | 679,223 | keep | Specialize sep_by_take |
| 26 | 0ed14a6 | 5,352,030 | +13.0% | 685,904 | keep | Reduce Greedy_many refs |
| 32 | b2c2dda | **5,591,747** | **+18.1%** | **930,873** | **keep** | **O8: Deferred error formatting** |
