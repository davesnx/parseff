# Autoresearch Dashboard: parseff-oxcaml

**Runs:** 21 | **Kept:** 4 | **Discarded:** 17 | **Crashed:** 0
**Baseline:** json_fused: 4,735,745 parses/sec (#1)
**Best:** json_fused: 5,098,529 parses/sec (#13, +7.7%)
**Confidence:** ~2.2x (improvement well above noise floor)

## Summary of Wins
1. **`-unbox-closures-factor 20`** (+4.2%) — more aggressive closure unboxing
2. **Remove `Fun.protect`** (+2.1%) — manual try/with avoids closure+wrapper overhead
3. **`[@inline always]` on 11 hot handlers** (+1.2%) — forces inlining of handler helpers
4. **Reorder effect arms + static `unreachable_exn`** (~neutral) — micro-optimization

## Key Discovery: No-Effects Branch
The `explore/no-effects` branch (direct state-passing, no algebraic effects) is **49% faster** on json_fused and **128% faster** on arith_fused. Effects overhead is the dominant cost.

## Dead Ends
- `-inline 100`, `-rounds 3`, `-Oclassic`, `-O2`: all worse or neutral
- `-inline-max-depth 3`: helps json but catastrophically hurts arith
- `-inline-max-unroll 2`: worse across the board
- Unrolled/recursive `scan_while`: both worse than while+ref loop
- Over-inlining (more than 11 handlers): icache pressure hurts
- `[@inline never]` on cold handlers: no benefit
- New fused effects (Skip_char_skip): extra match arm overhead > round-trip savings

| # | commit | json_fused | delta | status | description |
|---|--------|-----------|-------|--------|-------------|
| 1 | b9da14f | 4,735,745 | — | keep | baseline |
| 2 | 3c3a45c | 4,935,103 | +4.2% | keep | -unbox-closures-factor 20 |
| 3 | 3c3a45c | 4,676,940 | -1.2% | discard | -inline 100 |
| 4 | 3c3a45c | 4,902,201 | +3.5% | discard | -rounds 3 |
| 5 | 3c3a45c | 4,748,676 | +0.3% | discard | -Oclassic |
| 6 | 3c3a45c | 4,874,007 | +2.9% | discard | -O2 |
| 7 | 3c3a45c | 4,915,937 | +3.8% | discard | -inline 20 |
| 8 | no-efx | 7,346,729 | +55.1% | info | no-effects branch |
| 9 | no-efx | 7,307,004 | +54.3% | info | no-effects + unbox-closures-factor 20 |
| 10 | 95fc85b | 5,037,910 | +6.4% | keep | Remove Fun.protect |
| 11 | 95fc85b | 4,426,737 | -6.5% | discard | Unrolled scan_while 2x |
| 12 | 95fc85b | 3,448,752 | -27.2% | discard | Recursive scan_while |
| 13 | 8623200 | 5,098,529 | **+7.7%** | **keep** | [@inline always] hot handlers |
| 14 | 8623200 | 4,742,033 | +0.1% | discard | Over-inlining 3 more |
| 15 | 8623200 | 4,736,530 | +0.0% | discard | [@inline never] compose_branch_errors |
| 16 | abf72cd | 5,075,369 | +7.2% | keep | Reorder effect arms + static exn |
| 17 | abf72cd | 5,156,899 | +8.9% | discard | -inline-max-depth 3 |
| 18 | abf72cd | 4,807,692 | +1.5% | discard | -inline-max-unroll 2 |
| 19 | abf72cd | 4,754,886 | +0.4% | discard | Skip_char_skip fused effect |
| 20 | abf72cd | 4,897,519 | +3.4% | discard | [@inline never] cold handlers |
| 21 | abf72cd | 5,046,172 | +6.6% | discard | [@inline always] combinators |
