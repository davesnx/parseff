## Unreleased

## 0.3.0

- **`fail` now produces `` `Failure of string `` instead of `` `Expected ``.**
- **Add `catch` combinator.** `catch parser handler` intercepts `` `Failure `` errors, allowing recovery within backtracking contexts.
- **Add `Parseff.Utf8` module for Unicode-aware parsing.** `satisfy`, `char`, `any_char`, `take_while`, `skip_while`, `letter`, `digit`, `alphanum`, `whitespace` and `take_while_span`.
- **Add `Parseff.BE` and `Parseff.LE` binary parsing primitives.** Big-endian and little-endian readers for `any_uint8`, `any_int8`, `any_int16`, `any_uint16`, `any_int32`, `any_int64`, `any_float`, `any_double`, and validators `int16`, `int32`, `int64`.
- **Add `location` and `location_of_position` for line/column tracking.** `location ()` returns `{ offset; line; col }` during parsing (lazy, incremental, zero cost if unused). `location_of_position input pos` converts a byte offset after parsing — useful for error reporting.
- **BE/LE exact-match validators produce `` `Expected `` instead of `` `Failure ``.** `BE.int16`, `BE.int32`, `BE.int64` (and LE equivalents) are parser expectations, not user validation failures.
- **`or_` now composes errors from both branches.** When both branches fail at the same position, `Expected` messages are joined with `" or "` (e.g., `expected "foo" or expected "bar"`). When branches fail at different positions, the error from the branch that consumed more input is kept. This also improves `one_of`, which chains `or_` and now naturally produces composed messages across all alternatives.
- Register a printer for `Unhandled effects`, warning the user about bad organitzation of the parser function

## 0.2.0

- **Unified `*1` variants into base combinators with `~at_least` parameter.** `many1` → `many ~at_least:1`, `take_while1` → `take_while ~at_least:1`, `sep_by1` → `sep_by ~at_least:1`, `end_by1` → `end_by ~at_least:1`, `whitespace1` → `whitespace ~at_least:1`. The `~at_least` parameter generalizes beyond the binary zero-vs-one distinction (e.g., `many ~at_least:3` requires at least three matches). The old `chainl1` and `chainr1` were also removed in favor of making the `default` argument optional (see next item).
- **Renamed `chainl`/`chainr` to `fold_left`/`fold_right` with `~otherwise`.** `chainl1 p op` → `fold_left p op`, `chainl p op default` → `fold_left p op ~otherwise:default`, and likewise for `chainr`/`fold_right`. The new names better describe what the combinators do (parse and fold with associativity) and align with OCaml's `List.fold_left`/`List.fold_right` naming conventions. The `~otherwise` labeled argument replaces the old positional `default` parameter, providing a fallback value when zero elements match.

## 0.1.0

Initial release of Parseff
