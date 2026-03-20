## Unreleased

- **Unified `*1` variants into base combinators with `~at_least` parameter.**
  `many1` → `many ~at_least:1`, `take_while1` → `take_while ~at_least:1`,
  `sep_by1` → `sep_by ~at_least:1`, `end_by1` → `end_by ~at_least:1`,
  `whitespace1` → `whitespace ~at_least:1`. The `~at_least` parameter
  generalizes beyond the binary zero-vs-one distinction (e.g.,
  `many ~at_least:3` requires at least three matches). The old `chainl1` and
  `chainr1` were also removed in favor of making the `default` argument
  optional (see next item).

- **Renamed `chainl`/`chainr` to `fold_left`/`fold_right` with `~otherwise`.**
  `chainl1 p op` → `fold_left p op`, `chainl p op default` →
  `fold_left p op ~otherwise:default`, and likewise for `chainr`/`fold_right`.
  The new names better describe what the combinators do (parse and fold with
  associativity) and align with OCaml's `List.fold_left`/`List.fold_right`
  naming conventions. The `~otherwise` labeled argument replaces the old
  positional `default` parameter, providing a fallback value when zero elements
  match.

## 0.1.0

Initial release of Parseff -- a direct-style parser combinator library for
OCaml 5 powered by algebraic effects.

- Direct-style parsers as plain `unit -> 'a` functions, no monadic or binding operators needed
- Algebraic effects for control flow, backtracking, and streaming input
- Typed errors via polymorphic variants with position tracking
- Built-in error variants: `` `Expected ``, `` `Unexpected_end_of_input ``, `` `Depth_limit_exceeded ``
- Automatic backtracking with `or_`, `one_of`, and `one_of_labeled`
- Primitive parsers: `consume`, `satisfy`, `char`, `match_regex`, `take_while`, `skip_while`, `fail`, `error`
- Repetition combinators: `many`, `sep_by`, `between`, `end_by`, `count`, `optional`
- Operator chains: `fold_left`, `fold_right` for expression parsing
- Look-ahead parsing and depth-limited recursion via `rec_` (default depth: 128)
- Convenience parsers: `digit`, `letter`, `alphanum`, `whitespace`, `any_char`
- Zero-copy span APIs: `take_while_span`, `sep_by_take_span` returning `{ buf; off; len }` records
- Fused operations for hot paths: `sep_by_take`, `fused_sep_take`, `skip_while_then_char`
- Streaming support via `Source.of_string`, `Source.of_channel`, `Source.of_function`
- Backtrack-across-chunk-boundary support for streaming sources
- Non-fatal diagnostics with `warn` / `warn_at`, rolled back on backtracking
- `parse_until_end` / `parse_source_until_end` runners that collect diagnostics
- Error labeling with `expect` and `one_of_labeled` for clear error messages
- Domain-safe: no global mutable state, independent parses run in parallel across OCaml 5 domains
- 2-4x faster than Angstrom and MParser on equivalent parsers
- Single runtime dependency: `re` for regex support
