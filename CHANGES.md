## Unreleased

This is the 0.5.0 release.

- **Breaking: errors are structured values.** `parse` and `parse_source` return `Error e` where `e` is a `Parseff.error`: `{ kind; range; location; contexts; notes; excerpt }`, where `location` is the line and column in bytes. `location` is `None` when the line is not known (streaming, older than the buffered input). `kind : error_kind` is `Expected { items; found; more }`, `Failure`, `Custom`, `Depth_limit_exceeded` or `Backtrack_window_exceeded`, so a match on it is exhaustive. `found` is `Text`, `End`, `Short` or `Value`. The result type is `('a, error) Stdlib.result`, and `Parseff.Ok` and `Parseff.Error` are now `Stdlib.Ok` and `Stdlib.Error`. The type `error` and its parts are `private`: you can read and match them, not build them. `note` and `range` stay buildable, for `fail ~notes`. See "Migrating from 0.4" below.
- **Breaking: `error` takes an extensible variant.** `error` was `'e -> 'a` and the result carried a free `'e`, so a parser could raise an `int` and the caller could read a `string`: a segfault. `custom_error` is now an extensible variant, `error : ?from:int -> ?notes:note list -> custom_error -> 'a`, and `error_kind` has `Custom of custom_error`. `render_error ?custom` and `error_message ?custom` take a printer `custom_error -> string`. Declare your cases with `type Parseff.custom_error += Too_large of int` and match `Custom (Too_large n)`. A `Custom` match needs a final `_` case.
- **Breaking: diagnostics are typed by a collector.** `warn : 'd -> unit` had the same hole. `Diagnostics.create ()` makes a `'d Diagnostics.t`; `warn d x` and `warn_at d ~pos x` record into it; pass it to the runner with `~diagnostics:d` and read `Diagnostics.to_list d` after the parse, on `Ok` and on `Error`. The runner empties the collector first. `warn` into a collector the runner did not get does nothing. Diagnostics are still rolled back with a branch that backtracks. A collector in use by one parse raises `Invalid_argument` if another parse gets it, or if `Diagnostics.clear` is called on it.
- **Breaking: two runners.** `parse` and `parse_source` take `` ?consume:[ `All | `Partial ] `` (default `` `All ``) and `?diagnostics`. By default the parse fails with `Expected End_of_input` when input remains. `parse_until_end s p` is now `parse s p`, and `parse_source_until_end` is now `parse_source`. To parse a leading part and ignore the rest, which was what `parse s p` did in 0.4, pass `` ~consume:`Partial ``. Removed: `parse_until_end`, `parse_source_until_end`, the `result` alias (it shadowed `Stdlib.result` under `open Parseff`), `diagnostic`, `error_with_diagnostics` and `result_with_diagnostics`.
- **Add `render_error` and `error_message`.** `render_error ?name ?source ?custom e` prints a `name:line:col: [error] message` header, the source line with carets under the failure, a `╭─╯` and `╰──^` bar that joins the opening token to the caret for the construct that started on an earlier line, `* while parsing` lines for the open `context`s, and notes. Control bytes, invalid UTF-8 and C1 controls are escaped in the excerpt as in the message, tabs keep the carets aligned, long lines are cut to 240 bytes around the failure, and the columns of the header and of the `* while parsing` lines count Unicode characters, while `location.col` counts bytes. When streaming has dropped the start of the line and `~source` is not given, the header is `name: byte N: [error] ...` and the context line says `from byte N`.
- **Failures at the end of the input keep what was expected.** `[1, 2` reports `expected ',' or ']', found end of input` instead of the old end-of-input error without items, including through `expect` and `one_of_labeled`.
- **A successful `look_ahead` forgets the failures of its probe.** `look_ahead (many (char 'a')); char 'b'` on `aac` reports `expected 'b'` at offset 0 instead of the probe's `expected 'a'` at offset 2. A failing `look_ahead` is a failure like any other and merges with the earlier ones.
- **`commit` forgets earlier failures only if it sealed a frame**, and `expect` keeps relabeling correctly when a `commit` ran inside it.
- **Expected items past 64 are counted.** An error that expected more than 64 alternatives at one position keeps 64 distinct ones and counts the rest in `Expected.more`; duplicates are removed first, so they never hide an alternative. `expect` and `one_of_labeled` stay correct when the set overflows.
- **A truncated read says how much is left.** `take 5` on `abc` reports `expected 5 bytes, found only 3 (end of input)`, and `found end of input` when nothing is left. `found` is `Short 3` or `End` for it.
- **Fix the error report of a streaming window overflow and of `~from` or notes that point before the buffer.** The first raised `Invalid_argument "String.sub"` when the window was exceeded on a long line. `~from` older than the buffered input keeps its position, and `render_error` prints `byte N` instead of a line it cannot know.
- **Fix the excerpt:** a line cut at 240 bytes always ends with `...`, a lone carriage return at the end of the input is not shown, and C1 control characters are escaped in messages.
- **Breaking: removed API.** Each removed item has a replacement:
  - The type `backtrack_window_error` is removed. `Backtrack_window_exceeded` is an inline record `{ limit; oldest }`, so patterns `Backtrack_window_exceeded { limit; _ }` replace `Backtrack_window_exceeded info`.
  - `catch` is removed. It caught only `fail`, never `Expected`, and dropped `~from` and `~notes`. Check the condition before calling `fail`, or use `optional` and `or_` on parsers whose failure is an `Expected`.
  - `Utf8.digit` is removed. Use `digit`, which parses the same ASCII digits.
  - `BE.any_uint8`, `BE.any_int8`, `LE.any_uint8` and `LE.any_int8` become `any_uint8` and `any_int8` at the top level.
- **Breaking: `take` with a negative count raises `Invalid_argument "Parseff.take: count must be non-negative"`** instead of failing the parse.
- **`Backtrack_window_exceeded` reports the parser position.** Its error position, line and excerpt are those of the parser when the window was exceeded, not the end of the read that did not fit, and the open `* while parsing` lines follow the usual rule.
- **Polish the error output.**
  - A streaming error whose line is not known, such as a `~from` older than the buffered input, prints `input: byte N: [error] ...` with no excerpt instead of a line and column computed from the partial buffer. `render_error` also indents the `*` rows by two spaces then.
  - A mismatch of `BE.int16`, `BE.int32`, `BE.int64` and the `LE` versions reports `expected int16be 0x0001, found 0x0002`: the endianness is in the label and `found` is the integer read in the same width. `found` is `Value` for it.
  - `'` in a quoted character is escaped as `'\''`, a non-UTF-8 byte from `char '\xE9'` prints as `'\xE9'`, and a truncated `found` is `'abcdef'...`.
  - Non-breaking spaces, format characters, line and paragraph separators, and a combining mark at the start of the quoted text are escaped as `\u{...}` in `found` and in expected items.
  - A `Custom` error without a `~custom` printer says `custom error (no ~custom printer given)`.
  - A message lists at most 8 expected items and then `, or N more alternatives` (`or 1 more alternative` for one). `Expected.more` holds the count of alternatives past the 64 kept.
  - The excerpt shows control bytes, invalid UTF-8 and C1 controls as the escapes of the message, and the carets and the corner bar count the escape.
  - The rows the renderer adds never end with a space, including empty source rows and an empty message. Source text is printed byte for byte, so a source line with trailing spaces keeps them.
  - `Depth_limit_exceeded` and `Backtrack_window_exceeded` keep the `* while parsing` lines that were open and end with `* help: raise ~max_depth` or `* help: call commit or cut earlier, or raise ~backtrack_window`.
  - `* ... and 1 more` prints the frame instead, and the `...` row for hidden lines is aligned with the gutter.
  - The label of a regex without `~label` is `text matching the pattern`, and `one_of []` expects nothing: alone it fails with `Failure "no alternative matched"`, and next to other failures at the same position it adds nothing to them.
- **Add `?label` to `match_regex`.** A failed match reports `expected <label>` and merges with the other alternatives expected at the same position. Without `~label` a failed match expects `text matching the pattern`.
- **Add `?from` and `?notes` to `fail` and `error`.** `~from` makes the error cover the bytes from `from` to the current position, and `~notes` attaches annotations: a note with a range is underlined in the error excerpt, a note without one is printed as a `* note:` line. Plain calls behave as before. The error of a `fail` also lists the `context`s it unwound through.
- **Add `context`.** `context ?delimited name parser` runs `parser` inside a named construct. A failure inside it remembers the open constructs, outermost first, with their start offsets, for the error report. A parser that never calls `context` pays nothing; a call costs about 100 instructions.
- **Add `cut`.** `cut ()` declares the running construct decided. It seals every `or_`, `one_of`, `many`, `sep_by`, `optional` and `fold_*` frame opened since the innermost enclosing `context`, and stops at `look_ahead`. If that `context` then fails, the frames outside it are sealed too, so `many (fun () -> context "statement" (fun () -> consume "let"; cut (); ...))` reports the error of a broken statement instead of stopping the loop and ignoring the rest of the input. `commit ()` still seals the nearest frame only.
- **Report the furthest failure.** A failed parse now returns the error at the highest position where any primitive failed, also inside branches that `or_`, `many`, `sep_by`, `optional` or `fold_left` abandoned. `[1, 2, 3,]` reports the missing value at the `]`, and `sep_by (consume "ab") (char ',')` followed by `end_of_input` on `ab,ab,ax` reports the `x` at offset 7 instead of the trailing input at offset 5. Code that compares `pos` or the message of an error sees different values.
- **Merge, sort and deduplicate what was expected.** Failures at the same position give one message: `expected "else", "if", or "while"` instead of `expected "if" or expected "else" or expected "while"`, and `or_ digit digit` says `expected digit` once. The order of the branches does not change the message. Characters print as `'x'`, literals as `"abc"`.
- **`consume` fails at the first byte that differs.** `consume "ab"` on `ax` fails at offset 1 with `expected "b"`, and `consume "hello"` on `hel` fails at offset 3 with `expected "lo", found end of input`. Both used to report offset 0. When fewer bytes remain than the literal needs but the available bytes already differ, `consume` fails at the first differing byte, so `or_` composes the messages of its branches. Only input that ends inside a prefix of the literal has `found` set to `End`. A mismatch inside a multi-byte character is reported at the start of the character: `consume "é"` on `è` fails at the character, expecting `"é"`.
- **`expect` and `one_of_labeled` relabel only failures at their start.** A parser that got past its first byte keeps its own, more precise message. `expect "key" (fun () -> consume "id:"; digit ())` on `id:x` reports `expected digit` at offset 3. `expect` and `take_while ~label` messages now start with `expected`, and `one_of_labeled` reports `expected number or string` instead of `expected one of: number, string`.
- **Report a custom error from an abandoned branch when it is the furthest failure.** `error` still lets `or_`, `many` and `optional` backtrack. At the same position a custom error wins over `Expected`, and the first of two custom errors wins. It used to be the second.
- **`commit` forgets the failures at or beyond its position.** A committed branch reports only what it expected itself, not the failure of the previous branch.
- **`take_while ~at_least` without `~label` says `expected at least n characters`** instead of `take_while`.
- **`Utf8.char` no longer formats a label on every call.** Its failure reports the expected character, formatted only when an error is built.
- **Reduce the cost of failures.** Primitives record the furthest failure in the parse state instead of allocating an exception that carries a message, `or_` reuses its rewind frame for the right branch, and a finished parse allocates one closure less.
- **Improve runtime performance.** Combinators read the parse state without a `Domain.DLS` lookup, backtracking frames no longer allocate, internal failures skip backtrace recording, and streaming compaction no longer rebuilds line starts.
- **Fix `BE.int16` and `LE.int16` for values above `0x7FFF`.** They compared the signed result of `any_int16`, so `BE.int16 0xFFFE` never matched `"\xFF\xFE"`. They now compare the 16 bits, accept `-32768` to `65535` and raise `Invalid_argument` outside that range. `int16`, `int32` and `int64` mismatches are now reported at the start of the field instead of after it.
- **Fix concurrent parses on one domain.** Two `parse_source` calls whose `read` suspends, for example in two Eio fibers, corrupted each other's state and raised `Invalid_argument "Parseff.State.pop_rewind_frame"`. Parseff now installs the parse state again after every `read`, and a finished parse no longer leaves its state behind. Only `read` may suspend; a suspension inside a parser callback is still not supported.
- **Fix streaming memory growing with the input.** Compaction kept one line index entry per newline for the whole stream. The index now keeps only the retained buffer and a count of dropped lines, so `location` stays correct and memory is bounded by the window.
- **Fix `match_regex` hiding a full backtrack window.** When the window was full and the source had not ended, the regex ran on the short buffer and failed, so `or_` took another branch. It now returns a match that ends before the buffered end, and otherwise fails with `` `Backtrack_window_exceeded ``, as `take` does, because more input could change the result.
- **Check streaming arguments.** `parse_source` raises `Invalid_argument` for `~backtrack_window` below `2`, `Source.of_channel` for `~buf_size` below `1`, and a parse for a `read` result outside `0..len`. A window of `0` or `1` used to report a false end of input.
- **Fix streaming documentation.** `span` is an owned copy in streaming, `match_regex` reads ahead half a window instead of retrying at chunk boundaries, and a source is for one parse.
- **Fix `expect` and `one_of_labeled` relabeling custom errors.** A custom error raised through an `or_` or `one_of` with several branches was replaced by the `expect` or `one_of_labeled` message. It now passes through unchanged, as documented.
- **Fix `many`, `sep_by`, `end_by` and `fold_left` looping forever on parsers that consume no input.** The loop now stops after an iteration that consumes nothing and drops that iteration's result and diagnostics. `~at_least` still applies to the kept results.
- **Fix `take n` raising `Invalid_argument` for a very large `n`.** The length check overflowed. `take` now fails with an `Expected` error whose `found` is `Short` or `End`.
- **Document the cost of `match_regex` failures.** `Re` searches forward when the regex does not match at the current position, so a failed match scans the rest of the input and a loop of failing matches takes quadratic time.

### Migrating runners, `error` and `warn`

| Before | After |
|---|---|
| `parse_until_end s p` | `parse s p` |
| `parse_source_until_end src p` | `parse_source src p` |
| `parse s p` | `` parse ~consume:`Partial s p `` |
| `parse_source src p` | `` parse_source ~consume:`Partial src p `` |
| `Ok (v, ds)` and `Error { error; diagnostics }` | `Ok v` and `Error e`; read the diagnostics from the collector |
| `warn x` | `let d = Diagnostics.create ()`, `parse ~diagnostics:d`, `warn d x` |
| `warn_at ~pos x` | `warn_at d ~pos x` |
| `'d Parseff.diagnostic = { pos; diagnostic }` | `'d Parseff.Diagnostics.entry = { pos; value }` |
| `` error (`Too_large n) `` | `type Parseff.custom_error += Too_large of int`, then `error (Too_large n)` |
| `` Error { pos; error = `Too_large n } `` | `Error { kind = Custom (Too_large n); _ }`, with a final `_` case |
| `('a, 'e) Parseff.result` | `('a, Parseff.error) result` |

### Migrating from 0.4

The common case is one pattern. Replace the match on the built-in variants by a match on the error value:

```ocaml
(* 0.4 *)
match Parseff.parse input parser with
| Ok v -> v
| Error { pos; error = `Expected msg } -> Printf.eprintf "%d: %s\n" pos msg; exit 1
| Error { pos; error = `Unexpected_end_of_input } -> Printf.eprintf "%d: end of input\n" pos; exit 1
| Error _ -> exit 1

(* 0.5 *)
match Parseff.parse input parser with
| Ok v -> v
| Error e -> prerr_endline (Parseff.render_error ~name:"input" e); exit 1
```

- `Error { pos; error }` becomes `Error e`. The position is `e.range.start`, the message is `Parseff.error_message e`.
- Custom errors move into `kind`: `` Error { error = `Too_large n; _ } `` becomes `Error { kind = Custom (Too_large n); _ }`, with `type Parseff.custom_error += Too_large of int`.
- `` `Failure msg `` becomes `Failure msg`. The 0.4 messages of primitives that were not expectations (invalid UTF-8, a failed `match_regex` without a label, a mismatched `BE.int16`) are `Expected` items now (`valid UTF-8`, `text matching the pattern`, `int16be 0xFFFE`), so they merge with the alternatives expected at the same position.
- `` `Expected msg `` becomes `Expected { items; found; more }`. `Parseff.error_message e` gives `expected A, B, or C, found X` instead of `expected A or expected B`, sorted and without duplicates.
- `` `Unexpected_end_of_input `` becomes `Expected { found = End; _ }` and now has `items`.
- `` `Depth_limit_exceeded msg `` becomes `Depth_limit_exceeded { limit }`, and `` `Backtrack_window_exceeded info `` becomes `Backtrack_window_exceeded { limit; oldest }`.
- `Parseff.Ok` and `Parseff.Error` are `Stdlib.Ok` and `Stdlib.Error`. Code that writes `Ok v` and `Error e` keeps compiling.
- `parse_until_end` and `parse_source_until_end` become `parse` and `parse_source`. `parse` and `parse_source` now require all input to be consumed unless you pass `` ~consume:`Partial ``. Diagnostics are read from a `Diagnostics.t` collector. See the table above.
- Tests that compared error strings change: messages now name what was expected at the furthest failure, and `consume` fails at the first differing byte.

## 0.4.0

- **Breaking: streaming now uses a bounded sliding buffer.** `parse_source` and `parse_source_until_end` accept `~backtrack_window` (default `65536`) and fail with `` `Backtrack_window_exceeded of backtrack_window_error `` when a parser needs to keep more input than that for backtracking. This includes a single `take_while` or `take_while_span` token longer than the window. Exhaustive matches on the error type must handle the new case. Raise `~backtrack_window` if a parser needs more.
- **Add `commit`.** `commit ()` seals the innermost choice or repetition frame, improving error locality and allowing streaming sources to drop older buffered input earlier. It does not release outer frames: a `commit` inside the `or_` of `many (fun () -> or_ a b ())` leaves the `many` frame in place.
- **Breaking: streaming spans may be owned copies.** `take_while_span` and `sep_by_take_span` keep zero-copy behavior for `parse`, but `parse_source` may materialize owned spans so bounded streaming can reclaim old prefixes.
- **Improve runtime performance and reduce allocation.** Parseff now uses a lower-overhead runtime internally, making generic parsers and backtracking-heavy workloads faster while keeping the API largely unchanged.
- **Add `peek_char`.** Provides cheap single-character lookahead without backtracking.
- **Fix streaming results that differed from `parse`.** `Utf8.take_while` and related scans now report a truncated UTF-8 sequence at the end of input. `match_regex` now finds matches that need more than the buffered input, and `Re.bol` and word boundaries see the byte before the current position.
- **Fix `location` line numbers after backtracking.** Lines were counted twice when a parser asked for `location` after backtracking to an earlier position.
- **`match_regex` reads ahead in streaming mode.** It reads until about half the backtrack window is buffered ahead before it matches, so on an interactive source it waits for that input or the end of the source. A match cannot extend past the window.

## 0.3.0

- **Add `Source.of_seq`.** Creates a source from a `string Seq.t`.
- **Add `Source.of_chunks`.** Creates a source from a `(unit -> string option)` pull function.
- **`fail` now produces `` `Failure of string `` instead of `` `Expected ``.**
- **Add `catch` combinator.** `catch parser handler` intercepts `` `Failure `` errors, allowing recovery within backtracking contexts.
- **Add `Parseff.Utf8` module for Unicode-aware parsing.** `satisfy`, `char`, `any_char`, `take_while`, `skip_while`, `letter`, `digit`, `alphanum`, `whitespace` and `take_while_span`.
- **Add `Parseff.BE` and `Parseff.LE` binary parsing primitives.** Big-endian and little-endian readers for `any_uint8`, `any_int8`, `any_int16`, `any_uint16`, `any_int32`, `any_int64`, `any_float`, `any_double`, and validators `int16`, `int32`, `int64`.
- **Add `location` and `location_of_position` for line/column tracking.** `location ()` returns `{ offset; line; col }` during parsing (lazy, incremental, zero cost if unused). `location_of_position input pos` converts a byte offset after parsing — useful for error reporting.
- **BE/LE exact-match validators produce `` `Expected `` instead of `` `Failure ``.** `BE.int16`, `BE.int32`, `BE.int64` (and LE equivalents) are parser expectations, not user validation failures.
- **`or_` now composes errors from both branches.** When both branches fail at the same position, `Expected` messages are joined with `" or "` (e.g., `expected "foo" or expected "bar"`). When branches fail at different positions, the error from the branch that consumed more input is kept. This also improves `one_of`, which chains `or_` and now naturally produces composed messages across all alternatives.
- Register a printer for `Unhandled effects`, warning the user about a badly organized parser function

## 0.2.0

- **Unified `*1` variants into base combinators with `~at_least` parameter.** `many1` → `many ~at_least:1`, `take_while1` → `take_while ~at_least:1`, `sep_by1` → `sep_by ~at_least:1`, `end_by1` → `end_by ~at_least:1`, `whitespace1` → `whitespace ~at_least:1`. The `~at_least` parameter generalizes beyond the binary zero-vs-one distinction (e.g., `many ~at_least:3` requires at least three matches). The old `chainl1` and `chainr1` were also removed in favor of making the `default` argument optional (see next item).
- **Renamed `chainl`/`chainr` to `fold_left`/`fold_right` with `~otherwise`.** `chainl1 p op` → `fold_left p op`, `chainl p op default` → `fold_left p op ~otherwise:default`, and likewise for `chainr`/`fold_right`. The new names better describe what the combinators do (parse and fold with associativity) and align with OCaml's `List.fold_left`/`List.fold_right` naming conventions. The `~otherwise` labeled argument replaces the old positional `default` parameter, providing a fallback value when zero elements match.

## 0.1.0

Initial release of Parseff
