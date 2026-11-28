---
title: Error handling
description: Building better error messages in Parseff
---

<!-- This file is generated from doc/errors.mld. Do not edit directly. -->

# Error handling

Parseff exposes four main APIs for errors:

- [`Parseff.fail`](./Parseff.md#val-fail): fail with a string message
- [`Parseff.error`](./Parseff.md#val-error): fail with a typed [`Parseff.custom_error`](./Parseff.md#type-custom_error) value
- [`Parseff.expect`](./Parseff.md#val-expect): relabel `Expected` failures at a parser's start
- [`Parseff.context`](./Parseff.md#val-context) and [`Parseff.cut`](./Parseff.md#val-cut): name constructs and seal backtracking

## Built-in and custom errors

**Built-in errors** come from Parseff itself: primitive mismatches (`Expected`), messages from [`Parseff.fail`](./Parseff.md#val-fail) (`Failed`), or limit checks (`Limit_exceeded`, `Depth_limit_exceeded`, `Backtrack_window_exceeded`). [`Parseff.render_error`](./Parseff.md#val-render_error) formats all of them out of the box.

**Custom errors** are constructors you add to the extensible variant [`Parseff.custom_error`](./Parseff.md#type-custom_error), raised with [`Parseff.error`](./Parseff.md#val-error) and returned in `kind` as `Custom`. Use them when callers need to match on domain-specific validation failures in code. Pass `~custom` to [`Parseff.render_error`](./Parseff.md#val-render_error) to format them.

```ocaml
type Parseff.custom_error += Out_of_range of int

let rating () =
  let n = Char.code (Parseff.digit ()) - Char.code '0' in
  if n < 1 || n > 5 then Parseff.error (Out_of_range n) else n

let () =
  match Parseff.parse "7" rating with
  | Ok n -> Printf.printf "Got %d\n" n
  | Error { kind = Custom (Out_of_range n); _ } ->
      Printf.printf "%d is out of range\n" n
  | Error e -> prerr_endline (Parseff.render_error e)
```

## `fail` and `error`

Both [`Parseff.fail`](./Parseff.md#val-fail) and [`Parseff.error`](./Parseff.md#val-error) fail at the current position and participate in normal backtracking: [`Parseff.or_`](./Parseff.md#val-or_), [`Parseff.many`](./Parseff.md#val-many), [`Parseff.optional`](./Parseff.md#val-optional), and other combinators absorb them and try the next alternative. Call [`Parseff.cut`](./Parseff.md#val-cut) first to make a failure fatal.

```ocaml
val fail :
  ?from:int -> ?upto:int -> ?notes:note list -> ?help:string -> string -> 'a

val error :
  ?from:int -> ?upto:int -> ?notes:note list -> ?help:string -> custom_error -> 'a
```

- `~from` and `~upto` (both default to the current position) set the underlined byte range.
- `~notes` attaches annotations: a note with `at = Some range` underlines that range in the excerpt; `at = None` prints a `* note:` line.
- `~help` prints a `* help:` line after any notes.

```ocaml
let octet () =
  let start = Parseff.position () in
  let n =
    int_of_string
      (Parseff.take_while ~at_least:1 ~label:"digit" (fun c ->
           c >= '0' && c <= '9'))
  in
  if n <= 255 then n
  else (
    Parseff.cut ();
    Parseff.fail ~from:start
      ~notes:[ { Parseff.at = None; text = "octets go from 0 to 255" } ]
      "octet out of range")

let () =
  match
    Parseff.parse "10.300" (fun () ->
        let a = octet () in
        let _ = Parseff.char '.' in
        (a, octet ()))
  with
  | Error e -> print_endline (Parseff.render_error ~name:"address" e)
  | Ok _ -> ()
```

Output:

```
address:1:4: [error] octet out of range
1 | 10.300
  |    ^^^
  * note: octets go from 0 to 255
```

[`Parseff.expected_token`](./Parseff.md#val-expected_token) fails with an `Expected` item instead, for a token that a separate lexer found or any range the parser scanned: `found` and the range cover the bytes up to `~upto`, the item merges with other expected items at the same position, and [`Parseff.expect`](./Parseff.md#val-expect) relabels it. It takes `~help` and `~notes` like `fail`.

## `expect`

[`Parseff.expect`](./Parseff.md#val-expect) replaces `Expected` failures at the parser's starting position with `expected <description>`. Failures past the starting position, as well as `fail` and `error`, are preserved.

```ocaml
val expect : string -> (unit -> 'a) -> 'a
```

```ocaml
let dot () = Parseff.expect "a dot separator" (fun () -> Parseff.char '.')
```

## `context` and `cut`

[`Parseff.context`](./Parseff.md#val-context) names a grammar construct so failures inside it report `* while parsing <name>`. [`Parseff.cut`](./Parseff.md#val-cut) seals backtracking frames inside the enclosing `context` so a later failure becomes the parse error instead of retrying outer alternatives. See [`combinators`](./combinators.md).

To collect multiple errors and keep parsing, see the [Error recovery](./error-recovery.md) guide.

## Furthest-failure selection

When a parse fails, Parseff reports the failure at the highest byte offset reached across all branches, including branches abandoned by `or_`, `many`, `sep_by`, `optional`, or `fold_*`:

- Multiple primitive mismatches at that position merge into one sorted, deduplicated `Expected` list: `expected "else", "if", or "while", found 'x'`.
- At the same position, `fail` and `error` win over `Expected`; between two `fail`/`error` calls, the first wins.
- `literal` fails at the first differing byte and expects the remaining suffix.
- End-of-input failures preserve what was expected: `"[1, 2"] reports [expected ',' or ']', found end of input`.
- [`Parseff.commit`](./Parseff.md#val-commit) discards failures at or beyond its position so a committed branch reports only its own expectations.

## The `error` value

```ocaml
type error = private {
  severity : severity;
  kind : error_kind;
  range : range;
  location : location option;
  contexts : context list;
  notes : note list;
  help : string option;
  excerpt : excerpt;
}
```

`error` and its nested types are `private` (readable and matchable, not constructible), except [`Parseff.range`](./Parseff.md#type-range) and [`Parseff.note`](./Parseff.md#type-note).

`kind` cases:

- `Expected { items; found; more }`: primitive mismatch. `items` holds up to 64 sorted, unique expectations (`Char`, `Literal`, `Label`, `End_of_input`, `End_of_frame`); `more` counts overflow items. `found` is `Text` (input snippet), `End` (EOF), `Short n` (short fixed-width read), or `Value` (mismatched binary integer).
- `Failed msg`: raised by [`Parseff.fail`](./Parseff.md#val-fail), or `"no alternative matched"` for `one_of []`.
- `Custom e`: raised by [`Parseff.error`](./Parseff.md#val-error). Match with a fallback `_` case.
- `Limit_exceeded { limit }`: exceeded `~at_most` on `take_while`, `skip_while`, `Slice.take_while`, `take_until`, `skip_until`, `Utf8.take_while`, `Utf8.skip_while`, `Utf8.Slice.take_while`, `many`, `sep_by`, or `end_by`. Fatal like `cut (); fail`: no enclosing choice tries another option, up to the nearest [`Parseff.look_ahead`](./Parseff.md#val-look_ahead), [`Parseff.peek_with`](./Parseff.md#val-peek_with) or [`Parseff.capture`](./Parseff.md#val-capture). Use `capture` to recover; with `~release:true` it resumes after the item that crossed the limit.
- `Depth_limit_exceeded { limit }`: exceeded `~max_depth` on [`Parseff.rec_`](./Parseff.md#val-rec_) or stack depth in [`Parseff.fold_right`](./Parseff.md#val-fold_right).
- `Backtrack_window_exceeded { limit; oldest }`: exceeded `~backtrack_window` in a streaming parse.

## Rendering errors

[`Parseff.render_error`](./Parseff.md#val-render_error) formats an error with its source excerpt, carets, notes, help, and open constructs; [`Parseff.error_message`](./Parseff.md#val-error_message) returns the one-line message alone.

```
input.json:4:5: [error] expected ',' or '}', found '"'
2 |     "config": {
  | ╭─────────────╯
3 | │     "a": 1
4 | │     "b": 2
  | ╰─────^
  * while parsing object, from 2:13
  * while parsing object, from 1:1
```

Formatting details:

- **Header**: `name:line:col: [error] message` (or `line:col` without `~name`; `byte N` with no excerpt when the line is unknown in streaming). Lists up to 8 expected items before `, or N more alternatives`.
- **Columns and carets**: header and context columns count Unicode code points, while `location.byte_col` counts bytes. Carets and note underlines count terminal cells (wide characters like `名` take two, combining marks take zero), preserve tabs, and cover escaped control/invalid UTF-8 bytes (`\x1B`, `\u{00A0}`).
- **Streaming dropped prefixes**: when streaming dropped the start of the error line and `~source` is omitted, the header still reports the code-point column unless the error starts inside the dropped prefix (in which case it falls back to `location.byte_col`, and contexts in that prefix show `from byte N`). The excerpt prefixes `...` and places carets only under retained bytes. Pass `~source` if you have the full input.
- **Sources**: line and column restart at each [`Parseff.Source.parse`](./Parseff-Source.md#val-parse), so for its errors pass as `~source` the input from [`Parseff.Source.offset`](./Parseff-Source.md#val-offset), read before the parse, onward.
- **Contexts**: prints up to 5 innermost `* while parsing` lines (plus one more rather than hiding a single line). Delimited constructs spanning lines render the `╭─╯` / `│` / `╰──^` bar, collapsing multi-line gaps to `...`.
- **Notes and help**: ranged notes underline with `-`; unranged notes print as `* note:`; `~help` (and built-in hints for depth/window limits) prints as `* help:`.
- **Custom errors**: pass `~custom`; without it, `Custom` prints `custom error (no ~custom printer given)`.
