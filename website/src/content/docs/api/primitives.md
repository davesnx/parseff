---
title: Primitives
description: Basic parsing operations in Parseff
---

<!-- This file is generated from doc/primitives.mld. Do not edit directly. -->


# Primitives

Primitives are the building blocks of all parsers. These operations match input directly and form the foundation of more complex parsers.


## String and character matching


### `consume`

`Parseff.consume` matches an exact literal string. Returns the matched string.

```ocaml
val consume : string -> string
```
```ocaml
let parser () =
  let _ = Parseff.consume "hello" in
  Parseff.consume "world"

(* Matches "helloworld" *)
```
**Performance:** `consume` is optimized for literal string matching. It's much faster than using regex for exact matches.


### `char`

`Parseff.char` matches an exact character. Returns the matched character.

```ocaml
val char : char -> char
```
```ocaml
let comma () = Parseff.char ','
let left_paren () = Parseff.char '('
let right_paren () = Parseff.char ')'

(* Parse a comma-separated pair *)
let pair () =
  let _ = left_paren () in
  let a = Parseff.digit () in
  let _ = comma () in
  let b = Parseff.digit () in
  let _ = right_paren () in
  (a, b)

(* Matches "(1,2)" -> (1, 2) *)
```
**Tip:** Use `char` for single characters and `consume` for multi-character strings. They both match literals, but `char` makes the intent clear and avoids a string allocation for a single byte.


### `satisfy`

`Parseff.satisfy` matches a character satisfying the given predicate. The `~label` parameter is used in error messages.

```ocaml
val satisfy : (char -> bool) -> label:string -> char
```
```ocaml
(* Match any vowel *)
let vowel () =
  Parseff.satisfy
    (fun c -> String.contains "aeiouAEIOU" c)
    ~label:"vowel"

(* Match any digit *)
let digit_char () =
  Parseff.satisfy
    (fun c -> c >= '0' && c <= '9')
    ~label:"digit"

(* Match any uppercase letter *)
let uppercase () =
  Parseff.satisfy
    (fun c -> c >= 'A' && c <= 'Z')
    ~label:"uppercase letter"
```
**Tip:** The label parameter is shown in error messages. Prefer descriptive labels.


## Character scanning

These operations scan multiple characters efficiently.


### `take_while`

`Parseff.take_while` consumes characters while the predicate holds. Returns the matched string (may be empty). Always succeeds.

```ocaml
val take_while : (char -> bool) -> string
```
```ocaml
(* Parse digits *)
let digits () = Parseff.take_while (fun c -> c >= '0' && c <= '9')

(* Parse identifier *)
let identifier () =
  let first =
    Parseff.satisfy
      (fun c -> c = '_' || (c >= 'a' && c <= 'z'))
      ~label:"letter"
  in
  let rest =
    Parseff.take_while (fun c ->
        c = '_' || (c >= 'a' && c <= 'z') || (c >= '0' && c <= '9'))
  in
  String.make 1 first ^ rest

(* "foo_bar123" -> "foo_bar123" *)
(* "123abc" -> identifier fails (first char must be letter) *)
```

### `take_while ~at_least:1`

`Parseff.take_while` with `~at_least:1` requires at least one character. Fails if no characters match.

```ocaml
val take_while : ?at_least:int -> ?label:string -> (char -> bool) -> string
```
```ocaml
(* Parse non-empty digits *)
let digits1 () =
  Parseff.take_while ~at_least:1 (fun c -> c >= '0' && c <= '9') ~label:"digit"

(* Parse identifier (simpler than take_while approach) *)
let identifier1 () =
  Parseff.take_while ~at_least:1
    (fun c -> c = '_' || (c >= 'a' && c <= 'z') || (c >= '0' && c <= '9'))
    ~label:"identifier character"

(* Matches "foo123" -> "foo123" *)
(* Fails on "" -> Error: expected identifier character *)
```

### `skip_while`

`Parseff.skip_while` skips characters while the predicate holds (returns unit). Always succeeds. More efficient than `Parseff.take_while` when you don't need the matched string.

```ocaml
val skip_while : (char -> bool) -> unit
```
```ocaml
(* Skip spaces *)
let skip_spaces () = Parseff.skip_while (fun c -> c = ' ')

(* Parse comma-separated values *)
let csv_value () =
  skip_spaces ();
  let value = Parseff.take_while (fun c -> c <> ',' && c <> '\n') in
  skip_spaces ();
  value

(* "  hello  " -> "hello" *)
```
**Tip:** If you don't need the matched string, prefer `skip_while` over `take_while` to avoid allocating.


## Regular expressions


### `match_regex`

`Parseff.match_regex` matches a compiled regular expression. The regex must be compiled with `Re.compile`.

```ocaml
val match_regex : Re.re -> string
```
```ocaml
(* Pre-compile at module level. Never inside a parser function *)
let number_re = Re.compile (Re.Posix.re "[0-9]+")
let number_from_re () = Parseff.match_regex number_re
```
**Danger:** Always pre-compile regexes at module level. Compiling inside a parser function recompiles on every call (100x+ slower). For simple character classes, prefer `Parseff.take_while` which avoids regex overhead entirely:

```ocaml
(* Regex. Use for complex patterns (alternation, grouping) *)
let number_re = Re.compile (Re.Posix.re "[0-9]+")
let number () = Parseff.match_regex number_re

(* take_while. No regex overhead for simple predicates *)
let number () =
  Parseff.take_while ~at_least:1 (fun c -> c >= '0' && c <= '9') ~label:"digit"
```

## Control flow


### `fail`

`Parseff.fail` aborts parsing with an error message.

```ocaml
val fail : string -> 'a
```
```ocaml
(* Validate range *)
let byte () =
  let n =
    int_of_string
      (Parseff.take_while ~at_least:1
         (fun c -> c >= '0' && c <= '9')
         ~label:"digit")
  in
  if n >= 0 && n <= 255 then n
  else Parseff.fail "number must be between 0 and 255"

(* Parse: "128" -> 128 *)
(* Parse: "300" -> Error: number must be between 0 and 255 *)
```

### `error`

`Parseff.error` aborts parsing with a user-defined error value. Custom errors are caught by `Parseff.parse` and returned in the result.

```ocaml
val error : 'e -> 'a
```
```ocaml
let validated_number () =
  let s =
    Parseff.take_while ~at_least:1 (fun c -> c >= '0' && c <= '9') ~label:"digit"
  in
  let n = int_of_string s in
  if n < 0 then Parseff.error (`Negative n)
  else if n > 255 then Parseff.error (`Out_of_range n)
  else n

let () =
  match Parseff.parse "300" validated_number with
  | Ok n -> Printf.printf "Got %d\n" n
  | Error { error = `Out_of_range n; _ } ->
      Printf.printf "%d is too large (max 255)\n" n
  | Error { error = `Negative n; _ } ->
      Printf.printf "%d is negative\n" n
  | Error _ -> Printf.printf "Parse error\n"
```
**Tip:** Polymorphic variants work great with `error` for quick error types:

```ocaml
let number_checked () =
  let s =
    Parseff.take_while ~at_least:1 (fun c -> c >= '0' && c <= '9') ~label:"digit"
  in
  let n = int_of_string s in
  if n > 255 then Parseff.error `Too_large
  else if n < 0 then Parseff.error `Negative
  else n
```

### `end_of_input`

`Parseff.end_of_input` succeeds only if no input remains. Use this to ensure the entire input has been consumed.

```ocaml
val end_of_input : unit -> unit
```
```ocaml
(* Parse complete input *)
let complete_number () =
  let n = Parseff.digit () in
  Parseff.end_of_input ();
  n

(* Matches: "5" -> 5 *)
(* Fails: "52" -> Error: expected end of input *)

(* Without end_of_input, "52" would succeed and return 5,
   leaving "2" unparsed *)
```
**Caution:** Use `Parseff.end_of_input` when you want to ensure all input is consumed. Without it, `parse` happily returns a result even if there's leftover input.


### `position`

`Parseff.position` returns the current parser offset in bytes from the start of the input.

```ocaml
val position : unit -> int
```

### `location`

`Parseff.location` returns the current position as a `Parseff.location` record with byte offset, line number, and column. Lines and columns are 1-based. Columns count bytes, not characters (relevant for multibyte UTF-8 input).

```ocaml
type location = { offset : int; line : int; col : int }
val location : unit -> location
```
**Performance:** The line index is built lazily on the first call and extended incrementally on subsequent calls. If you never call `location`, no scanning overhead is incurred.

`Parseff.location_of_position` does the same conversion outside a parser. Pass the original input string and a byte offset (e.g. from an error result) to get line and column information.

```ocaml
val location_of_position : string -> int -> location
```
```ocaml
(* Annotate AST nodes with source spans *)
type expr =
  | Num of int * Parseff.location
  | Add of expr * expr

let number () =
  let loc = Parseff.location () in
  let digits = Parseff.many ~at_least:1 Parseff.digit () in
  let n = List.fold_left (fun acc d -> (acc * 10) + d) 0 digits in
  Num (n, loc)

(* Convert error positions after parsing *)
let report_error input result =
  match result with
  | Parseff.Error { pos; error = `Expected msg } ->
      let loc = Parseff.location_of_position input pos in
      Printf.printf "line %d, col %d: expected %s\n" loc.line loc.col msg
  | _ -> ()
```
**Tip:** Use `location` during parsing to attach source positions to AST nodes or other parsed structures. Use `location_of_position` after parsing to convert byte offsets from error results into line and column numbers for diagnostics.
