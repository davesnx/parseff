---
title: Core
description: Basic parsing operations in Parseff
---

<!-- This file is generated from doc/core.mld. Do not edit directly. -->

# Core

Core primitives are the building blocks of all parsers. These operations match input directly and form the foundation of more complex parsers.


## String and Character Matching


### `consume`

```ocaml
val consume : string -> string
```
`Parseff.consume` matches an exact literal string. Returns the matched string.

```ocaml
let parser () =
  let _ = Parseff.consume "hello" in
  Parseff.consume "world"

(* Matches "helloworld" *)
```
**Performance:** `consume` is optimized for literal string matching. It's much faster than using regex for exact matches.


### `char`

```ocaml
val char : char -> char
```
`Parseff.char` matches an exact character. Returns the matched character.

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

```ocaml
val satisfy : (char -> bool) -> label:string -> char
```
`Parseff.satisfy` matches a character satisfying the given predicate. The `~label` parameter is used in error messages.

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


## Character Scanning

These operations scan multiple characters efficiently.


### `take_while`

```ocaml
val take_while : (char -> bool) -> string
```
`Parseff.take_while` consumes characters while the predicate holds. Returns the matched string (may be empty). Always succeeds.

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

### `take_while1`

```ocaml
val take_while1 : (char -> bool) -> label:string -> string
```
`Parseff.take_while1` is like `Parseff.take_while` but requires at least one character. Fails if no characters match.

```ocaml
(* Parse non-empty digits *)
let digits1 () =
  Parseff.take_while1 (fun c -> c >= '0' && c <= '9') ~label:"digit"

(* Parse identifier (simpler than take_while approach) *)
let identifier1 () =
  Parseff.take_while1
    (fun c -> c = '_' || (c >= 'a' && c <= 'z') || (c >= '0' && c <= '9'))
    ~label:"identifier character"

(* Matches "foo123" -> "foo123" *)
(* Fails on "" -> Error: expected identifier character *)
```

### `skip_while`

```ocaml
val skip_while : (char -> bool) -> unit
```
`Parseff.skip_while` skips characters while the predicate holds (returns unit). Always succeeds. More efficient than `Parseff.take_while` when you don't need the matched string.

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


## Regular Expressions


### `match_regex`

```ocaml
val match_regex : Re.re -> string
```
`Parseff.match_regex` matches a compiled regular expression. The regex must be compiled with `Re.compile`.

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
  Parseff.take_while1 (fun c -> c >= '0' && c <= '9') ~label:"digit"
```

## Control Flow


### `fail`

```ocaml
val fail : string -> 'a
```
`Parseff.fail` aborts parsing with an error message.

```ocaml
(* Validate range *)
let byte () =
  let n =
    int_of_string
      (Parseff.take_while1
         (fun c -> c >= '0' && c <= '9')
         ~label:"digit")
  in
  if n >= 0 && n <= 255 then n
  else Parseff.fail "number must be between 0 and 255"

(* Parse: "128" -> 128 *)
(* Parse: "300" -> Error: number must be between 0 and 255 *)
```

### `error`

```ocaml
val error : 'e -> 'a
```
`Parseff.error` aborts parsing with a user-defined error value. Custom errors are caught by `Parseff.parse` and returned in the result.

```ocaml
let validated_number () =
  let s =
    Parseff.take_while1 (fun c -> c >= '0' && c <= '9') ~label:"digit"
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
    Parseff.take_while1 (fun c -> c >= '0' && c <= '9') ~label:"digit"
  in
  let n = int_of_string s in
  if n > 255 then Parseff.error `Too_large
  else if n < 0 then Parseff.error `Negative
  else n
```

### `end_of_input`

```ocaml
val end_of_input : unit -> unit
```
`Parseff.end_of_input` succeeds only if no input remains. Use this to ensure the entire input has been consumed.

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

```ocaml
val position : unit -> int
```
`Parseff.position` returns the current parser offset in bytes from the start of the input.
