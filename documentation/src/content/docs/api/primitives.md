---
title: Core
description: Basic parsing operations in Parseff
---

Core primitives are the building blocks of all parsers. These operations match input directly and form the foundation of more complex parsers.

## String and Character Matching

### `consume`

```ocaml
val consume : string -> string
```

Matches an exact literal string. Returns the matched string.

<div class="api-example">

**Example:**
```ocaml
let parser () =
  let _ = Parseff.consume "hello" in
  Parseff.consume "world"

(* Matches "helloworld" *)
```

</div>

:::tip[Performance]
`consume` is optimized for literal string matching. It's much faster than using regex for exact matches.
:::

---

### `char`

```ocaml
val char : char -> char
```

Matches an exact character. Returns the matched character.

<div class="api-example">

**Example:**
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

</div>

:::tip[`char` vs `consume`]
Use `char` for single characters and `consume` for multi-character strings. They both match literals, but `char` makes the intent clear and avoids a string allocation for a single byte.
:::

---

### `satisfy`

```ocaml
val satisfy : (char -> bool) -> label:string -> char
```

Matches a character satisfying the given predicate. The `~label` parameter is used in error messages.

<div class="api-example">

**Example:**
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

</div>

:::caution[Error Messages]
The label parameter is shown in error messages. Choose descriptive labels for better errors:
```ocaml
(* BAD: *)
Parseff.satisfy is_digit ~label:"char"  (* Error: expected 'char' *)

(* GOOD: *)
Parseff.satisfy is_digit ~label:"digit"  (* Error: expected 'digit' *)
```
:::

---

## Character Scanning

These operations scan multiple characters efficiently.

### `take_while`

```ocaml
val take_while : (char -> bool) -> string
```

Consumes characters while the predicate holds. Returns the matched string (may be empty). Always succeeds.

<div class="api-example">

**Example:**
```ocaml
(* Parse digits *)
let digits () = Parseff.take_while (fun c -> c >= '0' && c <= '9')

(* Parse identifier *)
let identifier () =
  let first = Parseff.satisfy (fun c -> c = '_' || (c >= 'a' && c <= 'z')) ~label:"letter" in
  let rest = Parseff.take_while (fun c -> 
    c = '_' || 
    (c >= 'a' && c <= 'z') || 
    (c >= '0' && c <= '9')
  ) in
  String.make 1 first ^ rest

(* "foo_bar123" -> "foo_bar123" *)
(* "123abc" -> identifier fails (first char must be letter) *)
```

</div>

:::tip[Performance]
`take_while` is **much faster** than regex for simple character class matching. Use it instead of `match_regex` when possible.

```ocaml
(* SLOW: *)
let digits = Parseff.match_regex (Re.compile (Re.Posix.re "[0-9]+"))

(* FAST: *)
let digits = Parseff.take_while (fun c -> c >= '0' && c <= '9')
```
:::

---

### `take_while1`

```ocaml
val take_while1 : (char -> bool) -> label:string -> string
```

Like `take_while`, but requires at least one character. Fails if no characters match.

<div class="api-example">

**Example:**
```ocaml
(* Parse non-empty digits *)
let digits1 () = Parseff.take_while1 (fun c -> c >= '0' && c <= '9') ~label:"digit"

(* Parse identifier (simpler than take_while approach) *)
let identifier () = 
  Parseff.take_while1 
    (fun c -> c = '_' || (c >= 'a' && c <= 'z') || (c >= '0' && c <= '9'))
    ~label:"identifier character"

(* Matches "foo123" -> "foo123" *)
(* Fails on "" -> Error: expected identifier character *)
```

</div>

---

### `skip_while`

```ocaml
val skip_while : (char -> bool) -> unit
```

Skips characters while the predicate holds (returns unit). Always succeeds. More efficient than `take_while` when you don't need the matched string.

<div class="api-example">

**Example:**
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

</div>

:::tip[Use skip_while when possible]
If you don't need the matched string, use `skip_while` instead of `take_while` for better performance:

```ocaml
(* UNNECESSARY ALLOCATION: *)
let _ = Parseff.take_while is_space in
do_something ()

(* BETTER: *)
Parseff.skip_while is_space;
do_something ()
```
:::

---

## Regular Expressions

### `match_regex`

```ocaml
val match_regex : Re.re -> string
```

Matches a compiled regular expression. The regex must be compiled with `Re.compile`.

<div class="api-example">

**Example:**
```ocaml
(* BAD: Compiles regex on every call *)
let number () =
  let re = Re.compile (Re.Posix.re "[0-9]+") in
  Parseff.match_regex re

(* GOOD: Compiles once at module initialization *)
let number_re = Re.compile (Re.Posix.re "[0-9]+")
let number () = Parseff.match_regex number_re

(* Parse email address *)
let email_re = Re.compile (Re.Posix.re "[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}")
let email () = Parseff.match_regex email_re

(* "user@example.com" -> "user@example.com" *)
```

</div>

:::danger[Performance Critical]
**Always pre-compile regexes at module level!** Compiling regexes is expensive. Never compile inside a parser function.

```ocaml
(* BAD — compiles on every parse *)
let number () =
  let re = Re.compile (Re.Posix.re "[0-9]+") in
  Parseff.match_regex re

(* GOOD — compiles once *)
let number_re = Re.compile (Re.Posix.re "[0-9]+")
let number () = Parseff.match_regex number_re
```

Consider using `take_while` instead of regex for simple patterns:
```ocaml
(* Instead of: *)
let number () = Parseff.match_regex (Re.compile (Re.Posix.re "[0-9]+"))

(* Use: *)
let number () = Parseff.take_while1 (fun c -> c >= '0' && c <= '9') ~label:"digit"
```
:::

---

## Control Flow

### `fail`

```ocaml
val fail : string -> 'a
```

Aborts parsing with an error message.

<div class="api-example">

**Example:**
```ocaml
(* Validate range *)
let byte () =
  let n = int_of_string (Parseff.take_while1 (fun c -> c >= '0' && c <= '9') ~label:"digit") in
  if n >= 0 && n <= 255 then n 
  else Parseff.fail "number must be between 0 and 255"

(* Parse: "128" -> 128 *)
(* Parse: "300" -> Error: number must be between 0 and 255 *)
```

</div>

---

### `error`

```ocaml
val error : 'e -> 'a
```

Aborts parsing with a user-defined error value. Custom errors are caught by `parse` and returned in the result.

<div class="api-example">

**Example:**
```ocaml
let validated_number () =
  let s = Parseff.take_while1 (fun c -> c >= '0' && c <= '9') ~label:"digit" in
  let n = int_of_string s in
  if n < 0 then Parseff.error (`Negative n)
  else if n > 255 then Parseff.error (`Out_of_range n)
  else n

match Parseff.parse "300" validated_number with
| Ok n -> Printf.printf "Got %d\n" n
| Error { error = `Out_of_range n; _ } ->
    Printf.printf "%d is too large (max 255)\n" n
| Error { error = `Negative n; _ } ->
    Printf.printf "%d is negative\n" n
| Error { error = `Expected expected; _ } -> 
    Printf.printf "Parse error: %s\n" expected
```

</div>

:::tip[Use polymorphic variants]
Polymorphic variants work great with `error` for quick error types:

```ocaml
let number () =
  let s = Parseff.take_while1 (fun c -> c >= '0' && c <= '9') ~label:"digit" in
  let n = int_of_string s in
  if n > 255 then Parseff.error `Too_large
  else if n < 0 then Parseff.error `Negative
  else n
```
:::

---

### `end_of_input`

```ocaml
val end_of_input : unit -> unit
```

Succeeds only if no input remains. Use this to ensure the entire input has been consumed.

<div class="api-example">

**Example:**
```ocaml
(* Parse complete input *)
let complete_number () =
  let n = Parseff.digit () in
  Parseff.end_of_input ();
  n

(* Matches: "5" -> 5 *)
(* Fails: "52" -> Error: expected end of input *)

(* Without end_of_input, "52" would succeed and return 5, leaving "2" unparsed *)
```

</div>

:::caution[Partial vs Complete Parsing]
Use `end_of_input` when you want to ensure all input is consumed:

```ocaml
(* Partial parsing - allows leftover input *)
let partial_digit () =
  let n = Parseff.digit () in
  let pos = Parseff.position () in
  (n, pos)

match Parseff.parse "123abc" partial_digit with
| Ok (n, pos) -> (* n=1, pos=1, "23abc" remains *)

(* Complete parsing - requires all input consumed *)
let complete_digit () =
  let n = Parseff.digit () in
  Parseff.end_of_input ();
  n

match Parseff.parse "123abc" complete_digit with
| Error { pos; error = `Expected expected } -> (* Error: expected end of input *)
```
:::


