---
title: Primitives
description: Basic parsing operations in Parseff
---

<!-- This file is generated from doc/primitives.mld. Do not edit directly. -->


# Primitives

Primitives are the building blocks of all parsers. These operations match input directly and form the foundation of more complex parsers.


## String and character matching


### `literal`

`Parseff.literal` matches the exact text of a string and returns `()`. On a mismatch it fails with an `Expected` error whose item is `Literal r`, where `r` is the rest of the string from the first byte that differs.

```ocaml
val literal : ?case_sensitive:bool -> string -> unit
```
```ocaml
let parser () =
  Parseff.literal "hello";
  Parseff.literal "world"

(* Matches "helloworld" *)
```
When the input differs from the literal, the error sits at the first byte that differs and expects the rest of the literal. `literal "ab"` on `"ax"` fails at offset 1 with `expected "b"`, not at offset 0\. When the input ends inside the literal, the error says `found end of input` at the end of the input. A mismatch inside a multi-byte character is reported at the start of that character.

On a stream, `literal` compares the buffered bytes first and reads more input only while they match the start of the literal, so a wrong byte fails at once. The error shows the bytes read so far: its `found` text can be shorter than on the whole input.

`~case_sensitive:false` ignores ASCII case, so `literal ~case_sensitive:false "content-length"` matches `Content-Length` and `CONTENT-LENGTH`. Only `A`\-`Z` and `a`\-`z` fold; other bytes must be equal.

**Performance:** `literal` is optimized for literal string matching.


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

(* Matches "(1,2)" -> ('1', '2') *)
```
**Tip:** Use `char` for single characters and `literal` for multi-character strings. They both match literals, but `char` makes the intent clear and avoids a string allocation for a single byte.


### `satisfy`

`Parseff.satisfy` matches a character satisfying the given predicate. The `~label` parameter is used in error messages. Without it, the error says `expected matching character`.

```ocaml
val satisfy : ?label:string -> (char -> bool) -> char
```
```ocaml
(* Match any vowel *)
let vowel () =
  Parseff.satisfy ~label:"vowel" (fun c -> String.contains "aeiouAEIOU" c)

(* Match any digit *)
let digit_char () =
  Parseff.satisfy ~label:"digit" (fun c -> c >= '0' && c <= '9')

(* Match any uppercase letter *)
let uppercase () =
  Parseff.satisfy ~label:"uppercase letter" (fun c -> c >= 'A' && c <= 'Z')
```
**Tip:** The label parameter is shown in error messages. Prefer descriptive labels.


## Character scanning

These operations scan multiple characters efficiently.


### `take_while`

`Parseff.take_while` consumes characters while the predicate holds. Returns the matched string (may be empty). It only fails when `~at_least` asks for more characters than match.

```ocaml
val take_while : ?at_least:int -> ?label:string -> (char -> bool) -> string
```
```ocaml
(* Parse digits *)
let digits () = Parseff.take_while (fun c -> c >= '0' && c <= '9')

(* Parse identifier *)
let identifier () =
  let first =
    Parseff.satisfy ~label:"letter" (fun c -> c = '_' || (c >= 'a' && c <= 'z'))
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
(* Fails on "!" -> Error: expected identifier character *)
(* Fails on ""  -> Error: expected identifier character, found end of input *)
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


### `take_until` and `skip_until`

`Parseff.take_until` reads up to the first occurrence of a terminator string and returns the bytes before it. Like `take_while`, it stops before the input that ends the run: it does not consume the terminator. `Parseff.skip_until` does the same without building the string.

```ocaml
val take_until : ?at_most:int -> ?label:string -> string -> string
```
```ocaml
val skip_until : ?at_most:int -> ?label:string -> string -> unit
```
```ocaml
(* Read a block comment *)
let block_comment () =
  Parseff.literal "/*";
  let body = Parseff.take_until "*/" in
  Parseff.literal "*/";
  body

(* "/* a * b */" -> " a * b "
   "/* a *"      -> Error at 6: expected "*/", found end of input *)
```
When the input ends before the terminator, the parse fails at the end of the input with `expected "<terminator>"`, or `expected <label>` with `~label`. `~at_most` limits the length of the result as in `take_while`. On a stream, `skip_until` keeps only the last `String.length terminator - 1` bytes it scanned, so it can skip a comment longer than the backtrack window; `take_until` keeps its whole result in the window.


## Control flow


### `fail`

`Parseff.fail` fails with an error message. Like any other failure, it lets `Parseff.or_` and the repetition combinators try something else. Call `Parseff.cut` before it to make the failure final.

```ocaml
val fail : ?from:int -> ?upto:int -> ?notes:note list -> ?help:string -> string -> 'a
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
val error : ?from:int -> ?upto:int -> ?notes:note list -> ?help:string -> custom_error -> 'a
```
```ocaml
type Parseff.custom_error += Negative of int | Out_of_range of int

let validated_number () =
  let s =
    Parseff.take_while ~at_least:1 (fun c -> c >= '0' && c <= '9') ~label:"digit"
  in
  let n = int_of_string s in
  if n < 0 then Parseff.error (Negative n)
  else if n > 255 then Parseff.error (Out_of_range n)
  else n

let () =
  match Parseff.parse "300" validated_number with
  | Ok n -> Printf.printf "Got %d\n" n
  | Error { kind = Custom (Out_of_range n); _ } ->
      Printf.printf "%d is too large (max 255)\n" n
  | Error { kind = Custom (Negative n); _ } ->
      Printf.printf "%d is negative\n" n
  | Error _ -> Printf.printf "Parse error\n"
```
**Tip:** Cases without a payload are quick error types:

```ocaml
type Parseff.custom_error += Too_large | Negative_number

let number_checked () =
  let s =
    Parseff.take_while ~at_least:1 (fun c -> c >= '0' && c <= '9') ~label:"digit"
  in
  let n = int_of_string s in
  if n > 255 then Parseff.error Too_large
  else if n < 0 then Parseff.error Negative_number
  else n
```

### `end_of_input`

`Parseff.end_of_input` succeeds only if no input remains. Use this to ensure the entire input has been consumed at a given point.

```ocaml
val end_of_input : unit -> unit
```
```ocaml
(* Parse complete input *)
let complete_number () =
  let n = Parseff.digit () in
  Parseff.end_of_input ();
  n

(* Matches: "5" -> '5' *)
(* Fails: "52" -> Error: expected end of input *)

(* parse already requires all input by default. end_of_input is for
   checks inside a parser, or when parsing with ~consume:`Partial *)
```
**Note:** `Parseff.parse` and `Parseff.parse_source` fail with `Expected End_of_input` when input remains, so a top-level parser does not need a final `end_of_input`. With `` ~consume:`Partial ``, leftover input is ignored.


### `position`

`Parseff.position` returns the current parser offset in bytes from the start of the input.

```ocaml
val position : unit -> int
```

### `with_range`

`Parseff.with_range` runs a parser and returns its result with the `Parseff.range` of bytes it consumed. Use it to point an error or a note at a whole construct.

```ocaml
val with_range : (unit -> 'a) -> 'a * range
```
```ocaml
let octet () =
  let n, range =
    Parseff.with_range (fun () ->
        int_of_string
          (Parseff.take_while ~at_least:1
             (fun c -> c >= '0' && c <= '9')
             ~label:"digit"))
  in
  if n > 255 then
    Parseff.fail
      ~notes:[ { at = Some range; text = "octets are 0 to 255" } ]
      "octet out of range";
  n
```

### `recognize` and `Slice.recognize`

`Parseff.recognize` runs a parser, drops its result, and returns the input text the parser consumed. `Parseff.Slice.recognize` returns it as a `Parseff.Slice.t`.

```ocaml
val recognize : (unit -> 'a) -> string
val Slice.recognize : (unit -> 'a) -> Slice.t
```
```ocaml
let identifier () =
  Parseff.recognize (fun () ->
      ignore (Parseff.letter ());
      ignore
        (Parseff.many
           (fun () -> Parseff.or_ Parseff.alphanum (fun () -> Parseff.char '_') ())
           ()))

let () =
  match Parseff.parse ~consume:`Partial "foo_1 = 2" identifier with
  | Ok name -> assert (name = "foo_1")
  | Error _ -> assert false
```

### `location`

`Parseff.location` returns the current position as a `Parseff.location` record with byte offset, line number, and column. Lines and columns are 1-based. Columns count bytes, not characters (relevant for multibyte UTF-8 input).

```ocaml
type location = { offset : int; line : int; byte_col : int }
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
  let value d = Char.code d - Char.code '0' in
  let n = List.fold_left (fun acc d -> (acc * 10) + value d) 0 digits in
  Num (n, loc)

(* Convert error positions after parsing *)
let report_error input result =
  match result with
  | Error e ->
      let loc = Parseff.location_of_position input e.Parseff.range.start in
      Printf.printf "line %d, byte %d: %s\n" loc.line loc.byte_col
        (Parseff.error_message e)
  | Ok _ -> ()
```
**Tip:** Use `location` during parsing to attach source positions to AST nodes or other parsed structures. Use `location_of_position` after parsing to convert byte offsets from error results into line and column numbers for diagnostics.
