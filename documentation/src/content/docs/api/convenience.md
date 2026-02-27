---
title: Convenience Combinators
description: Common parsing patterns made easy
---

Convenience combinators provide ready-to-use parsers for common patterns like digits, letters, and whitespace.

## Character Classes

### `digit`

```ocaml
val digit : unit -> int
```

Parses a decimal digit (0-9) and returns its integer value.

**Example:**
```ocaml
let d = digit () in  (* parses "7" -> 7 *)
Printf.printf "Got digit: %d\n" d

(* Parse two-digit number *)
let two_digits () =
  let a = digit () in
  let b = digit () in
  a * 10 + b

(* Matches "42" -> 42 *)
```

---

### `letter`

```ocaml
val letter : unit -> char
```

Parses an ASCII letter (a-z or A-Z).

**Example:**
```ocaml
let initial () = letter ()

(* Matches "A" -> 'A' *)
(* Matches "z" -> 'z' *)
(* Fails on "1" *)
```

---

### `alphanum`

```ocaml
val alphanum : unit -> char
```

Parses an alphanumeric character (letter or digit).

**Example:**
```ocaml
let username () =
  let first = letter () in
  let rest = many alphanum () in
  String.make 1 first ^ String.of_seq (List.to_seq rest)

(* Matches "user123" -> "user123" *)
(* Fails on "123user" (must start with letter) *)
```

---

### `any_char`

```ocaml
val any_char : unit -> char
```

Parses any character. Fails only at end of input.

**Example:**
```ocaml
(* Parse quoted string *)
let quoted_string () =
  let _ = char '"' in
  let chars = many (fun () ->
    (* Parse any char except quote *)
    let c = any_char () in
    if c = '"' then fail "unexpected quote"
    else c
  ) () in
  let _ = char '"' in
  String.of_seq (List.to_seq chars)

(* Matches "hello world" -> "hello world" *)
```

---

## Whitespace

### `is_whitespace`

```ocaml
val is_whitespace : char -> bool
```

Returns true for whitespace characters (space, tab, newline, carriage return).

**Example:**
```ocaml
let trim_parser () =
  skip_while is_whitespace;
  let value = take_while1 (fun c -> not (is_whitespace c)) "value" in
  skip_while is_whitespace;
  value

(* Matches "  hello  " -> "hello" *)
```

---

### `whitespace`

```ocaml
val whitespace : unit -> string
```

Parses zero or more whitespace characters. Returns the matched string. Always succeeds (returns empty string if no whitespace).

**Example:**
```ocaml
let spaced_values () =
  let a = digit () in
  let _ = whitespace () in
  let b = digit () in
  (a, b)

(* Matches "1 2" -> (1, 2) *)
(* Matches "1    2" -> (1, 2) *)
(* Matches "12" -> (1, 2) *)
```

---

### `whitespace1`

```ocaml
val whitespace1 : unit -> string
```

Parses one or more whitespace characters. Fails if no whitespace found.

**Example:**
```ocaml
let words () =
  sep_by1
    (fun () -> take_while1 (fun c -> not (is_whitespace c)) "word")
    (fun () -> whitespace1 ())
    ()

(* Matches "hello world" -> ["hello"; "world"] *)
(* Fails on "helloworld" (no whitespace separator) *)
```

---

### `skip_whitespace`

```ocaml
val skip_whitespace : unit -> unit
```

Skips zero or more whitespace characters (returns unit). More efficient than `whitespace` when you don't need the matched string.

**Example:**
```ocaml
(* Parse comma-separated list with flexible spacing *)
let flexible_list () =
  let _ = char '[' in
  skip_whitespace ();
  let values = sep_by
    (fun () ->
      skip_whitespace ();
      let n = digit () in
      skip_whitespace ();
      n
    )
    (fun () -> char ',')
    ()
  in
  skip_whitespace ();
  let _ = char ']' in
  values

(* All of these parse to [1; 2; 3]: *)
(* "[1,2,3]" *)
(* "[ 1 , 2 , 3 ]" *)
(* "[  1  ,  2  ,  3  ]" *)
```

:::tip[Performance]
Always use `skip_whitespace` instead of `whitespace` when you don't need the matched string:

```ocaml
(* SLOWER: *)
let _ = whitespace () in
parse_value ()

(* FASTER: *)
skip_whitespace ();
parse_value ()
```
:::

---

## Complete Example: Expression Parser

```ocaml
let number () =
  let digits = Parseff.many1 Parseff.digit () in
  List.fold_left (fun acc d -> acc * 10 + d) 0 digits

let addition () =
  Parseff.skip_whitespace ();
  let first = number () in
  Parseff.skip_whitespace ();
  let rest = Parseff.many (fun () ->
    let _ = Parseff.char '+' in
    Parseff.skip_whitespace ();
    let n = number () in
    Parseff.skip_whitespace ();
    n
  ) () in
  Parseff.end_of_input ();
  List.fold_left (+) first rest

let () =
  match Parseff.parse "1 + 2 + 3" addition with
  | Ok (result) -> Printf.printf "Result: %d\n" result
  | Error { pos; error = `Expected expected } -> Printf.printf "Error at %d: %s\n" pos expected
```

---

## Next Steps

- Explore [Zero-copy API](/parseff/api/zero-copy) for maximum performance
- Learn about [Optimization techniques](/parseff/guides/optimization)
