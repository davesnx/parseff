---
title: Repetition Combinators
description: Looping constructs for parsing repeated patterns
---

Repetition combinators allow you to parse patterns that occur zero or more times.

## Basic Repetition

### `many`

```ocaml
val many : (unit -> 'a) -> unit -> 'a list
```

Applies a parser zero or more times. Returns a list of results. Always succeeds (returns empty list if parser doesn't match).

**Example:**
```ocaml
let digits () = many digit ()

(* Matches "123" -> [1; 2; 3] *)
(* Matches "" -> [] *)
(* Matches "abc" -> [] *)
```

---

### `many1`

```ocaml
val many1 : (unit -> 'a) -> unit -> 'a list
```

Applies a parser one or more times. Fails if the parser doesn't match at least once.

**Example:**
```ocaml
let non_empty_digits () = many1 digit ()

(* Matches "123" -> [1; 2; 3] *)
(* Fails on "" *)
(* Fails on "abc" *)
```

---

### `count`

```ocaml
val count : int -> (unit -> 'a) -> unit -> 'a list
```

Applies a parser exactly `n` times. Fails if the parser doesn't match `n` times.

**Example:**
```ocaml
(* Parse exactly 3 digits *)
let three_digits () = count 3 digit ()

(* Parse RGB hex color: #RRGGBB *)
let hex_digit () = 
  satisfy (fun c -> 
    (c >= '0' && c <= '9') || 
    (c >= 'a' && c <= 'f') || 
    (c >= 'A' && c <= 'F')
  ) ~label:"hex digit"

let hex_color () =
  let _ = char '#' in
  let r = count 2 hex_digit () in
  let g = count 2 hex_digit () in
  let b = count 2 hex_digit () in
  end_of_input ();
  (r, g, b)

(* Matches "#ff00aa" -> (['f';'f'], ['0';'0'], ['a';'a']) *)
```

---

## Separated Lists

### `sep_by`

```ocaml
val sep_by : (unit -> 'a) -> (unit -> 'b) -> unit -> 'a list
```

Parses zero or more occurrences of a value separated by a separator. Always succeeds.

**Example:**
```ocaml
(* Parse CSV line *)
let csv_line () =
  sep_by
    (fun () -> take_while (fun c -> c <> ',' && c <> '\n'))
    (fun () -> char ',')
    ()

(* Matches "a,b,c" -> ["a"; "b"; "c"] *)
(* Matches "" -> [] *)

(* Parse array: [1, 2, 3] *)
let array () =
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

(* Matches "[1, 2, 3]" -> [1; 2; 3] *)
(* Matches "[]" -> [] *)
```

---

### `sep_by1`

```ocaml
val sep_by1 : (unit -> 'a) -> (unit -> 'b) -> unit -> 'a list
```

Like `sep_by`, but requires at least one occurrence.

**Example:**
```ocaml
let non_empty_csv () =
  sep_by1
    (fun () -> take_while1 (fun c -> c <> ',' && c <> '\n') "value")
    (fun () -> char ',')
    ()

(* Matches "a,b,c" -> ["a"; "b"; "c"] *)
(* Matches "a" -> ["a"] *)
(* Fails on "" *)
```

---

## Complete Example: JSON Array Parser

```ocaml
let is_whitespace c = 
  c = ' ' || c = '\t' || c = '\n' || c = '\r'

let integer () =
  let sign = Parseff.optional (fun () -> Parseff.char '-') () in
  let digits = Parseff.take_while1 (fun c -> c >= '0' && c <= '9') "digit" in
  let n = int_of_string digits in
  match sign with Some _ -> -n | None -> n

let json_array () =
  let _ = Parseff.char '[' in
  Parseff.skip_whitespace ();
  let values = Parseff.sep_by
    (fun () ->
      Parseff.skip_whitespace ();
      let n = integer () in
      Parseff.skip_whitespace ();
      n
    )
    (fun () -> Parseff.char ',')
    ()
  in
  Parseff.skip_whitespace ();
  let _ = Parseff.char ']' in
  Parseff.end_of_input ();
  values

let () =
  match Parseff.parse "[1, 2, 3, 4, 5]" json_array with
  | Ok (nums) -> 
      Printf.printf "Parsed: [%s]\n" 
        (String.concat ", " (List.map string_of_int nums))
  | Error { pos; error = `Expected expected } ->
      Printf.printf "Error at %d: %s\n" pos expected
```

---

## Next Steps

- Learn about [High-performance operations](/parseff/api/zero-copy)
- See [Optimization tips](/parseff/guides/optimization) for better performance
