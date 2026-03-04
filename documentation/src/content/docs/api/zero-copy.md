---
title: Zero-copy API
description: High-performance span-based parsing operations
---

The zero-copy API allows you to parse without allocating intermediate strings, significantly improving performance and reducing memory usage.

## Span Type

```ocaml
type span = { buf: string; off: int; len: int }

val span_to_string : span -> string
```

A `span` is a zero-copy slice of the input string. It holds a reference to the original buffer with an offset and length, avoiding string allocation until you explicitly call `span_to_string`.

---

## Span-Based Operations

### `take_while_span`

Like `take_while`, but returns a zero-copy span instead of allocating a string.

```ocaml
val take_while_span : (char -> bool) -> span
```

**Example:**
```ocaml
(* allocates a string *)
let digits = Parseff.take_while (fun c -> c >= '0' && c <= '9') in
let n = int_of_string digits

(* no allocation until you need it *)
let digits = Parseff.take_while_span (fun c -> c >= '0' && c <= '9') in
let n = int_of_span digits
```

**Real-world usage:**
```ocaml
(* Parse integers efficiently *)
let int_of_span span =
  let rec loop i acc =
    if i >= span.len then acc
    else
      let c = span.buf.[span.off + i] in
      loop (i + 1) (acc * 10 + (Char.code c - Char.code '0'))
  in
  loop 0 0

let fast_integer () =
  let digits = Parseff.take_while_span (fun c -> c >= '0' && c <= '9') in
  int_of_span digits
```

---

### `sep_by_take_span`

Like `sep_by_take`, but returns zero-copy spans. No `String.sub` allocations per element.

```ocaml
val sep_by_take_span : (char -> bool) -> char -> (char -> bool) -> span list
```

**Example:**
```ocaml
(* Parse CSV line with zero-copy *)
let csv_line () =
  let spans = Parseff.sep_by_take_span
    Parseff.is_whitespace  (* skip whitespace *)
    ','            (* separator *)
    (fun c -> c <> ',' && c <> '\n')  (* value characters *)
  in
  (* Only allocate strings when needed *)
  List.map Parseff.span_to_string spans

(* or process spans directly, avoiding string allocation entirely *)
let csv_to_ints () =
  let spans = Parseff.sep_by_take_span Parseff.is_whitespace ',' (fun c -> c >= '0' && c <= '9') in
  List.map int_of_span spans
```

---

### `fused_sep_take`

Performs: skip whitespace, match separator, skip whitespace, take_while1, all in a single effect dispatch. Much more efficient than separate calls.

```ocaml
val fused_sep_take : (char -> bool) -> char -> (char -> bool) -> string
```

**Example:**
```ocaml
(* multiple effect dispatches *)
let parse_value () =
  Parseff.skip_whitespace ();
  let _ = Parseff.char ',' in
  Parseff.skip_whitespace ();
  Parseff.take_while1 (fun c -> c >= '0' && c <= '9') ~label:"digit"

(* single effect dispatch *)
let parse_value () =
  Parseff.fused_sep_take Parseff.is_whitespace ',' (fun c -> c >= '0' && c <= '9')
```

---

### `skip_while_then_char`

Skips characters matching predicate, then matches a character. Fused operation for efficiency.

```ocaml
val skip_while_then_char : (char -> bool) -> char -> unit
```

**Example:**
```ocaml
(* two effect dispatches *)
let skip_ws_then_comma () =
  Parseff.skip_whitespace ();
  let _ = Parseff.char ',' in
  ()

(* single fused dispatch *)
let skip_ws_then_comma () =
  Parseff.skip_while_then_char Parseff.is_whitespace ','
```

---

### `sep_by_take`

Parses zero or more separated values entirely in the handler. Returns list of matched strings. Zero intermediate effect dispatches.

```ocaml
val sep_by_take : (char -> bool) -> char -> (char -> bool) -> string list
```

**Example:**
```ocaml
(* Parse array values efficiently *)
let array_values () =
  let _ = Parseff.char '[' in
  let values = Parseff.sep_by_take Parseff.is_whitespace ',' (fun c -> c >= '0' && c <= '9') in
  let _ = Parseff.char ']' in
  List.map int_of_string values

(* Matches "[1, 2, 3]" -> [1; 2; 3] *)
```

---

## Performance Comparison

### String Allocation

```ocaml
(* allocates intermediate strings *)
let parse_numbers () =
  Parseff.sep_by
    (fun () ->
      Parseff.skip_whitespace ();
      let s = Parseff.take_while1 (fun c -> c >= '0' && c <= '9') ~label:"digit" in
      int_of_string s
    )
    (fun () -> Parseff.char ',')
    ()

(* no allocations until conversion *)
let parse_numbers_fast () =
  let spans = Parseff.sep_by_take_span Parseff.is_whitespace ',' (fun c -> c >= '0' && c <= '9') in
  List.map int_of_span spans
```

### Effect Dispatches

```ocaml
(* multiple dispatches *)
let value () =
  Parseff.skip_whitespace ();
  let _ = Parseff.char ',' in
  Parseff.skip_whitespace ();
    Parseff.take_while1 (fun c -> c >= '0' && c <= '9') ~label:"digit"

(* single dispatch *)
let value () =
  Parseff.fused_sep_take Parseff.is_whitespace ',' (fun c -> c >= '0' && c <= '9')
```

---

## Complete Example: High-Performance JSON Array

```ocaml
let int_of_span (span : Parseff.span) =
  let rec loop i acc =
    if i >= span.len then acc
    else
      let c = span.buf.[span.off + i] in
      loop (i + 1) (acc * 10 + (Char.code c - Char.code '0'))
  in
  loop 0 0

let json_array () =
  let _ = Parseff.char '[' in
  let spans = Parseff.sep_by_take_span Parseff.is_whitespace ',' (fun c -> c >= '0' && c <= '9') in
  let _ = Parseff.char ']' in
  Parseff.end_of_input ();
  List.map int_of_span spans

let () =
  match Parseff.parse "[1, 2, 3, 4, 5]" json_array with
  | Ok (nums) -> 
      Printf.printf "Sum: %d\n" (List.fold_left (+) 0 nums)
  | Error { pos; error = `Expected expected } ->
      Printf.printf "Error at %d: %s\n" pos expected
```

The zero-copy version is roughly 2.5x faster than the standard API and uses 3x less memory. See the [Comparison with Angstrom](/parseff/guides/comparison) for benchmark methodology.

---

## When to Use Zero-Copy

**Use zero-copy when:**
- Parsing large inputs
- Performance is critical
- You can process spans directly (e.g., custom number parsing)
- Parsing hot paths (inner loops)

**Skip zero-copy when:**
- Readability is more important than performance
- You need to store parsed strings long-term (just use the regular API)
- The performance difference doesn't matter for your use case


