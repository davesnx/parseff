---
title: Zero-Copy API
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

```ocaml
val take_while_span : (char -> bool) -> span
```

Like `take_while`, but returns a zero-copy span instead of allocating a string.

**Example:**
```ocaml
(* Without span - allocates string *)
let digits = take_while (fun c -> c >= '0' && c <= '9') in
let n = int_of_string digits  (* uses allocated string *)

(* With span - no allocation until needed *)
let digits = take_while_span (fun c -> c >= '0' && c <= '9') in
let n = int_of_span digits  (* custom function, no intermediate string *)
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
  let digits = take_while_span (fun c -> c >= '0' && c <= '9') in
  int_of_span digits
```

---

### `sep_by_take_span`

```ocaml
val sep_by_take_span : (char -> bool) -> char -> (char -> bool) -> span list
```

Like `sep_by_take`, but returns zero-copy spans. No `String.sub` allocations per element.

**Example:**
```ocaml
(* Parse CSV line with zero-copy *)
let csv_line () =
  let spans = sep_by_take_span
    is_whitespace  (* skip whitespace *)
    ','            (* separator *)
    (fun c -> c <> ',' && c <> '\n')  (* value characters *)
  in
  (* Only allocate strings when needed *)
  List.map span_to_string spans

(* Even better - process spans directly *)
let csv_to_ints () =
  let spans = sep_by_take_span is_whitespace ',' (fun c -> c >= '0' && c <= '9') in
  List.map int_of_span spans
```

---

### `fused_sep_take`

```ocaml
val fused_sep_take : (char -> bool) -> char -> (char -> bool) -> string
```

Performs: skip whitespace, match separator, skip whitespace, take_while1 — all in a single effect dispatch. Much more efficient than separate calls.

**Example:**
```ocaml
(* Inefficient - multiple effect dispatches *)
let parse_value () =
  skip_whitespace ();
  let _ = char ',' in
  skip_whitespace ();
  take_while1 (fun c -> c >= '0' && c <= '9') "digit"

(* Efficient - single effect dispatch *)
let parse_value () =
  fused_sep_take is_whitespace ',' (fun c -> c >= '0' && c <= '9')
```

---

### `skip_while_then_char`

```ocaml
val skip_while_then_char : (char -> bool) -> char -> unit
```

Skips characters matching predicate, then matches a character. Fused operation for efficiency.

**Example:**
```ocaml
(* Inefficient *)
let comma_after_space () =
  skip_whitespace ();
  char ','

(* Efficient - fused *)
let comma_after_space () =
  skip_while_then_char is_whitespace ','
```

---

### `sep_by_take`

```ocaml
val sep_by_take : (char -> bool) -> char -> (char -> bool) -> string list
```

Parses zero or more separated values entirely in the handler. Returns list of matched strings. Zero intermediate effect dispatches.

**Example:**
```ocaml
(* Parse array values efficiently *)
let array_values () =
  let _ = char '[' in
  let values = sep_by_take is_whitespace ',' (fun c -> c >= '0' && c <= '9') in
  let _ = char ']' in
  List.map int_of_string values

(* Matches "[1, 2, 3]" -> [1; 2; 3] *)
```

---

## Performance Comparison

### String Allocation

```ocaml
(* Standard API - allocates intermediate strings *)
let parse_numbers () =
  sep_by
    (fun () ->
      skip_whitespace ();
      let s = take_while1 (fun c -> c >= '0' && c <= '9') "digit" in
      int_of_string s
    )
    (fun () -> char ',')
    ()

(* Zero-copy API - no allocations until conversion *)
let parse_numbers_fast () =
  let spans = sep_by_take_span is_whitespace ',' (fun c -> c >= '0' && c <= '9') in
  List.map int_of_span spans
```

### Effect Dispatches

```ocaml
(* Multiple effects - slower *)
let value () =
  skip_whitespace ();
  let _ = char ',' in
  skip_whitespace ();
  take_while1 (fun c -> c >= '0' && c <= '9') "digit"

(* Single effect - faster *)
let value () =
  fused_sep_take is_whitespace ',' (fun c -> c >= '0' && c <= '9')
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

**Performance:**
- Standard API: ~1,930,000 parses/second
- Zero-copy API: ~4,940,000 parses/second **(2.5x faster)**
- Memory: 3x reduction

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

---

## Next Steps

- Read [Optimization Guide](/parseff/guides/optimization) for more performance tips
- See [Core Primitives](/parseff/api/primitives) for basic operations
