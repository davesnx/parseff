---
title: Optimization Tips
description: Get maximum performance from Parseff
---

This guide covers techniques for writing fast parsers with Parseff.

## Quick Wins

### 1. Pre-compile regexes

Compile regexes at module level, not inside parser functions:

```ocaml
(* BAD — compiles on every call (100x+ slower) *)
let number () =
  let re = Re.compile (Re.Posix.re "[0-9]+") in
  match_regex re

(* GOOD — compiles once at module init *)
let number_re = Re.compile (Re.Posix.re "[0-9]+")
let number () = match_regex number_re
```

Impact: 100x+ speedup.

---

### 2. Use `take_while` instead of regex

For simple character classes, `take_while` is much faster:

```ocaml
(* SLOW — regex overhead *)
let digits = match_regex (Re.compile (Re.Posix.re "[0-9]+"))

(* FAST — direct character scanning *)
let digits = take_while1 (fun c -> c >= '0' && c <= '9') "digit"
```

Impact: 5-10x speedup.

---

### 3. Use `skip_whitespace` instead of `whitespace`

When you don't need the matched string:

```ocaml
(* WASTEFUL — allocates a string you discard *)
let _ = whitespace () in
parse_value ()

(* EFFICIENT — skips without allocation *)
skip_whitespace ();
parse_value ()
```

Impact: small but cumulative in hot loops.

---

### 4. Use fused operations

Combined operations reduce effect dispatches:

```ocaml
(* SLOW — 4 effect dispatches *)
skip_whitespace ();
let _ = char ',' in
skip_whitespace ();
let value = take_while1 is_digit "digit" in

(* FAST — 1 effect dispatch *)
let value = fused_sep_take is_whitespace ',' is_digit
```

Impact: 2-4x speedup in hot paths.

---

### 5. Use zero-copy spans

Avoid intermediate string allocations:

```ocaml
(* SLOW — allocates strings *)
let parse_ints () =
  sep_by
    (fun () -> int_of_string (take_while1 is_digit "digit"))
    (fun () -> char ',')
    ()

(* FAST — zero-copy spans *)
let int_of_span span =
  (* custom conversion, no String.sub *)
  ...

let parse_ints () =
  let spans = sep_by_take_span is_whitespace ',' is_digit in
  List.map int_of_span spans
```

Impact: 2-3x speedup + 3x less memory.

---

## Advanced Optimization

### Minimize `or_` Usage

Each alternation creates a handler for backtracking. This is expensive in hot loops:

```ocaml
(* EXPENSIVE — handler per alternation *)
let keyword () =
  or_
    (fun () -> consume "class")
    (fun () ->
      or_
        (fun () -> consume "function")
        (fun () ->
          or_ (fun () -> consume "const") (fun () -> consume "let") ())
        ())
    ()

(* BETTER — parse then validate *)
let keyword () =
  let w = take_while1 is_alphanum "keyword" in
  match w with
  | "class" | "function" | "const" | "let" -> w
  | _ -> fail "expected keyword"
```

**When alternation is OK:**
- Top-level choices (not in loops)
- Complex parsers that can't be validated easily
- When readability matters more than performance

---

### Use Handler-Level Operations

For hot paths, push work into the handler:

```ocaml
(* Standard - good *)
let parse_list () =
  sep_by parse_int (fun () -> char ',') ()

(* Handler-level - best *)
let parse_list () =
  sep_by_take is_whitespace ',' is_digit
  |> List.map int_of_string
```

Available handler-level operations:
- `sep_by_take` - separated list of strings
- `sep_by_take_span` - separated list of spans
- `skip_while_then_char` - skip + match
- `fused_sep_take` - whitespace + sep + whitespace + take

---

### Profile Your Parser

Use OCaml profiling to find hot spots:

```bash
# Build with profiling
ocamlfind ocamlopt -package parseff -p -linkpkg your_parser.ml -o your_parser

# Run
./your_parser

# Generate profile
gprof your_parser gmon.out > profile.txt
```

Look for:
- Regex compilation in hot paths
- Excessive string allocations
- Deep recursion
- Alternation in loops

---

## Performance Checklist

Before releasing your parser, check:

- [ ] All regexes compiled at module level
- [ ] `take_while` used instead of regex for simple patterns
- [ ] `skip_whitespace` used instead of `whitespace` when string not needed
- [ ] Fused operations used in hot paths
- [ ] Zero-copy spans used for performance-critical sections
- [ ] `or_` minimized or avoided in loops
- [ ] Benchmarked against alternative implementations

---

## Real-World Example

Here's a JSON array parser optimized using these techniques:

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
```

Optimizations applied:
1. Zero-copy spans
2. Handler-level `sep_by_take_span`
3. Custom `int_of_span` avoids `int_of_string`
4. No regex
5. No alternations in hot path

---

## Next Steps

- See [Zero-Copy API](/parseff/api/zero-copy) for span operations
- Read [Performance](/parseff/performance) for benchmarks
- Check the `OPTIMIZATION_LOG.md` in the repo for the full optimization journey
