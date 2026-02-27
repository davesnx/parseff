---
title: Making Parsers Fast
description: Techniques for writing high-performance parsers with Parseff
---

This guide covers practical techniques for getting the most performance out of Parseff. Each section shows a slow pattern, a fast alternative, and the expected impact.

## Quick wins

### Pre-compile regexes

Compile regexes once at module level, not inside parser functions:

```ocaml
(* Slow — compiles on every call *)
let number () =
  let re = Re.compile (Re.Posix.re "[0-9]+") in
  Parseff.match_regex re

(* Fast — compiles once at module init *)
let number_re = Re.compile (Re.Posix.re "[0-9]+")
let number () = Parseff.match_regex number_re
```

**Impact:** 100x+ improvement. Regex compilation is expensive — dozens of allocations and automaton construction per call.

---

### Use `take_while` instead of regex

For simple character classes, `take_while` runs a tight loop with no regex overhead:

```ocaml
(* Slow — regex *)
let digits () = Parseff.match_regex (Re.compile (Re.Posix.re "[0-9]+"))

(* Fast — direct scanning *)
let digits () = Parseff.take_while1 (fun c -> c >= '0' && c <= '9') ~label:"digit"
```

**Impact:** 5-10x improvement. Use regex only when you need its pattern-matching power (alternation, repetition, grouping). For "all characters matching a predicate," `take_while` is the right tool.

---

### Use `skip_whitespace` instead of `whitespace`

When you just need to move past whitespace:

```ocaml
(* Wasteful — allocates a string you discard *)
let _ = Parseff.whitespace () in
parse_value ()

(* Efficient — skips without allocation *)
Parseff.skip_whitespace ();
parse_value ()
```

**Impact:** Small per-call, but cumulative in tight loops that skip whitespace between every token.

---

### Use fused operations

Combined operations dispatch a single effect instead of multiple:

```ocaml
(* Slow — 4 effect dispatches *)
Parseff.skip_whitespace ();
let _ = Parseff.char ',' in
Parseff.skip_whitespace ();
let value = Parseff.take_while1 is_digit ~label:"digit" in
...

(* Fast — 1 effect dispatch *)
let value = Parseff.fused_sep_take is_whitespace ',' is_digit
```

**Impact:** 2-4x improvement in hot paths. Each effect dispatch has overhead (handler lookup, continuation management). Fusing reduces the number of dispatches.

Available fused operations:

| Operation | Replaces |
|-----------|----------|
| `skip_while_then_char f c` | `skip_while f; char c` |
| `fused_sep_take ws sep pred` | `skip_whitespace; char sep; skip_whitespace; take_while1 pred` |
| `sep_by_take ws sep pred` | `sep_by (take_while pred) (skip_ws; char sep; skip_ws)` |
| `sep_by_take_span ws sep pred` | Same, but returns spans instead of strings |

---

### Use zero-copy spans

Avoid intermediate string allocations when you can work with slices:

```ocaml
(* Slow — allocates strings *)
let parse_values () =
  Parseff.sep_by
    (fun () -> int_of_string (Parseff.take_while1 is_digit ~label:"digit"))
    (fun () -> Parseff.char ',')
    ()

(* Fast — zero-copy spans *)
let parse_values () =
  let spans = Parseff.sep_by_take_span is_whitespace ',' is_digit in
  List.map int_of_span spans
```

**Impact:** 2-3x improvement + 3x less memory allocation. Each `take_while1` call allocates a new string via `String.sub`. Spans point into the original input buffer — no allocation until you call `span_to_string`.

See the [Zero-Copy Performance example](/parseff/examples/zero-copy-performance) for a detailed walkthrough.

---

## Advanced techniques

### Minimize `or_` in loops

Each `or_` installs an effect handler for backtracking. In a `many` loop that runs thousands of times, this overhead compounds:

```ocaml
(* Expensive — handler per alternation, per iteration *)
let keyword () =
  Parseff.or_
    (fun () -> Parseff.consume "class")
    (fun () ->
      Parseff.or_
        (fun () -> Parseff.consume "function")
        (fun () -> Parseff.consume "let")
        ())
    ()

(* Better — parse then validate *)
let keyword () =
  let w = Parseff.take_while1 (fun c -> c >= 'a' && c <= 'z') ~label:"keyword" in
  match w with
  | "class" | "function" | "let" -> w
  | _ -> Parseff.fail "expected keyword"
```

**When `or_` is fine:**
- Top-level choices (not inside tight loops)
- Complex parsers where each branch has distinct structure
- When the number of alternatives is small (2-3)

---

### Push work into handler-level operations

Handler-level operations run entirely inside the effect handler, avoiding the overhead of dispatching multiple effects:

```ocaml
(* Standard — good *)
let parse_list () =
  Parseff.sep_by
    (fun () -> int_of_string (Parseff.take_while1 is_digit ~label:"digit"))
    (fun () -> Parseff.char ',')
    ()

(* Handler-level — better *)
let parse_list () =
  Parseff.sep_by_take is_whitespace ',' is_digit
  |> List.map int_of_string
```

The handler-level version runs the entire separator-delimited scan in a single operation, then you map over the results.

---

### Profile before optimizing

Use OCaml profiling to find actual bottlenecks:

```bash
# Build with profiling
ocamlfind ocamlopt -package parseff -p -linkpkg your_parser.ml -o your_parser

# Run and generate profile
./your_parser
gprof your_parser gmon.out > profile.txt
```

Look for:
- Regex compilation in hot paths
- Excessive string allocations
- Deep recursion (consider depth limits)
- Alternation inside `many` loops

---

## Performance checklist

Before shipping a parser, check:

- [ ] All regexes compiled at module level
- [ ] `take_while` used instead of regex for simple character classes
- [ ] `skip_whitespace` used instead of `whitespace` when the string isn't needed
- [ ] Fused operations used in hot paths
- [ ] Zero-copy spans used where string allocation is avoidable
- [ ] `or_` minimized in inner loops
- [ ] Benchmarked against target throughput


