---
title: Making parsers fast
description: Techniques for writing high-performance parsers with Parseff
---

# Guide: Making parsers fast

Techniques for writing high-performance parsers with Parseff.

This guide covers practical techniques for getting the most performance out of Parseff. Each section shows a few non-recommended slow patterns and a fast alternative with an approximate expected impact.

## Quick wins

### Pre-compile regexes

Compile regexes once at module level, not inside parser functions. Regex compilation is expensive (dozens of allocations and automaton construction per call) so hoisting it out gives roughly a 100x improvement:

```ocaml
let number () =
  (* compiles on every call *)
  let re = Re.compile (Re.Posix.re "[0-9]+") in
  Parseff.match_regex re

let number_re = Re.compile (Re.Posix.re "[0-9]+")
let number () = Parseff.match_regex number_re
```

### Use `take_while` instead of regex

For simple character classes, `Parseff.take_while` runs a tight loop with no regex overhead, giving a 5-10x improvement. Use regex only when you need its pattern-matching power (alternation, repetition, grouping). For "all characters matching a predicate," `take_while` is the right tool:

```ocaml
let digits_with_regex () = Parseff.match_regex (Re.compile (Re.Posix.re "[0-9]+"))
let digits_with_take_while1 () = Parseff.take_while1 (fun c -> c >= '0' && c <= '9') ~label:"digit"
```

### Use `skip_whitespace` instead of `whitespace`

When you just need to move past whitespace, `Parseff.skip_whitespace` avoids allocating a string you'd immediately discard. The per-call saving is small, but it adds up in tight loops that skip whitespace between every token:

```ocaml
(* allocates a string you immediately discard *)
let _ = Parseff.whitespace () in
parse_value ()

(* skips without allocation *)
Parseff.skip_whitespace ();
parse_value ()
```

### Use fused operations

Fewer round-trips between your parser and the effect handler means less work overall. Fused operations combine several steps into one, yielding a 2-4x improvement in hot paths by doing in a single call what would otherwise require multiple:

```ocaml
(* 4 effect dispatches *)
Parseff.skip_whitespace ();
let _ = Parseff.char ',' in
Parseff.skip_whitespace ();
let value = Parseff.take_while1 is_digit ~label:"digit" in
ignore value

(* 1 effect dispatch *)
let value = Parseff.fused_sep_take is_whitespace ',' is_digit in
ignore value
```

Available fused operations:

```
Operation                       Replaces
skip_while_then_char f c        skip_while f; char c
fused_sep_take ws sep pred      skip_whitespace; char sep; skip_whitespace; take_while1 pred
sep_by_take ws sep pred         sep_by (take_while pred) (skip_ws; char sep; skip_ws)
sep_by_take_span ws sep pred    Same, but returns spans instead of strings
```

### Use zero-copy spans

Avoid intermediate string allocations when you can work with slices. Spans point into the original input buffer with no allocation until you call `Parseff.span_to_string`, giving a 2-3x speed improvement and roughly 3x less memory allocation:

```ocaml
(* allocates a string per element *)
let parse_values () =
  Parseff.sep_by
    (fun () -> int_of_string (Parseff.take_while1 is_digit ~label:"digit"))
    (fun () -> Parseff.char ',')
    ()

(* zero-copy: spans point into the original buffer *)
let parse_values () =
  let spans = Parseff.sep_by_take_span is_whitespace ',' is_digit in
  List.map int_of_span spans
```

See the Zero-copy API reference for more details.

## Advanced techniques

### Minimize `or_` in loops

Each `Parseff.or_` sets up a backtracking checkpoint, which is extra work the runtime has to do. In a `Parseff.many` loop that runs thousands of times, doing less per iteration makes a real difference:

```ocaml
let keyword () =
  (* installs a handler per alternation, per iteration *)
  Parseff.or_
    (fun () -> Parseff.consume "class")
    (fun () ->
      Parseff.or_
        (fun () -> Parseff.consume "function")
        (fun () -> Parseff.consume "let")
        ())
    ()

let keyword () =
  (* parse then validate, no backtracking *)
  let w = Parseff.take_while1 (fun c -> c >= 'a' && c <= 'z') ~label:"keyword" in
  match w with
  | "class" | "function" | "let" -> w
  | _ -> Parseff.fail "expected keyword"
```

**When `or_` is fine:**

- Top-level choices (not inside tight loops)
- Complex parsers where each branch has distinct structure
- When the number of alternatives is small

### Push work into handler-level operations

The less your parser has to bounce back and forth with the effect handler, the faster it runs. Handler-level operations do the entire scan in one go instead of making repeated round-trips:

```ocaml
let parse_list () =
  (* using combinators *)
  Parseff.sep_by
    (fun () -> int_of_string (Parseff.take_while1 is_digit ~label:"digit"))
    (fun () -> Parseff.char ',')
    ()

let parse_list () =
  (* runs the entire scan in one operation *)
  Parseff.sep_by_take is_whitespace ',' is_digit
  |> List.map int_of_string
```
