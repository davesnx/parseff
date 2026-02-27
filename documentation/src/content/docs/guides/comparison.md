---
title: Comparison with Angstrom
description: How Parseff compares to Angstrom in performance, API style, and trade-offs
---

[Angstrom](https://github.com/inhabitedtype/angstrom) is the most widely used parser combinator library in the OCaml ecosystem. This page compares Parseff and Angstrom side by side — performance, API style, and when to use each.

## Performance

Benchmarked on a JSON array parser (`[1, 2, 3, ..., 10]`) over 100,000 iterations. Sources: [`bench/bench_json.ml`](https://github.com/davesnx/parseff/blob/main/bench/bench_json.ml), [`bench/bench_vs_angstrom.ml`](https://github.com/davesnx/parseff/blob/main/bench/bench_vs_angstrom.ml).

| Parser | Parses/sec | vs. Angstrom | Minor allocs |
|--------|-----------|-------------|--------|
| Parseff (zero-copy) | ~5,270,000 | 4.8x faster | 168 MB |
| Parseff (fair) | ~1,930,000 | 1.8x faster | 184 MB |
| MParser | ~1,330,000 | 1.2x faster | 466 MB |
| Angstrom | ~1,090,000 | baseline | 584 MB |

**Zero-copy** uses `sep_by_take_span` with a custom `float_of_span` that avoids `float_of_string`. This represents the fastest path when you control the conversion logic.

**Fair** uses the same `float_of_string` call as MParser and Angstrom, isolating parsing overhead from number conversion.

All parsers produce the same output (`float list`) from the same input.

### Why Parseff is faster

**Direct character scanning.** `take_while` runs a tight `while` loop with character predicates. No regex compilation, no automaton overhead.

**Fewer allocations.** Span-based APIs return `{ buf; off; len }` slices of the input string without calling `String.sub`. Angstrom's `take_while1` allocates a new string per call.

**Fused operations.** `sep_by_take_span` parses an entire separated list in a single effect dispatch. Angstrom's equivalent chains `sep_by`, `char`, `skip_while`, and `take_while1` through monadic operators, each creating closures.

**No monadic overhead.** Parsers are direct function calls. No CPS, no closure allocation for sequencing.

### Optimization journey

Starting from ~12,000 parses/sec (101x slower than Angstrom), systematic optimization achieved a 412x improvement:

1. Replacing regex with `take_while` character scanning
2. Fused operations that reduce effect dispatches
3. Zero-copy span APIs that avoid `String.sub` allocations
4. Handler-level batch operations (`sep_by_take`, `skip_while_then_char`)

## API style

The fundamental difference: Parseff uses direct-style imperative code. Angstrom uses monadic composition.

### Sequencing

**Parseff:**
```ocaml
let key_value () =
  let key = Parseff.take_while1 (fun c -> c <> ':') ~label:"key" in
  let _ = Parseff.char ':' in
  Parseff.skip_whitespace ();
  let value = Parseff.take_while1 (fun c -> c <> '\n') ~label:"value" in
  (key, value)
```

**Angstrom:**
```ocaml
let key_value =
  take_while1 (fun c -> c <> ':') >>= fun key ->
  char ':' >>= fun _ ->
  skip_while is_ws >>= fun () ->
  take_while1 (fun c -> c <> '\n') >>= fun value ->
  return (key, value)
```

Both do the same thing. Parseff reads like sequential OCaml code. Angstrom threads results through `>>=` and `return`.

### Alternation

**Parseff:**
```ocaml
let value () =
  Parseff.one_of
    [ null_parser; bool_parser; number_parser; string_parser ]
    ()
```

**Angstrom:**
```ocaml
let value =
  null_parser <|> bool_parser <|> number_parser <|> string_parser
```

Similar readability. Angstrom's `<|>` is more concise. Parseff's `one_of` is explicit about the list structure.

### Repetition

**Parseff:**
```ocaml
let numbers () =
  Parseff.sep_by
    (fun () ->
      Parseff.skip_whitespace ();
      let s = Parseff.take_while1 is_digit ~label:"digit" in
      Parseff.skip_whitespace ();
      int_of_string s)
    (fun () -> Parseff.char ',')
    ()
```

**Angstrom:**
```ocaml
let numbers =
  sep_by (ws *> char ',' <* ws)
    (take_while1 is_digit >>| int_of_string)
```

Angstrom is more concise here thanks to applicative operators (`*>`, `<*`). Parseff is more explicit — whitespace handling is visible, not hidden in operator chains.

### A complete side-by-side

Here's the same expression parser in both libraries:

**Parseff:**
```ocaml
let rec expr () =
  Parseff.chainl1
    term
    (fun () ->
      Parseff.skip_whitespace ();
      let _ = Parseff.char '+' in
      Parseff.skip_whitespace ();
      fun a b -> a + b)
    ()

and term () =
  Parseff.chainl1
    factor
    (fun () ->
      Parseff.skip_whitespace ();
      let _ = Parseff.char '*' in
      Parseff.skip_whitespace ();
      fun a b -> a * b)
    ()

and factor () =
  Parseff.or_
    (fun () ->
      let _ = Parseff.char '(' in
      let e = expr () in
      let _ = Parseff.char ')' in
      e)
    (fun () -> Parseff.digit ())
    ()
```

**Angstrom:**
```ocaml
let expr =
  fix (fun expr ->
    let factor =
      char '(' *> expr <* char ')'
      <|> (satisfy is_digit >>| fun c -> Char.code c - 48)
    in
    let term =
      chainl1 factor (ws *> char '*' <* ws >>| fun _ -> ( * ))
    in
    chainl1 term (ws *> char '+' <* ws >>| fun _ -> ( + ))
  )
```

Angstrom is denser. Parseff is more readable for people who aren't fluent in monadic/applicative operators.

## Feature comparison

| Feature | Parseff | Angstrom |
|---------|---------|----------|
| OCaml version | 5.3+ | 4.x+ |
| API style | Imperative (direct effects) | Monadic (CPS-based) |
| Streaming | `parse_source` with `Source.t` | `Buffered` / `Unbuffered` modules |
| Backtracking | Automatic via `or_` | Automatic via `<|>` |
| Zero-copy | `span` type + fused ops | Not built-in |
| Recursion safety | `rec_` with `~max_depth` | Manual (no built-in depth limit) |
| Custom errors | `error` with polymorphic variants | Limited (string-based) |
| Error labels | `expect`, `one_of_labeled` | `<?>` operator |
| Async support | Not built-in (wrap in Domain) | Incremental API with `Partial` |
| Maturity | New | Battle-tested, widely used |

## Broader comparison

| Feature | Parseff | [Angstrom](https://github.com/inhabitedtype/angstrom) | [MParser](https://github.com/cakeplus/mparser) | [Opal](https://github.com/pyrocat101/opal) |
|---|---|---|---|---|
| Imperative-style API | Yes | No | No | No |
| Monadic interface | No | Yes | Yes | Yes |
| Backtracking by default | Yes | Yes | No | No |
| Unbounded lookahead | Yes | Yes | Yes | No |
| Custom error types | Yes | No | No | No |
| Zero-copy API | Yes | Yes | No | No |
| Streaming/incremental input | Yes | Yes | No | No |
| Requires OCaml 5+ | Yes | No | No | No |

MParser and Opal require explicit backtracking (like Parsec's `try`). Angstrom and Parseff backtrack automatically on alternation. MParser and Opal don't support streaming input. Only Parseff supports custom typed errors beyond strings.

## When to use which

**Choose Parseff when:**
- You prefer imperative-style code over monadic operators
- Performance is critical (Parseff's fused/zero-copy API is faster)
- You need custom typed errors beyond strings
- You need built-in recursion depth limiting
- You're already on OCaml 5.3+

**Choose Angstrom when:**
- You need OCaml 4.x compatibility
- You need incremental parsing with `Partial` results (event-driven architectures)
- Your team is comfortable with monadic style
- You want a battle-tested library with a large ecosystem of examples
- You need Async/Lwt integration

## Migrating from Angstrom

Common patterns and their Parseff equivalents:

| Angstrom | Parseff |
|----------|---------|
| `string "foo"` | `Parseff.consume "foo"` |
| `char 'x'` | `Parseff.char 'x'` |
| `satisfy f` | `Parseff.satisfy f ~label:"..."` |
| `take_while f` | `Parseff.take_while f` |
| `take_while1 f` | `Parseff.take_while1 f ~label:"label"` |
| `skip_while f` | `Parseff.skip_while f` |
| `a >>= fun x -> b` | `let x = a () in b ()` |
| `a *> b` | `let _ = a () in b ()` |
| `a <* b` | `let x = a () in let _ = b () in x` |
| `a >>| f` | `f (a ())` |
| `return x` | `x` |
| `fail msg` | `Parseff.fail msg` |
| `a <|> b` | `Parseff.or_ a b ()` |
| `many p` | `Parseff.many p ()` |
| `many1 p` | `Parseff.many1 p ()` |
| `sep_by sep p` | `Parseff.sep_by p sep ()` |
| `fix (fun p -> ...)` | `let rec p () = Parseff.rec_ (fun () -> ...)` |
| `end_of_input` | `Parseff.end_of_input ()` |
| `p <?> msg` | `Parseff.expect msg p` |
| `peek_char` | `Parseff.optional (fun () -> Parseff.look_ahead Parseff.any_char) ()` |

The main adjustment: every parser call becomes a function call with `()`. Angstrom parsers are values; Parseff parsers are functions.


