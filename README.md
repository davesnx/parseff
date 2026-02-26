# Parseff

Write parsers as plain functions. Algebraic effects handle the rest.

Parseff is a parser combinator library for OCaml 5 where parsers are regular functions (`unit -> 'a`), not monadic values. Algebraic effects manage backtracking, error reporting, and input state behind the scenes.

**[Documentation](https://davesnx.github.io/parseff/)**

## Installation

```bash
opam install parseff
```

Requires OCaml >= 5.3.

## Example

```ocaml
let number () =
  let digits = Parseff.many1 Parseff.digit () in
  let n = List.fold_left (fun acc d -> (acc * 10) + d) 0 digits in
  if n >= 0 && n <= 255 then n
  else Parseff.error (`Out_of_range n)

let ip_address () =
  let a = number () in
  let _ = Parseff.char '.' in
  let b = number () in
  let _ = Parseff.char '.' in
  let c = number () in
  let _ = Parseff.char '.' in
  let d = number () in
  Parseff.end_of_input ();
  (a, b, c, d)

let () =
  match Parseff.parse "192.168.1.1" ip_address with
  | Ok ((a, b, c, d), _) ->
      Printf.printf "Parsed: %d.%d.%d.%d\n" a b c d
  | Error { pos; error = `Out_of_range n } ->
      Printf.printf "Error at %d: %d out of range (0-255)\n" pos n
  | Error { pos; error = `Expected msg } ->
      Printf.printf "Error at %d: %s\n" pos msg
```

No `>>=`, no `let*`, no `*>`. Just `let` and `;`.

## Features

- **Imperative style** — parsers are `unit -> 'a` functions composed through ordinary `let` bindings, without monadic operators (`>>=`, `>>|`, `*>`) or binding operators (`let*`, `let+`, `and+`)
- **Backtracking by default** — `or_` automatically saves and restores position on failure
- **Streaming input** — parse from strings, files, channels, or custom readers with the same parser code
- **Custom error types** — return domain-specific errors via polymorphic variants, not just strings
- **Zero-copy parsing** — span-based APIs avoid string allocations on hot paths
- **Fused operations** — combined primitives (`sep_by_take`, `skip_while_then_char`) reduce effect dispatches
- **Minimal dependencies** — only `re` for regex support

## Comparison

| Feature | Parseff | Angstrom | MParser | Opal |
|---|---|---|---|---|
| Imperative-style API | Yes | No | No | No |
| Backtracking by default | Yes | Yes | No | No |
| Unbounded lookahead | Yes | Yes | Yes | No |
| Custom error types | Yes | No | No | No |
| Zero-copy API | Yes | Yes | No | No |
| Streaming/incremental input | Yes | Yes | No | No |
| Requires OCaml 5+ | Yes | No | No | No |

## Performance

Benchmarked on a JSON array parser (`[1, 2, 3, ..., 10]`) over 100,000 iterations:

| | Parses/sec | vs. Angstrom |
|---|---|---|
| Parseff (zero-copy spans) | ~4,940,000 | 4.3x faster |
| Parseff (fair comparison) | ~1,930,000 | 1.6x faster |
| Angstrom | ~1,150,000 | baseline |

Memory: Parseff 197 MB vs Angstrom 584 MB (3x less).

See [bench/bench_vs_angstrom.ml](./bench/bench_vs_angstrom.ml) for the benchmark source.

## API at a Glance

| Category | Functions |
|---|---|
| Core | `consume`, `char`, `satisfy`, `take_while`, `take_while1`, `skip_while`, `match_re`, `end_of_input` |
| Combinators | `or_`, `one_of`, `one_of_labeled`, `expect`, `look_ahead`, `optional` |
| Repetition | `many`, `many1`, `sep_by`, `sep_by1`, `count` |
| Errors | `fail`, `error` |
| Convenience | `digit`, `letter`, `alphanum`, `whitespace`, `skip_whitespace`, `any_char` |
| Zero-copy | `take_while_span`, `sep_by_take_span`, `fused_sep_take`, `skip_while_then_char` |
| Streaming | `Source.of_string`, `Source.of_channel`, `Source.of_function`, `parse_source` |

See [`lib/parseff.mli`](./lib/parseff.mli) for the full API reference.

## References

- Krishnaswami & Yallop (PLDI '19) — [A Typed, Algebraic Approach to Parsing](https://www.cl.cam.ac.uk/~jdy22/papers/a-typed-algebraic-approach-to-parsing.pdf)
- Kiselyov, O. et al. — [Algebraic Effects and Effect Handlers](https://okmij.org/ftp/Computation/variables-effects.html)
- [yieldparser](https://github.com/JavaScriptRegenerated/yieldparser) — JavaScript generator-based parsing (inspiration for the effects-as-communication-channel approach)

## Contributing

1. Open an issue to discuss proposed changes
2. Write tests for new features
3. Run `make fmt` before submitting
4. Ensure all tests pass with `make test`

## License

MIT — see [LICENSE](./LICENSE) file for details.
