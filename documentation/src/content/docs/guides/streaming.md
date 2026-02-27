---
title: Streaming Input
description: Parse from files, channels, and custom readers
---

Parseff can parse input that arrives incrementally — from files, network sockets, pipes, or any byte source. The same parser code works for both complete strings and streaming sources.

## Why It Works

Traditional parser combinator libraries (like Angstrom) implement streaming through CPS — every parser carries `fail` and `succ` callbacks, and suspension is encoded as closures returned in a `Partial` state. This forces the entire API into monadic style (`>>=`, `>>|`, `*>`).

Parseff uses algebraic effects. Parsers are plain functions that perform effects when they need input. The effect handler interprets those effects. For streaming, the handler calls a refill function when it runs out of data — the parser never knows the difference.

```ocaml
(* This parser works with both parse and parse_source — no changes needed *)
let ip_address () =
  let a = number () in
  let _ = Parseff.char '.' in
  let b = number () in
  let _ = Parseff.char '.' in
  let c = number () in
  let _ = Parseff.char '.' in
  let d = number () in
  (a, b, c, d)

(* From a string *)
Parseff.parse "192.168.1.1" ip_address

(* From a file — same parser, different runner *)
let ic = open_in "addresses.txt" in
Parseff.parse_source (Parseff.Source.of_channel ic) ip_address
```

---

## The Source Module

`Source.t` wraps any readable byte stream behind a uniform interface.

### `Source.of_string`

Creates a source from a complete string. Useful for testing streaming code paths with known input:

```ocaml
let source = Parseff.Source.of_string "{\"key\": 42}" in
Parseff.parse_source source json_parser
```

### `Source.of_channel`

Reads from an `in_channel`. The optional `buf_size` parameter controls the internal read buffer (default 4096 bytes):

```ocaml
let ic = open_in "data.json" in
let source = Parseff.Source.of_channel ~buf_size:8192 ic in
let result = Parseff.parse_source source json_parser in
close_in ic;
result
```

### `Source.of_function`

For custom readers. The function signature is `bytes -> int -> int -> int` — given a buffer, offset, and max length, fill the buffer and return the number of bytes written. Return `0` for EOF:

```ocaml
let source = Parseff.Source.of_function (fun buf off len ->
  Unix.read fd buf off len
) in
Parseff.parse_source source my_parser
```

---

## How It Works Internally

The effect handler maintains a growable buffer. When a parser primitive (like `consume`, `satisfy`, or `take_while`) needs more data than is currently buffered:

1. The handler calls the source's read function to fetch more bytes
2. New bytes are appended to the buffer
3. The parser primitive retries with the larger buffer

This happens transparently — no new effects, no continuations, no changes to parser code. Backtracking works across chunk boundaries because the buffer grows monotonically (old data is never discarded).

---

## Limitations

**Memory**: The buffer grows monotonically. For very large inputs, memory usage equals total input size. A future `commit` mechanism may allow discarding consumed data.

**Blocking**: The read callback blocks the calling thread. This is inherent to the pull-based model. For async/event-driven architectures, wrap the parse in a thread or wait for a future push-based API.

**Regex at chunk boundaries**: When a regex match extends to the end of the current buffer, the handler refills and retries until the match no longer touches the boundary (or EOF). This is correct for typical patterns but may behave unexpectedly with zero-width matches at the end of input.

---

## Next Steps

- [API Overview](/parseff/api/overview) — full combinator reference
- [Optimization Tips](/parseff/guides/optimization) — performance techniques
- [Error Handling](/parseff/guides/errors) — custom error types
