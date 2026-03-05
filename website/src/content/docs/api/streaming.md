---
title: Streaming
description: Parse from files, channels, and custom readers
---

<!-- This file is generated from doc/streaming.mld. Do not edit directly. -->

# Streaming

The streaming API lets you parse input that isn't fully available upfront: files, network sockets, pipes, or any byte source. The same parser code works with both `Parseff.parse` (strings) and `Parseff.parse_source` (streams), with no changes needed.


## `parse_source`

`Parseff.parse_source` runs a parser pulling input from a `Parseff.Source.t` on demand. Behaves identically to `Parseff.parse`: same parsers, same result type, same error handling.

```ocaml
val parse_source :
  ?max_depth:int ->
  Source.t ->
  (unit -> 'a) ->
  ('a, [> `Expected of string | `Unexpected_end_of_input ]) result
```
```ocaml
  let ic = open_in "data.json" in
  let source = Parseff.Source.of_channel ic in
  let result = Parseff.parse_source source json in
  close_in ic;
  result
```
The `~max_depth` parameter works the same as in `Parseff.parse`.


## `parse_source_until_end`

`Parseff.parse_source_until_end` is the streaming equivalent of `Parseff.parse_until_end`. It enforces full input consumption and returns diagnostics in both cases. See [`diagnostics`](./diagnostics.md).

```ocaml
val parse_source_until_end :
  ?max_depth:int ->
  Source.t ->
  (unit -> 'a) ->
  ('a, [> `Expected of string | `Unexpected_end_of_input ], 'd)
  result_with_diagnostics
```
```ocaml
  let ic = open_in "data.json" in
  let source = Parseff.Source.of_channel ic in
  let outcome = Parseff.parse_source_until_end source json in
  close_in ic;
  outcome
```

## The Source module

`Parseff.Source` wraps a readable byte stream behind a uniform interface. Three constructors cover the common cases.


### `Source.of_string`

`Parseff.Source.of_string` creates a source from a complete string. Useful for testing streaming code paths with known input:

```ocaml
val of_string : string -> Source.t
```
```ocaml
  let source = Parseff.Source.of_string "[1, 2, 3]" in
  Parseff.parse_source source json_array
```

### `Source.of_channel`

`Parseff.Source.of_channel` creates a source that reads from an `in_channel`. The `~buf_size` parameter controls the internal read buffer (default: 4096 bytes).

```ocaml
val of_channel : ?buf_size:int -> in_channel -> Source.t
```
```ocaml
  let ic = open_in "large_dataset.json" in
  let source = Parseff.Source.of_channel ~buf_size:8192 ic in
  let result = Parseff.parse_source source json in
  close_in ic;
  result
```
A larger buffer means fewer system calls but more memory. For most files, the default 4096 is fine. For multi-megabyte files, 64KB or larger reduces overhead.


### `Source.of_function`

`Parseff.Source.of_function` creates a source from a custom read function. The function signature is `read buf off len`. Fill `buf` starting at `off` with up to `len` bytes, and return the number of bytes written. Return `0` to signal EOF.

```ocaml
val of_function : (bytes -> int -> int -> int) -> Source.t
```
```ocaml
  (* Unix file descriptor *)
  let source = Parseff.Source.of_function (fun buf off len ->
    Unix.read fd buf off len
  ) in
  Parseff.parse_source source my_parser
```
This is the escape hatch for any byte source not covered by the other constructors: network sockets, memory-mapped files, decompression streams, etc.

```ocaml
  (* Decompression stream *)
  let source = Parseff.Source.of_function (fun buf off len ->
    Zlib.inflate zstream buf off len
  )

  (* Network socket *)
  let source = Parseff.Source.of_function (fun buf off len ->
    Unix.recv socket buf off len []
  )
```

## Why the same parser works for both

Parseff uses algebraic effects. Parsers are plain functions that perform effects when they need input. The effect handler interprets those effects differently depending on how you run the parser:

- `Parseff.parse`: the handler reads from a fixed string
- `Parseff.parse_source`: the handler reads from a `Parseff.Source.t`, fetching more data on demand
The parser doesn't know which one it's running under. The code is identical:

```ocaml
  (* This parser works with both runners *)
  let number () =
    let s =
      Parseff.take_while1 (fun c -> c >= '0' && c <= '9') ~label:"digit"
    in
    int_of_string s

  (* From a string *)
  let _ = Parseff.parse "42" number

  (* From a source *)
  let _ =
    Parseff.parse_source (Parseff.Source.of_string "42") number
```
Traditional parser combinator libraries (like Angstrom) implement streaming through CPS. Every parser carries `fail` and `succ` callbacks, and suspension is encoded as closures returned in a `Partial` state. This forces the entire API into monadic style. In Parseff, effects make streaming transparent.


## How backtracking works across chunks

The internal buffer grows monotonically. When the streaming handler needs more data, it appends to the buffer and never discards old data. This means backtracking (`Parseff.or_`, `Parseff.look_ahead`) works correctly even when the data spans multiple reads.

Example: parsing `"hello"` from a source that yields 3 bytes at a time:

1. First read: buffer = `"hel"`
2. `Parseff.consume "hello"` needs 5 bytes, only 3 available
3. Handler reads again: buffer = `"hello "`
4. `Parseff.consume "hello"` succeeds
If `Parseff.or_` needs to backtrack to a position before the current chunk, the data is still there in the buffer.


## Thread safety

`Parseff.parse_source` (and `Parseff.parse`) are safe to call from multiple OCaml 5 domains concurrently. All mutable state (the parser position, internal buffer, recursion depth counter) is created locally inside each call and never shared across calls. There is no global mutable state in the library.

```ocaml
  (* Parse different files in parallel *)
  let results =
    List.init num_workers (fun i ->
      Domain.spawn (fun () ->
        let ic = open_in files.(i) in
        let source = Parseff.Source.of_channel ic in
        let result = Parseff.parse_source source json in
        close_in ic;
        result))
    |> List.map Domain.join
```
The one constraint: do not share a single `Parseff.Source.t` across domains. Each source has mutable read state (buffer position, EOF flag), so concurrent access would race. Create a separate source per domain.


## Limitations


### Memory growth

The buffer never shrinks. For a 100MB file, the buffer will eventually hold all 100MB. This is the price of correct backtracking, since any position might need to be revisited.


### Blocking reads

`Parseff.Source.of_channel` and `Parseff.Source.of_function` block the calling thread when waiting for data. For async/event-driven architectures, wrap the parse call in a thread:

```ocaml
  let result = Domain.spawn (fun () ->
    Parseff.parse_source source json
  ) |> Domain.join
```

### Regex at chunk boundaries

When a regex match extends to the end of the current buffer, the handler refills and retries until the match no longer touches the boundary (or EOF). This is correct for typical patterns but may behave unexpectedly with zero-width matches at the end of input.


### No partial results

`Parseff.parse_source` runs the parser to completion and returns a single result. There's no way to get partial results as data arrives. For incremental processing (e.g., streaming JSON objects from a newline-delimited file), parse one unit at a time:

```ocaml
  let parse_jsonl ic =
    let results = ref [] in
    try
      while true do
        let line = input_line ic in
        match Parseff.parse line json with
        | Ok v -> results := v :: !results
        | Error _ -> ()
      done;
      List.rev !results
    with End_of_file ->
      List.rev !results
```