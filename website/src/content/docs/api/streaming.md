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
  ?backtrack_window:int ->
  Source.t ->
  (unit -> 'a) ->
  ('a, [> `Expected of string | `Unexpected_end_of_input | `Backtrack_window_exceeded of Parseff.backtrack_window_error | `Depth_limit_exceeded of string ]) result
```
```ocaml
  let ic = open_in "data.json" in
  let source = Parseff.Source.of_channel ic in
  let result = Parseff.parse_source source json in
  close_in ic;
  result
```
The `~max_depth` parameter works the same as in `Parseff.parse`. The `~backtrack_window` parameter bounds how much buffered input Parseff may retain for backtracking and in-flight streaming operations. It defaults to 65536 bytes.


## `parse_source_until_end`

`Parseff.parse_source_until_end` is the streaming equivalent of `Parseff.parse_until_end`. It enforces full input consumption and returns diagnostics in both cases. See [`diagnostics`](./diagnostics.md).

```ocaml
val parse_source_until_end :
  ?max_depth:int ->
  ?backtrack_window:int ->
  Source.t ->
  (unit -> 'a) ->
  ('a, [> `Expected of string | `Unexpected_end_of_input | `Backtrack_window_exceeded of Parseff.backtrack_window_error | `Depth_limit_exceeded of string ], 'd)
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

`Parseff.Source` wraps a readable byte stream behind a uniform interface. Five constructors cover the common cases, ordered here from simplest to most low-level.


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


### `Source.of_chunks`

`Parseff.Source.of_chunks` creates a source from a function that returns string chunks. Return `Some s` with a non-empty string for data, or `None` to signal EOF. Empty strings (`Some ""`) are silently skipped.

```ocaml
val of_chunks : (unit -> string option) -> Source.t
```
This is the recommended way to wrap stateful or callback-based data sources. The library handles buffer management internally — no manual `Bytes.blit_string`, position tracking, or `min` calculations needed.

```ocaml
  (* Eio promise — await once, then signal EOF *)
  let fetched = ref false in
  let source = Parseff.Source.of_chunks (fun () ->
      if !fetched then None
      else begin
        fetched := true;
        Some (Promise.await_exn promise)
      end) in
  Parseff.parse_source source json
```
```ocaml
  (* HTTP body arriving in pieces *)
  let source = Parseff.Source.of_chunks (fun () ->
      match Http.Body.read body with
      | `Data chunk -> Some chunk
      | `Eof -> None) in
  Parseff.parse_source source my_parser
```
Compare with the equivalent `Parseff.Source.of_function` version, which requires the caller to manage a position cursor, calculate byte counts, and blit into a pre-allocated buffer:

```ocaml
  (* Same Eio example with of_function — more boilerplate *)
  let data = ref None in
  let pos = ref 0 in
  let source = Parseff.Source.of_function (fun buf off len ->
      let s = match !data with
        | Some s -> s
        | None ->
            let s = Promise.await_exn promise in
            data := Some s; s
      in
      let remaining = String.length s - !pos in
      let n = min len remaining in
      Bytes.blit_string s !pos buf off n;
      pos := !pos + n;
      n) in
  Parseff.parse_source source json
```

### `Source.of_seq`

`Parseff.Source.of_seq` creates a source from a lazy sequence of string chunks. Each element is a chunk of input; the sequence ending (`Seq.Nil`) signals EOF.

```ocaml
val of_seq : string Seq.t -> Source.t
```
```ocaml
  (* From a list of chunks *)
  let source = Parseff.Source.of_seq
    (List.to_seq ["{ \"key\""; ": "; "\"value\" }"]) in
  Parseff.parse_source source json
```
`of_seq` is useful when the data is already structured as a sequence — for example, reading lines from a file or splitting a large string into fixed-size pieces:

```ocaml
  (* Fixed-size chunks from a string, useful for testing *)
  let chunk_seq ?(chunk_size = 4096) input =
    let len = String.length input in
    let rec go pos () =
      if pos >= len then Seq.Nil
      else
        let n = min chunk_size (len - pos) in
        Seq.Cons (String.sub input pos n, go (pos + n))
    in
    go 0

  let source = Parseff.Source.of_seq (chunk_seq ~chunk_size:2 "hello world")
```

### `Source.of_function`

`Parseff.Source.of_function` creates a source from a low-level read function. The function signature matches POSIX `read(2)`: `read buf off len` fills `buf` starting at `off` with up to `len` bytes and returns the count actually written. Return `0` to signal EOF.

```ocaml
val of_function : (bytes -> int -> int -> int) -> Source.t
```
This is the low-level escape hatch for byte sources that natively fill a pre-allocated buffer. For most use cases, prefer `Parseff.Source.of_chunks` or `Parseff.Source.of_seq` which handle buffer management automatically.

```ocaml
  let unix_file_descriptor = Parseff.Source.of_function (fun buf off len ->
    Unix.read fd buf off len
  ) in
  Parseff.parse_source source my_parser

  let decompression_stream = Parseff.Source.of_function (fun buf off len ->
    Zlib.inflate zstream buf off len
  )

  let network_socket = Parseff.Source.of_function (fun buf off len ->
    Unix.recv socket buf off len []
  )
```

## Why the same parser works for both

Parseff uses the same parser API for both in-memory and streaming input. The runner decides where bytes come from:

- `Parseff.parse`: reads from a fixed string
- `Parseff.parse_source`: reads from a `Parseff.Source.t`, fetching more data on demand

The parser doesn't know which one it's running under. The code is identical:

```ocaml
  (* This parser works with both runners *)
  let number () =
    let s =
      Parseff.take_while ~at_least:1 (fun c -> c >= '0' && c <= '9') ~label:"digit"
    in
    int_of_string s

  (* From a string *)
  let _ = Parseff.parse "42" number

  (* From a source *)
  let _ =
    Parseff.parse_source (Parseff.Source.of_string "42") number
```
In Parseff, streaming is still transparent at the parser-function level: the same parser works against a fixed string or a streaming source. The runtime, however, now uses a bounded sliding buffer instead of an append-forever buffer.


## How backtracking works across chunks

When Parseff refills a source, it keeps only the region that is still needed for backtracking. The retained window starts at the oldest active rewind point and ends at the newest buffered byte.

Two things keep old bytes alive:

- active backtracking frames such as `Parseff.or_`, `Parseff.look_ahead`, and the internal rewind points used by repetition combinators
- in-flight streaming primitives that still need earlier bytes to finish building a value (for example `Parseff.take_while`)

### General advice

Most parsers become streaming parsers by changing only the runner: keep the same parser functions and switch from `Parseff.parse` to `Parseff.parse_source` or `Parseff.parse_source_until_end`. Start there, then tune the streaming behavior only if the grammar needs it.

In practice:

- Think of `~backtrack_window` as retained undo history, not as a chunk size.
- Prefer grammars that dispatch early, so the runtime does not need to keep a long ambiguous prefix alive.
- Call `Parseff.commit` only when a branch has become logically certain.
- If you hit <code>`Backtrack_window_exceeded</code>, first try to commit earlier; only increase `~backtrack_window` when the ambiguity is real.

The guides section includes a small CSV walkthrough that starts with a normal string parser and only changes the runner, plus a second guide that focuses on `commit` and `~backtrack_window` for grammars with long-lived ambiguity.

A small example:

```ocaml
let http_method () =
  Parseff.or_
    (fun () ->
      let _ = Parseff.consume "GET " in
      Parseff.commit ();
      `GET)
    (fun () ->
      let _ = Parseff.consume "POST " in
      Parseff.commit ();
      `POST)
    ()
```
Before `commit ()`, Parseff must keep enough buffered input to rewind and try the other branch. After `commit ()`, that rewind point stops pinning the old prefix.

If the retained region would exceed `~backtrack_window`, parsing fails with <code>`Backtrack_window_exceeded</code>. This is a real contract, not a best-effort hint.


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


### Bounded backtracking

The buffer no longer grows forever, but a parser can still exceed the configured `~backtrack_window` if it keeps an old rewind point alive for too long. Typical fixes are:

- dispatch earlier (for example with `Parseff.peek_char`)
- call `Parseff.commit` once the grammar has clearly committed to a branch
- increase `~backtrack_window` for genuinely ambiguous grammars

### Blocking reads

`Parseff.Source.of_channel`, `Parseff.Source.of_chunks`, `Parseff.Source.of_function`, and `Parseff.Source.of_seq` all block the calling thread when waiting for data. For async/event-driven architectures, wrap the parse call in a thread:

```ocaml
  let result = Domain.spawn (fun () ->
    Parseff.parse_source source json
  ) |> Domain.join
```

### Regex at chunk boundaries

When a regex match extends to the end of the current buffer, the runtime refills and retries until the match no longer touches the boundary (or EOF). This is correct for typical patterns but may behave unexpectedly with zero-width matches at the end of input.


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
