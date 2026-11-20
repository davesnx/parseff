---
title: Streaming
description: Pull and push streaming from files, channels, and chunk callbacks
---

<!-- This file is generated from doc/streaming.mld. Do not edit directly. -->


# Streaming

Parseff provides two streaming interfaces for input that arrives incrementally:

- `Parseff.Source`: pull-based streaming (files, channels, sockets, sequences, Eio flows).
- `Parseff.Push`: push-based streaming where your caller owns the read loop (Lwt, Async, callback APIs).
Ordinary `unit -> 'a` parsers work unchanged with `Parseff.parse`, `Parseff.Source.parse`, and `Parseff.Push.parse`.


## Pull streaming: `Source.parse`

`Parseff.Source.parse` runs a parser pulling bytes from a `Parseff.Source.t` on demand.

```ocaml
val Source.parse :
  ?max_depth:int ->
  ?backtrack_window:int ->
  ?diagnostics:Diagnostics.t ->
  ?trace:(trace_event -> unit) ->
  ?consume:[ `All | `Partial ] ->
  Source.t ->
  (unit -> 'a) ->
  ('a, error) Stdlib.result
```
```ocaml
let ic = open_in "data.json" in
let source = Parseff.Source.of_channel ic in
let result = Parseff.Source.parse source json in
close_in ic;
result
```
- `~consume` defaults to `` `All ``, pulling one extra byte after the parser returns to check EOF without reading the rest of the source. Pass `` ~consume:`Partial `` to leave remaining bytes unread.
- `~backtrack_window` bounds how much buffered history Parseff retains for backtracking (default: `65536` bytes; minimum `2`). Exceeding it fails with `Backtrack_window_exceeded`.
- Error positions are absolute byte offsets from the start of the stream. `found` text reflects only bytes already buffered (Parseff does not pull more bytes just to lengthen a snippet).
- `~diagnostics` collects diagnostics as in `Parseff.parse`. See [`diagnostics`](./diagnostics.md).

## Streaming multiple values: `Source.parse_each`

`Parseff.Source.parse_each` parses a lazy sequence of values (such as NDJSON or log records) from one source, bounding memory by `~backtrack_window`.

```ocaml
val Source.parse_each :
  ?max_depth:int ->
  ?backtrack_window:int ->
  ?diagnostics:Diagnostics.t ->
  ?trace:(trace_event -> unit) ->
  ?pending:(unit -> bool) ->
  Source.t ->
  (unit -> 'a) ->
  ('a, error) Stdlib.result Seq.t
```
Each element runs the parser like `` Source.parse ~consume:`Partial ``. The parser must consume its trailing separator (a zero-width run yields an `Error`). The sequence stops at EOF or after the first `Error`, and is ephemeral (forcing an element twice raises `Invalid_argument`). Pass `~pending` if your parser buffers lookahead tokens past its value.

```ocaml
let record () =
  let id = Parseff.take_while ~at_least:1 (fun c -> c >= '0' && c <= '9') in
  let _ = Parseff.char '\n' in
  int_of_string id

let () =
  Parseff.Source.parse_each (Parseff.Source.of_string "1\n22\n333\n") record
  |> Seq.iter (function
       | Ok id -> Printf.printf "%d\n" id
       | Error e -> prerr_endline (Parseff.render_error e))
```

## Source constructors

A `Parseff.Source.t` is for a single `Source.parse` or `Source.parse_each` call (only `of_string` restarts from offset 0; other sources continue where the last read stopped and drop buffered lookahead).


### `Source.of_string`

Wraps a string as a source (useful for testing streaming code paths).

```ocaml
val of_string : string -> Source.t
```

### `Source.of_channel`

Reads from an `in_channel` with buffer size `~buf_size` (default `4096`; raises `Invalid_argument` if `< 1`).

```ocaml
val of_channel : ?buf_size:int -> in_channel -> Source.t
```

### `Source.of_chunks`

Pulls chunks from a callback returning `Some s` for data (empty strings are skipped) or `None` for EOF. Chunks may be used without copying and must not be mutated after the callback returns (relevant only with `Bytes.unsafe_to_string`).

```ocaml
val of_chunks : (unit -> string option) -> Source.t
```
```ocaml
let source =
  Parseff.Source.of_chunks (fun () ->
      match Http.Body.read body with
      | `Data chunk -> Some chunk
      | `Eof -> None)
in
Parseff.Source.parse source my_parser
```

### `Source.of_seq`

Reads chunks from a `string Seq.t` (same chunk-immutability rule as `of_chunks`).

```ocaml
val of_seq : string Seq.t -> Source.t
```
```ocaml
let source =
  Parseff.Source.of_seq (List.to_seq [ "1\n"; "22\n"; "333\n" ])
```

### `Source.of_bigstring_chunks`

Pulls slices `Some (buf, off, len)` of a bigstring (`Bigstringaf.t`, `Cstruct.buffer`, `Unix.read_bigarray`) or `None` for EOF. Copies at most 2040 bytes at a time into Parseff's input buffer; slices must remain unchanged until the next callback invocation, which may reuse the same buffer.

```ocaml
type bigstring =
  (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

val of_bigstring_chunks :
  (unit -> (bigstring * int * int) option) -> Source.t
```
```ocaml
let parse_bigstring buf parser =
  let sent = ref false in
  let source =
    Parseff.Source.of_bigstring_chunks (fun () ->
        if !sent then None
        else (
          sent := true;
          Some (buf, 0, Bigarray.Array1.dim buf)))
  in
  Parseff.Source.parse source parser

let () =
  let text = "key=value" in
  let buf =
    Bigarray.Array1.init Bigarray.char Bigarray.c_layout (String.length text)
      (String.get text)
  in
  let pair () =
    let key = Parseff.take_while (fun c -> c <> '=') in
    let _ = Parseff.char '=' in
    (key, Parseff.take_while (fun _ -> true))
  in
  match parse_bigstring buf pair with
  | Ok pair -> assert (pair = ("key", "value"))
  | Error e -> failwith (Parseff.render_error e)
```

### `Source.of_function`

Low-level escape hatch matching POSIX `read(2)`: `read buf off len` writes up to `len` bytes into `buf` at `off` and returns the byte count (`0` for EOF). Starts with a buffer of `~initial_buffer_size` bytes (default `2040`) and grows once to `max initial_buffer_size 4096` after a read fills it. Prefer `of_chunks` or `of_seq` unless wrapping `Unix.read`, `Unix.recv`, or similar buffer-filling APIs.

```ocaml
val of_function :
  ?initial_buffer_size:int -> (bytes -> int -> int -> int) -> Source.t
```
```ocaml
let source =
  Parseff.Source.of_function ~initial_buffer_size:512 (fun buf off len ->
      Unix.read fd buf off len)
in
Parseff.Source.parse source my_parser
```

## Push streaming: `Push`

Use `Parseff.Push` when your event loop pushes chunks into the parser (Lwt, Async, network callbacks).

```ocaml
module Push : sig
  type 'a t
  type 'a step =
    | Await
    | Done of ('a * string, error) Stdlib.result

  val parse :
    ?max_depth:int ->
    ?backtrack_window:int ->
    ?diagnostics:Diagnostics.t ->
    ?trace:(trace_event -> unit) ->
    (unit -> 'a) ->
    'a t
  val feed : 'a t -> string -> 'a step
  val finish : 'a t -> ('a * string, error) Stdlib.result

  type each_step =
    | Need_input
    | Stopped of string
    | Error_of of error
    | Ended

  val parse_each :
    ?max_depth:int ->
    ?backtrack_window:int ->
    ?diagnostics:Diagnostics.t ->
    ?trace:(trace_event -> unit) ->
    on_value:('a -> [ `Continue | `Stop ]) ->
    (unit -> 'a) ->
    string option ->
    each_step
end
```
`Parseff.Push.parse`, `Parseff.Push.feed`, and `Parseff.Push.finish` (unstable) parse a single value and return any leftover bytes in `Done (Ok (value, leftover))`. End your parser with `Parseff.end_of_input` if you want to require full consumption. Always call `Push.finish` on an unfinished parse if you passed `~diagnostics` or mutated `Parseff.Ref` cells so those resources are released.

`Parseff.Push.parse_each` parses a stream of values on a single effect fiber (the push counterpart of `Parseff.Source.parse_each`). Push `Some chunk` for data and `None` at EOF:

```ocaml
let line () =
  let s = Parseff.take_while (fun c -> c <> '\n') in
  let _ = Parseff.char '\n' in
  s

let push_lines chunks =
  let seen = ref [] in
  let push =
    Parseff.Push.parse_each
      ~on_value:(fun s ->
        seen := s :: !seen;
        `Continue)
      line
  in
  let rec go = function
    | [] -> push None
    | chunk :: rest -> (
        match push (Some chunk) with
        | Parseff.Push.Need_input -> go rest
        | step -> step)
  in
  match go chunks with
  | Parseff.Push.Ended -> Ok (List.rev !seen)
  | Parseff.Push.Error_of e -> Error e
  | Parseff.Push.Need_input | Parseff.Push.Stopped _ -> assert false

let () = assert (push_lines [ "ab"; "c\nde\n" ] = Ok [ "abc"; "de" ])
```

## Backtracking and `~backtrack_window`

Parseff retains buffered bytes from the oldest active rewind point to the newest read byte. Two things pin earlier bytes:

- Open backtracking frames (`or_`, `one_of`, `look_ahead`, repetition loops).
- Primitives building a single value across chunks (`take_while`, `recognize`).
To stay within `~backtrack_window`:

- Dispatch early with `Parseff.peek_char` rather than trying long speculative branches.
- Call `Parseff.commit` to release the nearest frame once an alternative is chosen, or `Parseff.cut` to release every frame inside the enclosing `Parseff.context` once a construct is decided.
- Use `Parseff.many_iter`, `Parseff.sep_by_iter`, `Parseff.take_iter`, or `Parseff.take_while_iter` to stream items or large byte payloads past `~backtrack_window`.
- Raise `~backtrack_window` only when the grammar genuinely requires longer lookahead.
```ocaml
let http_method () =
  Parseff.or_
    (fun () ->
      Parseff.literal "GET ";
      Parseff.commit ();
      `GET)
    (fun () ->
      Parseff.literal "POST ";
      Parseff.commit ();
      `POST)
```

## Concurrency and domains

- **Multiple domains**: `Parseff.parse` and `Parseff.Source.parse` store active parse state per domain and can run concurrently across OCaml 5 domains. Do not share one `Parseff.Source.t` or `Parseff.Push.t` concurrently across domains.
- **Cooperative fibers on one domain**: multiple `Parseff.Source.parse` calls (e.g., Eio fibers) and `Parseff.Push` parses can interleave on one domain, provided only the source's read function suspends (suspending inside a parser callback is not supported).
- **System threads**: OCaml `Thread`s sharing a domain can preempt mid-parser; serialize parses on one domain with a `Mutex` or run them on separate domains.