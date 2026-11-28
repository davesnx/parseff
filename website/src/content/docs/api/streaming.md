---
title: Streaming
description: Pull and push streaming from files, channels, and chunk callbacks
---

<!-- This file is generated from doc/streaming.mld. Do not edit directly. -->

# Streaming

Parseff provides two streaming interfaces for input that arrives incrementally:

- [`Parseff.Source`](./Parseff-Source.md): pull-based streaming (files, channels, sockets, sequences, Eio flows).
- [`Parseff.Push`](./Parseff-Push.md): push-based streaming where your caller owns the read loop (Lwt, Async, callback APIs).

Ordinary `unit -> 'a` parsers work unchanged with [`Parseff.parse`](./Parseff.md#val-parse), [`Parseff.Source.parse`](./Parseff-Source.md#val-parse), and [`Parseff.Push.parse`](./Parseff-Push.md#val-parse).

## Pull streaming: `Source.parse`

[`Parseff.Source.parse`](./Parseff-Source.md#val-parse) runs a parser pulling bytes from a [`Parseff.Source.t`](./Parseff-Source.md#type-t) on demand.

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

- `~consume` defaults to `` `All ``, pulling one extra byte after the parser returns to check EOF without reading the rest of the source. Pass ``~consume:`Partial`` to leave remaining bytes unread.
- `~backtrack_window` bounds how much buffered history Parseff retains for backtracking (default: `65536` bytes; minimum `2`). Exceeding it fails with `Backtrack_window_exceeded`.
- Error positions are byte offsets from where this parse starts in the stream. `found` text covers only whole characters already buffered (Parseff does not pull more bytes just to lengthen a snippet), so it can be a prefix of the text [`Parseff.parse`](./Parseff.md#val-parse) reports and `range.stop` can be smaller. `Text ""` means no whole character is buffered at the failure position (it is at the end of the input read so far, or the character there is incomplete): the range is empty and the message has no `found` part.
- `~diagnostics` collects diagnostics as in [`Parseff.parse`](./Parseff.md#val-parse). See [`diagnostics`](./diagnostics.md).

## Streaming multiple values: `Source.parse_each`

[`Parseff.Source.parse_each`](./Parseff-Source.md#val-parse_each) parses a lazy sequence of values (such as NDJSON or log records) from one source, bounding memory by `~backtrack_window`.

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

Each element runs the parser like ``Source.parse ~consume:`Partial``. The parser must consume its trailing separator (a zero-width run yields an `Error`). The sequence stops at EOF or after the first `Error`, and is ephemeral (forcing an element twice raises `Invalid_argument`). Pass `~pending` if your parser buffers lookahead tokens past its value: the run after `pending` returns `true` may consume no input, but a second zero-width run in a row at the same position yields an `Error`. [`Parseff.Push.parse_each`](./Parseff-Push.md#val-parse_each) follows the same rule.

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

The next `Source.parse` or `Source.parse_each` on the same source starts where the last one stopped (see [`Parseff.Source.parse`](./Parseff-Source.md#val-parse)), so a message head and its body can be parsed in turn, and a bad record can be skipped. Positions restart at each parse; [`Parseff.Source.offset`](./Parseff-Source.md#val-offset) gives the absolute byte offset where the next parse starts.

### `Source.of_string`

Wraps a string as a source (useful for testing streaming code paths).

```ocaml
val of_string : string -> Source.t
```

### `Source.of_channel`

Reads from an `in_channel` with buffer size `~buffer_size` (default `4096`; raises `Invalid_argument` if `< 1`).

```ocaml
val of_channel : ?buffer_size:int -> in_channel -> Source.t
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

Use [`Parseff.Push`](./Parseff-Push.md) when your event loop pushes chunks into the parser (Lwt, Async, network callbacks).

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
  val cancel : 'a t -> unit

  type each_step =
    | Await
    | Stopped of string
    | Error of error
    | Ended

  val parse_each :
    ?max_depth:int ->
    ?backtrack_window:int ->
    ?diagnostics:Diagnostics.t ->
    ?trace:(trace_event -> unit) ->
    ?pending:(unit -> bool) ->
    on_value:('a -> [ `Continue | `Stop ]) ->
    (unit -> 'a) ->
    (string option -> each_step) * (unit -> unit)
end
```

[`Parseff.Push.parse`](./Parseff-Push.md#val-parse), [`Parseff.Push.feed`](./Parseff-Push.md#val-feed), and [`Parseff.Push.finish`](./Parseff-Push.md#val-finish) parse a single value and return any leftover bytes in `Done (Ok (value, leftover))`. End your parser with [`Parseff.end_of_input`](./Parseff.md#val-end_of_input) if you want to require full consumption. To drop an unfinished parse, for example when the client disconnects, call [`Parseff.Push.cancel`](./Parseff-Push.md#val-cancel): it releases the [`Parseff.Ref`](./Parseff-Ref.md) cells and the `~diagnostics` collector that the parse holds, without giving it more input.

[`Parseff.Push.parse_each`](./Parseff-Push.md#val-parse_each) parses a stream of values on a single effect fiber (the push counterpart of [`Parseff.Source.parse_each`](./Parseff-Source.md#val-parse_each)). It returns a `push` function and a `cancel` function. Push `Some chunk` for data and `None` at EOF:

```ocaml
let line () =
  let s = Parseff.take_while (fun c -> c <> '\n') in
  let _ = Parseff.char '\n' in
  s

let push_lines chunks =
  let seen = ref [] in
  let push, _cancel =
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
        | Parseff.Push.Await -> go rest
        | step -> step)
  in
  match go chunks with
  | Parseff.Push.Ended -> Ok (List.rev !seen)
  | Parseff.Push.Error e -> Error e
  | Parseff.Push.Await | Parseff.Push.Stopped _ -> assert false

let () = assert (push_lines [ "ab"; "c\nde\n" ] = Ok [ "abc"; "de" ])
```

## Backtracking and `~backtrack_window`

Parseff retains buffered bytes from the oldest active rewind point to the newest read byte. Two things pin earlier bytes:

- Open backtracking frames (`or_`, `one_of`, `look_ahead`, repetition loops).
- Primitives building a single value across chunks (`take`, `take_while`, `recognize`), even after [`Parseff.commit`](./Parseff.md#val-commit) or [`Parseff.cut`](./Parseff.md#val-cut).

To stay within `~backtrack_window`:

- Dispatch early with [`Parseff.peek_char`](./Parseff.md#val-peek_char) rather than trying long speculative branches.
- Call [`Parseff.commit`](./Parseff.md#val-commit) to release the nearest frame once an alternative is chosen, or [`Parseff.cut`](./Parseff.md#val-cut) to release every frame inside the enclosing [`Parseff.context`](./Parseff.md#val-context) once a construct is decided.
- Use [`Parseff.Iter`](./Parseff-Iter.md) to stream items or large byte payloads past `~backtrack_window` (see [`Iter`](#iter)).
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

## `Iter`

[`Parseff.Iter`](./Parseff-Iter.md) passes each value or byte piece to a callback `f` instead of returning it. Memory stays bounded by one element or one piece, so the input can be much larger than `~backtrack_window`.

```ocaml
module Iter : sig
  val many : ?at_least:int -> (unit -> 'a) -> ('a -> unit) -> unit
  val sep_by :
    ?at_least:int -> (unit -> 'a) -> (unit -> 'b) -> ('a -> unit) -> unit
  val take : int -> (Slice.t -> unit) -> unit
  val take_while :
    ?at_most:int ->
    ?set:Charset.t ->
    (char -> bool) ->
    (Slice.t -> unit) ->
    unit
end
```

- [`Parseff.Iter.many`](./Parseff-Iter.md#val-many) and [`Parseff.Iter.sep_by`](./Parseff-Iter.md#val-sep_by) work like [`Parseff.many`](./Parseff.md#val-many) and [`Parseff.sep_by`](./Parseff.md#val-sep_by), but give each element to `f` and do not build a list.
- [`Parseff.Iter.take`](./Parseff-Iter.md#val-take) and [`Parseff.Iter.take_while`](./Parseff-Iter.md#val-take_while) work like [`Parseff.take`](./Parseff.md#val-take) and [`Parseff.take_while`](./Parseff.md#val-take_while), but give the bytes to `f` as non-empty [`Parseff.Slice.t`](./Parseff-Slice.md#type-t) pieces. A piece borrows the input buffer and is valid only during `f`. Call [`Parseff.Slice.to_string`](./Parseff-Slice.md#val-to_string) to keep it.
- `Iter.take_while ~at_most` fails with `Limit_exceeded` after `f` has received `at_most` bytes, and the bytes need not fit in `~backtrack_window`.

```ocaml
let sum_ints input =
  let total = ref 0 in
  let r =
    Parseff.parse input (fun () ->
        Parseff.Iter.sep_by Parseff.integer (fun () -> Parseff.char ',')
          (fun n -> total := !total + n))
  in
  Result.map (fun () -> !total) r
```

All four functions share these rules:

- **Sealing.** Before each call to `f`, they seal every open backtracking frame, like [`Parseff.cut`](./Parseff.md#val-cut) outside any [`Parseff.context`](./Parseff.md#val-context). The source can then drop earlier input. A later failure cannot rewind before a value that `f` already received. [`Parseff.recognize`](./Parseff.md#val-recognize) and [`Parseff.Slice.recognize`](./Parseff-Slice.md#val-recognize) still keep all of their input.
- **Callback.** `f` runs outside the parser. Do not call parsers or [`Parseff.warn`](./Parseff.md#val-warn) inside `f`. An exception from `f` stops the parse and propagates out of the runner.
- **Frames that cannot be sealed.** A call inside [`Parseff.look_ahead`](./Parseff.md#val-look_ahead), [`Parseff.peek_with`](./Parseff.md#val-peek_with), [`Parseff.not_followed_by`](./Parseff.md#val-not_followed_by), a [`Parseff.precedence`](./Parseff.md#val-precedence) lookup, or [`Parseff.capture`](./Parseff.md#val-capture) without `~release:true` raises `Invalid_argument`. Some other frames raise only when `~diagnostics` is active, so test these parsers with `~diagnostics`.

See [`Parseff.Iter`](./Parseff-Iter.md) for the full contract.

[`examples/streaming_json_array/main.ml`](https://github.com/davesnx/parseff/blob/main/examples/streaming_json_array/main.ml) parses 20000 array elements through `Iter.sep_by` under `Source.parse ~backtrack_window:4096`, so the input is far larger than the window.

### `Iter` or `parse_each`

| Input | Use |
| --- | --- |
| A top-level stream of values, such as NDJSON or log records. | [`Parseff.Source.parse_each`](./Parseff-Source.md#val-parse_each) or [`Parseff.Push.parse_each`](./Parseff-Push.md#val-parse_each). |
| Repeated items inside a larger structure, such as the elements of a JSON array between its brackets. | [`Parseff.Iter.many`](./Parseff-Iter.md#val-many) or [`Parseff.Iter.sep_by`](./Parseff-Iter.md#val-sep_by) inside one [`Parseff.Source.parse`](./Parseff-Source.md#val-parse). |
| A body larger than `~backtrack_window`, such as a length-prefixed payload. | [`Parseff.Iter.take`](./Parseff-Iter.md#val-take) or [`Parseff.Iter.take_while`](./Parseff-Iter.md#val-take_while). |

## Concurrency and domains

- **Multiple domains**: [`Parseff.parse`](./Parseff.md#val-parse) and [`Parseff.Source.parse`](./Parseff-Source.md#val-parse) store active parse state per domain and can run concurrently across OCaml 5 domains. Do not share one [`Parseff.Source.t`](./Parseff-Source.md#type-t) or [`Parseff.Push.t`](./Parseff-Push.md#type-t) concurrently across domains.
- **Cooperative fibers on one domain**: multiple [`Parseff.Source.parse`](./Parseff-Source.md#val-parse) calls (e.g., Eio fibers) and [`Parseff.Push`](./Parseff-Push.md) parses can interleave on one domain, provided only the source's read function suspends (suspending inside a parser callback is not supported).
- **System threads**: OCaml `Thread`s sharing a domain can preempt mid-parser; serialize parses on one domain with a `Mutex` or run them on separate domains.
