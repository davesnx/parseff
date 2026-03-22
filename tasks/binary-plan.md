# Binary Parsing Support for Parseff

## Motivation

Angstrom provides `BE` and `LE` submodules for parsing binary data — multi-byte integers in big-endian and little-endian byte order, floats, doubles, and raw byte reads. Parseff currently has no equivalent. Since OCaml `string` is an immutable byte sequence, the infrastructure already handles binary data at the transport level; what's missing is the combinator layer to interpret those bytes as typed values.

## Design Principles

1. **Zero-allocation reads.** Integer parsers should not allocate. Use `String.get_int16_be` and friends (available since OCaml 4.13; parseff requires 5.3) to read directly from the input buffer.
2. **Effect per width.** One effect per byte-width (1, 2, 4, 8) avoids round-tripping through `any_char` N times or allocating intermediate strings. The handler reads the value and advances the position in a single step.
3. **Streaming-aware.** Each handler case must call `ensure_bytes` in the streaming path before reading, exactly like `Consume` and `Satisfy` do today.
4. **Mirror Angstrom's proven API** but adapted to parseff's direct-style `unit -> 'a` convention.

## Public API

### Top-level primitive

```ocaml
val take : int -> string
(** [take n] reads exactly [n] bytes and returns them as a string.
    Fails if fewer than [n] bytes remain. Useful for length-prefixed
    binary fields and fixed-width text fields alike. *)
```

`take` lives at the top level because it is useful in both text and binary
contexts.

### `BE` submodule (big-endian)

```ocaml
module BE : sig
  val any_uint8 : unit -> int
  (** Read one byte, return it as an unsigned integer (0-255). *)

  val any_int8 : unit -> int
  (** Read one byte, return it as a signed integer (-128 to 127). *)

  val any_int16 : unit -> int
  (** Read 2 bytes as a big-endian signed 16-bit integer. *)

  val any_uint16 : unit -> int
  (** Read 2 bytes as a big-endian unsigned 16-bit integer. *)

  val any_int32 : unit -> int32
  (** Read 4 bytes as a big-endian signed 32-bit integer. *)

  val any_int64 : unit -> int64
  (** Read 8 bytes as a big-endian signed 64-bit integer. *)

  val any_float : unit -> float
  (** Read 4 bytes as a big-endian IEEE 754 single-precision float. *)

  val any_double : unit -> float
  (** Read 8 bytes as a big-endian IEEE 754 double-precision float. *)

  val int16 : int -> unit
  (** [int16 i] matches exactly the 2 bytes that encode [i] in big-endian
      order. Fails if the bytes don't match. *)

  val int32 : int32 -> unit
  (** [int32 i] matches exactly the 4 bytes that encode [i] in big-endian
      order. *)

  val int64 : int64 -> unit
  (** [int64 i] matches exactly the 8 bytes that encode [i] in big-endian
      order. *)
end
```

### `LE` submodule (little-endian)

Same signature as `BE`, with little-endian byte order.

### Usage

Users pick their endianness module and work entirely within it:

```ocaml
open Parseff

let parse_png_header () =
  let _ = consume "\x89PNG\r\n\x1a\n" in
  let chunk_len = BE.any_int32 () in
  let chunk_type = take 4 in
  (chunk_len, chunk_type)

let parse_wav_header () =
  let _ = consume "RIFF" in
  let file_size = LE.any_int32 () in
  let _ = consume "WAVE" in
  file_size
```

Single-byte reads (`any_uint8`, `any_int8`) are identical in both modules —
a deliberate redundancy so that users never need to reach outside their chosen
endianness module.

## Internal Design

### New effects

Add one effect per byte-width, parameterised by endianness:

```ocaml
type endian = Big | Little

type _ Effect.t +=
  | Any_uint8 : int Effect.t
  | Any_int8 : int Effect.t
  | Any_int16 : endian -> int Effect.t
  | Any_int32 : endian -> int32 Effect.t
  | Any_int64 : endian -> int64 Effect.t
  | Take : int -> string Effect.t
```

Six new effect constructors total. The `endian` parameter avoids doubling the
constructors while still letting the handler branch on a single value to pick
the correct `String.get_*` function.

### Handler implementation (non-streaming)

Each case follows the same pattern — check bounds, read, advance:

```ocaml
| effect Any_uint8, k ->
    if st.pos < input_len then begin
      let v = String.get_uint8 st.input st.pos in
      st.pos <- st.pos + 1;
      Effect.Deep.continue k v
    end else
      Effect.Deep.discontinue k (Unexpected_eof st.pos)

| effect Any_int16 endian, k ->
    if st.pos + 2 <= input_len then begin
      let v = match endian with
        | Big -> String.get_int16_be st.input st.pos
        | Little -> String.get_int16_le st.input st.pos
      in
      st.pos <- st.pos + 2;
      Effect.Deep.continue k v
    end else
      Effect.Deep.discontinue k (Unexpected_eof st.pos)

| effect Take n, k ->
    if st.pos + n <= input_len then begin
      let s = String.sub st.input st.pos n in
      st.pos <- st.pos + n;
      Effect.Deep.continue k s
    end else
      Effect.Deep.discontinue k (Unexpected_eof st.pos)
```

`Any_int32`, `Any_int64` are identical in structure, using
`String.get_int32_be/le` and `String.get_int64_be/le`.

### Handler implementation (streaming)

Same as above, but preceded by `ensure_bytes`:

```ocaml
| effect Any_int16 endian, k ->
    ignore (ensure_bytes src st.pos 2);
    sync_to_source ();
    if st.pos + 2 <= src.input_len then begin
      let v = match endian with
        | Big -> String.get_int16_be src.input st.pos
        | Little -> String.get_int16_le src.input st.pos
      in
      st.pos <- st.pos + 2;
      Effect.Deep.continue k v
    end else
      Effect.Deep.discontinue k (Unexpected_eof st.pos)
```

### Combinator layer

The public functions are thin wrappers:

```ocaml
let take n =
  if n < 0 then invalid_arg "Parseff.take: negative count";
  if n = 0 then ""
  else Effect.perform (Take n)

module BE = struct
  let any_uint8 () = Effect.perform Any_uint8
  let any_int8 () = Effect.perform Any_int8
  let any_int16 () = Effect.perform (Any_int16 Big)
  let any_uint16 () =
    let v = Effect.perform (Any_int16 Big) in
    v land 0xFFFF
  let any_int32 () = Effect.perform (Any_int32 Big)
  let any_int64 () = Effect.perform (Any_int64 Big)
  let any_float () = Int32.float_of_bits (any_int32 ())
  let any_double () = Int64.float_of_bits (any_int64 ())

  let int16 expected =
    let actual = any_int16 () in
    if actual <> expected then
      fail (Printf.sprintf "expected int16 0x%04X, got 0x%04X" expected actual)

  let int32 expected =
    let actual = any_int32 () in
    if actual <> expected then
      fail (Printf.sprintf "expected int32 0x%08lX, got 0x%08lX" expected actual)

  let int64 expected =
    let actual = any_int64 () in
    if actual <> expected then
      fail (Printf.sprintf "expected int64 0x%016LX, got 0x%016LX" expected actual)
end

module LE = struct
  let any_uint8 () = Effect.perform Any_uint8
  let any_int8 () = Effect.perform Any_int8
  let any_int16 () = Effect.perform (Any_int16 Little)
  let any_uint16 () =
    let v = Effect.perform (Any_int16 Little) in
    v land 0xFFFF
  let any_int32 () = Effect.perform (Any_int32 Little)
  let any_int64 () = Effect.perform (Any_int64 Little)
  let any_float () = Int32.float_of_bits (any_int32 ())
  let any_double () = Int64.float_of_bits (any_int64 ())

  let int16 expected =
    let actual = any_int16 () in
    if actual <> expected then
      fail (Printf.sprintf "expected int16 0x%04X, got 0x%04X" expected actual)

  let int32 expected =
    let actual = any_int32 () in
    if actual <> expected then
      fail (Printf.sprintf "expected int32 0x%08lX, got 0x%08lX" expected actual)

  let int64 expected =
    let actual = any_int64 () in
    if actual <> expected then
      fail (Printf.sprintf "expected int64 0x%016LX, got 0x%016LX" expected actual)
end
```

`any_float`/`any_double` are derived — no dedicated effects needed. IEEE 754
bit-pattern conversion is handled by `Int32.float_of_bits` and
`Int64.float_of_bits` from the stdlib.

## Testing Plan

### `test/test_binary.ml`

1. **Round-trip for each width and endianness.** Construct a string with known
   bytes, parse it, check the value.
   ```
   "\x01\x02" -> BE.any_int16 = 0x0102
   "\x01\x02" -> LE.any_uint16 = 0x0201
   ```

2. **Signed vs unsigned.** Verify `BE.any_int8` returns -1 for `\xFF` while
    `BE.any_uint8` returns 255. Same for 16-bit. Confirm `BE` and `LE` byte
    readers return identical results.

3. **Exact-match parsers.** `BE.int16 0x0102` succeeds on `"\x01\x02"`, fails
   on `"\x02\x01"`.

4. **`take` combinator.** `take 4` on `"abcdef"` returns `"abcd"` with position
   at 4. `take 10` on `"abc"` fails with unexpected end of input.

5. **Composition with existing parsers.** Parse a binary header (magic bytes via
    `consume`, version via `BE.any_uint8`, length via `BE.any_int32`, payload
    via `take`).

6. **Streaming.** Same tests run through `parse_source` with a
   `Source.of_string` and a `Source.of_function` that delivers one byte at a
   time, to exercise `ensure_bytes` refill.

7. **Backtracking.** `or_` with a binary parser that fails should restore
   position correctly.

8. **Float/double.** Verify that `BE.any_float` and `BE.any_double` decode
   IEEE 754 representations of known values (0.0, 1.0, -1.0, infinity, NaN).

## File Changes

| File | Change |
|---|---|
| `lib/parseff.ml` | Add `endian` type, 6 new effect constructors, handler cases in both `run_deep` and `run_deep_source`, combinator functions, `BE`/`LE` modules |
| `lib/parseff.mli` | Add public API: `take`, `BE`/`LE` modules (each containing `any_uint8`, `any_int8`, `any_int16`, `any_uint16`, `any_int32`, `any_int64`, `any_float`, `any_double`, `int16`, `int32`, `int64`) |
| `test/test_binary.ml` | New test file covering all cases above |
| `test/dune` | Add `test_binary` to test targets |

## Example: TLV Message Parser

A practical example parsing a binary TLV (Type-Length-Value) wire format. Each
message has a fixed header followed by a sequence of TLV fields:

```
Header (10 bytes):
  magic    : 4 bytes  "MSG\x00"
  version  : 1 byte   uint8
  flags    : 1 byte   uint8
  num_fields : 2 bytes  uint16 big-endian
  total_len  : 2 bytes  uint16 big-endian

Field:
  tag      : 1 byte   uint8
  length   : 2 bytes  uint16 big-endian
  value    : <length> bytes
```

```ocaml
(* examples/tlv.ml *)

type field = { tag : int; value : string }

type message = {
  version : int;
  flags : int;
  fields : field list;
}

let parse_field () =
  let tag = Parseff.BE.any_uint8 () in
  let length = Parseff.BE.any_uint16 () in
  let value = Parseff.take length in
  { tag; value }

let parse_message () =
  (* magic *)
  let _ = Parseff.consume "MSG\x00" in
  let version = Parseff.BE.any_uint8 () in
  let flags = Parseff.BE.any_uint8 () in
  let num_fields = Parseff.BE.any_uint16 () in
  let _total_len = Parseff.BE.any_uint16 () in
  let fields = Parseff.count num_fields parse_field () in
  Parseff.end_of_input ();
  { version; flags; fields }

(* A hand-built message:
   "MSG\x00"                    magic
   "\x01"                       version = 1
   "\x02"                       flags = 2
   "\x00\x02"                   num_fields = 2
   "\x00\x0c"                   total_len = 12
   "\x0a" "\x00\x03" "foo"      field: tag=10, len=3, value="foo"
   "\x14" "\x00\x02" "hi"       field: tag=20, len=2, value="hi"
*)
let input =
  "MSG\x00\x01\x02\x00\x02\x00\x0c\
   \x0a\x00\x03foo\
   \x14\x00\x02hi"

let () =
  match Parseff.parse input parse_message with
  | Ok msg ->
      Printf.printf "version=%d flags=%d\n" msg.version msg.flags;
      List.iter
        (fun f -> Printf.printf "  tag=%d value=%S\n" f.tag f.value)
        msg.fields
  | Error { pos; error = `Expected msg } ->
      Printf.printf "Error at %d: %s\n" pos msg
  | Error { pos; error = `Unexpected_end_of_input } ->
      Printf.printf "Unexpected end of input at %d\n" pos
  | Error _ ->
      Printf.printf "Unknown error\n"
```

Expected output:

```
version=1 flags=2
  tag=10 value="foo"
  tag=20 value="hi"
```

This demonstrates the key pattern: `consume` for magic bytes, `BE.any_uint8`
and `BE.any_uint16` for header fields, `take` driven by a parsed length field,
and `count` to repeat a sub-parser — all composing naturally with existing
combinators.

## Non-goals (for now)

- **Bit-level parsing.** Parsing individual bits within a byte (like `bitstring`
  does) is a different problem. Not in scope.
- **Writing/serialization.** This is a parser library. Encoding values back to
  binary is out of scope.
- **Bigarray input.** Angstrom supports `Bigstringaf.t` as input. Parseff works
  on `string`. Supporting `bigarray` input would require changes to the core
  state type, not just adding combinators.
