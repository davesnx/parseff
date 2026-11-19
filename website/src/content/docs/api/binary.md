---
title: Binary parsing
description: Big-endian and little-endian integers, floats and varints
---

<!-- This file is generated from doc/binary.mld. Do not edit directly. -->


# Binary Parsing

The `Parseff.BE` and `Parseff.LE` modules provide parsers for multi-byte integers, floats, and doubles in big-endian and little-endian byte order respectively. Pick the module matching your wire format's byte order.

Single-byte readers (`Parseff.any_uint8`, `Parseff.any_int8`) and the varint reader (`Parseff.any_varint`) are at the top level of `Parseff`, since endianness does not affect them.


## Integer parsers


### `any_uint8` and `any_int8`

`Parseff.any_uint8` reads one byte as an unsigned integer (0--255). `Parseff.any_int8` reads one byte as a signed integer (-128--127).

```ocaml
val any_uint8 : unit -> int
```
```ocaml
val any_int8 : unit -> int
```
```ocaml
let color_type () = Parseff.any_uint8 ()    (* PNG color type: 0–6 *)
let temperature () = Parseff.any_int8 ()    (* signed sensor reading *)
```

### `any_int16` and `any_uint16`

`Parseff.BE.any_int16` reads 2 bytes as a signed 16-bit integer. `Parseff.BE.any_uint16` reads 2 bytes as an unsigned 16-bit integer.

```ocaml
val any_int16 : unit -> int
```
```ocaml
val any_uint16 : unit -> int
```
```ocaml
let dns_query_count () = Parseff.BE.any_uint16 ()   (* QDCOUNT field *)
let wav_channels () = Parseff.LE.any_uint16 ()      (* number of channels *)
```

### `any_int32` and `any_uint32`

`Parseff.BE.any_int32` reads 4 bytes as a signed 32-bit integer, returned as `int32`. `Parseff.BE.any_uint32` reads 4 bytes as an unsigned 32-bit integer, from `0` to `4294967295`, returned as `int`.

```ocaml
val any_int32 : unit -> int32
```
```ocaml
val any_uint32 : unit -> int
```
```ocaml
let png_chunk_length () = Parseff.BE.any_int32 ()
let elf_entry_point () = Parseff.LE.any_int32 ()
let zip_entry_size () = Parseff.LE.any_uint32 ()
```

### `any_int64`

`Parseff.BE.any_int64` reads 8 bytes as a signed 64-bit integer, returned as `int64`.

```ocaml
val any_int64 : unit -> int64
```
```ocaml
let timestamp () = Parseff.LE.any_int64 ()   (* 64-bit Unix timestamp *)
```

### `any_varint`

`Parseff.any_varint` reads an unsigned LEB128 varint, a variable-length integer: `\x01` is `1` and `\xAC\x02` is `300`. A value above `max_int` fails, so a negative Protocol Buffers `int32` or `int64` is rejected. There is no zigzag reader; decode a zigzag value with `(n lsr 1) lxor (- (n land 1))`.

```ocaml
val any_varint : unit -> int
```
```ocaml
let protobuf_bytes_field () =
  let len = Parseff.any_varint () in
  Parseff.take len

let () =
  assert (Parseff.parse "\xAC\x02" Parseff.any_varint = Ok 300)
```

## Floating-point parsers


### `any_float`

`Parseff.BE.any_float` reads 4 bytes as an IEEE 754 single-precision float. Internally reads an `int32` and converts via `Int32.float_of_bits`.

```ocaml
val any_float : unit -> float
```

### `any_double`

`Parseff.BE.any_double` reads 8 bytes as an IEEE 754 double-precision float. Internally reads an `int64` and converts via `Int64.float_of_bits`.

```ocaml
val any_double : unit -> float
```
```ocaml
let coordinates () =
  let lat = Parseff.BE.any_double () in
  let lon = Parseff.BE.any_double () in
  (lat, lon)
```

## Exact-match parsers

`Parseff.BE.int16`, `Parseff.BE.int32`, and `Parseff.BE.int64` read the corresponding number of bytes and assert that the decoded value matches an expected constant. They fail with an error message showing both the expected and actual values in hexadecimal.

```ocaml
val int16 : int -> unit
```
```ocaml
val int32 : int32 -> unit
```
```ocaml
val int64 : int64 -> unit
```
Use these for magic numbers, version fields, and fixed protocol headers:

```ocaml
let png_magic () =
  (* PNG files start with this 4-byte sequence *)
  Parseff.BE.int32 0x89504E47l

let wav_header () =
  Parseff.literal "RIFF";
  let size = Parseff.LE.any_int32 () in
  Parseff.literal "WAVE";
  size
```

## `take`

`Parseff.take` reads exactly `n` bytes and returns them as a string. While not inside `Parseff.BE` or `Parseff.LE` (byte blobs have no endianness), it is essential for binary parsing — reading magic bytes, fixed-width fields, and length-prefixed payloads.

```ocaml
val take : int -> string
```
Returns `""` for `n = 0` without consuming input. Fails if fewer than `n` bytes remain, with `expected 5 bytes, found only 3 (end of input)`, or `found end of input` when none remain. A negative `n` fails with `expected non-negative length, found -1`, an `Expected` error that `or_` and `optional` recover from.

```ocaml
(* Length-prefixed payload *)
let payload () =
  let len = Parseff.BE.any_uint16 () in
  Parseff.take len

(* Fixed-width chunk type *)
let chunk_type () = Parseff.take 4
```
`Parseff.Slice.take` returns a slice instead, so that `Parseff.parse` does not copy the bytes. See [Zero-copy API](./zero-copy.md).


## Mixing with text primitives

Binary and text primitives can be freely mixed. Use `Parseff.literal` for ASCII magic strings and `Parseff.BE`/`Parseff.LE` for numeric fields:

```ocaml
let parse_png_ihdr () =
  let length = Parseff.BE.any_int32 () in
  let chunk_type = Parseff.take 4 in
  let width = Parseff.BE.any_int32 () in
  let height = Parseff.BE.any_int32 () in
  let bit_depth = Parseff.any_uint8 () in
  let color_type = Parseff.any_uint8 () in
  (chunk_type, width, height, bit_depth, color_type)
```

## Composition patterns


### TLV (Type-Length-Value)

Many binary formats use tag-length-value structures. The binary primitives compose naturally for this pattern:

```ocaml
let tlv_field () =
  let tag = Parseff.any_uint8 () in
  let len = Parseff.BE.any_uint16 () in
  let value = Parseff.take len in
  (tag, value)

let tlv_message () =
  Parseff.literal "TL";
  let version = Parseff.any_uint8 () in
  let count = Parseff.BE.any_uint16 () in
  let fields = Parseff.count count tlv_field in
  (version, fields)
```

### Backtracking

Binary parsers support backtracking with `Parseff.or_` just like text parsers. On failure the position resets, so the alternative branch re-reads from the same offset:

```ocaml
let version_field () =
  Parseff.or_
    (fun () ->
      Parseff.BE.int16 0xFFFF;
      `Legacy)
    (fun () ->
      let v = Parseff.BE.any_uint16 () in
      `Version v)
```

## Error handling

All binary parsers fail with `expected N bytes` when there are not enough bytes remaining. The position in the error points to the byte where the read was attempted, not the end of input, and the error says how many bytes are left: `expected 4 bytes, found only 3 (end of input)`, or `found end of input` when nothing is left.

Exact-match parsers (`Parseff.BE.int16`, `Parseff.BE.int32`, `Parseff.BE.int64`) fail with the expected value in hexadecimal as an expected item with the endianness, and `found` shows the integer that was read in the same width, e.g. `expected int32be 0x01020304, found 0xDE000000`. Like any other expected item it merges with what the alternatives expected at the same position.


## Position tracking

Positions are byte offsets, consistent with the rest of parseff. Each parser advances the position by the number of bytes it reads:

- `Parseff.any_uint8`, `Parseff.any_int8`: 1 byte
- `Parseff.BE.any_int16`, `Parseff.BE.any_uint16`: 2 bytes
- `Parseff.BE.any_int32`, `Parseff.BE.any_uint32`, `Parseff.BE.any_float`: 4 bytes
- `Parseff.BE.any_int64`, `Parseff.BE.any_double`: 8 bytes
- `Parseff.any_varint`: 1 to 9 bytes
- `Parseff.take`, `Parseff.Slice.take`: `n` bytes

## Performance

Integer parsers use the OCaml stdlib's `String.get_int16_be`, `String.get_int32_le`, etc. directly on the input buffer with no intermediate allocations. Float and double parsers are derived from the integer parsers using `Int32.float_of_bits` and `Int64.float_of_bits` respectively — this avoids extra indirection while remaining allocation-free.


## Streaming support

All binary parsers work with streaming input (`Parseff.parse_source`). Multi-byte reads that span chunk boundaries are handled correctly — the streaming runtime ensures enough bytes are buffered before each read.
