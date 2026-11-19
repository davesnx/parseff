---
title: Binary parsing
description: Big-endian and little-endian integers, floats and varints
---

<!-- This file is generated from doc/binary.mld. Do not edit directly. -->


# Binary parsing

`Parseff.BE` (big-endian) and `Parseff.LE` (little-endian) provide multi-byte integer and IEEE 754 float parsers. Single-byte readers (`Parseff.any_uint8`, `Parseff.any_int8`) and `Parseff.any_varint` live at the top level of `Parseff`.


## Integer parsers

```ocaml
(* Top-level *)
val any_uint8 : unit -> int    (* 1 byte, 0..255 *)
val any_int8 : unit -> int     (* 1 byte, -128..127 *)
val any_varint : unit -> int   (* 1..9 bytes, unsigned LEB128 *)

(* Inside both Parseff.BE and Parseff.LE *)
val any_int16 : unit -> int    (* 2 bytes, signed *)
val any_uint16 : unit -> int   (* 2 bytes, unsigned *)
val any_uint24 : unit -> int   (* 3 bytes, 0..16777215 *)
val any_int32 : unit -> int32  (* 4 bytes, signed *)
val any_uint32 : unit -> int   (* 4 bytes, 0..4294967295 *)
val any_int64 : unit -> int64  (* 8 bytes, signed *)
```
- `Parseff.any_varint` reads an unsigned LEB128 varint (7 bits per byte, LSB first; `"\xAC\x02"` is `300`). Fails if longer than 9 bytes or above `max_int` (rejecting negative Protobuf `int32`/`int64`). Decode zigzag values manually with `(n lsr 1) lxor (-(n land 1))`.
- On platforms where `int` has fewer than 63 bits (such as `js_of_ocaml`), large values in `any_varint` or `any_uint32` overflow silently.
```ocaml
let png_chunk_length () = Parseff.BE.any_int32 ()
let http2_frame_length () = Parseff.BE.any_uint24 ()
let zip_entry_size () = Parseff.LE.any_uint32 ()

let protobuf_bytes_field () =
  let len = Parseff.any_varint () in
  Parseff.take len

let () = assert (Parseff.parse "\xAC\x02" Parseff.any_varint = Ok 300)
```

## Floating-point parsers

Inside both `Parseff.BE` and `Parseff.LE`:

```ocaml
val any_float : unit -> float   (* 4 bytes, IEEE 754 single precision *)
val any_double : unit -> float  (* 8 bytes, IEEE 754 double precision *)
```
```ocaml
let coordinates () =
  let lat = Parseff.BE.any_double () in
  let lon = Parseff.BE.any_double () in
  (lat, lon)
```

## Exact-match parsers

`Parseff.BE.int16`, `Parseff.BE.int32`, and `Parseff.BE.int64` (and their `Parseff.LE` counterparts) assert that the next bytes encode an exact constant. `int16` accepts `-32768`..`65535` and raises `Invalid_argument` outside that range.

```ocaml
val int16 : int -> unit
val int32 : int32 -> unit
val int64 : int64 -> unit
```
```ocaml
let png_magic () = Parseff.BE.int32 0x89504E47l

let wav_header () =
  Parseff.literal "RIFF";
  let size = Parseff.LE.any_int32 () in
  Parseff.literal "WAVE";
  size
```

## Fixed-length reads and framed payloads

`Parseff.take` reads exactly `n` bytes as a string (fails with an `Expected` error if fewer than `n` bytes remain or `n < 0`). `Parseff.Slice.take` returns a zero-copy `Parseff.Slice.t` (see [Zero-copy API](./zero-copy.md)). `Parseff.within` (unstable) runs an inner parser bounded to the next `n` bytes without copying.

```ocaml
val take : int -> string
val Slice.take : int -> Slice.t
val within : int -> (unit -> 'a) -> 'a
```

## Mixing binary and text primitives

Binary and text primitives compose freely, including backtracking with `Parseff.or_`:

```ocaml
let tlv_field () =
  let tag = Parseff.any_uint8 () in
  let len = Parseff.BE.any_uint16 () in
  let value = Parseff.take len in
  (tag, value)

let tlv_message () =
  Parseff.literal "TL";
  let version =
    Parseff.or_
      (fun () ->
        Parseff.BE.int16 0xFFFF;
        `Legacy)
      (fun () -> `Version (Parseff.BE.any_uint16 ()))
  in
  let count = Parseff.BE.any_uint16 () in
  let fields = Parseff.count count tlv_field in
  (version, fields)
```

## Errors, positions, and streaming

- **Short reads**: fail at the start of the field with `expected N bytes, found only K (end of input)` (or `found end of input` if no bytes remain).
- **Exact-match mismatches**: fail at the start of the field with an `Expected` error formatted in hexadecimal, e.g. `expected int32be 0x01020304, found 0xDE000000`, merging with other alternatives at the same offset.
- **Positions**: always byte offsets, advancing by the field width.
- **Streaming and performance**: multi-byte reads use the stdlib's `String.get_int16_be`/`String.get_int32_le` primitives directly on the buffer and work across chunk boundaries in `Parseff.Source.parse` and `Parseff.Push`.