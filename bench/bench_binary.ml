open Benchmark

(* Single byte: 0xAB *)
let one_byte = "\xAB"

(* 4 bytes: big-endian int32 = 0x01020304 *)
let four_bytes = "\x01\x02\x03\x04"

(* 8 bytes: big-endian int64 = 0x0102030405060708 *)
let eight_bytes = "\x01\x02\x03\x04\x05\x06\x07\x08"

(* 16 bytes for `take 16` *)
let sixteen_bytes =
  "\x00\x01\x02\x03\x04\x05\x06\x07\x08\x09\x0A\x0B\x0C\x0D\x0E\x0F"

(* TLV: 10 entries, each = 1-byte tag + 2-byte BE length + N-byte value.
   We use tag=0x01, length=4, value=4 zero bytes for each entry = 7 bytes/entry. *)
let tlv_input =
  let buf = Buffer.create 70 in
  for _ = 1 to 10 do
    Buffer.add_char buf '\x01';
    (* BE uint16 length = 4 *)
    Buffer.add_char buf '\x00';
    Buffer.add_char buf '\x04';
    (* 4 value bytes *)
    Buffer.add_string buf "\xDE\xAD\xBE\xEF"
  done;
  Buffer.contents buf

(* Streaming source that delivers `four_bytes` one byte at a time *)
let streaming_source_of_string ?(chunk_size = 1) s =
  let pos = ref 0 in
  Parseff.Source.of_function (fun buf off len ->
      let available = String.length s - !pos in
      let n = min (min chunk_size len) available in
      Bytes.blit_string s !pos buf off n;
      pos := !pos + n;
      n
  )

let bench_be_any_uint8 () =
  match Parseff.parse one_byte (fun () -> ignore (Parseff.BE.any_uint8 ())) with
  | Ok _ ->
      ()
  | Error _ ->
      failwith "bench_be_any_uint8 failed"

let bench_be_any_int32 () =
  match
    Parseff.parse four_bytes (fun () -> ignore (Parseff.BE.any_int32 ()))
  with
  | Ok _ ->
      ()
  | Error _ ->
      failwith "bench_be_any_int32 failed"

let bench_be_any_int64 () =
  match
    Parseff.parse eight_bytes (fun () -> ignore (Parseff.BE.any_int64 ()))
  with
  | Ok _ ->
      ()
  | Error _ ->
      failwith "bench_be_any_int64 failed"

let bench_take_16 () =
  match Parseff.parse sixteen_bytes (fun () -> ignore (Parseff.take 16)) with
  | Ok _ ->
      ()
  | Error _ ->
      failwith "bench_take_16 failed"

let bench_tlv_10x () =
  match
    Parseff.parse tlv_input (fun () ->
        for _ = 1 to 10 do
          let _tag = Parseff.BE.any_uint8 () in
          let len = Parseff.BE.any_uint16 () in
          let _value = Parseff.take len in
          ()
        done
    )
  with
  | Ok _ ->
      ()
  | Error _ ->
      failwith "bench_tlv_10x failed"

let bench_streaming_be_any_int32 () =
  let src = streaming_source_of_string ~chunk_size:1 four_bytes in
  match
    Parseff.parse_source src (fun () -> ignore (Parseff.BE.any_int32 ()))
  with
  | Ok _ ->
      ()
  | Error _ ->
      failwith "bench_streaming_be_any_int32 failed"

let run_section title iterations benches =
  Printf.printf "%s\n" title;
  Printf.printf "%s\n\n" (String.make (String.length title) '-');
  let results = latencyN ~repeat:3 iterations benches in
  print_newline ();
  tabulate results;
  print_newline ()

let () =
  (* Warmup *)
  for _ = 1 to 1000 do
    bench_be_any_uint8 ();
    bench_be_any_int32 ();
    bench_be_any_int64 ();
    bench_take_16 ();
    bench_tlv_10x ();
    bench_streaming_be_any_int32 ()
  done;

  Printf.printf "Binary Parsing Benchmarks\n";
  Printf.printf "=========================\n\n";

  run_section "Fixed-width reads" 20000000L
    [
      ("BE.any_uint8 (1 byte)", bench_be_any_uint8, ());
      ("BE.any_int32 (4 bytes)", bench_be_any_int32, ());
      ("BE.any_int64 (8 bytes)", bench_be_any_int64, ());
      ("take 16", bench_take_16, ());
    ];

  run_section "Structured and streaming parses" 2000000L
    [
      ("TLV 10x (tag+len+val)", bench_tlv_10x, ());
      ("streaming BE.any_int32", bench_streaming_be_any_int32, ());
    ]
