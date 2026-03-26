open Benchmark

(* Single 3-byte UTF-8 char: U+4E16 (CJK "world") = 0xE4 0xB8 0x96 *)
let cjk_1 = "\xE4\xB8\x96"

(* 10 ASCII chars for take_while comparison *)
let ascii_for_take = "abcdefghij"

(* Mixed multibyte: 3 ASCII + 2-byte (U+00E9 = e-acute) + 3-byte (U+4E16) + ASCII
   = "abc" + "\xC3\xA9" + "\xE4\xB8\x96" + "xyz" = 11 bytes, 7 codepoints *)
let mixed_utf8 = "abc\xC3\xA9\xE4\xB8\x96xyz"

let is_alpha_ascii c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')

let is_alpha_uchar u =
  let i = Uchar.to_int u in
  (* Accept ASCII alpha + any non-ASCII (Latin, CJK, etc.) *)
  (i >= 0x61 && i <= 0x7A) || (i >= 0x41 && i <= 0x5A) || i > 0x7F

let bench_utf8_satisfy_ascii () =
  match
    Parseff.parse "a" (fun () ->
        ignore
          (Parseff.Utf8.satisfy (fun u -> Uchar.to_int u >= 0x61) ~label:"alpha")
    )
  with
  | Ok _ ->
      ()
  | Error _ ->
      failwith "bench_utf8_satisfy_ascii failed"

let bench_utf8_satisfy_3byte () =
  match
    Parseff.parse cjk_1 (fun () ->
        ignore
          (Parseff.Utf8.satisfy (fun u -> Uchar.to_int u = 0x4E16) ~label:"CJK")
    )
  with
  | Ok _ ->
      ()
  | Error _ ->
      failwith "bench_utf8_satisfy_3byte failed"

let bench_utf8_take_while_ascii () =
  match
    Parseff.parse ascii_for_take (fun () ->
        ignore (Parseff.Utf8.take_while is_alpha_uchar)
    )
  with
  | Ok _ ->
      ()
  | Error _ ->
      failwith "bench_utf8_take_while_ascii failed"

let bench_byte_take_while_ascii () =
  match
    Parseff.parse ascii_for_take (fun () ->
        ignore (Parseff.take_while is_alpha_ascii)
    )
  with
  | Ok _ ->
      ()
  | Error _ ->
      failwith "bench_byte_take_while_ascii failed"

let bench_utf8_take_while_mixed () =
  match
    Parseff.parse mixed_utf8 (fun () ->
        ignore (Parseff.Utf8.take_while is_alpha_uchar)
    )
  with
  | Ok _ ->
      ()
  | Error _ ->
      failwith "bench_utf8_take_while_mixed failed"

let bench_utf8_skip_while_ascii () =
  match
    Parseff.parse ascii_for_take (fun () ->
        Parseff.Utf8.skip_while is_alpha_uchar
    )
  with
  | Ok _ ->
      ()
  | Error _ ->
      failwith "bench_utf8_skip_while_ascii failed"

let bench_byte_skip_while_ascii () =
  match
    Parseff.parse ascii_for_take (fun () -> Parseff.skip_while is_alpha_ascii)
  with
  | Ok _ ->
      ()
  | Error _ ->
      failwith "bench_byte_skip_while_ascii failed"

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
    bench_utf8_satisfy_ascii ();
    bench_utf8_satisfy_3byte ();
    bench_utf8_take_while_ascii ();
    bench_byte_take_while_ascii ();
    bench_utf8_take_while_mixed ();
    bench_utf8_skip_while_ascii ();
    bench_byte_skip_while_ascii ()
  done;

  Printf.printf "UTF-8 Parsing Benchmarks\n";
  Printf.printf "========================\n\n";

  run_section "Codepoint checks" 18000000L
    [
      ("Utf8.satisfy (ASCII)", bench_utf8_satisfy_ascii, ());
      ("Utf8.satisfy (3-byte CJK)", bench_utf8_satisfy_3byte, ());
    ];

  run_section "Scanning" 12000000L
    [
      ("Utf8.take_while (10 ASCII)", bench_utf8_take_while_ascii, ());
      ("byte take_while (10 ASCII)", bench_byte_take_while_ascii, ());
      ("Utf8.take_while (mixed)", bench_utf8_take_while_mixed, ());
      ("Utf8.skip_while (10 ASCII)", bench_utf8_skip_while_ascii, ());
      ("byte skip_while (10 ASCII)", bench_byte_skip_while_ascii, ());
    ]
