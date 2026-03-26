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

let run_section title benches =
  let iterations =
    match benches with
    | first :: _ ->
        Bench_case.iterations first
    | [] ->
        invalid_arg "run_section requires at least one benchmark case"
  in
  Bench_style.print_section title;
  let results =
    latencyN ~repeat:3 iterations (List.map Bench_case.to_benchmark benches)
  in
  print_newline ();
  tabulate results;
  print_newline ();
  let section =
    Bench_report.print_gc_quick ~title:"GC Quick Stats (single batch)" benches
  in
  { section with title }

let () =
  let codepoint_cases =
    [
      Bench_case.make ~name:"Utf8.satisfy (ASCII)" ~iterations:18000000L
        bench_utf8_satisfy_ascii;
      Bench_case.make ~name:"Utf8.satisfy (3-byte CJK)" ~iterations:18000000L
        bench_utf8_satisfy_3byte;
    ]
  in
  let scanning_cases =
    [
      Bench_case.make ~name:"Utf8.take_while (10 ASCII)" ~iterations:12000000L
        bench_utf8_take_while_ascii;
      Bench_case.make ~name:"byte take_while (10 ASCII)" ~iterations:12000000L
        bench_byte_take_while_ascii;
      Bench_case.make ~name:"Utf8.take_while (mixed)" ~iterations:12000000L
        bench_utf8_take_while_mixed;
      Bench_case.make ~name:"Utf8.skip_while (10 ASCII)" ~iterations:12000000L
        bench_utf8_skip_while_ascii;
      Bench_case.make ~name:"byte skip_while (10 ASCII)" ~iterations:12000000L
        bench_byte_skip_while_ascii;
    ]
  in
  let all_cases = codepoint_cases @ scanning_cases in
  (* Warmup *)
  for _ = 1 to 1000 do
    List.iter Bench_case.run all_cases
  done;

  Bench_style.print_banner "UTF-8 Parsing Benchmarks";

  let codepoint_section = run_section "Codepoint checks" codepoint_cases in

  let scanning_section = run_section "Scanning" scanning_cases in
  Bench_report.write_gc_quick_artifacts ~artifact_name:"bench_utf8"
    ~bench_name:"UTF-8 Parsing Benchmarks"
    [ codepoint_section; scanning_section ]
