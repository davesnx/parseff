let env_int name default =
  match Sys.getenv_opt name with
  | None ->
      default
  | Some value ->
      int_of_string value

let env_int64 name default =
  match Sys.getenv_opt name with
  | None ->
      default
  | Some value ->
      Int64.of_string value

let record = "field1,field2,field3,field4\n"
let record_len = String.length record
let repetitions = env_int "PARSEFF_STREAM_LINES" 100_000
let chunk_size = env_int "PARSEFF_STREAM_CHUNK" 65_536
let runs = env_int64 "PARSEFF_STREAM_RUNS" 1L
let warmup = env_int "PARSEFF_STREAM_WARMUP" 0

let total_bytes = record_len * repetitions

let source_of_repeated_record ~chunk_size ~repetitions record =
  let record_len = String.length record in
  let record_index = ref 0 in
  let record_offset = ref 0 in
  Parseff.Source.of_chunks (fun () ->
      if !record_index >= repetitions then
        None
      else
        let remaining_bytes =
          ((repetitions - !record_index) * record_len) - !record_offset
        in
        let target = min chunk_size remaining_bytes in
        let buf = Bytes.create target in
        let rec fill pos =
          if pos >= target then
            ()
          else
            let available = record_len - !record_offset in
            let n = min (target - pos) available in
            Bytes.blit_string record !record_offset buf pos n;
            record_offset := !record_offset + n;
            if !record_offset = record_len then begin
              record_offset := 0;
              incr record_index
            end;
            fill (pos + n)
        in
        fill 0;
        Some (Bytes.unsafe_to_string buf)
  )

let parse_repeated_record ~repetitions record () =
  for _ = 1 to repetitions do
    ignore (Parseff.consume record)
  done;
  Parseff.end_of_input ();
  repetitions

let run_streaming_case () =
  let source = source_of_repeated_record ~chunk_size ~repetitions record in
  match
    Parseff.parse_source source (parse_repeated_record ~repetitions record)
  with
  | Ok parsed when parsed = repetitions ->
      ()
  | Ok parsed ->
      failwith
        (Printf.sprintf "expected %d streamed records, got %d" repetitions
           parsed
        )
  | Error _ ->
      failwith "streaming retention benchmark failed"

let mib bytes = float_of_int bytes /. (1024. *. 1024.)

let () =
  let case =
    Bench_case.make ~warmup ~name:"Parseff streaming consume" ~iterations:runs
      run_streaming_case
  in
  Bench_style.print_banner "Streaming Retention Benchmark";
  Bench_style.print_label_value "Record" (Printf.sprintf "%S" record);
  Bench_style.print_label_value "Records per run" (string_of_int repetitions);
  Bench_style.print_label_value "Chunk size"
    (Printf.sprintf "%d bytes" chunk_size);
  Bench_style.print_label_value "Approx input per run"
    (Printf.sprintf "%.2f MiB" (mib total_bytes));
  Bench_style.print_label_value "Measured runs" (Int64.to_string runs);
  print_newline ();
  let section =
    Bench_report.print_gc_full ~title:"GC Full Stats + Linux RSS" [ case ]
  in
  Bench_report.write_gc_full_artifacts ~artifact_name:"bench_streaming"
    ~bench_name:"Streaming Retention Benchmark"
    [ { section with title = "Streaming retention case" } ];
  Bench_style.print_notice "Tip"
    "rerun with PARSEFF_STREAM_CHUNK=4096 or PARSEFF_STREAM_CHUNK=64 to stress \
     refill and buffer growth more aggressively."
