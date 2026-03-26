type gc_quick_measurement = {
  name : string;
  iterations : int64;
  elapsed_s : float;
  parses_per_sec : float;
  gc : Bench_gc_quick.delta;
}

type gc_full_measurement = {
  name : string;
  iterations : int64;
  elapsed_s : float;
  parses_per_sec : float;
  gc : Bench_gc_full.delta;
  rss : Bench_rss_linux.snapshot;
}

type gc_quick_section = { title : string; rows : gc_quick_measurement list }

type gc_full_section = { title : string; rows : gc_full_measurement list }

let bytes_per_word = float_of_int (Sys.word_size / 8)

let words_to_mib words = words *. bytes_per_word /. (1024. *. 1024.)

let words_int_to_mib words = words_to_mib (float_of_int words)

let kib_to_mib kib = float_of_int kib /. 1024.

let string_of_opt_mib = function
  | Some kib ->
      Printf.sprintf "%.2f" (kib_to_mib kib)
  | None ->
      "n/a"

let float_json value = Printf.sprintf "%.6f" value

let int_option_json = function
  | Some value ->
      string_of_int value
  | None ->
      "null"

let float_option_json = function
  | Some value ->
      float_json value
  | None ->
      "null"

let json_string s =
  let buf = Buffer.create (String.length s + 8) in
  Buffer.add_char buf '"';
  String.iter
    (function
      | '"' ->
          Buffer.add_string buf "\\\""
      | '\\' ->
          Buffer.add_string buf "\\\\"
      | '\n' ->
          Buffer.add_string buf "\\n"
      | '\r' ->
          Buffer.add_string buf "\\r"
      | '\t' ->
          Buffer.add_string buf "\\t"
      | c when Char.code c < 0x20 ->
          Buffer.add_string buf (Printf.sprintf "\\u%04x" (Char.code c))
      | c ->
          Buffer.add_char buf c
      )
    s;
  Buffer.add_char buf '"';
  Buffer.contents buf

let csv_string s =
  let buf = Buffer.create (String.length s + 8) in
  Buffer.add_char buf '"';
  String.iter
    (function '"' -> Buffer.add_string buf "\"\"" | c -> Buffer.add_char buf c)
    s;
  Buffer.add_char buf '"';
  Buffer.contents buf

let utc_timestamp () =
  let tm = Unix.gmtime (Unix.gettimeofday ()) in
  Printf.sprintf "%04d-%02d-%02dT%02d:%02d:%02dZ" (tm.tm_year + 1900)
    (tm.tm_mon + 1) tm.tm_mday tm.tm_hour tm.tm_min tm.tm_sec

let results_dir () =
  match Sys.getenv_opt "PARSEFF_BENCH_RESULTS_DIR" with
  | Some dir when dir <> "" ->
      dir
  | _ ->
      "bench/results"

let rec ensure_dir path =
  if path = "" || path = "." || path = "/" then
    ()
  else if Sys.file_exists path then
    begin if not (Sys.is_directory path) then
      invalid_arg (Printf.sprintf "%s exists and is not a directory" path)
  end
  else begin
    let parent = Filename.dirname path in
    if parent <> path then ensure_dir parent;
    Unix.mkdir path 0o755
  end

let with_out path f =
  let oc = open_out path in
  Fun.protect ~finally:(fun () -> close_out_noerr oc) (fun () -> f oc)

let artifact_paths artifact_name =
  let dir = results_dir () in
  ensure_dir dir;
  ( Filename.concat dir (artifact_name ^ ".json"),
    Filename.concat dir (artifact_name ^ ".csv")
  )

let optional_json_string = function
  | Some value ->
      json_string value
  | None ->
      "null"

let case_name_width min_width names =
  List.fold_left
    (fun width name -> max width (String.length name))
    min_width names

let quick_row_json (row : gc_quick_measurement) =
  Printf.sprintf
    "{\"name\":%s,\"iterations\":%s,\"elapsed_s\":%s,\"parses_per_sec\":%s,\"gc\":{\"minor_words\":%s,\"major_words\":%s,\"promoted_words\":%s,\"minor_mib\":%s,\"major_mib\":%s,\"promoted_mib\":%s,\"minor_collections\":%d,\"major_collections\":%d,\"forced_major_collections\":%d,\"heap_words_after\":%d,\"top_heap_words_after\":%d,\"heap_mib_after\":%s,\"top_heap_mib_after\":%s}}"
    (json_string row.name)
    (Int64.to_string row.iterations)
    (float_json row.elapsed_s)
    (float_json row.parses_per_sec)
    (float_json row.gc.minor_words)
    (float_json row.gc.major_words)
    (float_json row.gc.promoted_words)
    (float_json (words_to_mib row.gc.minor_words))
    (float_json (words_to_mib row.gc.major_words))
    (float_json (words_to_mib row.gc.promoted_words))
    row.gc.minor_collections row.gc.major_collections
    row.gc.forced_major_collections row.gc.heap_words_after
    row.gc.top_heap_words_after
    (float_json (words_int_to_mib row.gc.heap_words_after))
    (float_json (words_int_to_mib row.gc.top_heap_words_after))

let full_row_json (row : gc_full_measurement) =
  Printf.sprintf
    "{\"name\":%s,\"iterations\":%s,\"elapsed_s\":%s,\"parses_per_sec\":%s,\"gc\":{\"minor_words\":%s,\"major_words\":%s,\"promoted_words\":%s,\"minor_mib\":%s,\"major_mib\":%s,\"promoted_mib\":%s,\"minor_collections\":%d,\"major_collections\":%d,\"forced_major_collections\":%d,\"compactions\":%d,\"heap_words_after\":%d,\"top_heap_words_after\":%d,\"live_words_after\":%d,\"free_words_after\":%d,\"stack_size_after\":%d,\"heap_mib_after\":%s,\"top_heap_mib_after\":%s,\"live_mib_after\":%s,\"free_mib_after\":%s},\"rss\":{\"vm_rss_kib\":%s,\"vm_hwm_kib\":%s,\"vm_rss_mib\":%s,\"vm_hwm_mib\":%s}}"
    (json_string row.name)
    (Int64.to_string row.iterations)
    (float_json row.elapsed_s)
    (float_json row.parses_per_sec)
    (float_json row.gc.minor_words)
    (float_json row.gc.major_words)
    (float_json row.gc.promoted_words)
    (float_json (words_to_mib row.gc.minor_words))
    (float_json (words_to_mib row.gc.major_words))
    (float_json (words_to_mib row.gc.promoted_words))
    row.gc.minor_collections row.gc.major_collections
    row.gc.forced_major_collections row.gc.compactions row.gc.heap_words_after
    row.gc.top_heap_words_after row.gc.live_words_after row.gc.free_words_after
    row.gc.stack_size_after
    (float_json (words_int_to_mib row.gc.heap_words_after))
    (float_json (words_int_to_mib row.gc.top_heap_words_after))
    (float_json (words_int_to_mib row.gc.live_words_after))
    (float_json (words_int_to_mib row.gc.free_words_after))
    (int_option_json row.rss.vm_rss_kib)
    (int_option_json row.rss.vm_hwm_kib)
    (float_option_json (Option.map kib_to_mib row.rss.vm_rss_kib))
    (float_option_json (Option.map kib_to_mib row.rss.vm_hwm_kib))

let write_text_file path content =
  with_out path (fun oc -> output_string oc content)

let print_artifact_paths json_path csv_path =
  Bench_style.print_paths json_path csv_path;
  print_newline ()

let write_gc_quick_artifacts ~artifact_name ~bench_name sections =
  let json_path, csv_path = artifact_paths artifact_name in
  let generated_at = utc_timestamp () in
  let git_sha = Sys.getenv_opt "GITHUB_SHA" in
  let sections_json =
    sections
    |> List.map (fun (section : gc_quick_section) ->
        Printf.sprintf "{\"title\":%s,\"rows\":[%s]}"
          (json_string section.title)
          (String.concat "," (List.map quick_row_json section.rows))
    )
    |> String.concat ","
  in
  let json =
    Printf.sprintf
      "{\"schema_version\":1,\"bench_name\":%s,\"mode\":\"gc_quick\",\"generated_at_utc\":%s,\"ocaml_version\":%s,\"os_type\":%s,\"word_size\":%d,\"hostname\":%s,\"git_sha\":%s,\"sections\":[%s]}"
      (json_string bench_name) (json_string generated_at)
      (json_string Sys.ocaml_version)
      (json_string Sys.os_type) Sys.word_size
      (json_string (Unix.gethostname ()))
      (optional_json_string git_sha)
      sections_json
  in
  let csv_buf = Buffer.create 1024 in
  Buffer.add_string csv_buf
    "section,case,mode,iterations,elapsed_s,parses_per_sec,minor_words,major_words,promoted_words,minor_mib,major_mib,promoted_mib,minor_collections,major_collections,forced_major_collections,heap_words_after,top_heap_words_after,heap_mib_after,top_heap_mib_after\n";
  List.iter
    (fun (section : gc_quick_section) ->
      List.iter
        (fun (row : gc_quick_measurement) ->
          Printf.bprintf csv_buf
            "%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%d,%d,%d,%d,%d,%s,%s\n"
            (csv_string section.title) (csv_string row.name)
            (csv_string "gc_quick")
            (Int64.to_string row.iterations)
            (float_json row.elapsed_s)
            (float_json row.parses_per_sec)
            (float_json row.gc.minor_words)
            (float_json row.gc.major_words)
            (float_json row.gc.promoted_words)
            (float_json (words_to_mib row.gc.minor_words))
            (float_json (words_to_mib row.gc.major_words))
            (float_json (words_to_mib row.gc.promoted_words))
            row.gc.minor_collections row.gc.major_collections
            row.gc.forced_major_collections row.gc.heap_words_after
            row.gc.top_heap_words_after
            (float_json (words_int_to_mib row.gc.heap_words_after))
            (float_json (words_int_to_mib row.gc.top_heap_words_after))
        )
        section.rows
    )
    sections;
  write_text_file json_path json;
  write_text_file csv_path (Buffer.contents csv_buf);
  print_artifact_paths json_path csv_path

let write_gc_full_artifacts ~artifact_name ~bench_name sections =
  let json_path, csv_path = artifact_paths artifact_name in
  let generated_at = utc_timestamp () in
  let git_sha = Sys.getenv_opt "GITHUB_SHA" in
  let sections_json =
    sections
    |> List.map (fun (section : gc_full_section) ->
        Printf.sprintf "{\"title\":%s,\"rows\":[%s]}"
          (json_string section.title)
          (String.concat "," (List.map full_row_json section.rows))
    )
    |> String.concat ","
  in
  let json =
    Printf.sprintf
      "{\"schema_version\":1,\"bench_name\":%s,\"mode\":\"gc_full\",\"generated_at_utc\":%s,\"ocaml_version\":%s,\"os_type\":%s,\"word_size\":%d,\"hostname\":%s,\"git_sha\":%s,\"sections\":[%s]}"
      (json_string bench_name) (json_string generated_at)
      (json_string Sys.ocaml_version)
      (json_string Sys.os_type) Sys.word_size
      (json_string (Unix.gethostname ()))
      (optional_json_string git_sha)
      sections_json
  in
  let csv_buf = Buffer.create 1024 in
  Buffer.add_string csv_buf
    "section,case,mode,iterations,elapsed_s,parses_per_sec,minor_words,major_words,promoted_words,minor_mib,major_mib,promoted_mib,minor_collections,major_collections,forced_major_collections,compactions,heap_words_after,top_heap_words_after,live_words_after,free_words_after,stack_size_after,heap_mib_after,top_heap_mib_after,live_mib_after,free_mib_after,vm_rss_kib,vm_hwm_kib\n";
  List.iter
    (fun (section : gc_full_section) ->
      List.iter
        (fun (row : gc_full_measurement) ->
          Printf.bprintf csv_buf
            "%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%d,%d,%d,%d,%d,%d,%d,%d,%d,%s,%s,%s,%s,%s,%s\n"
            (csv_string section.title) (csv_string row.name)
            (csv_string "gc_full")
            (Int64.to_string row.iterations)
            (float_json row.elapsed_s)
            (float_json row.parses_per_sec)
            (float_json row.gc.minor_words)
            (float_json row.gc.major_words)
            (float_json row.gc.promoted_words)
            (float_json (words_to_mib row.gc.minor_words))
            (float_json (words_to_mib row.gc.major_words))
            (float_json (words_to_mib row.gc.promoted_words))
            row.gc.minor_collections row.gc.major_collections
            row.gc.forced_major_collections row.gc.compactions
            row.gc.heap_words_after row.gc.top_heap_words_after
            row.gc.live_words_after row.gc.free_words_after
            row.gc.stack_size_after
            (float_json (words_int_to_mib row.gc.heap_words_after))
            (float_json (words_int_to_mib row.gc.top_heap_words_after))
            (float_json (words_int_to_mib row.gc.live_words_after))
            (float_json (words_int_to_mib row.gc.free_words_after))
            ( match row.rss.vm_rss_kib with
            | Some value ->
                string_of_int value
            | None ->
                ""
            )
            ( match row.rss.vm_hwm_kib with
            | Some value ->
                string_of_int value
            | None ->
                ""
            )
        )
        section.rows
    )
    sections;
  write_text_file json_path json;
  write_text_file csv_path (Buffer.contents csv_buf);
  print_artifact_paths json_path csv_path

let measure_gc_quick case : gc_quick_measurement =
  Bench_gc_quick.stabilize ();
  Bench_case.warmup case;
  let before = Bench_gc_quick.take () in
  let start = Unix.gettimeofday () in
  Bench_case.run_iterations case;
  let elapsed_s = Unix.gettimeofday () -. start in
  let after = Bench_gc_quick.take () in
  let parses_per_sec =
    if elapsed_s <= 0. then
      0.
    else
      Int64.to_float (Bench_case.iterations case) /. elapsed_s
  in
  {
    name = Bench_case.name case;
    iterations = Bench_case.iterations case;
    elapsed_s;
    parses_per_sec;
    gc = Bench_gc_quick.diff ~before ~after;
  }

let measure_gc_full case : gc_full_measurement =
  Bench_gc_full.stabilize ();
  Bench_case.warmup case;
  let before = Bench_gc_full.take () in
  let start = Unix.gettimeofday () in
  Bench_case.run_iterations case;
  let elapsed_s = Unix.gettimeofday () -. start in
  let rss = Bench_rss_linux.take () in
  Bench_gc_full.stabilize ();
  let after = Bench_gc_full.take () in
  let parses_per_sec =
    if elapsed_s <= 0. then
      0.
    else
      Int64.to_float (Bench_case.iterations case) /. elapsed_s
  in
  {
    name = Bench_case.name case;
    iterations = Bench_case.iterations case;
    elapsed_s;
    parses_per_sec;
    gc = Bench_gc_full.diff ~before ~after;
    rss;
  }

let print_gc_quick ?(title = "GC Quick Stats") cases : gc_quick_section =
  let rows : gc_quick_measurement list = List.map measure_gc_quick cases in
  let name_width =
    case_name_width 22
      (List.map (fun (row : gc_quick_measurement) -> row.name) rows)
  in
  Bench_style.print_section ~before:0 title;
  let header_line =
    Printf.sprintf "%-*s %11s %8s %10s %10s %10s %6s %6s %9s %9s" name_width
      "Case" "Parses/s" "Secs" "Minor MiB" "Major MiB" "Promo MiB" "minGC"
      "majGC" "Heap MiB" "Top MiB"
  in
  Printf.printf "%s\n" (Bench_style.header_text header_line);
  List.iter
    (fun (row : gc_quick_measurement) ->
      Printf.printf
        "%-*s %11.0f %8.2f %10.2f %10.2f %10.2f %6d %6d %9.2f %9.2f\n"
        name_width row.name row.parses_per_sec row.elapsed_s
        (words_to_mib row.gc.minor_words)
        (words_to_mib row.gc.major_words)
        (words_to_mib row.gc.promoted_words)
        row.gc.minor_collections row.gc.major_collections
        (words_int_to_mib row.gc.heap_words_after)
        (words_int_to_mib row.gc.top_heap_words_after)
    )
    rows;
  print_newline ();
  Bench_style.print_notice "Note"
    "GC stats come from one measured batch per case using Gc.quick_stat.";
  print_newline ();
  { title; rows }

let print_gc_full ?(title = "GC Full Stats") cases : gc_full_section =
  let rows : gc_full_measurement list = List.map measure_gc_full cases in
  let name_width =
    case_name_width 22
      (List.map (fun (row : gc_full_measurement) -> row.name) rows)
  in
  Bench_style.print_section ~before:0 title;
  let header_line =
    Printf.sprintf "%-*s %11s %8s %10s %10s %10s %9s %9s %9s %9s %6s %6s"
      name_width "Case" "Parses/s" "Secs" "Minor MiB" "Major MiB" "Promo MiB"
      "Live MiB" "Heap MiB" "RSS MiB" "HWM MiB" "minGC" "majGC"
  in
  Printf.printf "%s\n" (Bench_style.header_text header_line);
  List.iter
    (fun (row : gc_full_measurement) ->
      Printf.printf
        "%-*s %11.0f %8.2f %10.2f %10.2f %10.2f %9.2f %9.2f %9s %9s %6d %6d\n"
        name_width row.name row.parses_per_sec row.elapsed_s
        (words_to_mib row.gc.minor_words)
        (words_to_mib row.gc.major_words)
        (words_to_mib row.gc.promoted_words)
        (words_int_to_mib row.gc.live_words_after)
        (words_int_to_mib row.gc.heap_words_after)
        (string_of_opt_mib row.rss.vm_rss_kib)
        (string_of_opt_mib row.rss.vm_hwm_kib)
        row.gc.minor_collections row.gc.major_collections
    )
    rows;
  print_newline ();
  Bench_style.print_notice "Note"
    "Full stats run Gc.stat after a post-run full major GC; RSS/HWM comes from \
     /proc/self/status when available.";
  print_newline ();
  { title; rows }
