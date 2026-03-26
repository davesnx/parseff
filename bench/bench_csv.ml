open Benchmark

let csv_input = "hello,world,foo,bar,baz,qux,quux,corge,grault,garply"

(** {1 Angstrom} *)

module Angstrom_CSV = struct
  open Angstrom

  let field = take_while (function ',' -> false | _ -> true)
  let csv = sep_by (char ',') field
  let parse input = parse_string ~consume:All csv input |> Result.to_option
end

(** {1 Angstrom (optimized)} *)

module Angstrom_CSV_Optimized = struct
  open Angstrom

  let field = take_while (function ',' -> false | _ -> true)

  (* Manual loop with peek_char — no sep_by backtracking *)
  let csv =
    field >>= fun first ->
    let rec loop acc =
      peek_char >>= function
      | Some ',' ->
          advance 1 *> field >>= fun f -> loop (f :: acc)
      | _ ->
          return (List.rev acc)
    in
    loop [ first ]

  let parse input = parse_string ~consume:All csv input |> Result.to_option
end

(** {1 MParser} *)

module MParser_CSV = struct
  open MParser

  let field : (string, unit) t = many1_satisfy (fun c -> c <> ',') <|> return ""
  let csv : (string list, unit) t = sep_by1 field (char ',') << eof

  let parse input =
    match parse_string csv input () with
    | MParser.Success result ->
        Some result
    | MParser.Failed _ ->
        None
end

(** {1 Parseff (generic)} *)

module Parseff_CSV_Generic = struct
  let field () = Parseff.take_while (fun c -> c <> ',')

  let csv () =
    let fields =
      Parseff.sep_by field (fun () -> ignore (Parseff.char ',')) ()
    in
    Parseff.end_of_input ();
    fields

  let parse input =
    match Parseff.parse input csv with
    | Ok result ->
        Some result
    | Error _ ->
        None
end

module Parseff_CSV_Fused = struct
  let[@inline always] is_not_comma c = c <> ','
  let csv () =
    let spans = Parseff.sep_by_take_span (fun _ -> false) ',' is_not_comma in
    let fields = List.map Parseff.span_to_string spans in
    Parseff.end_of_input ();
    fields

  let parse input =
    match Parseff.parse input csv with
    | Ok result ->
        Some result
    | Error _ ->
        None
end

let () =
  let benches =
    [
      Bench_case.make ~name:"Parseff (fused)" ~iterations:2000000L (fun () ->
          ignore (Parseff_CSV_Fused.parse csv_input)
      );
      Bench_case.make ~name:"Parseff (generic)" ~iterations:2000000L (fun () ->
          ignore (Parseff_CSV_Generic.parse csv_input)
      );
      Bench_case.make ~name:"Angstrom" ~iterations:2000000L (fun () ->
          ignore (Angstrom_CSV.parse csv_input)
      );
      Bench_case.make ~name:"Angstrom (opt)" ~iterations:2000000L (fun () ->
          ignore (Angstrom_CSV_Optimized.parse csv_input)
      );
      Bench_case.make ~name:"MParser" ~iterations:2000000L (fun () ->
          ignore (MParser_CSV.parse csv_input)
      );
    ]
  in
  (* warmup *)
  for _ = 1 to 1000 do
    List.iter Bench_case.run benches
  done;

  Bench_style.print_banner "CSV Benchmark: Parseff vs Angstrom vs MParser";
  Bench_style.print_label_value "Input" csv_input;
  print_newline ();

  let results =
    latencyN ~repeat:3 2000000L (List.map Bench_case.to_benchmark benches)
  in

  print_newline ();
  tabulate results;
  Printf.printf "\n";
  let section =
    Bench_report.print_gc_quick ~title:"GC Quick Stats (single batch)" benches
  in
  Bench_report.write_gc_quick_artifacts ~artifact_name:"bench_csv"
    ~bench_name:"CSV Benchmark: Parseff vs Angstrom vs MParser"
    [ { section with title = "Main benchmark cases" } ];

  print_newline ();
  Bench_style.print_notice "Note" "Higher throughput (N/s) is better."
