open Benchmark

let csv_input = "hello,world,foo,bar,baz,qux,quux,corge,grault,garply"

(** {1 Angstrom} *)

module Angstrom_CSV = struct
  open Angstrom

  let field = take_while (function ',' -> false | _ -> true)
  let csv = sep_by (char ',') field
  let bench input = parse_string ~consume:All csv input |> Result.to_option
end

(** {1 MParser} *)

module MParser_CSV = struct
  open MParser

  let field : (string, unit) t = many1_satisfy (fun c -> c <> ',') <|> return ""
  let csv : (string list, unit) t = sep_by1 field (char ',')

  let bench input =
    match parse_string csv input () with
    | MParser.Success result -> Some result
    | MParser.Failed _ -> None
end

(** {1 Runner} *)

let () =
  (* warmup *)
  for _ = 1 to 1000 do
    ignore (Parseff_bench.parse_csv csv_input);
    ignore (Angstrom_CSV.bench csv_input);
    ignore (MParser_CSV.bench csv_input)
  done;

  Printf.printf "CSV Benchmark: Parseff vs Angstrom vs MParser\n";
  Printf.printf "===============================================\n\n";
  Printf.printf "Input: %s\n\n" csv_input;

  let results =
    latencyN ~repeat:3 100000L
      [
        ("Parseff", (fun () -> ignore (Parseff_bench.parse_csv csv_input)), ());
        ("Angstrom", (fun () -> ignore (Angstrom_CSV.bench csv_input)), ());
        ("MParser", (fun () -> ignore (MParser_CSV.bench csv_input)), ());
      ]
  in

  print_newline ();
  tabulate results;

  Printf.printf "\nNote: Higher throughput (N/s) is better.\n"
