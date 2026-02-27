open Benchmark

let json_input = {|[1, 2, 3, 4, 5, 6, 7, 8, 9, 10]|}

(** {1 Angstrom} *)

module Angstrom_JSON = struct
  open Angstrom

  let ws = skip_while (function ' ' | '\t' | '\n' | '\r' -> true | _ -> false)

  let number =
    take_while1 (function '0' .. '9' | '-' | '.' -> true | _ -> false)
    >>| float_of_string

  let json_array =
    char '[' *> ws *> sep_by (ws *> char ',' *> ws) number <* ws <* char ']'

  let bench input =
    parse_string ~consume:All json_array input |> Result.to_option
end

(** {1 MParser} *)

module MParser_JSON = struct
  open MParser

  let ws : (unit, unit) t =
    skip_many (satisfy (fun c -> c = ' ' || c = '\t' || c = '\n' || c = '\r'))

  let number : (float, unit) t =
    many1_satisfy (function '0' .. '9' | '-' | '.' -> true | _ -> false)
    |>> float_of_string

  let json_array : (float list, unit) t =
    char '[' >> ws
    >> sep_by (ws >> char ',' >> ws >> number) number
    << ws << char ']'

  let bench input =
    match parse_string json_array input () with
    | MParser.Success result -> Some result
    | MParser.Failed _ -> None
end

(** {1 Runner} *)

let () =
  (* warmup *)
  for _ = 1 to 1000 do
    ignore (Parseff_bench.parse_json_array json_input);
    ignore (Angstrom_JSON.bench json_input);
    ignore (MParser_JSON.bench json_input)
  done;

  Printf.printf "JSON Array Benchmark: Parseff vs Angstrom vs MParser\n";
  Printf.printf "=====================================================\n\n";
  Printf.printf "Input: %s\n\n" json_input;

  let results =
    latencyN ~repeat:3 100000L
      [
        ( "Parseff",
          (fun () -> ignore (Parseff_bench.parse_json_array json_input)),
          () );
        ("Angstrom", (fun () -> ignore (Angstrom_JSON.bench json_input)), ());
        ("MParser", (fun () -> ignore (MParser_JSON.bench json_input)), ());
      ]
  in

  print_newline ();
  tabulate results;

  Printf.printf "\nNote: Higher throughput (N/s) is better.\n"
