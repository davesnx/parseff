(** Benchmark Parseff vs Angstrom *)

open Benchmark

(** Test input: JSON array *)
let json_input = {|[1, 2, 3, 4, 5, 6, 7, 8, 9, 10]|}

(** Parseff JSON parser *)
module Parseff_JSON = struct
  open Parseff

  let ws () =
    let re = Re.compile (Re.Posix.re "[ \t\n\r]*") in
    match_re re

  let number_parser () =
    let re = Re.compile (Re.Posix.re "-?[0-9]+(\\.[0-9]+)?") in
    let s = match_re re in
    float_of_string s

  let json_array () =
    let _ = consume "[" in
    let _ = ws () in
    let elements =
      ((fun () ->
          let first = number_parser () in
          let rest =
            many
              (fun () ->
                let _ = ws () in
                let _ = consume "," in
                let _ = ws () in
                number_parser ())
              ()
          in
          first :: rest)
      <|> fun () -> [])
        ()
    in
    let _ = ws () in
    let _ = consume "]" in
    elements

  let parse input =
    match run input json_array with
    | Ok (result, _) -> Some result
    | Error _ -> None
end

(** Angstrom JSON parser *)
module Angstrom_JSON = struct
  open Angstrom

  let ws = skip_while (function ' ' | '\t' | '\n' | '\r' -> true | _ -> false)

  let number =
    take_while1 (function '0' .. '9' | '-' | '.' -> true | _ -> false)
    >>| float_of_string

  let json_array =
    char '['
    *> ws
    *> sep_by (ws *> char ',' *> ws) number
    <* ws
    <* char ']'

  let parse input = parse_string ~consume:All json_array input |> Result.to_option
end

let () =
  (* Warmup *)
  for _ = 1 to 100 do
    ignore (Parseff_JSON.parse json_input);
    ignore (Angstrom_JSON.parse json_input)
  done;

  Printf.printf "Benchmarking Parseff vs Angstrom\n";
  Printf.printf "==================================\n\n";
  Printf.printf "Input: %s\n\n" json_input;

  let results =
    latencyN
      ~repeat:3
      10000L
      [
        ("Parseff", (fun () -> ignore (Parseff_JSON.parse json_input)), ());
        ("Angstrom", (fun () -> ignore (Angstrom_JSON.parse json_input)), ());
      ]
  in

  print_newline ();
  tabulate results;
  
  Printf.printf "\nNote: Lower latency is better. Angstrom is likely faster.\n"
