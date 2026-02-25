(** Benchmark Parseff vs Angstrom *)

open Benchmark

(** Test input: JSON array *)
let json_input = {|[1, 2, 3, 4, 5, 6, 7, 8, 9, 10]|}

let is_digit_or_sign c = (c >= '0' && c <= '9') || c = '-' || c = '.'
let is_ws c = c = ' ' || c = '\t' || c = '\n' || c = '\r'

(** Parseff JSON parser - optimized with fused operations *)
module Parseff_JSON = struct
  open Parseff

  let json_array () =
    skip_while_then_char is_ws '[';
    skip_while is_ws;
    (* Parse entire comma-separated list in a single effect dispatch *)
    let str_elements = sep_by_take is_ws ',' is_digit_or_sign in
    let elements = List.map float_of_string str_elements in
    skip_while_then_char is_ws ']';
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
  for _ = 1 to 1000 do
    ignore (Parseff_JSON.parse json_input);
    ignore (Angstrom_JSON.parse json_input)
  done;

  Printf.printf "Benchmarking Parseff vs Angstrom\n";
  Printf.printf "==================================\n\n";
  Printf.printf "Input: %s\n\n" json_input;

  let results =
    latencyN
      ~repeat:3
      100000L
      [
        ("Parseff", (fun () -> ignore (Parseff_JSON.parse json_input)), ());
        ("Angstrom", (fun () -> ignore (Angstrom_JSON.parse json_input)), ());
      ]
  in

  print_newline ();
  tabulate results;
  
  Printf.printf "\nNote: Lower latency is better.\n"
