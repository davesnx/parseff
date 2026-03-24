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

(** {1 Angstrom (optimized)} *)

module Angstrom_JSON_Optimized = struct
  open Angstrom

  let is_ws = function ' ' | '\t' | '\n' | '\r' -> true | _ -> false
  let is_digit_or_sign = function '0' .. '9' | '-' | '.' -> true | _ -> false

  let number =
    Unsafe.take_while1 is_digit_or_sign (fun bs ~off ~len ->
        if len = 1 then
          Float.of_int
            (Char.code (Bigstringaf.unsafe_get bs off) - Char.code '0')
        else if len = 2 && Bigstringaf.unsafe_get bs off >= '1' then
          Float.of_int
            (((Char.code (Bigstringaf.unsafe_get bs off) - Char.code '0') * 10)
            + Char.code (Bigstringaf.unsafe_get bs (off + 1))
            - Char.code '0'
            )
        else
          float_of_string (Bigstringaf.substring bs ~off ~len)
    )

  let json_array =
    char '[' *> skip_while is_ws
    *> ( peek_char >>= function
         | Some ']' ->
             advance 1 *> return []
         | _ ->
             number >>= fun first ->
             let rec loop acc =
               skip_while is_ws *> peek_char >>= function
               | Some ',' ->
                   advance 1 *> skip_while is_ws *> number >>= fun n ->
                   loop (n :: acc)
               | _ ->
                   skip_while is_ws *> char ']' *> return (List.rev acc)
             in
             loop [ first ]
       )

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
    | MParser.Success result ->
        Some result
    | MParser.Failed _ ->
        None
end

(** {1 Parseff (generic)} *)

module Parseff_JSON_Generic = struct
  let is_digit_or_sign c = (c >= '0' && c <= '9') || c = '-' || c = '.'
  let is_ws c = c = ' ' || c = '\t' || c = '\n' || c = '\r'

  let json_array () =
    let _ = Parseff.char '[' in
    Parseff.skip_while is_ws;
    let number () =
      let s = Parseff.take_while ~at_least:1 ~label:"number" is_digit_or_sign in
      float_of_string s
    in
    let comma () =
      Parseff.skip_while is_ws;
      let _ = Parseff.char ',' in
      Parseff.skip_while is_ws
    in
    let elements = Parseff.sep_by number comma () in
    Parseff.skip_while is_ws;
    let _ = Parseff.char ']' in
    elements

  let bench input =
    match Parseff.parse input json_array with
    | Ok result ->
        Some result
    | Error _ ->
        None
end

(** {1 Runner} *)

let () =
  (* warmup *)
  for _ = 1 to 1000 do
    ignore (Parseff_bench.parse_json_array json_input);
    ignore (Parseff_JSON_Generic.bench json_input);
    ignore (Angstrom_JSON.bench json_input);
    ignore (Angstrom_JSON_Optimized.bench json_input);
    ignore (MParser_JSON.bench json_input)
  done;

  Printf.printf "JSON Array Benchmark: Parseff vs Angstrom vs MParser\n";
  Printf.printf "=====================================================\n\n";
  Printf.printf "Input: %s\n\n" json_input;

  let results =
    latencyN ~repeat:3 100000L
      [
        ( "Parseff (fused)",
          (fun () -> ignore (Parseff_bench.parse_json_array json_input)),
          ()
        );
        ( "Parseff (generic)",
          (fun () -> ignore (Parseff_JSON_Generic.bench json_input)),
          ()
        );
        ("Angstrom", (fun () -> ignore (Angstrom_JSON.bench json_input)), ());
        ( "Angstrom (opt)",
          (fun () -> ignore (Angstrom_JSON_Optimized.bench json_input)),
          ()
        );
        ("MParser", (fun () -> ignore (MParser_JSON.bench json_input)), ());
      ]
  in

  print_newline ();
  tabulate results;

  Printf.printf "\nNote: Higher throughput (N/s) is better.\n"
