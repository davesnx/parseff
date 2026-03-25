open Benchmark

let json_input = {|[1, 2, 3, 4, 5, 6, 7, 8, 9, 10]|}
let is_digit_or_sign = function '0' .. '9' | '-' | '.' -> true | _ -> false
let is_ws = function ' ' | '\t' | '\n' | '\r' -> true | _ -> false

module Parseff_JSON = struct
  let[@inline always] float_of_span (s : Parseff.span) =
    if s.len = 1 then
      Float.of_int (Char.code (String.unsafe_get s.buf s.off) - Char.code '0')
    else if s.len = 2 && String.unsafe_get s.buf s.off >= '1' then
      Float.of_int
        (((Char.code (String.unsafe_get s.buf s.off) - Char.code '0') * 10)
        + (Char.code (String.unsafe_get s.buf (s.off + 1)) - Char.code '0')
        )
    else
      float_of_string (Parseff.span_to_string s)

  let json_array () =
    Parseff.skip_while_then_char is_ws '[';
    Parseff.skip_while is_ws;
    let spans = Parseff.sep_by_take_span is_ws ',' is_digit_or_sign in
    let elements = List.map float_of_span spans in
    Parseff.skip_while_then_char is_ws ']';
    elements

  let number () =
    let s = Parseff.take_while is_digit_or_sign in
    float_of_string s

  let ws () = Parseff.skip_while is_ws

  let json_array_fair () =
    Parseff.skip_while_then_char is_ws '[';
    ws ();
    let sep () = ws (); ignore (Parseff.char ','); ws () in
    let elements = Parseff.sep_by number sep () in
    Parseff.skip_while_then_char is_ws ']';
    elements

  let bench_span input =
    match Parseff.parse input json_array with
    | Ok result ->
        Some result
    | Error _ ->
        None

  let bench_fair input =
    match Parseff.parse input json_array_fair with
    | Ok result ->
        Some result
    | Error _ ->
        None
end

module Angstrom_JSON = struct
  open Angstrom

  let ws = skip_while is_ws

  let number =
    take_while1 is_digit_or_sign
    >>| float_of_string

  let json_array =
    ws *> char '[' *> ws *> sep_by (ws *> char ',' *> ws) number <* ws <* char ']'

  let bench input =
    parse_string ~consume:All json_array input |> Result.to_option
end

let () =
  for _ = 1 to 1000 do
    ignore (Parseff_JSON.bench_span json_input);
    ignore (Angstrom_JSON.bench json_input)
  done;

  Printf.printf "Benchmarking Parseff vs Angstrom\n";
  Printf.printf "==================================\n\n";
  Printf.printf "Input: %s\n\n" json_input;

  let results =
    latencyN ~repeat:3 100000L
      [
        ( "Parseff (span)",
          (fun () -> ignore (Parseff_JSON.bench_span json_input)),
          ()
        );
        ( "Parseff (fair)",
          (fun () -> ignore (Parseff_JSON.bench_fair json_input)),
          ()
        );
        ("Angstrom", (fun () -> ignore (Angstrom_JSON.bench json_input)), ());
      ]
  in

  print_newline ();
  tabulate results;

  Printf.printf "\nNote: Lower latency is better.\n"
