open Benchmark

let json_input = {|[1, 2, 3, 4, 5, 6, 7, 8, 9, 10]|}
let is_digit_or_sign c = (c >= '0' && c <= '9') || c = '-' || c = '.'
let is_ws c = c = ' ' || c = '\t' || c = '\n' || c = '\r'

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

  let[@inline] float_of_span_fair (s : Parseff.span) =
    float_of_string (Parseff.span_to_string s)

  let json_array () =
    Parseff.skip_while_then_char is_ws '[';
    Parseff.skip_while is_ws;
    let spans = Parseff.sep_by_take_span is_ws ',' is_digit_or_sign in
    let elements = List.map float_of_span spans in
    Parseff.skip_while_then_char is_ws ']';
    elements

  let json_array_fair () =
    Parseff.skip_while_then_char is_ws '[';
    Parseff.skip_while is_ws;
    let spans = Parseff.sep_by_take_span is_ws ',' is_digit_or_sign in
    let elements = List.map float_of_span_fair spans in
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

  (* Generic combinator style — same approach as Angstrom:
     uses char, sep_by, skip_while, take_while. Exercises Match_char (O2),
     structured errors in or_/sep_by (O1), and skip_while. *)
  let json_array_generic () =
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

  let bench_generic input =
    match Parseff.parse input json_array_generic with
    | Ok result ->
        Some result
    | Error _ ->
        None
end

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

module Angstrom_JSON_Optimized = struct
  open Angstrom

  let is_ws = function ' ' | '\t' | '\n' | '\r' -> true | _ -> false
  let is_digit_or_sign = function '0' .. '9' | '-' | '.' -> true | _ -> false

  (* Zero-copy number parsing via Unsafe — same fast-path as Parseff's float_of_span *)
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

  (* Manual loop with peek_char — no sep_by backtracking overhead *)
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

let () =
  for _ = 1 to 1000 do
    ignore (Parseff_JSON.bench_span json_input);
    ignore (Parseff_JSON.bench_generic json_input);
    ignore (Angstrom_JSON.bench json_input);
    ignore (Angstrom_JSON_Optimized.bench json_input)
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
        ( "Parseff (generic)",
          (fun () -> ignore (Parseff_JSON.bench_generic json_input)),
          ()
        );
        ("Angstrom", (fun () -> ignore (Angstrom_JSON.bench json_input)), ());
        ( "Angstrom (optimized)",
          (fun () -> ignore (Angstrom_JSON_Optimized.bench json_input)),
          ()
        );
      ]
  in

  print_newline ();
  tabulate results;

  Printf.printf "\nNote: Lower latency is better.\n"
