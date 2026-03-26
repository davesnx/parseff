open Benchmark

let json_input = {|[1, 2, 3, 4, 5, 6, 7, 8, 9, 10]|}

let[@inline always] is_digit_or_sign = function
  | '0' .. '9' | '-' | '.' ->
      true
  | _ ->
      false

let[@inline always] is_ws = function
  | ' ' | '\t' | '\n' | '\r' ->
      true
  | _ ->
      false

module Parseff_JSON = struct
  let json_array_generic () =
    Parseff.skip_whitespace ();
    let _ = Parseff.char '[' in
    Parseff.skip_whitespace ();
    let number () =
      let s = Parseff.take_while ~at_least:1 ~label:"number" is_digit_or_sign in
      float_of_string s
    in
    let comma () =
      Parseff.skip_whitespace ();
      let _ = Parseff.char ',' in
      Parseff.skip_whitespace ()
    in
    let elements = Parseff.sep_by number comma () in
    Parseff.skip_whitespace ();
    let _ = Parseff.char ']' in
    Parseff.end_of_input ();
    elements

  let parse input =
    match Parseff.parse input json_array_generic with
    | Ok result ->
        Some result
    | Error _ ->
        None
end

module Parseff_JSON_Optimized = struct
  let[@inline always] float_of_span (s : Parseff.span) =
    float_of_string (Parseff.span_to_string s)

  let[@inline always] is_num c = (c >= '0' && c <= '9') || c = '-' || c = '.'

  let json_array () =
    Parseff.skip_while_then_char Parseff.is_whitespace '[';
    let spans = Parseff.sep_by_take_span Parseff.is_whitespace ',' is_num in
    let elements = List.map float_of_span spans in
    Parseff.skip_while_then_char Parseff.is_whitespace ']';
    Parseff.end_of_input ();
    elements

  let parse input =
    match Parseff.parse input json_array with
    | Ok result ->
        Some result
    | Error _ ->
        None
end

module Angstrom_JSON = struct
  open Angstrom

  let ws = skip_while is_ws
  let number = take_while1 is_digit_or_sign >>| float_of_string

  let json_array =
    ws *> char '[' *> ws *> sep_by (ws *> char ',' *> ws) number
    <* ws <* char ']'

  let parse input =
    parse_string ~consume:All json_array input |> Result.to_option
end

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

  let parse input =
    parse_string ~consume:All json_array input |> Result.to_option
end

let run_section title input iterations benches =
  Bench_style.print_banner title;
  Bench_style.print_label_value "Input" input;
  print_newline ();
  let results =
    latencyN ~repeat:3 iterations (List.map Bench_case.to_benchmark benches)
  in
  print_newline ();
  tabulate results;
  Printf.printf "\n";
  let section =
    Bench_report.print_gc_quick ~title:"GC Quick Stats (single batch)" benches
  in
  { section with title }

let () =
  let benches =
    [
      Bench_case.make ~name:"Parseff (optimized)" ~iterations:1000000L
        (fun () -> ignore (Parseff_JSON_Optimized.parse json_input)
      );
      Bench_case.make ~name:"Parseff (generic)" ~iterations:1000000L (fun () ->
          ignore (Parseff_JSON.parse json_input)
      );
      Bench_case.make ~name:"Angstrom (generic)" ~iterations:1000000L (fun () ->
          ignore (Angstrom_JSON.parse json_input)
      );
      Bench_case.make ~name:"Angstrom (optimized)" ~iterations:1000000L
        (fun () -> ignore (Angstrom_JSON_Optimized.parse json_input)
      );
    ]
  in
  for _ = 1 to 1000 do
    List.iter Bench_case.run benches
  done;

  let section =
    run_section "JSON Benchmark: Parseff vs Angstrom" json_input 1000000L
      benches
  in
  Bench_report.write_gc_quick_artifacts ~artifact_name:"bench_json"
    ~bench_name:"JSON Benchmark: Parseff vs Angstrom" [ section ];

  Bench_style.print_notice "Note" "Higher throughput (N/s) is better."
