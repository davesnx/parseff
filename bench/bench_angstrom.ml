open Benchmark

let json_input = {|[1, 2, 3, 4, 5, 6, 7, 8, 9, 10]|}
let csv_input = "hello,world,foo,bar,baz,qux,quux,corge,grault,garply"
let arith_input = "1+2*3+4"

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
    Parseff.skip_while is_ws;
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
    Parseff.end_of_input ();
    elements

  let bench_optimized = Parseff_bench.parse_json_array

  let bench_generic input =
    match Parseff.parse input json_array_generic with
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

  let bench input =
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

  let bench input =
    parse_string ~consume:All json_array input |> Result.to_option
end

module Parseff_CSV = struct
  let csv_generic () =
    let fields =
      Parseff.sep_by
        (fun () -> Parseff.take_while (fun c -> c <> ','))
        (fun () -> ignore (Parseff.char ','))
        ()
    in
    Parseff.end_of_input ();
    fields

  let bench_fused input = Parseff_bench.parse_csv input

  let bench_generic input =
    match Parseff.parse input csv_generic with
    | Ok result ->
        Some result
    | Error _ ->
        None
end

module Angstrom_CSV = struct
  open Angstrom

  let field = take_while (function ',' -> false | _ -> true)
  let csv = sep_by (char ',') field
  let bench input = parse_string ~consume:All csv input |> Result.to_option
end

module Angstrom_CSV_Optimized = struct
  open Angstrom

  let field = take_while (function ',' -> false | _ -> true)

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

  let bench input = parse_string ~consume:All csv input |> Result.to_option
end

module Parseff_Arith = struct
  let number () =
    let s =
      Parseff.take_while ~at_least:1 ~label:"digit" (fun c ->
          c >= '0' && c <= '9'
      )
    in
    int_of_string s

  let mul_op () =
    Parseff.or_
      (fun () ->
        let _ = Parseff.char '*' in
        ( * )
      )
      (fun () ->
        let _ = Parseff.char '/' in
        ( / )
      )
      ()

  let add_op () =
    Parseff.or_
      (fun () ->
        let _ = Parseff.char '+' in
        ( + )
      )
      (fun () ->
        let _ = Parseff.char '-' in
        ( - )
      )
      ()

  let term () = Parseff.fold_left number mul_op ()

  let expr () =
    let result = Parseff.fold_left term add_op () in
    Parseff.end_of_input ();
    result

  let bench_fused input = Parseff_bench.parse_arithmetic input

  let bench_generic input =
    match Parseff.parse input expr with
    | Ok result ->
        Some result
    | Error _ ->
        None
end

module Angstrom_Arith = struct
  open Angstrom

  let is_digit c = c >= '0' && c <= '9'
  let number = take_while1 is_digit >>| int_of_string

  let chainl1 p op =
    let rec go acc =
      op >>= (fun f -> p >>= fun x -> go (f acc x)) <|> return acc
    in
    p >>= fun init -> go init

  let mul_op = char '*' *> return ( * ) <|> char '/' *> return ( / )
  let add_op = char '+' *> return ( + ) <|> char '-' *> return ( - )
  let term = chainl1 number mul_op
  let expr = chainl1 term add_op

  let bench input = parse_string ~consume:All expr input |> Result.to_option
end

module Angstrom_Arith_Optimized = struct
  open Angstrom

  let is_digit c = c >= '0' && c <= '9'

  let number =
    Unsafe.take_while1 is_digit (fun bs ~off ~len ->
        let rec go acc i =
          if i >= off + len then
            acc
          else
            let d = Char.code (Bigstringaf.unsafe_get bs i) - Char.code '0' in
            go ((acc * 10) + d) (i + 1)
        in
        go 0 off
    )

  let term =
    number >>= fun init ->
    let rec loop acc =
      peek_char >>= function
      | Some '*' ->
          advance 1 *> number >>= fun n -> loop (acc * n)
      | Some '/' ->
          advance 1 *> number >>= fun n -> loop (acc / n)
      | _ ->
          return acc
    in
    loop init

  let expr =
    term >>= fun init ->
    let rec loop acc =
      peek_char >>= function
      | Some '+' ->
          advance 1 *> term >>= fun t -> loop (acc + t)
      | Some '-' ->
          advance 1 *> term >>= fun t -> loop (acc - t)
      | _ ->
          return acc
    in
    loop init

  let bench input = parse_string ~consume:All expr input |> Result.to_option
end

let run_section title input iterations benches =
  Printf.printf "%s\n" title;
  Printf.printf "%s\n\n" (String.make (String.length title) '=');
  Printf.printf "Input: %s\n\n" input;
  let results = latencyN ~repeat:3 iterations benches in
  print_newline ();
  tabulate results;
  Printf.printf "\n"

let () =
  for _ = 1 to 1000 do
    ignore (Parseff_JSON.bench_optimized json_input);
    ignore (Parseff_JSON.bench_generic json_input);
    ignore (Angstrom_JSON.bench json_input);
    ignore (Angstrom_JSON_Optimized.bench json_input);
    ignore (Parseff_CSV.bench_fused csv_input);
    ignore (Parseff_CSV.bench_generic csv_input);
    ignore (Angstrom_CSV.bench csv_input);
    ignore (Angstrom_CSV_Optimized.bench csv_input);
    ignore (Parseff_Arith.bench_fused arith_input);
    ignore (Parseff_Arith.bench_generic arith_input);
    ignore (Angstrom_Arith.bench arith_input);
    ignore (Angstrom_Arith_Optimized.bench arith_input)
  done;

  run_section "JSON Benchmark: Parseff vs Angstrom" json_input 1000000L
    [
      ( "Parseff (optimized)",
        (fun () -> ignore (Parseff_JSON.bench_optimized json_input)),
        ()
      );
      ( "Parseff (generic)",
        (fun () -> ignore (Parseff_JSON.bench_generic json_input)),
        ()
      );
      ( "Angstrom (generic)",
        (fun () -> ignore (Angstrom_JSON.bench json_input)),
        ()
      );
      ( "Angstrom (optimized)",
        (fun () -> ignore (Angstrom_JSON_Optimized.bench json_input)),
        ()
      );
    ];

  run_section "CSV Benchmark: Parseff vs Angstrom" csv_input 2000000L
    [
      ( "Parseff (fused)",
        (fun () -> ignore (Parseff_CSV.bench_fused csv_input)),
        ()
      );
      ( "Parseff (generic)",
        (fun () -> ignore (Parseff_CSV.bench_generic csv_input)),
        ()
      );
      ( "Angstrom (generic)",
        (fun () -> ignore (Angstrom_CSV.bench csv_input)),
        ()
      );
      ( "Angstrom (optimized)",
        (fun () -> ignore (Angstrom_CSV_Optimized.bench csv_input)),
        ()
      );
    ];

  run_section "Arithmetic Benchmark: Parseff vs Angstrom" arith_input 5000000L
    [
      ( "Parseff (fused)",
        (fun () -> ignore (Parseff_Arith.bench_fused arith_input)),
        ()
      );
      ( "Parseff (generic)",
        (fun () -> ignore (Parseff_Arith.bench_generic arith_input)),
        ()
      );
      ( "Angstrom (generic)",
        (fun () -> ignore (Angstrom_Arith.bench arith_input)),
        ()
      );
      ( "Angstrom (optimized)",
        (fun () -> ignore (Angstrom_Arith_Optimized.bench arith_input)),
        ()
      );
    ];

  Printf.printf "Note: Higher throughput (N/s) is better.\n"
