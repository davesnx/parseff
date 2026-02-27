open Benchmark

let arith_input = "1+2*3+4"

(** {1 Angstrom} *)

module Angstrom_Arith = struct
  open Angstrom

  let is_digit c = c >= '0' && c <= '9'
  let number = take_while1 (fun c -> is_digit c) >>| int_of_string

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

(** {1 MParser} *)

module MParser_Arith = struct
  open MParser

  let decimal : (int, unit) t =
    many1_satisfy (function '0' .. '9' -> true | _ -> false) |>> int_of_string

  let mul_op : (int -> int -> int, unit) t =
    char '*' >>$ ( * ) <|> (char '/' >>$ ( / ))

  let add_op : (int -> int -> int, unit) t =
    char '+' >>$ ( + ) <|> (char '-' >>$ ( - ))

  let term : (int, unit) t = chain_left1 decimal mul_op
  let expr : (int, unit) t = chain_left1 term add_op

  let bench input =
    match parse_string (expr << eof) input () with
    | MParser.Success result -> Some result
    | MParser.Failed _ -> None
end

(** {1 Runner} *)

let () =
  (* warmup *)
  for _ = 1 to 1000 do
    ignore (Parseff_bench.parse_arithmetic arith_input);
    ignore (Angstrom_Arith.bench arith_input);
    ignore (MParser_Arith.bench arith_input)
  done;

  Printf.printf "Arithmetic Benchmark: Parseff vs Angstrom vs MParser\n";
  Printf.printf "=====================================================\n\n";
  Printf.printf "Input: %s\n\n" arith_input;

  let results =
    latencyN ~repeat:3 100000L
      [
        ( "Parseff",
          (fun () -> ignore (Parseff_bench.parse_arithmetic arith_input)),
          () );
        ("Angstrom", (fun () -> ignore (Angstrom_Arith.bench arith_input)), ());
        ("MParser", (fun () -> ignore (MParser_Arith.bench arith_input)), ());
      ]
  in

  print_newline ();
  tabulate results;

  Printf.printf "\nNote: Higher throughput (N/s) is better.\n"
