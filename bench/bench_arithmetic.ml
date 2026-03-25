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
  let parse input = parse_string ~consume:All expr input |> Result.to_option
end

(** {1 Angstrom (optimized)} *)

module Angstrom_Arith_Optimized = struct
  open Angstrom

  let is_digit c = c >= '0' && c <= '9'

  (* Zero-copy integer parsing via Unsafe *)
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

  (* Manual precedence loop with peek_char — no chainl1/<|> backtracking *)
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

  let parse input = parse_string ~consume:All expr input |> Result.to_option
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

  let parse input =
    match parse_string (expr << eof) input () with
    | MParser.Success result ->
        Some result
    | MParser.Failed _ ->
        None
end

(** {1 Parseff (generic)} *)

module Parseff_Arith_Generic = struct
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

  let parse input =
    match Parseff.parse input expr with
    | Ok result ->
        Some result
    | Error _ ->
        None
end

module Parseff_Arith_Fused = struct
  let[@inline always] is_digit c = c >= '0' && c <= '9'

  let[@inline] int_of_span (s : Parseff.span) =
    let rec go acc i =
      if i >= s.off + s.len then
        acc
      else
        let d = Char.code (String.unsafe_get s.buf i) - Char.code '0' in
        go ((acc * 10) + d) (i + 1)
    in
    go 0 s.off

  let is_arith_op c = c = '+' || c = '-' || c = '*' || c = '/'

  let expr () =
    (* Parse the full expression as: number (op number)* using bulk reads *)
    let first_span = Parseff.take_while_span is_digit in
    let first = int_of_span first_span in
    (* Collect all (op, number) pairs *)
    let rec collect_ops nums ops =
      let op_span = Parseff.take_while_span is_arith_op in
      if op_span.len = 0 then
        (nums, ops)
      else
        let op_char = String.unsafe_get op_span.buf op_span.off in
        let num_span = Parseff.take_while_span is_digit in
        let num = int_of_span num_span in
        collect_ops (num :: nums) (op_char :: ops)
    in
    let nums_rev, ops_rev = collect_ops [] [] in
    let nums = List.rev nums_rev in
    let ops = List.rev ops_rev in
    (* Two-pass evaluation respecting precedence *)
    (* Pass 1: evaluate * and / left-to-right, collecting + and - *)
    let rec pass1 acc nums ops =
      match (nums, ops) with
      | n :: rest_nums, '*' :: rest_ops ->
          pass1 (acc * n) rest_nums rest_ops
      | n :: rest_nums, '/' :: rest_ops ->
          pass1 (acc / n) rest_nums rest_ops
      | _, ('+' | '-') :: _ ->
          (acc, nums, ops)
      | [], [] ->
          (acc, [], [])
      | _ ->
          (acc, nums, ops)
    in
    let rec pass2 result nums ops =
      match (nums, ops) with
      | [], [] ->
          result
      | n :: rest_nums, '+' :: rest_ops ->
          let term, rest_nums', rest_ops' = pass1 n rest_nums rest_ops in
          pass2 (result + term) rest_nums' rest_ops'
      | n :: rest_nums, '-' :: rest_ops ->
          let term, rest_nums', rest_ops' = pass1 n rest_nums rest_ops in
          pass2 (result - term) rest_nums' rest_ops'
      | _ ->
          result
    in
    let first_term, rest_nums, rest_ops = pass1 first nums ops in
    let result = pass2 first_term rest_nums rest_ops in
    Parseff.end_of_input ();
    result

  let parse input =
    match Parseff.parse input expr with
    | Ok result ->
        Some result
    | Error _ ->
        None
end

let () =
  (* warmup *)
  for _ = 1 to 1000 do
    ignore (Parseff_Arith_Fused.parse arith_input);
    ignore (Parseff_Arith_Generic.parse arith_input);
    ignore (Angstrom_Arith.parse arith_input);
    ignore (Angstrom_Arith_Optimized.parse arith_input);
    ignore (MParser_Arith.parse arith_input)
  done;

  Printf.printf "Arithmetic Benchmark: Parseff vs Angstrom vs MParser\n";
  Printf.printf "=====================================================\n\n";
  Printf.printf "Input: %s\n\n" arith_input;

  let results =
    latencyN ~repeat:3 5000000L
      [
        ( "Parseff (fused)",
          (fun () -> ignore (Parseff_Arith_Fused.parse arith_input)),
          ()
        );
        ( "Parseff (generic)",
          (fun () -> ignore (Parseff_Arith_Generic.parse arith_input)),
          ()
        );
        ("Angstrom", (fun () -> ignore (Angstrom_Arith.parse arith_input)), ());
        ( "Angstrom (opt)",
          (fun () -> ignore (Angstrom_Arith_Optimized.parse arith_input)),
          ()
        );
        ("MParser", (fun () -> ignore (MParser_Arith.parse arith_input)), ());
      ]
  in

  print_newline ();
  tabulate results;

  Printf.printf "\nNote: Higher throughput (N/s) is better.\n"
