(* Fast benchmark for autoresearch — runs tight loops and reports METRIC lines *)

let json_input = {|[1, 2, 3, 4, 5, 6, 7, 8, 9, 10]|}
let csv_input = "hello,world,foo,bar,baz,qux,quux,corge,grault,garply"
let arith_input = "1+2*3+4"

(* Generic JSON parser using char/sep_by/or_ *)
let is_digit_or_sign c = (c >= '0' && c <= '9') || c = '-' || c = '.'
let is_ws c = c = ' ' || c = '\t' || c = '\n' || c = '\r'

let parse_json_generic input =
  match
    Parseff.parse input (fun () ->
        let _ = Parseff.char '[' in
        Parseff.skip_while is_ws;
        let[@inline] fast_float_of_string s =
          let len = String.length s in
          if len = 1 then
            Float.of_int (Char.code (String.unsafe_get s 0) - 48)
          else if len = 2 then
            Float.of_int
              (((Char.code (String.unsafe_get s 0) - 48) * 10)
              + (Char.code (String.unsafe_get s 1) - 48)
              )
          else
            float_of_string s
        in
        let number () =
          let s =
            Parseff.take_while ~at_least:1 ~label:"number" is_digit_or_sign
          in
          fast_float_of_string s
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
    )
  with
  | Ok r ->
      Some r
  | Error _ ->
      None

(* Generic CSV parser using char/sep_by *)
let parse_csv_generic input =
  match
    Parseff.parse input (fun () ->
        Parseff.sep_by
          (fun () -> Parseff.take_while (fun c -> c <> ','))
          (fun () -> ignore (Parseff.char ','))
          ()
    )
  with
  | Ok r ->
      Some r
  | Error _ ->
      None

(* Generic arithmetic parser using char/or_/fold_left *)
let parse_arith_generic input =
  match
    Parseff.parse input (fun () ->
        let[@inline] fast_int_of_string s =
          let len = String.length s in
          if len = 1 then
            Char.code (String.unsafe_get s 0) - 48
          else
            int_of_string s
        in
        let number () =
          let s =
            Parseff.take_while ~at_least:1 ~label:"digit" (fun c ->
                c >= '0' && c <= '9'
            )
          in
          fast_int_of_string s
        in
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
        in
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
        in
        let term () = Parseff.fold_left number mul_op () in
        Parseff.fold_left term add_op ()
    )
  with
  | Ok r ->
      Some r
  | Error _ ->
      None

let time_it name n f =
  (* warmup *)
  for _ = 1 to 5000 do
    ignore (Sys.opaque_identity (f ()))
  done;
  (* 3 runs, take median *)
  let runs =
    Array.init 3 (fun _ ->
        let t0 = Sys.time () in
        for _ = 1 to n do
          ignore (Sys.opaque_identity (f ()))
        done;
        let t1 = Sys.time () in
        Float.of_int n /. (t1 -. t0)
    )
  in
  Array.sort Float.compare runs;
  let median = runs.(1) in
  Printf.printf "METRIC %s=%.0f\n" name median

let () =
  let n = 200_000 in

  time_it "json_fused" n (fun () -> Parseff_bench.parse_json_array json_input);
  time_it "json_generic" n (fun () -> parse_json_generic json_input);
  time_it "csv_fused" n (fun () -> Parseff_bench.parse_csv csv_input);
  time_it "csv_generic" n (fun () -> parse_csv_generic csv_input);
  time_it "arith_fused" n (fun () -> Parseff_bench.parse_arithmetic arith_input);
  time_it "arith_generic" n (fun () -> parse_arith_generic arith_input)
