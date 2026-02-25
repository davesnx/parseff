(** Microbenchmarks to measure individual effect costs *)

open Benchmark

let input_10 = "aaaaaaaaaa"  (* 10 a's *)

(* Measure cost of a single Satisfy effect *)
let bench_satisfy () =
  let open Parseff in
  match run input_10 (fun () ->
    let _ = satisfy (fun c -> c = 'a') "a" in
    let _ = satisfy (fun c -> c = 'a') "a" in
    let _ = satisfy (fun c -> c = 'a') "a" in
    let _ = satisfy (fun c -> c = 'a') "a" in
    let _ = satisfy (fun c -> c = 'a') "a" in
    let _ = satisfy (fun c -> c = 'a') "a" in
    let _ = satisfy (fun c -> c = 'a') "a" in
    let _ = satisfy (fun c -> c = 'a') "a" in
    let _ = satisfy (fun c -> c = 'a') "a" in
    let _ = satisfy (fun c -> c = 'a') "a" in
    ()
  ) with
  | Ok _ -> ()
  | Error _ -> ()

(* Measure cost of a single take_while effect *)
let bench_take_while () =
  let open Parseff in
  match run input_10 (fun () ->
    let _ = take_while (fun c -> c = 'a') in
    ()
  ) with
  | Ok _ -> ()
  | Error _ -> ()

(* Measure cost of skip_while *)
let bench_skip_while () =
  let open Parseff in
  match run input_10 (fun () ->
    skip_while (fun c -> c = 'a');
    ()
  ) with
  | Ok _ -> ()
  | Error _ -> ()

(* Measure cost of a Choose *)
let bench_choose () =
  let open Parseff in
  match run "a" (fun () ->
    ((fun () -> let _ = consume "a" in "a") <|> fun () -> consume "b") ()
  ) with
  | Ok _ -> ()
  | Error _ -> ()

(* Measure cost of many with small parser *)
let bench_many () =
  let open Parseff in
  match run input_10 (fun () ->
    many (fun () -> satisfy (fun c -> c = 'a') "a") ()
  ) with
  | Ok _ -> ()
  | Error _ -> ()

(* Baseline: just run + return *)
let bench_baseline () =
  let open Parseff in
  match run "" (fun () -> ()) with
  | Ok _ -> ()
  | Error _ -> ()

let () =
  for _ = 1 to 1000 do
    bench_satisfy ();
    bench_take_while ();
    bench_skip_while ();
    bench_choose ();
    bench_many ();
    bench_baseline ()
  done;

  Printf.printf "Effect Microbenchmarks\n";
  Printf.printf "======================\n\n";

  let results =
    latencyN
      ~repeat:3
      100000L
      [
        ("baseline (run+ret)", bench_baseline, ());
        ("10x satisfy", bench_satisfy, ());
        ("1x take_while(10)", bench_take_while, ());
        ("1x skip_while(10)", bench_skip_while, ());
        ("1x choose (success)", bench_choose, ());
        ("many(10x satisfy)", bench_many, ());
      ]
  in

  print_newline ();
  tabulate results
