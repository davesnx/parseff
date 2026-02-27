open Benchmark

let ten_a_chars = "aaaaaaaaaa"
let is_a c = c = 'a'

let bench_10x_satisfy () =
  match
    Parseff.parse ten_a_chars (fun () ->
        let _ = Parseff.satisfy is_a ~label:"a" in
        let _ = Parseff.satisfy is_a ~label:"a" in
        let _ = Parseff.satisfy is_a ~label:"a" in
        let _ = Parseff.satisfy is_a ~label:"a" in
        let _ = Parseff.satisfy is_a ~label:"a" in
        let _ = Parseff.satisfy is_a ~label:"a" in
        let _ = Parseff.satisfy is_a ~label:"a" in
        let _ = Parseff.satisfy is_a ~label:"a" in
        let _ = Parseff.satisfy is_a ~label:"a" in
        let _ = Parseff.satisfy is_a ~label:"a" in
        ())
  with
  | Ok _ -> ()
  | Error _ -> ()

let bench_single_take_while () =
  match
    Parseff.parse ten_a_chars (fun () ->
        let _ = Parseff.take_while is_a in
        ())
  with
  | Ok _ -> ()
  | Error _ -> ()

let bench_single_skip_while () =
  match
    Parseff.parse ten_a_chars (fun () ->
        Parseff.skip_while is_a;
        ())
  with
  | Ok _ -> ()
  | Error _ -> ()

let bench_choose_first_branch_succeeds () =
  match
    Parseff.parse "a" (fun () ->
        Parseff.or_
          (fun () ->
            let _ = Parseff.consume "a" in
            "a")
          (fun () -> Parseff.consume "b")
          ())
  with
  | Ok _ -> ()
  | Error _ -> ()

let bench_many_10x_satisfy () =
  match
    Parseff.parse ten_a_chars (fun () ->
        Parseff.many (fun () -> Parseff.satisfy is_a ~label:"a") ())
  with
  | Ok _ -> ()
  | Error _ -> ()

let bench_parse_and_return () =
  match Parseff.parse "" (fun () -> ()) with Ok _ -> () | Error _ -> ()

let () =
  for _ = 1 to 1000 do
    bench_10x_satisfy ();
    bench_single_take_while ();
    bench_single_skip_while ();
    bench_choose_first_branch_succeeds ();
    bench_many_10x_satisfy ();
    bench_parse_and_return ()
  done;

  Printf.printf "Effect Microbenchmarks\n";
  Printf.printf "======================\n\n";

  let results =
    latencyN ~repeat:3 100000L
      [
        ("baseline (parse+ret)", bench_parse_and_return, ());
        ("10x satisfy", bench_10x_satisfy, ());
        ("1x take_while(10)", bench_single_take_while, ());
        ("1x skip_while(10)", bench_single_skip_while, ());
        ("1x choose (success)", bench_choose_first_branch_succeeds, ());
        ("many(10x satisfy)", bench_many_10x_satisfy, ());
      ]
  in

  print_newline ();
  tabulate results
