let iterations = 100_000
let warmup_iterations = 1_000

let run_n n f =
  for _ = 1 to n do
    ignore (Sys.opaque_identity (f ()))
  done

let words_per_call f =
  run_n warmup_iterations f;
  Gc.full_major ();
  let before = Gc.minor_words () in
  run_n iterations f;
  let after = Gc.minor_words () in
  (after -. before) /. Float.of_int iterations

let measure_in_parse f =
  match Parseff.parse ~consume:`Partial "" (fun () -> words_per_call f) with
  | Ok words ->
      words
  | Error error ->
      Alcotest.fail (Parseff.render_error error)

let check_words name maximum f =
  let actual = measure_in_parse f in
  if actual > maximum then
    Alcotest.failf
      "%s allocated %.6f minor words per call, expected at most %.0f" name
      actual maximum

let test_stopping_failures () =
  check_words "many" 0. (fun () -> Parseff.many (fun () -> Parseff.char 'x') ());
  check_words "sep_by" 0. (fun () ->
      Parseff.sep_by
        (fun () -> Parseff.char 'x')
        (fun () -> Parseff.char ',')
        ()
  );
  check_words "optional" 4. (fun () ->
      Parseff.optional (fun () -> Parseff.char 'x') ()
  );
  check_words "or_" 0. (fun () ->
      Parseff.or_ (fun () -> Parseff.char 'x') (fun () -> 'y') ()
  )

let () =
  Alcotest.run "Allocations"
    [
      ( "failure paths",
        [ Alcotest.test_case "stopping failures" `Quick test_stopping_failures ]
      );
    ]
