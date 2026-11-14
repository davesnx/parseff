open Parseff

let ok_exn = function Ok v -> v | Error e -> Alcotest.fail (render_error e)

let test_ref_kept_after_backtrack () =
  let r = ref 0 in
  let d = Diagnostics.create () in
  let bump () =
    incr r;
    warn d "bump"
  in
  let after_or =
    ok_exn
      (parse ~consume:`Partial ~diagnostics:d "b"
         (or_
            (fun () ->
              bump ();
              char 'a'
            )
            (fun () -> char 'b')
         )
      )
  in
  Alcotest.(check char) "or_ took right" 'b' after_or;
  Alcotest.(check int) "ref kept after or_" 1 !r;
  Alcotest.(check int) "diagnostic rolled back" 0 (Diagnostics.length d);
  r := 0;
  ignore
    (ok_exn
       (parse ~consume:`Partial "aab"
          (many (fun () ->
               bump ();
               char 'a'
           )
          )
       )
    );
  Alcotest.(check int) "ref kept after many" 3 !r;
  r := 0;
  ignore
    (ok_exn
       (parse ~consume:`Partial "b"
          (optional (fun () ->
               bump ();
               char 'a'
           )
          )
       )
    );
  Alcotest.(check int) "ref kept after optional" 1 !r;
  r := 0;
  ignore
    (ok_exn
       (parse ~consume:`Partial "a" (fun () ->
            look_ahead (fun () ->
                bump ();
                char 'a'
            )
        )
       )
    );
  Alcotest.(check int) "ref kept after look_ahead" 1 !r

let test_or_rolls_back () =
  let c = Ref.create 0 in
  let v =
    ok_exn
      (parse "b"
         (or_
            (fun () ->
              Ref.set c 1;
              let _ = char 'a' in
              -1
            )
            (fun () ->
              let seen = Ref.get c in
              let _ = char 'b' in
              seen
            )
         )
      )
  in
  Alcotest.(check int) "right branch sees the old value" 0 v;
  Alcotest.(check int) "value after the parse" 0 (Ref.get c)

let chunked_source input =
  let pos = ref 0 in
  Source.of_function (fun buf off len ->
      let n = min (min 1 len) (String.length input - !pos) in
      Bytes.blit_string input !pos buf off n;
      pos := !pos + n;
      n
  )

let outcome = function Ok () -> "ok" | Error e -> "error " ^ error_message e

let check_final name input expected_outcome expected make =
  let run label runner =
    let cell, parser = make () in
    let result = runner parser in
    Alcotest.(check string)
      (name ^ " " ^ label ^ " outcome")
      expected_outcome (outcome result);
    Alcotest.(check int) (name ^ " " ^ label ^ " value") expected (Ref.get cell)
  in
  run "parse" (fun p -> parse ~consume:`Partial input p);
  run "parse_source" (fun p ->
      parse_source ~consume:`Partial (chunked_source input) p
  )

let incr_cell c = Ref.set c (Ref.get c + 1)

let with_cell f () =
  let c = Ref.create 0 in
  (c, f c)

let test_one_of () =
  check_final "one_of" "c" "ok" 0
    (with_cell (fun c () ->
         ignore
           (one_of
              [
                (fun () ->
                  Ref.set c 1;
                  char 'a'
                );
                (fun () ->
                  Ref.set c 2;
                  char 'b'
                );
                (fun () ->
                  if Ref.get c <> 0 then fail "not rolled back";
                  char 'c'
                );
              ]
              ()
           )
     )
    )

let test_many () =
  check_final "many" "aab" "ok" 2
    (with_cell (fun c () ->
         ignore
           (many
              (fun () ->
                incr_cell c;
                char 'a'
              )
              ()
           )
     )
    );
  check_final "many at_least" "aab" "ok" 2
    (with_cell (fun c () ->
         ignore
           (many ~at_least:1
              (fun () ->
                incr_cell c;
                char 'a'
              )
              ()
           )
     )
    )

let test_optional () =
  check_final "optional" "b" "ok" 0
    (with_cell (fun c () ->
         ignore
           (optional
              (fun () ->
                Ref.set c 1;
                char 'a'
              )
              ()
           )
     )
    )

let test_look_ahead () =
  check_final "look_ahead success" "a" "ok" 0
    (with_cell (fun c () ->
         ignore
           (look_ahead (fun () ->
                Ref.set c 1;
                char 'a'
            )
           )
     )
    );
  check_final "look_ahead failure" "b" "ok" 0
    (with_cell (fun c () ->
         ignore
           (optional
              (fun () ->
                look_ahead (fun () ->
                    Ref.set c 1;
                    char 'a'
                )
              )
              ()
           )
     )
    )

let test_sep_by () =
  check_final "sep_by" "1,2,x" "ok" 2
    (with_cell (fun c () ->
         ignore
           (sep_by
              (fun () ->
                incr_cell c;
                digit ()
              )
              (fun () -> char ',')
              ()
           )
     )
    );
  check_final "sep_by first element" "x" "ok" 0
    (with_cell (fun c () ->
         ignore
           (sep_by
              (fun () ->
                incr_cell c;
                digit ()
              )
              (fun () -> char ',')
              ()
           )
     )
    )

let test_fold_left () =
  check_final "fold_left" "1+2+" "ok" 1
    (with_cell (fun c () ->
         ignore
           (fold_left integer
              (fun () ->
                incr_cell c;
                let _ = char '+' in
                ( + )
              )
              ()
           )
     )
    )

let test_kept_on_success () =
  check_final "or_ left wins" "a" "ok" 1
    (with_cell (fun c () ->
         ignore
           (or_
              (fun () ->
                Ref.set c 1;
                char 'a'
              )
              (fun () -> char 'b')
              ()
           )
     )
    );
  check_final "set outside any branch" "a" "ok" 1
    (with_cell (fun c () ->
         Ref.set c 1;
         ignore (optional (fun () -> char 'b') ());
         ignore (char 'a')
     )
    )

let test_kept_after_commit () =
  check_final "commit" "ac" "error expected 'b', found 'c'" 1
    (with_cell (fun c () ->
         ignore
           (or_
              (fun () ->
                Ref.set c 1;
                let _ = char 'a' in
                commit ();
                char 'b'
              )
              (fun () -> char 'a')
              ()
           )
     )
    );
  check_final "commit seals one frame" "ac" "ok" 0
    (with_cell (fun c () ->
         ignore
           (optional
              (or_
                 (fun () ->
                   Ref.set c 1;
                   let _ = char 'a' in
                   commit ();
                   char 'b'
                 )
                 (fun () -> char 'a')
              )
              ()
           )
     )
    )

let test_kept_after_cut () =
  check_final "cut" "ac" "error expected 'b', found 'c'" 1
    (with_cell (fun c () ->
         ignore
           (optional
              (fun () ->
                context "item" (fun () ->
                    Ref.set c 1;
                    let _ = char 'a' in
                    cut ();
                    char 'b'
                )
              )
              ()
           )
     )
    )

let test_commit_after_set () =
  let c = Ref.create 0 in
  let result =
    parse ~consume:`Partial "ac" (fun () ->
        or_
          (fun () ->
            Ref.set c 1;
            or_
              (fun () ->
                let _ = char 'a' in
                commit ();
                let _ = char 'b' in
                `Inner_left
              )
              (fun () -> `Inner_right)
              ()
          )
          (fun () -> `Outer_right)
          ()
    )
  in
  match result with
  | Ok `Outer_right ->
      Alcotest.(check int) "set undone" 0 (Ref.get c)
  | Ok `Inner_right ->
      Alcotest.fail "commit sealed the outer or_, not the inner one"
  | Ok `Inner_left ->
      Alcotest.fail "unexpected success"
  | Error e ->
      Alcotest.fail (render_error e)

let test_nested () =
  let seen = ref [] in
  let make () =
    seen := [];
    let c = Ref.create 0 in
    let parser () =
      ignore
        (or_
           (fun () ->
             Ref.set c 1;
             ignore
               (or_
                  (fun () ->
                    Ref.set c 2;
                    char 'a'
                  )
                  (fun () ->
                    seen := Ref.get c :: !seen;
                    char 'x'
                  )
                  ()
               );
             char 'z'
           )
           (fun () ->
             seen := Ref.get c :: !seen;
             char 'a'
           )
           ()
        )
    in
    (c, parser)
  in
  check_final "nested" "ay" "ok" 0 make;
  Alcotest.(check (list int)) "inner success, outer backtrack" [ 0 ] !seen;
  check_final "nested" "y" "error expected 'a' or 'x', found 'y'" 0 make;
  Alcotest.(check (list int)) "inner and outer backtrack" [ 0; 1 ] !seen

let test_two_cells () =
  let names = Ref.create [] in
  let make () =
    Ref.set names [];
    let count = Ref.create 0 in
    let parser () =
      Ref.set names [ "kept" ];
      ignore
        (optional
           (fun () ->
             Ref.set names ("dropped" :: Ref.get names);
             incr_cell count;
             char 'a'
           )
           ()
        );
      incr_cell count
    in
    (count, parser)
  in
  check_final "two cells" "b" "ok" 1 make;
  Alcotest.(check (list string)) "list cell" [ "kept" ] (Ref.get names)

let test_across_parses () =
  let c = Ref.create 0 in
  let set_then_backtrack v () =
    Ref.set c v;
    ignore
      (optional
         (fun () ->
           Ref.set c (v * 10);
           char 'a'
         )
         ()
      )
  in
  ignore (ok_exn (parse "" (set_then_backtrack 5)));
  Alcotest.(check int) "kept after first parse" 5 (Ref.get c);
  ignore
    (ok_exn
       (parse ""
          (optional (fun () ->
               Ref.set c 7;
               char 'a'
           )
          )
       )
    );
  Alcotest.(check int) "second parse restores the first value" 5 (Ref.get c)

let test_outside_parse () =
  let c = Ref.create "a" in
  Ref.set c "b";
  Alcotest.(check string) "set and get work outside a parse" "b" (Ref.get c);
  ignore (ok_exn (parse "" (fun () -> Ref.set c "c")));
  Ref.set c "d";
  Alcotest.(check string) "after a parse" "d" (Ref.get c)

open Effect
open Effect.Deep

type _ Effect.t += Yield : unit Effect.t

let run_fibers fibers =
  let queue = Queue.create () in
  let spawn f =
    Queue.add
      (fun () ->
        match_with f ()
          {
            retc = (fun () -> ());
            exnc = raise;
            effc =
              (fun (type a) (e : a Effect.t) ->
                match e with
                | Yield ->
                    Some
                      (fun (k : (a, _) continuation) ->
                        Queue.add (fun () -> continue k ()) queue
                      )
                | _ ->
                    None
              );
          }
      )
      queue
  in
  List.iter spawn fibers;
  while not (Queue.is_empty queue) do
    (Queue.pop queue) ()
  done

let yielding_source data =
  let p = ref 0 in
  Source.of_function (fun b o len ->
      perform Yield;
      let n = min (min 1 len) (String.length data - !p) in
      Bytes.blit_string data !p b o n;
      p := !p + n;
      n
  )

let test_concurrent_parses () =
  let c = Ref.create 0 in
  let a = ref "unset" and b = ref "unset" in
  let go r v () =
    r :=
      match
        parse_source (yielding_source "xx") (fun () ->
            let _ = char 'x' in
            Ref.set c v;
            char 'x'
        )
      with
      | Ok _ ->
          "ok"
      | Error e ->
          error_message e
      | exception Invalid_argument m ->
          m
  in
  run_fibers [ go a 1; go b 2 ];
  Alcotest.(check string) "first parse" "ok" !a;
  Alcotest.(check string)
    "second parse" "Parseff.Ref.set: another running parse uses this cell" !b;
  Alcotest.(check int) "value of the first parse" 1 (Ref.get c);
  a := "unset";
  run_fibers [ go a 3 ];
  Alcotest.(check string) "a later parse can use the cell" "ok" !a;
  Alcotest.(check int) "value of the later parse" 3 (Ref.get c)

let test_set_while_parse_suspended () =
  let free = Ref.create 0 and owned = Ref.create 0 in
  let result = ref "unset" and other = ref "unset" in
  let parser () =
    or_
      (fun () ->
        Ref.set owned 1;
        char 'a'
      )
      (fun () -> char 'b')
      ()
  in
  let suspended () =
    result :=
      match parse_source (yielding_source "b") parser with
      | Ok _ ->
          "ok"
      | Error e ->
          error_message e
  in
  let outside () =
    Ref.set free 42;
    other :=
      match Ref.set owned 7 with
      | () ->
          "set"
      | exception Invalid_argument m ->
          m
  in
  run_fibers [ suspended; outside ];
  Alcotest.(check string) "suspended parse" "ok" !result;
  Alcotest.(check int) "set outside the parse kept" 42 (Ref.get free);
  Alcotest.(check string)
    "cell of the suspended parse"
    "Parseff.Ref.set: another running parse uses this cell" !other;
  Alcotest.(check int)
    "cell of the suspended parse rolled back" 0 (Ref.get owned)

let test_parse_continues_after_read_raises () =
  let c = Ref.create 0 in
  let raised = ref false in
  let src =
    Source.of_function (fun buf off _ ->
        if !raised then begin
          Bytes.set buf off 'b';
          1
        end else begin
          raised := true;
          raise Exit
        end
    )
  in
  let parser () =
    (try ignore (char 'a') with Exit -> ());
    or_
      (fun () ->
        Ref.set c 1;
        char 'a'
      )
      (fun () -> char 'b')
      ()
  in
  Alcotest.(check char)
    "parse after the exception" 'b'
    (ok_exn (parse_source ~consume:`Partial src parser));
  Alcotest.(check int) "set rolled back" 0 (Ref.get c)

let a_source n =
  let sent = ref 0 in
  Source.of_function (fun buf off len ->
      let k = min len (n - !sent) in
      Bytes.fill buf off k 'a';
      sent := !sent + k;
      k
  )

let live_words_after_many n =
  let c = Ref.create 0 in
  let parser () =
    let items =
      many
        (fun () ->
          or_
            (fun () ->
              Ref.set c (Ref.get c + 1);
              char 'a'
            )
            (fun () -> char 'b')
            ()
        )
        ()
    in
    Gc.full_major ();
    let live = (Gc.stat ()).live_words in
    ignore (Sys.opaque_identity items);
    live
  in
  match parse_source ~backtrack_window:64 (a_source n) parser with
  | Ok live ->
      Alcotest.(check int) "every set kept" n (Ref.get c);
      live
  | Error e ->
      Alcotest.fail (render_error e)

let test_many_after_dropped_log () =
  check_final "many after dropped log" "ax" "ok" 1
    (with_cell (fun c () ->
         ignore
           (many
              (fun () ->
                or_
                  (fun () ->
                    incr_cell c;
                    char 'a'
                  )
                  (fun () ->
                    Ref.set c 100;
                    char 'z'
                  )
                  ()
              )
              ()
           )
     )
    )

let[@inline never] parse_and_forget_input c w =
  let input = String.make 100_000 'a' in
  Weak.set w 0 (Some input);
  ignore (parse ~consume:`Partial input (fun () -> Ref.set c 1))

let test_cell_does_not_keep_input () =
  let c = Ref.create 0 in
  let w = Weak.create 1 in
  parse_and_forget_input c w;
  Gc.full_major ();
  Alcotest.(check bool) "input collected" false (Weak.check w 0);
  Alcotest.(check int) "cell still reachable" 1 (Ref.get c)

type custom_error += Escaping

let test_escaping_exceptions () =
  let run wrap raise_it =
    let c = Ref.create 0 in
    let d = Diagnostics.create () in
    let outcome =
      match
        parse ~diagnostics:d "a" (fun () ->
            wrap (fun () ->
                Ref.set c 1;
                warn d "w";
                raise_it ()
            )
        )
      with
      | Ok () ->
          "ok"
      | Error e ->
          "error " ^ error_message e
      | exception Exit ->
          "Exit"
    in
    (outcome, Ref.get c, Diagnostics.length d)
  in
  let raise_exit () = raise Exit in
  let call_fail () = fail "stop" in
  let call_error () = error Escaping in
  let custom = "error custom error (no ~custom printer given)" in
  let check name wrap raise_it expected =
    Alcotest.(check (triple string int int)) name expected (run wrap raise_it)
  in
  let in_or p = or_ p (fun () -> ()) () in
  let in_optional p = ignore (optional p ()) in
  let in_many p = ignore (many p ()) in
  let in_sep_by p = ignore (sep_by p (fun () -> char ',') ()) in
  let in_fold_left p =
    fold_left
      (fun () -> ())
      (fun () ->
        p ();
        fun () () -> ()
      )
      ()
  in
  check "or_ Exit" in_or raise_exit ("Exit", 1, 1);
  check "or_ fail" in_or call_fail ("error stop", 1, 1);
  check "or_ error" in_or call_error (custom, 0, 0);
  check "optional error" in_optional call_error (custom, 0, 0);
  check "optional Exit" in_optional raise_exit ("Exit", 1, 1);
  check "many Exit" in_many raise_exit ("Exit", 1, 1);
  check "sep_by Exit" in_sep_by raise_exit ("Exit", 1, 1);
  check "fold_left Exit" in_fold_left raise_exit ("Exit", 1, 1);
  check "look_ahead Exit" look_ahead raise_exit ("Exit", 0, 0);
  check "look_ahead fail" look_ahead call_fail ("error stop", 0, 0)

let test_log_bounded () =
  let small = live_words_after_many 1_000 in
  let large = live_words_after_many 200_000 in
  let per_iteration = float (large - small) /. 199_000. in
  if per_iteration > 3.5 then
    Alcotest.failf
      "%.2f live words per iteration; the result list takes 3, so the undo log \
       keeps the rest"
      per_iteration

let test_nested_parse () =
  let c = Ref.create 0 in
  let result =
    parse "" (fun () ->
        Ref.set c 1;
        match parse "" (fun () -> Ref.set c 2) with
        | _ ->
            "no error"
        | exception Invalid_argument m ->
            m
    )
  in
  Alcotest.(check string)
    "inner parse" "Parseff.Ref.set: another running parse uses this cell"
    (ok_exn result)

let () =
  Alcotest.run "Ref"
    [
      ( "ref",
        [
          Alcotest.test_case "kept after backtrack" `Quick
            test_ref_kept_after_backtrack;
        ]
      );
      ( "rollback",
        [
          Alcotest.test_case "or_" `Quick test_or_rolls_back;
          Alcotest.test_case "one_of" `Quick test_one_of;
          Alcotest.test_case "many" `Quick test_many;
          Alcotest.test_case "optional" `Quick test_optional;
          Alcotest.test_case "look_ahead" `Quick test_look_ahead;
          Alcotest.test_case "sep_by" `Quick test_sep_by;
          Alcotest.test_case "fold_left" `Quick test_fold_left;
          Alcotest.test_case "nested" `Quick test_nested;
          Alcotest.test_case "two cells" `Quick test_two_cells;
        ]
      );
      ( "kept",
        [
          Alcotest.test_case "on success" `Quick test_kept_on_success;
          Alcotest.test_case "after commit" `Quick test_kept_after_commit;
          Alcotest.test_case "after cut" `Quick test_kept_after_cut;
          Alcotest.test_case "commit after set" `Quick test_commit_after_set;
        ]
      );
      ( "lifetime",
        [
          Alcotest.test_case "across parses" `Quick test_across_parses;
          Alcotest.test_case "outside a parse" `Quick test_outside_parse;
          Alcotest.test_case "concurrent parses" `Quick test_concurrent_parses;
          Alcotest.test_case "set while a parse is suspended" `Quick
            test_set_while_parse_suspended;
          Alcotest.test_case "parse continues after a read raises" `Quick
            test_parse_continues_after_read_raises;
          Alcotest.test_case "nested parse" `Quick test_nested_parse;
          Alcotest.test_case "undo log bounded in many" `Quick test_log_bounded;
          Alcotest.test_case "many after dropped log" `Quick
            test_many_after_dropped_log;
          Alcotest.test_case "cell does not keep the input" `Quick
            test_cell_does_not_keep_input;
          Alcotest.test_case "exceptions that escape a branch" `Quick
            test_escaping_exceptions;
        ]
      );
    ]
