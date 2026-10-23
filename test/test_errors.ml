let check_parseff_error exn =
  let msg = Printexc.to_string exn in
  Alcotest.(check bool)
    "starts with Parseff:" true
    (String.length msg >= 7 && String.sub msg 0 7 = "Parseff");
  Alcotest.(check bool)
    "mentions parse context" true
    (Re.(execp (compile (str "parse context"))) msg)

let test_fold_left_user_exception () =
  let op () =
    ignore (Parseff.char '+');
    fun a b ->
      if a = 2 then
        raise Exit
      else
        a + b
  in
  ( match Parseff.parse "1+1+1" (Parseff.fold_left Parseff.digit op) with
  | _ ->
      Alcotest.fail "expected Exit"
  | exception Exit ->
      ()
  | exception exn ->
      Alcotest.failf "wrong exception: %s" (Printexc.to_string exn)
  );
  match Parseff.position () with
  | _ ->
      Alcotest.fail "expected failure outside parse"
  | exception _ ->
      ()

let test_unhandled_effect_digit () =
  match Parseff.digit () with
  | _ ->
      Alcotest.fail "Expected unhandled effect exception"
  | exception exn ->
      check_parseff_error exn

let test_unhandled_effect_consume () =
  match Parseff.consume "hello" with
  | _ ->
      Alcotest.fail "Expected unhandled effect exception"
  | exception exn ->
      check_parseff_error exn

let test_unhandled_effect_position () =
  match Parseff.position () with
  | _ ->
      Alcotest.fail "Expected unhandled effect exception"
  | exception exn ->
      check_parseff_error exn

let test_unhandled_effect_many () =
  match Parseff.many Parseff.digit () with
  | _ ->
      Alcotest.fail "Expected unhandled effect exception"
  | exception exn ->
      check_parseff_error exn

let test_unhandled_effect_satisfy () =
  match Parseff.satisfy (fun c -> c >= '0' && c <= '9') ~label:"digit" with
  | _ ->
      Alcotest.fail "Expected unhandled effect exception"
  | exception exn ->
      check_parseff_error exn

let test_unhandled_effect_end_of_input () =
  match Parseff.end_of_input () with
  | _ ->
      Alcotest.fail "Expected unhandled effect exception"
  | exception exn ->
      check_parseff_error exn

let test_unhandled_effect_message_content () =
  match Parseff.digit () with
  | _ ->
      Alcotest.fail "Expected unhandled effect exception"
  | exception exn ->
      let msg = Printexc.to_string exn in
      Alcotest.(check bool)
        "mentions Parseff.parse" true
        (Re.(execp (compile (str "Parseff.parse"))) msg);
      Alcotest.(check bool)
        "mentions parse_until_end" true
        (Re.(execp (compile (str "Parseff.parse_until_end"))) msg);
      Alcotest.(check bool)
        "mentions parse_source" true
        (Re.(execp (compile (str "Parseff.parse_source"))) msg)

let check_outside_parse () =
  match Parseff.position () with
  | _ ->
      Alcotest.fail "Expected Not_in_parse_context"
  | exception exn ->
      check_parseff_error exn

let test_escaping_exception_leaves_context () =
  ( match
      Parseff.parse "abc" (fun () ->
          ignore (Parseff.char 'a');
          failwith "boom"
      )
    with
  | _ ->
      Alcotest.fail "Expected Failure"
  | exception Failure _ ->
      ()
  );
  check_outside_parse ()

let test_escaping_exception_leaves_context_source () =
  let src = Parseff.Source.of_string "abc" in
  ( match
      Parseff.parse_source src (fun () ->
          ignore (Parseff.char 'a');
          failwith "boom"
      )
    with
  | _ ->
      Alcotest.fail "Expected Failure"
  | exception Failure _ ->
      ()
  );
  check_outside_parse ()

let test_nested_parse_exception_keeps_outer_state () =
  match
    Parseff.parse "abcdef" (fun () ->
        ignore (Parseff.char 'a');
        ( try
            ignore
              (Parseff.parse "zzzzzz" (fun () ->
                   ignore (Parseff.take 4);
                   failwith "x"
               )
              )
          with Failure _ -> ()
        );
        Parseff.position ()
    )
  with
  | Ok pos ->
      Alcotest.(check int) "outer position" 1 pos
  | Error _ ->
      Alcotest.fail "Expected success"

let test_nested_parse_source_exception_keeps_outer_state () =
  match
    Parseff.parse "abcdef" (fun () ->
        ignore (Parseff.char 'a');
        ( try
            ignore
              (Parseff.parse_source (Parseff.Source.of_string "zzzzzz")
                 (fun () ->
                   ignore (Parseff.take 4);
                   failwith "x"
               )
              )
          with Failure _ -> ()
        );
        Parseff.position ()
    )
  with
  | Ok pos ->
      Alcotest.(check int) "outer position" 1 pos
  | Error _ ->
      Alcotest.fail "Expected success"

let test_or_right_commit_stops_at_or () =
  match
    Parseff.parse "z" (fun () ->
        Parseff.or_
          (fun () ->
            ignore (Parseff.consume "x");
            "x"
          )
          (fun () ->
            Parseff.commit ();
            ignore (Parseff.char 'y');
            "y"
          )
          ()
    )
  with
  | Ok _ ->
      Alcotest.fail "Expected failure"
  | Error { pos; error = `Expected msg } ->
      Alcotest.(check int) "error position" 0 pos;
      Alcotest.(check string) "right error only" "expected 'y'" msg
  | Error _ ->
      Alcotest.fail "Unexpected error type"

let test_or_right_commit_does_not_seal_many () =
  match
    Parseff.parse "abd" (fun () ->
        let items =
          Parseff.many
            (fun () ->
              Parseff.or_
                (fun () -> Parseff.consume "a")
                (fun () ->
                  ignore (Parseff.char 'b');
                  Parseff.commit ();
                  ignore (Parseff.char 'c');
                  "bc"
                )
                ()
            )
            ()
        in
        String.concat "|" items ^ "|" ^ Parseff.consume "bd"
    )
  with
  | Ok s ->
      Alcotest.(check string) "result" "a|bd" s
  | Error _ ->
      Alcotest.fail "Expected success"

let () =
  let open Alcotest in
  run "Errors"
    [
      ( "context restoration",
        [
          test_case "exception leaves context" `Quick
            test_escaping_exception_leaves_context;
          test_case "exception leaves context (source)" `Quick
            test_escaping_exception_leaves_context_source;
          test_case "nested exception keeps outer state" `Quick
            test_nested_parse_exception_keeps_outer_state;
          test_case "nested source exception keeps outer state" `Quick
            test_nested_parse_source_exception_keeps_outer_state;
          test_case "or_ right commit stops at or_" `Quick
            test_or_right_commit_stops_at_or;
          test_case "or_ right commit does not seal many" `Quick
            test_or_right_commit_does_not_seal_many;
        ]
      );
      ( "fold exceptions",
        [
          test_case "fold_left user exception propagates" `Quick
            test_fold_left_user_exception;
        ]
      );
      ( "unhandled effect message",
        [
          test_case "digit outside parse" `Quick test_unhandled_effect_digit;
          test_case "consume outside parse" `Quick test_unhandled_effect_consume;
          test_case "position outside parse" `Quick
            test_unhandled_effect_position;
          test_case "many outside parse" `Quick test_unhandled_effect_many;
          test_case "satisfy outside parse" `Quick test_unhandled_effect_satisfy;
          test_case "end_of_input outside parse" `Quick
            test_unhandled_effect_end_of_input;
          test_case "message mentions runner functions" `Quick
            test_unhandled_effect_message_content;
        ]
      );
    ]
