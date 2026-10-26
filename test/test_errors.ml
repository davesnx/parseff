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

let show_error = function
  | `Expected s ->
      Printf.sprintf "Expected %S" s
  | `Unexpected_end_of_input ->
      "Unexpected_end_of_input"
  | `Failure s ->
      Printf.sprintf "Failure %S" s
  | `E ->
      "E"
  | `Other ->
      "Other"
  | _ ->
      "other"

let show_result show_v = function
  | Parseff.Ok v ->
      "Ok " ^ show_v v
  | Parseff.Error { pos; error } ->
      Printf.sprintf "Error@%d %s" pos (show_error error)

let show_unit () = "()"
let ws () = Parseff.skip_whitespace ()

let comma () =
  ws ();
  ignore (Parseff.char ',')

let rec json_value () =
  Parseff.rec_ (fun () ->
      ws ();
      Parseff.expect "value" (fun () ->
          Parseff.one_of
            [
              (fun () -> ignore (Parseff.consume "null"));
              json_array;
              json_object;
              (fun () ->
                ignore
                  (Parseff.take_while ~at_least:1 ~label:"number" (fun c ->
                       c >= '0' && c <= '9'
                   )
                  )
              );
            ]
            ()
      )
  )

and json_array () =
  ignore (Parseff.char '[');
  ws ();
  ignore (Parseff.sep_by json_value comma ());
  ws ();
  ignore (Parseff.char ']')

and json_object () =
  ignore (Parseff.char '{');
  ws ();
  ignore (Parseff.sep_by json_member comma ());
  ws ();
  ignore (Parseff.char '}')

and json_member () =
  ignore (Parseff.char '"');
  ignore (Parseff.take_while (fun c -> c <> '"'));
  ignore (Parseff.char '"');
  ws ();
  ignore (Parseff.char ':');
  json_value ()

let both_runners show_v input parser expected =
  Alcotest.(check string)
    "parse" expected
    (show_result show_v (Parseff.parse input parser));
  Alcotest.(check string)
    "parse_source" expected
    (show_result show_v
       (Parseff.parse_source (Parseff.Source.of_string input) parser)
    )

let test_trailing_comma_reports_value () =
  both_runners show_unit "[1, 2, 3,]" json_value
    {|Error@9 Expected "expected value"|}

let test_missing_colon () =
  both_runners show_unit {|{"a" 1}|} json_value
    {|Error@5 Expected "expected ':'"|}

let test_unclosed_array_end_of_input () =
  both_runners show_unit "[1, 2" json_value "Error@5 Unexpected_end_of_input"

let test_loop_exit_failure_is_furthest () =
  let parser () =
    ignore (Parseff.sep_by (fun () -> Parseff.consume "ab") comma ());
    Parseff.end_of_input ()
  in
  both_runners show_unit "ab,ab,ax" parser {|Error@7 Expected "expected \"b\""|}

let test_loop_exit_ties_with_next_item () =
  let parser () =
    ignore (Parseff.many Parseff.digit ());
    Parseff.end_of_input ()
  in
  both_runners show_unit "12x" parser
    {|Error@2 Expected "expected digit or end of input"|}

let test_duplicate_labels_once () =
  let parser () =
    ignore (Parseff.or_ Parseff.digit Parseff.digit ());
    ()
  in
  both_runners show_unit "x" parser {|Error@0 Expected "expected digit"|}

let test_alternatives_sorted () =
  let keyword k () = ignore (Parseff.consume k) in
  let order_a = Parseff.one_of [ keyword "if"; keyword "else"; keyword "while" ]
  and order_b =
    Parseff.one_of [ keyword "while"; keyword "if"; keyword "else" ]
  in
  let expected =
    {|Error@0 Expected "expected \"else\", \"if\", or \"while\""|}
  in
  both_runners show_unit "xyz" order_a expected;
  both_runners show_unit "xyz" order_b expected

let test_literal_suffix_after_matched_prefix () =
  both_runners show_unit "ax"
    (fun () -> ignore (Parseff.consume "ab"))
    {|Error@1 Expected "expected \"b\""|};
  both_runners show_unit "hel"
    (fun () -> ignore (Parseff.consume "hello"))
    "Error@3 Unexpected_end_of_input"

let test_expect_relabels_at_start_only () =
  let parser () =
    Parseff.expect "key" (fun () ->
        ignore (Parseff.consume "id:");
        ignore (Parseff.digit ())
    )
  in
  both_runners show_unit "foo" parser {|Error@0 Expected "expected key"|};
  both_runners show_unit "id:x" parser {|Error@3 Expected "expected digit"|}

let test_one_of_labeled_relabels_at_start_only () =
  let number () = ignore (Parseff.take_while ~at_least:1 (fun c -> c = '1')) in
  let string_ () =
    ignore (Parseff.char '"');
    ignore (Parseff.take_while (fun c -> c <> '"'));
    ignore (Parseff.char '"')
  in
  let parser =
    Parseff.one_of_labeled [ ("number", number); ("string", string_) ]
  in
  both_runners show_unit "?" parser
    {|Error@0 Expected "expected number or string"|};
  both_runners show_unit {|"abc|} parser "Error@4 Unexpected_end_of_input"

let test_commit_forgets_earlier_alternative () =
  let parser () =
    Parseff.or_
      (fun () -> ignore (Parseff.consume "ab"))
      (fun () ->
        ignore (Parseff.consume "a");
        Parseff.commit ();
        ignore (Parseff.consume "z")
      )
      ()
  in
  both_runners show_unit "ax" parser {|Error@1 Expected "expected \"z\""|}

let test_fail_escapes_or_many_expect () =
  let boom () = Parseff.fail "boom" in
  both_runners show_unit "x"
    (fun () -> Parseff.or_ boom (fun () -> ()) ())
    {|Error@0 Failure "boom"|};
  both_runners show_unit "x"
    (fun () -> ignore (Parseff.many boom ()))
    {|Error@0 Failure "boom"|};
  both_runners show_unit "x"
    (fun () -> Parseff.expect "thing" boom)
    {|Error@0 Failure "boom"|};
  both_runners show_unit "x"
    (fun () -> ignore (Parseff.one_of_labeled [ ("a", boom) ] ()))
    {|Error@0 Failure "boom"|}

let test_user_error_absorbed_by_or () =
  both_runners
    (fun s -> s)
    "x"
    (fun () ->
      Parseff.or_
        (fun () -> Parseff.error `E)
        (fun () -> Parseff.consume "x")
        ()
    )
    {|Ok x|}

let test_user_error_reported_when_furthest () =
  let parser () =
    Parseff.or_
      (fun () ->
        ignore (Parseff.consume "ab");
        Parseff.error `E
      )
      (fun () -> Parseff.consume "q")
      ()
  in
  both_runners (fun s -> s) "abc" parser "Error@2 E"

let test_user_error_beats_expected_at_tie () =
  let parser () =
    Parseff.or_ (fun () -> Parseff.error `E) (fun () -> Parseff.consume "q") ()
  in
  both_runners (fun s -> s) "z" parser "Error@0 E"

let test_behind_failure_ignored () =
  let parser () =
    Parseff.or_
      (fun () ->
        ignore (Parseff.consume "abc");
        ignore (Parseff.consume "d");
        "left"
      )
      (fun () -> Parseff.consume "x")
      ()
  in
  both_runners (fun s -> s) "abq" parser {|Error@2 Expected "expected \"c\""|}

let test_item_buffer_overflow () =
  let alternatives =
    List.init 100 (fun i () ->
        ignore (Parseff.consume (Printf.sprintf "item%03d" i))
    )
  in
  match Parseff.parse "zzz" (Parseff.one_of alternatives) with
  | Parseff.Error { pos = 0; error = `Expected msg } ->
      let items = List.length (String.split_on_char ',' msg) in
      Alcotest.(check bool) "at most 64 items" true (items <= 64);
      Alcotest.(check bool) "at least 60 items" true (items >= 60)
  | _ ->
      Alcotest.fail "Expected an Expected error at 0"

let test_item_buffer_dedupes_on_overflow () =
  let alternatives =
    List.init 200 (fun i () ->
        ignore (Parseff.consume (Printf.sprintf "item%d" (i mod 3)))
    )
  in
  match Parseff.parse "zzz" (Parseff.one_of alternatives) with
  | Parseff.Error { pos = 0; error = `Expected msg } ->
      Alcotest.(check string)
        "three items" {|expected "item0", "item1", or "item2"|} msg
  | _ ->
      Alcotest.fail "Expected an Expected error at 0"

let test_utf8_char_does_not_format_labels () =
  let u = Uchar.of_int 0x61 in
  let words = ref 0. in
  match
    Parseff.parse (String.make 1000 'a') (fun () ->
        let before = Gc.minor_words () in
        for _ = 1 to 1000 do
          ignore (Parseff.Utf8.char u)
        done;
        words := Gc.minor_words () -. before
    )
  with
  | Parseff.Ok () ->
      Alcotest.(check bool) "no formatting per call" true (!words /. 1000. < 20.)
  | Parseff.Error _ ->
      Alcotest.fail "Expected success"

let test_utf8_char_error_text () =
  both_runners show_unit "b"
    (fun () -> ignore (Parseff.Utf8.char (Uchar.of_int 0x61)))
    {|Error@0 Expected "expected 'a'"|};
  both_runners show_unit "b"
    (fun () -> ignore (Parseff.Utf8.char (Uchar.of_int 0xE9)))
    {|Error@0 Expected "expected '\195\169'"|}

let test_recorded_failures_do_not_fail_success () =
  let parser () =
    ignore
      (Parseff.optional
         (fun () -> Parseff.look_ahead (fun () -> Parseff.consume "ab"))
         ()
      );
    ignore (Parseff.consume "a")
  in
  both_runners show_unit "ax" parser "Ok ()"

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
      ( "furthest failure",
        [
          test_case "trailing comma reports value" `Quick
            test_trailing_comma_reports_value;
          test_case "missing colon" `Quick test_missing_colon;
          test_case "unclosed array at end of input" `Quick
            test_unclosed_array_end_of_input;
          test_case "loop exit failure is furthest" `Quick
            test_loop_exit_failure_is_furthest;
          test_case "loop exit ties with next item" `Quick
            test_loop_exit_ties_with_next_item;
          test_case "duplicate labels once" `Quick test_duplicate_labels_once;
          test_case "alternatives sorted" `Quick test_alternatives_sorted;
          test_case "literal suffix after matched prefix" `Quick
            test_literal_suffix_after_matched_prefix;
          test_case "expect relabels at start only" `Quick
            test_expect_relabels_at_start_only;
          test_case "one_of_labeled relabels at start only" `Quick
            test_one_of_labeled_relabels_at_start_only;
          test_case "commit forgets earlier alternative" `Quick
            test_commit_forgets_earlier_alternative;
          test_case "fail escapes or_, many, expect" `Quick
            test_fail_escapes_or_many_expect;
          test_case "user error absorbed by or_" `Quick
            test_user_error_absorbed_by_or;
          test_case "user error reported when furthest" `Quick
            test_user_error_reported_when_furthest;
          test_case "user error beats expected at tie" `Quick
            test_user_error_beats_expected_at_tie;
          test_case "behind failure ignored" `Quick test_behind_failure_ignored;
          test_case "item buffer overflow" `Quick test_item_buffer_overflow;
          test_case "item buffer dedupes on overflow" `Quick
            test_item_buffer_dedupes_on_overflow;
          test_case "Utf8.char does not format labels" `Quick
            test_utf8_char_does_not_format_labels;
          test_case "Utf8.char error text" `Quick test_utf8_char_error_text;
          test_case "recorded failures do not fail a success" `Quick
            test_recorded_failures_do_not_fail_success;
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
