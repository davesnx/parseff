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

module E = Parseff.Internal.Error

let frames_of input parser =
  match Parseff.Internal.parse input parser with
  | Stdlib.Ok _ ->
      "ok"
  | Stdlib.Error (e : unit E.t) ->
      String.concat "; "
        (List.map
           (fun (f : E.frame) ->
             Printf.sprintf "%s@%d%s" f.name f.start
               ( if f.delimited then
                   "!"
                 else
                   ""
               )
           )
           e.frames
        )

let ctx_array_parser () =
  let rec value () =
    Parseff.rec_ (fun () ->
        ws ();
        Parseff.expect "value" (fun () ->
            Parseff.one_of
              [
                array;
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
  and array () =
    Parseff.context ~delimited:true "array" (fun () ->
        ignore (Parseff.char '[');
        ws ();
        ignore (Parseff.sep_by value comma ());
        ws ();
        ignore (Parseff.char ']')
    )
  in
  value ()

let test_context_chain_at_advance () =
  Alcotest.(check string)
    "frames that start at the failure are dropped" "array@0!"
    (frames_of "[1, 2, 3,]" ctx_array_parser);
  Alcotest.(check string)
    "nested frames" "array@0!; array@1!"
    (frames_of "[[1,]]" ctx_array_parser)

let test_context_chain_without_context () =
  Alcotest.(check string)
    "no frames" ""
    (frames_of "x" (fun () -> ignore (Parseff.char 'a')))

let test_context_chain_after_frame_closes () =
  let parser () =
    Parseff.context "outer" (fun () ->
        ignore
          (Parseff.optional
             (fun () ->
               Parseff.context "inner" (fun () ->
                   ignore (Parseff.consume "ab");
                   ignore (Parseff.consume "c")
               )
             )
             ()
          );
        ignore (Parseff.consume "ax")
    )
  in
  Alcotest.(check string)
    "chain copied when inner closed" "outer@0; inner@0" (frames_of "abx" parser);
  match Parseff.parse "abx" parser with
  | Parseff.Error { pos = 2; error = `Expected "expected \"c\"" } ->
      ()
  | _ ->
      Alcotest.fail "unexpected error"

let test_context_start_offsets () =
  let parser () =
    ignore (Parseff.consume "ab");
    Parseff.context "second" (fun () ->
        ignore (Parseff.consume "cd");
        Parseff.context "third" ~delimited:true (fun () ->
            ignore (Parseff.consume "!")
        )
    )
  in
  Alcotest.(check string) "offsets" "second@2" (frames_of "abcdx" parser)

let test_context_siblings_do_not_leak () =
  let parser () =
    Parseff.context "a" (fun () -> ignore (Parseff.consume "x"));
    Parseff.context "b" (fun () -> ignore (Parseff.consume "yy"))
  in
  Alcotest.(check string) "only the open context" "b@1" (frames_of "xyz" parser)

let test_context_tie_keeps_first_chain () =
  let parser () =
    Parseff.or_
      (fun () ->
        Parseff.context "left" (fun () ->
            ignore (Parseff.char 'a');
            ignore (Parseff.char 'q')
        )
      )
      (fun () ->
        Parseff.context "right" (fun () ->
            ignore (Parseff.char 'a');
            ignore (Parseff.char 'r')
        )
      )
      ()
  in
  Alcotest.(check string) "first group's chain" "left@0" (frames_of "az" parser)

let test_context_after_user_exception () =
  let parser () =
    ( try
        Parseff.context "broken" (fun () ->
            ignore (Parseff.char 'a');
            raise Exit
        )
      with Exit -> ()
    );
    Parseff.context "fine" (fun () -> ignore (Parseff.consume "xz"))
  in
  Alcotest.(check string) "stack is consistent" "fine@1" (frames_of "axy" parser)

let test_context_grows_deep () =
  let rec nest n () =
    ignore (Parseff.char 'a');
    if n = 0 then
      ignore (Parseff.char 'x')
    else
      Parseff.context (string_of_int n) (nest (n - 1))
  in
  let frames = frames_of (String.make 42 'a') (nest 40) in
  Alcotest.(check bool)
    "forty frames" true
    (String.length frames > 0
    && List.length (String.split_on_char ';' frames) = 40
    )

let test_context_does_not_change_result () =
  both_runners show_unit "[1, 2, 3,]" ctx_array_parser
    {|Error@9 Expected "expected value"|}

let let_statement_parser () =
  let ident () =
    ignore
      (Parseff.take_while ~at_least:1 ~label:"identifier" (fun c ->
           c >= 'a' && c <= 'z'
       )
      )
  in
  let expr () =
    Parseff.expect "expression" (fun () ->
        Parseff.one_of
          [
            (fun () ->
              ignore
                (Parseff.take_while ~at_least:1 (fun c -> c >= '0' && c <= '9'))
            );
            ident;
          ]
          ()
    )
  in
  let let_stmt () =
    ignore (Parseff.consume "let");
    Parseff.cut ();
    ws ();
    ident ();
    ws ();
    ignore (Parseff.char '=');
    ws ();
    expr ();
    ignore (Parseff.char ';')
  in
  let expr_stmt () =
    expr ();
    ignore (Parseff.char ';')
  in
  let stmt () =
    Parseff.context "statement" (fun () -> Parseff.or_ let_stmt expr_stmt ())
  in
  let stmts =
    Parseff.many
      (fun () ->
        ws ();
        stmt ()
      )
      ()
  in
  List.length stmts

let test_cut_seals_whole_construct () =
  both_runners string_of_int "let x = ;" let_statement_parser
    {|Error@8 Expected "expected expression"|};
  both_runners string_of_int "let x = 1; 2;" let_statement_parser "Ok 2";
  Alcotest.(check string)
    "frames" "statement@0"
    (frames_of "let x = ;" (fun () -> ignore (let_statement_parser ())))

let test_cut_without_context () =
  let parser () =
    Parseff.or_
      (fun () ->
        ignore (Parseff.consume "a");
        Parseff.cut ();
        Parseff.consume "b"
      )
      (fun () -> Parseff.consume "ac")
      ()
  in
  both_runners (fun s -> s) "ac" parser {|Error@1 Expected "expected \"b\""|};
  both_runners (fun s -> s) "ab" parser "Ok b"

let test_cut_stops_at_look_ahead () =
  let parser () =
    Parseff.or_
      (fun () ->
        Parseff.look_ahead (fun () ->
            ignore (Parseff.consume "a");
            Parseff.cut ();
            Parseff.consume "b"
        )
      )
      (fun () -> Parseff.consume "ac")
      ()
  in
  both_runners (fun s -> s) "ac" parser "Ok ac"

let test_cut_does_not_leave_context_on_success () =
  let parser () =
    Parseff.or_
      (fun () ->
        Parseff.context "c" (fun () ->
            ignore (Parseff.consume "a");
            Parseff.cut ()
        );
        Parseff.consume "b"
      )
      (fun () -> Parseff.consume "ac")
      ()
  in
  both_runners (fun s -> s) "ac" parser "Ok ac"

let test_cut_seals_outer_frames_only_on_failure () =
  let item () =
    Parseff.context "item" (fun () ->
        ignore (Parseff.consume "k");
        Parseff.cut ();
        ignore (Parseff.consume "v")
    )
  in
  let parser () = List.length (Parseff.many item ()) in
  both_runners string_of_int "kvkv" parser "Ok 2";
  both_runners string_of_int "kvkx" parser {|Error@3 Expected "expected \"v\""|}

let test_cut_inside_nested_context_keeps_outer_open () =
  let parser () =
    Parseff.or_
      (fun () ->
        Parseff.context "outer" (fun () ->
            ignore (Parseff.consume "a");
            Parseff.context "inner" (fun () ->
                ignore (Parseff.consume "b");
                Parseff.cut ();
                ignore (Parseff.consume "c")
            )
        )
      )
      (fun () -> ignore (Parseff.consume "abd"))
      ()
  in
  both_runners
    (fun _ -> "ok")
    "abd" parser {|Error@2 Expected "expected \"c\""|}

let lines l = String.concat "\n" l

let render ?(name = "input.json") ?source ?columns input parser =
  match Parseff.Internal.parse_until_end input parser with
  | Stdlib.Ok _ ->
      "ok"
  | Stdlib.Error (e : unit E.t) ->
      E.render ~name ?source ?columns e

let check_render label expected actual =
  Alcotest.(check string) label expected actual

let ctx_json_parser =
  let ws () = Parseff.skip_whitespace () in
  let comma () =
    ws ();
    ignore (Parseff.char ',')
  in
  let rec value () =
    Parseff.rec_ (fun () ->
        ws ();
        Parseff.expect "value" (fun () ->
            Parseff.one_of
              [
                (fun () -> ignore (Parseff.consume "null"));
                array;
                object_;
                string_;
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
  and array () =
    Parseff.context ~delimited:true "array" (fun () ->
        ignore (Parseff.char '[');
        ws ();
        ignore (Parseff.sep_by value comma ());
        ws ();
        ignore (Parseff.char ']')
    )
  and object_ () =
    Parseff.context ~delimited:true "object" (fun () ->
        ignore (Parseff.char '{');
        ws ();
        ignore (Parseff.sep_by member comma ());
        ws ();
        ignore (Parseff.char '}')
    )
  and member () =
    string_ ();
    ws ();
    ignore (Parseff.char ':');
    ws ();
    value ()
  and string_ () =
    ignore (Parseff.char '"');
    ignore (Parseff.take_while (fun c -> c <> '"'));
    ignore (Parseff.char '"')
  in
  value

let test_render_trailing_comma () =
  check_render "A"
    (lines
       [
         "input.json:1:10: error: expected value, found ']'";
         "1 | [1, 2, 3,]";
         "  |          ^";
         "  = while parsing array, from 1:1";
       ]
    )
    (render "[1, 2, 3,]" ctx_json_parser)

let test_render_missing_colon () =
  check_render "B"
    (lines
       [
         "input.json:1:6: error: expected ':', found '1'";
         "1 | {\"a\" 1}";
         "  |      ^";
         "  = while parsing object, from 1:1";
       ]
    )
    (render {|{"a" 1}|} ctx_json_parser)

let test_render_missing_comma_opened_here () =
  check_render "C"
    (lines
       [
         "input.json:3:3: error: expected ',' or '}', found '\"'";
         "1 | {";
         "  | - opened here";
         "2 |   \"a\": 1";
         "3 |   \"b\": 2";
         "  |   ^";
         "  = while parsing object, from 1:1";
       ]
    )
    (render "{\n  \"a\": 1\n  \"b\": 2\n}" ctx_json_parser)

let test_render_loop_exit_literal () =
  let parser () =
    ignore (Parseff.sep_by (fun () -> ignore (Parseff.consume "ab")) comma ())
  in
  check_render "D"
    (lines
       [
         "input.json:1:8: error: expected \"b\", found 'x'";
         "1 | ab,ab,ax";
         "  |        ^";
       ]
    )
    (render "ab,ab,ax" (fun () ->
         parser ();
         Parseff.end_of_input ()
     )
    )

let test_render_duplicates_and_sorting () =
  check_render "E digit"
    (lines
       [ "input.json:1:1: error: expected digit, found 'x'"; "1 | x"; "  | ^" ]
    )
    (render "x" (fun () -> ignore (Parseff.or_ Parseff.digit Parseff.digit ())));
  let keyword k () = ignore (Parseff.consume k) in
  let expected =
    lines
      [
        "input.json:1:1: error: expected \"else\", \"if\", or \"while\", found \
         'xyz'";
        "1 | xyz";
        "  | ^^^";
      ]
  in
  check_render "E keywords" expected
    (render "xyz"
       (Parseff.one_of [ keyword "if"; keyword "else"; keyword "while" ])
    );
  check_render "E keywords reordered" expected
    (render "xyz"
       (Parseff.one_of [ keyword "while"; keyword "if"; keyword "else" ])
    )

let test_render_relabel_at_start_only () =
  let key () =
    Parseff.expect "key" (fun () ->
        ignore (Parseff.consume "id:");
        ignore (Parseff.digit ())
    )
  in
  check_render "F start"
    (lines
       [
         "input.json:1:1: error: expected key, found 'foo'";
         "1 | foo";
         "  | ^^^";
       ]
    )
    (render "foo" key);
  check_render "F deeper"
    (lines
       [
         "input.json:1:4: error: expected digit, found 'x'";
         "1 | id:x";
         "  |    ^";
       ]
    )
    (render "id:x" key);
  let literal () =
    ignore
      (Parseff.one_of_labeled
         [
           ( "number",
             fun () ->
               ignore
                 (Parseff.take_while ~at_least:1 (fun c -> c >= '0' && c <= '9'))
           );
           ( "string",
             fun () ->
               ignore (Parseff.char '"');
               ignore (Parseff.take_while (fun c -> c <> '"'));
               ignore (Parseff.char '"')
           );
         ]
         ()
      )
  in
  check_render "F labeled"
    (lines
       [
         "input.json:1:1: error: expected number or string, found '?'";
         "1 | ?";
         "  | ^";
       ]
    )
    (render "?" literal);
  check_render "F unterminated string"
    (lines
       [
         "input.json:1:5: error: expected '\"', found end of input";
         "1 | \"abc";
         "  |     ^";
       ]
    )
    (render "\"abc" literal)

let test_render_end_of_input () =
  check_render "G array"
    (lines
       [
         "input.json:1:6: error: expected ',' or ']', found end of input";
         "1 | [1, 2";
         "  |      ^";
         "  = while parsing array, from 1:1";
       ]
    )
    (render "[1, 2" ctx_json_parser);
  check_render "G literal"
    (lines
       [
         "input.json:1:4: error: expected \"lo\", found end of input";
         "1 | hel";
         "  |    ^";
       ]
    )
    (render "hel" (fun () -> ignore (Parseff.consume "hello")))

let test_render_spans_and_columns () =
  check_render "H identifier run"
    (lines
       [
         "input.json:1:7: error: expected ',' or ']', found 'foo'";
         "1 | [1, 2 foo]";
         "  |       ^^^";
         "  = while parsing array, from 1:1";
       ]
    )
    (render "[1, 2 foo]" ctx_json_parser);
  let input = "[\"\xC3\xA9\" \"b\"]" in
  let body =
    [
      "1 | [\"\xC3\xA9\" \"b\"]";
      "  |      ^";
      "  = while parsing array, from 1:1";
    ]
  in
  check_render "H bytes"
    (lines ("input.json:1:7: error: expected ',' or ']', found '\"'" :: body))
    (render input ctx_json_parser);
  check_render "H code points"
    (lines ("input.json:1:6: error: expected ',' or ']', found '\"'" :: body))
    (render ~columns:`Code_points input ctx_json_parser)

let let_stmt_parser () = ignore (let_statement_parser ())

let test_render_cut_statement () =
  check_render "I"
    (lines
       [
         "input.json:1:9: error: expected expression, found ';'";
         "1 | let x = ;";
         "  |         ^";
         "  = while parsing statement, from 1:1";
       ]
    )
    (render "let x = ;" let_stmt_parser)

let test_render_without_name () =
  match Parseff.Internal.parse "x" (fun () -> ignore (Parseff.digit ())) with
  | Stdlib.Error (e : unit E.t) ->
      Alcotest.(check string)
        "no name"
        (lines [ "1:1: error: expected digit, found 'x'"; "1 | x"; "  | ^" ])
        (E.render e);
      Alcotest.(check string) "message" "expected digit, found 'x'" (E.message e);
      Alcotest.(check int) "pos" 0 (E.pos e)
  | Stdlib.Ok _ ->
      Alcotest.fail "expected an error"

let test_render_tabs () =
  check_render "tabs"
    (lines
       [
         "input.json:1:7: error: expected ',' or ']', found '3'";
         "1 | [1,\t2\t3]";
         "  |    \t \t^";
         "  = while parsing array, from 1:1";
       ]
    )
    (render "[1,\t2\t3]" ctx_json_parser)

let test_render_crlf () =
  check_render "crlf"
    (lines
       [
         "input.json:3:3: error: expected ',' or '}', found '\"'";
         "1 | {";
         "  | - opened here";
         "2 |   \"a\": 1";
         "3 |   \"b\": 2";
         "  |   ^";
         "  = while parsing object, from 1:1";
       ]
    )
    (render "{\r\n  \"a\": 1\r\n  \"b\": 2\r\n}" ctx_json_parser)

let test_render_empty_line () =
  check_render "empty line before"
    (lines
       [
         "input.json:3:3: error: expected ',' or ']', found '2'";
         "1 | [";
         "  | - opened here";
         "2 | ";
         "3 | 1 2]";
         "  |   ^";
         "  = while parsing array, from 1:1";
       ]
    )
    (render "[\n\n1 2]" ctx_json_parser)

let test_render_eof_at_line_end () =
  check_render "eof after newline"
    (lines
       [
         "input.json:2:1: error: expected value, found end of input";
         "1 | [1,";
         "  | - opened here";
         "2 | ";
         "  | ^";
         "  = while parsing array, from 1:1";
       ]
    )
    (render "[1,\n" ctx_json_parser);
  check_render "empty input"
    (lines
       [
         "input.json:1:1: error: expected value, found end of input";
         "1 | ";
         "  | ^";
       ]
    )
    (render "" ctx_json_parser)

let test_render_gap () =
  let many_lines = String.concat "" (List.init 40 (fun _ -> "1,\n")) in
  check_render "many hidden lines"
    (lines
       [
         "input.json:42:1: error: expected value, found 'x'";
         " 1 | [";
         "   | - opened here";
         "...";
         "41 | 1,";
         "42 | x]";
         "   | ^";
         "   = while parsing array, from 1:1";
       ]
    )
    (render ("[\n" ^ many_lines ^ "x]") ctx_json_parser);
  check_render "exactly one hidden line"
    (lines
       [
         "input.json:4:2: error: expected value, found 'x'";
         "1 | [";
         "  | - opened here";
         "2 | 1,";
         "3 | 2,";
         "4 |  x]";
         "  |  ^";
         "  = while parsing array, from 1:1";
       ]
    )
    (render "[\n1,\n2,\n x]" ctx_json_parser)

let test_render_frame_cap () =
  let rec nest n () =
    ignore (Parseff.char 'a');
    if n = 0 then
      ignore (Parseff.char 'x')
    else
      Parseff.context (Printf.sprintf "level%d" n) (nest (n - 1))
  in
  check_render "five frames"
    (lines
       [
         "input.json:1:8: error: expected 'x', found end of input";
         "1 | aaaaaaa";
         "  |        ^";
         "  = while parsing level1, from 1:7";
         "  = while parsing level2, from 1:6";
         "  = while parsing level3, from 1:5";
         "  = while parsing level4, from 1:4";
         "  = while parsing level5, from 1:3";
         "  = ... and 1 more";
       ]
    )
    (render (String.make 7 'a') (nest 6))

let test_render_invalid_utf8_and_controls () =
  check_render "invalid byte"
    (lines
       [
         "input.json:1:5: error: expected value, found '\\xFF'";
         "1 | [1, ? 2]";
         "  |     ^";
         "  = while parsing array, from 1:1";
       ]
    )
    (render "[1, \xFF 2]" ctx_json_parser);
  check_render "control byte"
    (lines
       [
         "input.json:1:3: error: expected ',' or ']', found '\\x01'";
         "1 | [1?]";
         "  |   ^";
         "  = while parsing array, from 1:1";
       ]
    )
    (render "[1\x01]" ctx_json_parser)

let test_render_window_cut () =
  let input = "[" ^ String.concat ", " (List.init 200 string_of_int) ^ " x]" in
  let out = render input ctx_json_parser in
  match String.split_on_char '\n' out with
  | _ :: row :: caret :: _ ->
      Alcotest.(check bool)
        "row is cut on the left" true
        (String.length row > 7 && String.sub row 0 7 = "1 | ...");
      Alcotest.(check bool)
        "row is at most the window plus markers" true
        (String.length row <= 4 + 3 + 240 + 3);
      let at s c = String.rindex s c in
      Alcotest.(check int) "caret under the error" (at row 'x') (at caret '^')
  | _ ->
      Alcotest.fail "unexpected rendering"

let test_render_source_and_pp () =
  let input = "{\n  \"a\": 1\n  \"b\": 2\n}" in
  match Parseff.Internal.parse input ctx_json_parser with
  | Stdlib.Error (e : unit E.t) ->
      Alcotest.(check string)
        "source gives the same text"
        (E.render ~name:"input.json" e)
        (E.render ~name:"input.json" ~source:input e);
      Alcotest.(check string)
        "pp is render"
        (E.render ~name:"input.json" e)
        (Format.asprintf "%a"
           (E.pp ~name:"input.json" ?source:None ?columns:None ?user:None)
           e
        )
  | Stdlib.Ok _ ->
      Alcotest.fail "expected an error"

let test_render_other_kinds () =
  ( match Parseff.Internal.parse "abc" (fun () -> Parseff.fail "boom") with
  | Stdlib.Error (e : unit E.t) ->
      Alcotest.(check string)
        "fail"
        (lines [ "1:1: error: boom"; "1 | abc"; "  | ^" ])
        (E.render e)
  | Stdlib.Ok _ ->
      Alcotest.fail "expected an error"
  );
  ( match
      Parseff.Internal.parse ~max_depth:2 "[[[" (fun () ->
          let rec f () =
            Parseff.rec_ (fun () ->
                ignore (Parseff.char '[');
                f ()
            )
          in
          f ()
      )
    with
  | Stdlib.Error (e : unit E.t) ->
      Alcotest.(check string)
        "depth"
        (lines
           [
             "1:3: error: maximum nesting depth 2 exceeded";
             "1 | [[[";
             "  |   ^";
           ]
        )
        (E.render e)
  | Stdlib.Ok _ ->
      Alcotest.fail "expected an error"
  );
  match Parseff.Internal.parse "z" (fun () -> Parseff.error `Boom) with
  | Stdlib.Error e ->
      Alcotest.(check string)
        "user"
        (lines [ "1:1: error: boom!"; "1 | z"; "  | ^" ])
        (E.render ~user:(fun `Boom -> "boom!") e);
      Alcotest.(check string)
        "user without printer"
        (lines [ "1:1: error: error"; "1 | z"; "  | ^" ])
        (E.render e)
  | Stdlib.Ok _ ->
      Alcotest.fail "expected an error"

let streaming_list_parser () =
  Parseff.context ~delimited:true "list" (fun () ->
      ignore (Parseff.consume "[");
      ignore (Parseff.many (fun () -> Parseff.consume "ab\n") ());
      ignore (Parseff.consume "]")
  )

let test_render_streaming_opener_dropped () =
  let input = "[" ^ String.concat "" (List.init 500 (fun _ -> "ab\n")) ^ "x" in
  let chunked =
    let pos = ref 0 in
    Parseff.Source.of_function (fun buf off len ->
        let n = min (min 7 len) (String.length input - !pos) in
        Bytes.blit_string input !pos buf off n;
        pos := !pos + n;
        n
    )
  in
  match
    Parseff.Internal.parse_source ~backtrack_window:64 chunked
      streaming_list_parser
  with
  | Stdlib.Error (e : unit E.t) ->
      let out = E.render ~name:"stream" e in
      let has sub =
        let n = String.length sub in
        let rec go i =
          i + n <= String.length out && (String.sub out i n = sub || go (i + 1))
        in
        go 0
      in
      Alcotest.(check bool)
        "header" true
        (has "stream:501:1: error: expected \"]\" or \"ab\\n\", found 'x'");
      Alcotest.(check bool) "error line" true (has "501 | x");
      Alcotest.(check bool)
        "frame start is a byte offset" true
        (has "= while parsing list, from byte 0");
      Alcotest.(check bool) "no opener row" false (has "opened here");
      let full = E.render ~name:"stream" ~source:input e in
      let has_full sub =
        let n = String.length sub in
        let rec go i =
          i + n <= String.length full
          && (String.sub full i n = sub || go (i + 1))
        in
        go 0
      in
      Alcotest.(check bool)
        "source restores the opener" true (has_full "opened here");
      Alcotest.(check bool)
        "source restores the frame position" true
        (has_full "= while parsing list, from 1:1")
  | Stdlib.Ok _ ->
      Alcotest.fail "expected an error"

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
      ( "context",
        [
          test_case "chain at advance" `Quick test_context_chain_at_advance;
          test_case "no context, no frames" `Quick
            test_context_chain_without_context;
          test_case "chain after a frame closes" `Quick
            test_context_chain_after_frame_closes;
          test_case "start offsets" `Quick test_context_start_offsets;
          test_case "siblings do not leak" `Quick
            test_context_siblings_do_not_leak;
          test_case "tie keeps the first chain" `Quick
            test_context_tie_keeps_first_chain;
          test_case "user exception leaves the stack consistent" `Quick
            test_context_after_user_exception;
          test_case "deep nesting" `Quick test_context_grows_deep;
          test_case "does not change the result" `Quick
            test_context_does_not_change_result;
        ]
      );
      ( "cut",
        [
          test_case "seals the whole construct" `Quick
            test_cut_seals_whole_construct;
          test_case "without context" `Quick test_cut_without_context;
          test_case "stops at look_ahead" `Quick test_cut_stops_at_look_ahead;
          test_case "success leaves outer frames" `Quick
            test_cut_does_not_leave_context_on_success;
          test_case "outer frames sealed on failure only" `Quick
            test_cut_seals_outer_frames_only_on_failure;
          test_case "nested context" `Quick
            test_cut_inside_nested_context_keeps_outer_open;
        ]
      );
      ( "rendering",
        [
          test_case "A trailing comma" `Quick test_render_trailing_comma;
          test_case "B missing colon" `Quick test_render_missing_colon;
          test_case "C opened here" `Quick test_render_missing_comma_opened_here;
          test_case "D literal after a loop" `Quick
            test_render_loop_exit_literal;
          test_case "E duplicates and sorting" `Quick
            test_render_duplicates_and_sorting;
          test_case "F relabel at start only" `Quick
            test_render_relabel_at_start_only;
          test_case "G end of input" `Quick test_render_end_of_input;
          test_case "H spans and columns" `Quick test_render_spans_and_columns;
          test_case "I cut statement" `Quick test_render_cut_statement;
          test_case "no name, message, pos" `Quick test_render_without_name;
          test_case "tabs" `Quick test_render_tabs;
          test_case "CRLF" `Quick test_render_crlf;
          test_case "empty line" `Quick test_render_empty_line;
          test_case "end of input at line end" `Quick
            test_render_eof_at_line_end;
          test_case "gap" `Quick test_render_gap;
          test_case "frame cap" `Quick test_render_frame_cap;
          test_case "invalid UTF-8 and control bytes" `Quick
            test_render_invalid_utf8_and_controls;
          test_case "window cut" `Quick test_render_window_cut;
          test_case "source and pp" `Quick test_render_source_and_pp;
          test_case "other kinds" `Quick test_render_other_kinds;
          test_case "streaming opener dropped" `Quick
            test_render_streaming_opener_dropped;
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
