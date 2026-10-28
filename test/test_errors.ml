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
  | Error ({ kind = Expected { found = Text _; _ }; _ } as e) ->
      let pos = e.range.start in
      let msg = Parseff.error_message e in
      Alcotest.(check int) "error position" 0 pos;
      Alcotest.(check string) "right error only" "expected 'y', found 'z'" msg
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

type Parseff.custom_error +=
  | Cause_e
  | Boom
  | Unit_user
  | Unterminated
  | Too_large of int

let show_error (e : Parseff.error) =
  match e.kind with
  | Expected _ ->
      Printf.sprintf "Expected %S" (Parseff.error_message e)
  | Failure s ->
      Printf.sprintf "Failure %S" s
  | Custom Cause_e ->
      "E"
  | _ ->
      "other"

let show_result show_v = function
  | Ok v ->
      "Ok " ^ show_v v
  | Error e ->
      Printf.sprintf "Error@%d %s" e.Parseff.range.start (show_error e)

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
    {|Error@9 Expected "expected value, found ']'"|}

let test_missing_colon () =
  both_runners show_unit {|{"a" 1}|} json_value
    {|Error@5 Expected "expected ':', found '1'"|}

let test_unclosed_array_end_of_input () =
  both_runners show_unit "[1, 2" json_value
    {|Error@5 Expected "expected ',' or ']', found end of input"|}

let test_loop_exit_failure_is_furthest () =
  let parser () =
    ignore (Parseff.sep_by (fun () -> Parseff.consume "ab") comma ());
    Parseff.end_of_input ()
  in
  both_runners show_unit "ab,ab,ax" parser
    {|Error@7 Expected "expected \"b\", found 'x'"|}

let test_loop_exit_ties_with_next_item () =
  let parser () =
    ignore (Parseff.many Parseff.digit ());
    Parseff.end_of_input ()
  in
  both_runners show_unit "12x" parser
    {|Error@2 Expected "expected digit or end of input, found 'x'"|}

let test_duplicate_labels_once () =
  let parser () =
    ignore (Parseff.or_ Parseff.digit Parseff.digit ());
    ()
  in
  both_runners show_unit "x" parser
    {|Error@0 Expected "expected digit, found 'x'"|}

let test_alternatives_sorted () =
  let keyword k () = ignore (Parseff.consume k) in
  let order_a = Parseff.one_of [ keyword "if"; keyword "else"; keyword "while" ]
  and order_b =
    Parseff.one_of [ keyword "while"; keyword "if"; keyword "else" ]
  in
  let expected =
    {|Error@0 Expected "expected \"else\", \"if\", or \"while\", found 'xyz'"|}
  in
  both_runners show_unit "xyz" order_a expected;
  both_runners show_unit "xyz" order_b expected

let test_literal_suffix_after_matched_prefix () =
  both_runners show_unit "ax"
    (fun () -> ignore (Parseff.consume "ab"))
    {|Error@1 Expected "expected \"b\", found 'x'"|};
  both_runners show_unit "hel"
    (fun () -> ignore (Parseff.consume "hello"))
    {|Error@3 Expected "expected \"lo\", found end of input"|}

let test_expect_relabels_at_start_only () =
  let parser () =
    Parseff.expect "key" (fun () ->
        ignore (Parseff.consume "id:");
        ignore (Parseff.digit ())
    )
  in
  both_runners show_unit "foo" parser
    {|Error@0 Expected "expected key, found 'foo'"|};
  both_runners show_unit "id:x" parser
    {|Error@3 Expected "expected digit, found 'x'"|}

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
    {|Error@0 Expected "expected number or string, found '?'"|};
  both_runners show_unit {|"abc|} parser
    {|Error@4 Expected "expected '\"', found end of input"|}

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
  both_runners show_unit "ax" parser
    {|Error@1 Expected "expected \"z\", found 'x'"|}

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
        (fun () -> Parseff.error Cause_e)
        (fun () -> Parseff.consume "x")
        ()
    )
    {|Ok x|}

let test_user_error_reported_when_furthest () =
  let parser () =
    Parseff.or_
      (fun () ->
        ignore (Parseff.consume "ab");
        Parseff.error Cause_e
      )
      (fun () -> Parseff.consume "q")
      ()
  in
  both_runners (fun s -> s) "abc" parser "Error@2 E"

let test_user_error_beats_expected_at_tie () =
  let parser () =
    Parseff.or_
      (fun () -> Parseff.error Cause_e)
      (fun () -> Parseff.consume "q")
      ()
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
  both_runners
    (fun s -> s)
    "abq" parser {|Error@2 Expected "expected \"c\", found 'q'"|}

let overflow_message total =
  let shown = List.init 8 (fun i -> Printf.sprintf "\"item%03d\"" i) in
  "expected " ^ String.concat ", " shown
  ^ Printf.sprintf ", or %d more alternatives, found 'zzz'" (total - 8)

let test_item_buffer_overflow () =
  let alternatives =
    List.init 100 (fun i () ->
        ignore (Parseff.consume (Printf.sprintf "item%03d" i))
    )
  in
  match Parseff.parse "zzz" (Parseff.one_of alternatives) with
  | Error ({ kind = Expected { items = _, rest; more; _ }; _ } as e)
    when e.range.start = 0 ->
      Alcotest.(check int) "items kept" 64 (List.length rest + 1);
      Alcotest.(check int) "items counted" 36 more;
      Alcotest.(check string)
        "message" (overflow_message 100) (Parseff.error_message e)
  | _ ->
      Alcotest.fail "Expected an Expected error at 0"

let test_item_buffer_overflow_counts_distinct () =
  let alternatives =
    List.init 300 (fun i () ->
        ignore (Parseff.consume (Printf.sprintf "item%03d" (i mod 100)))
    )
  in
  match Parseff.parse "zzz" (Parseff.one_of alternatives) with
  | Error e ->
      Alcotest.(check string)
        "duplicates are not counted twice" (overflow_message 100)
        (Parseff.error_message e)
  | Ok _ ->
      Alcotest.fail "expected an error"

let test_item_buffer_dedupes_on_overflow () =
  let alternatives =
    List.init 200 (fun i () ->
        ignore (Parseff.consume (Printf.sprintf "item%d" (i mod 3)))
    )
  in
  match Parseff.parse "zzz" (Parseff.one_of alternatives) with
  | Error ({ kind = Expected { found = Text _; _ }; _ } as e)
    when e.range.start = 0 ->
      let msg = Parseff.error_message e in
      Alcotest.(check string)
        "three items" {|expected "item0", "item1", or "item2", found 'zzz'|} msg
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
  | Ok () ->
      Alcotest.(check bool) "no formatting per call" true (!words /. 1000. < 20.)
  | Error _ ->
      Alcotest.fail "Expected success"

let test_utf8_char_error_text () =
  both_runners show_unit "b"
    (fun () -> ignore (Parseff.Utf8.char (Uchar.of_int 0x61)))
    {|Error@0 Expected "expected 'a', found 'b'"|};
  both_runners show_unit "b"
    (fun () -> ignore (Parseff.Utf8.char (Uchar.of_int 0xE9)))
    {|Error@0 Expected "expected '\195\169', found 'b'"|}

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

let contexts_of input parser =
  match Parseff.parse input parser with
  | Stdlib.Ok _ ->
      "ok"
  | Stdlib.Error (e : Parseff.error) ->
      String.concat "; "
        (List.map
           (fun (f : Parseff.context) ->
             Printf.sprintf "%s@%d%s" f.name f.start
               ( if f.delimited then
                   "!"
                 else
                   ""
               )
           )
           e.contexts
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
    "contexts that start at the failure are dropped" "array@0!"
    (contexts_of "[1, 2, 3,]" ctx_array_parser);
  Alcotest.(check string)
    "nested contexts" "array@0!; array@1!"
    (contexts_of "[[1,]]" ctx_array_parser)

let test_context_chain_without_context () =
  Alcotest.(check string)
    "no contexts" ""
    (contexts_of "x" (fun () -> ignore (Parseff.char 'a')))

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
    "chain copied when inner closed" "outer@0; inner@0"
    (contexts_of "abx" parser);
  match Parseff.parse "abx" parser with
  | Error ({ kind = Expected { found = Text _; _ }; _ } as e)
    when e.range.start = 2 ->
      Alcotest.(check string)
        "message" "expected \"c\", found 'x'" (Parseff.error_message e)
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
  Alcotest.(check string) "offsets" "second@2" (contexts_of "abcdx" parser)

let test_context_siblings_do_not_leak () =
  let parser () =
    Parseff.context "a" (fun () -> ignore (Parseff.consume "x"));
    Parseff.context "b" (fun () -> ignore (Parseff.consume "yy"))
  in
  Alcotest.(check string)
    "only the open context" "b@1" (contexts_of "xyz" parser)

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
  Alcotest.(check string)
    "first group's chain" "left@0" (contexts_of "az" parser)

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
  Alcotest.(check string)
    "stack is consistent" "fine@1" (contexts_of "axy" parser)

let test_context_grows_deep () =
  let rec nest n () =
    ignore (Parseff.char 'a');
    if n = 0 then
      ignore (Parseff.char 'x')
    else
      Parseff.context (string_of_int n) (nest (n - 1))
  in
  let contexts = contexts_of (String.make 42 'a') (nest 40) in
  Alcotest.(check bool)
    "forty contexts" true
    (String.length contexts > 0
    && List.length (String.split_on_char ';' contexts) = 40
    )

let test_context_does_not_change_result () =
  both_runners show_unit "[1, 2, 3,]" ctx_array_parser
    {|Error@9 Expected "expected value, found ']'"|}

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
    {|Error@8 Expected "expected expression, found ';'"|};
  both_runners string_of_int "let x = 1; 2;" let_statement_parser "Ok 2";
  Alcotest.(check string)
    "contexts" "statement@0"
    (contexts_of "let x = ;" (fun () -> ignore (let_statement_parser ())))

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
  both_runners
    (fun s -> s)
    "ac" parser {|Error@1 Expected "expected \"b\", found 'c'"|};
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
  both_runners string_of_int "kvkx" parser
    {|Error@3 Expected "expected \"v\", found 'x'"|}

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
    "abd" parser {|Error@2 Expected "expected \"c\", found 'd'"|}

let lines l = String.concat "\n" l

let render ?(name = "input.json") ?source input parser =
  match
    Parseff.parse input (fun () ->
        let v = parser () in
        Parseff.end_of_input ();
        v
    )
  with
  | Stdlib.Ok _ ->
      "ok"
  | Stdlib.Error (e : Parseff.error) ->
      Parseff.render_error ~name ?source e

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
         "input.json:1:10: [error] expected value, found ']'";
         "1 | [1, 2, 3,]";
         "  |          ^";
         "  * while parsing array, from 1:1";
       ]
    )
    (render "[1, 2, 3,]" ctx_json_parser)

let test_render_missing_colon () =
  check_render "B"
    (lines
       [
         "input.json:1:6: [error] expected ':', found '1'";
         "1 | {\"a\" 1}";
         "  |      ^";
         "  * while parsing object, from 1:1";
       ]
    )
    (render {|{"a" 1}|} ctx_json_parser)

let test_render_missing_comma_opened_here () =
  check_render "C"
    (lines
       [
         "input.json:3:3: [error] expected ',' or '}', found '\"'";
         "1 |   {";
         "  | ╭─╯";
         "2 | │   \"a\": 1";
         "3 | │   \"b\": 2";
         "  | ╰───^";
         "  * while parsing object, from 1:1";
       ]
    )
    (render "{\n  \"a\": 1\n  \"b\": 2\n}" ctx_json_parser)

let test_render_opener_mid_line () =
  check_render "mid-line opener"
    (lines
       [
         "input.json:4:5: [error] expected ',' or '}', found '\"'";
         "2 |     \"config\": {";
         "  | ╭─────────────╯";
         "3 | │     \"a\": 1";
         "4 | │     \"b\": 2";
         "  | ╰─────^";
         "  * while parsing object, from 2:13";
         "  * while parsing object, from 1:1";
       ]
    )
    (render "{\n  \"config\": {\n    \"a\": 1\n    \"b\": 2\n  }\n}"
       ctx_json_parser
    )

let test_render_opener_tab () =
  check_render "tab before the opener"
    (lines
       [
         "input.json:3:4: [error] expected ',' or ']', found '3'";
         "2 |   \t[1,";
         "  | ╭───╯";
         "3 | │ \t2 3]";
         "  | ╰─────^";
         "  * while parsing array, from 2:2";
         "  * while parsing array, from 1:1";
       ]
    )
    (render "[\n\t[1,\n\t2 3]\n]" ctx_json_parser)

let test_render_opener_utf8 () =
  check_render "multi-byte char before the opener"
    (lines
       [
         "input.json:2:3: [error] expected ',' or ']', found '3'";
         "1 |   [\"é\", [1,";
         "  | ╭───────╯";
         "2 | │ 2 3]]";
         "  | ╰───^";
         "  * while parsing array, from 1:7";
         "  * while parsing array, from 1:1";
       ]
    )
    (render "[\"é\", [1,\n2 3]]" ctx_json_parser)

let test_render_loop_exit_literal () =
  let parser () =
    ignore (Parseff.sep_by (fun () -> ignore (Parseff.consume "ab")) comma ())
  in
  check_render "D"
    (lines
       [
         "input.json:1:8: [error] expected \"b\", found 'x'";
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
       [ "input.json:1:1: [error] expected digit, found 'x'"; "1 | x"; "  | ^" ]
    )
    (render "x" (fun () -> ignore (Parseff.or_ Parseff.digit Parseff.digit ())));
  let keyword k () = ignore (Parseff.consume k) in
  let expected =
    lines
      [
        "input.json:1:1: [error] expected \"else\", \"if\", or \"while\", \
         found 'xyz'";
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
         "input.json:1:1: [error] expected key, found 'foo'";
         "1 | foo";
         "  | ^^^";
       ]
    )
    (render "foo" key);
  check_render "F deeper"
    (lines
       [
         "input.json:1:4: [error] expected digit, found 'x'";
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
         "input.json:1:1: [error] expected number or string, found '?'";
         "1 | ?";
         "  | ^";
       ]
    )
    (render "?" literal);
  check_render "F unterminated string"
    (lines
       [
         "input.json:1:5: [error] expected '\"', found end of input";
         "1 | \"abc";
         "  |     ^";
       ]
    )
    (render "\"abc" literal)

let test_render_end_of_input () =
  check_render "G array"
    (lines
       [
         "input.json:1:6: [error] expected ',' or ']', found end of input";
         "1 | [1, 2";
         "  |      ^";
         "  * while parsing array, from 1:1";
       ]
    )
    (render "[1, 2" ctx_json_parser);
  check_render "G literal"
    (lines
       [
         "input.json:1:4: [error] expected \"lo\", found end of input";
         "1 | hel";
         "  |    ^";
       ]
    )
    (render "hel" (fun () -> ignore (Parseff.consume "hello")))

let test_render_spans_and_columns () =
  check_render "H identifier run"
    (lines
       [
         "input.json:1:7: [error] expected ',' or ']', found 'foo'";
         "1 | [1, 2 foo]";
         "  |       ^^^";
         "  * while parsing array, from 1:1";
       ]
    )
    (render "[1, 2 foo]" ctx_json_parser);
  let input = "[\"\xC3\xA9\" \"b\"]" in
  let body =
    [
      "1 | [\"\xC3\xA9\" \"b\"]";
      "  |      ^";
      "  * while parsing array, from 1:1";
    ]
  in
  let expected =
    lines ("input.json:1:6: [error] expected ',' or ']', found '\"'" :: body)
  in
  check_render "H code points" expected (render input ctx_json_parser);
  check_render "H code points with ~source" expected
    (render ~source:input input ctx_json_parser)

let let_stmt_parser () = ignore (let_statement_parser ())

let test_render_cut_statement () =
  check_render "I"
    (lines
       [
         "input.json:1:9: [error] expected expression, found ';'";
         "1 | let x = ;";
         "  |         ^";
         "  * while parsing statement, from 1:1";
       ]
    )
    (render "let x = ;" let_stmt_parser)

let test_render_without_name () =
  match Parseff.parse "x" (fun () -> ignore (Parseff.digit ())) with
  | Stdlib.Error (e : Parseff.error) ->
      Alcotest.(check string)
        "no name"
        (lines [ "1:1: [error] expected digit, found 'x'"; "1 | x"; "  | ^" ])
        (Parseff.render_error e);
      Alcotest.(check string)
        "message" "expected digit, found 'x'" (Parseff.error_message e);
      Alcotest.(check int) "pos" 0 e.range.start
  | Stdlib.Ok _ ->
      Alcotest.fail "expected an error"

let test_render_tabs () =
  check_render "tabs"
    (lines
       [
         "input.json:1:7: [error] expected ',' or ']', found '3'";
         "1 | [1,\t2\t3]";
         "  |    \t \t^";
         "  * while parsing array, from 1:1";
       ]
    )
    (render "[1,\t2\t3]" ctx_json_parser)

let test_render_crlf () =
  check_render "crlf"
    (lines
       [
         "input.json:3:3: [error] expected ',' or '}', found '\"'";
         "1 |   {";
         "  | ╭─╯";
         "2 | │   \"a\": 1";
         "3 | │   \"b\": 2";
         "  | ╰───^";
         "  * while parsing object, from 1:1";
       ]
    )
    (render "{\r\n  \"a\": 1\r\n  \"b\": 2\r\n}" ctx_json_parser)

let test_render_empty_line () =
  check_render "empty line before"
    (lines
       [
         "input.json:3:3: [error] expected ',' or ']', found '2'";
         "1 |   [";
         "  | ╭─╯";
         "2 | │";
         "3 | │ 1 2]";
         "  | ╰───^";
         "  * while parsing array, from 1:1";
       ]
    )
    (render "[\n\n1 2]" ctx_json_parser)

let test_render_eof_at_line_end () =
  check_render "eof after newline"
    (lines
       [
         "input.json:2:1: [error] expected value, found end of input";
         "1 |   [1,";
         "  | ╭─╯";
         "2 | │";
         "  | ╰─^";
         "  * while parsing array, from 1:1";
       ]
    )
    (render "[1,\n" ctx_json_parser);
  check_render "empty input"
    (lines
       [
         "input.json:1:1: [error] expected value, found end of input";
         "1 |";
         "  | ^";
       ]
    )
    (render "" ctx_json_parser)

let test_render_gap () =
  let many_lines = String.concat "" (List.init 40 (fun _ -> "1,\n")) in
  check_render "many hidden lines"
    (lines
       [
         "input.json:42:1: [error] expected value, found 'x'";
         " 1 |   [";
         "   | ╭─╯";
         "   | │ ...";
         "41 | │ 1,";
         "42 | │ x]";
         "   | ╰─^";
         "   * while parsing array, from 1:1";
       ]
    )
    (render ("[\n" ^ many_lines ^ "x]") ctx_json_parser);
  check_render "exactly one hidden line"
    (lines
       [
         "input.json:4:2: [error] expected value, found 'x'";
         "1 |   [";
         "  | ╭─╯";
         "2 | │ 1,";
         "3 | │ 2,";
         "4 | │  x]";
         "  | ╰──^";
         "  * while parsing array, from 1:1";
       ]
    )
    (render "[\n1,\n2,\n x]" ctx_json_parser)

let test_render_context_cap () =
  let rec nest n () =
    ignore (Parseff.char 'a');
    if n = 0 then
      ignore (Parseff.char 'x')
    else
      Parseff.context (Printf.sprintf "level%d" n) (nest (n - 1))
  in
  check_render "six contexts are all printed"
    (lines
       [
         "input.json:1:8: [error] expected 'x', found end of input";
         "1 | aaaaaaa";
         "  |        ^";
         "  * while parsing level1, from 1:7";
         "  * while parsing level2, from 1:6";
         "  * while parsing level3, from 1:5";
         "  * while parsing level4, from 1:4";
         "  * while parsing level5, from 1:3";
         "  * while parsing level6, from 1:2";
       ]
    )
    (render (String.make 7 'a') (nest 6));
  check_render "seven contexts hide two"
    (lines
       [
         "input.json:1:9: [error] expected 'x', found end of input";
         "1 | aaaaaaaa";
         "  |         ^";
         "  * while parsing level1, from 1:8";
         "  * while parsing level2, from 1:7";
         "  * while parsing level3, from 1:6";
         "  * while parsing level4, from 1:5";
         "  * while parsing level5, from 1:4";
         "  * ... and 2 more";
       ]
    )
    (render (String.make 8 'a') (nest 7))

let test_render_invalid_utf8_and_controls () =
  check_render "invalid byte"
    (lines
       [
         "input.json:1:5: [error] expected value, found '\\xFF'";
         "1 | [1, \\xFF 2]";
         "  |     ^^^^";
         "  * while parsing array, from 1:1";
       ]
    )
    (render "[1, \xFF 2]" ctx_json_parser);
  check_render "control byte"
    (lines
       [
         "input.json:1:3: [error] expected ',' or ']', found '\\x01'";
         "1 | [1\\x01]";
         "  |   ^^^^";
         "  * while parsing array, from 1:1";
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

let test_render_source () =
  let input = "{\n  \"a\": 1\n  \"b\": 2\n}" in
  match Parseff.parse input ctx_json_parser with
  | Stdlib.Error (e : Parseff.error) ->
      Alcotest.(check string)
        "source gives the same text"
        (Parseff.render_error ~name:"input.json" e)
        (Parseff.render_error ~name:"input.json" ~source:input e)
  | Stdlib.Ok _ ->
      Alcotest.fail "expected an error"

let test_render_other_kinds () =
  ( match Parseff.parse "abc" (fun () -> Parseff.fail "boom") with
  | Stdlib.Error (e : Parseff.error) ->
      Alcotest.(check string)
        "fail"
        (lines [ "1:1: [error] boom"; "1 | abc"; "  | ^" ])
        (Parseff.render_error e)
  | Stdlib.Ok _ ->
      Alcotest.fail "expected an error"
  );
  ( match
      Parseff.parse ~max_depth:2 "[[[" (fun () ->
          let rec f () =
            Parseff.rec_ (fun () ->
                ignore (Parseff.char '[');
                f ()
            )
          in
          f ()
      )
    with
  | Stdlib.Error (e : Parseff.error) ->
      Alcotest.(check string)
        "depth"
        (lines
           [
             "1:3: [error] maximum nesting depth 2 exceeded";
             "1 | [[[";
             "  |   ^";
             "  * help: raise ~max_depth";
           ]
        )
        (Parseff.render_error e)
  | Stdlib.Ok _ ->
      Alcotest.fail "expected an error"
  );
  match Parseff.parse "z" (fun () -> Parseff.error Boom) with
  | Stdlib.Error e ->
      Alcotest.(check string)
        "user"
        (lines [ "1:1: [error] boom!"; "1 | z"; "  | ^" ])
        (Parseff.render_error ~custom:(function Boom -> "boom!" | _ -> "?") e);
      Alcotest.(check string)
        "user without printer"
        (lines
           [
             "1:1: [error] custom error (no ~custom printer given)";
             "1 | z";
             "  | ^";
           ]
        )
        (Parseff.render_error e)
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
    Parseff.parse_source ~backtrack_window:64 chunked streaming_list_parser
  with
  | Stdlib.Error (e : Parseff.error) ->
      let out = Parseff.render_error ~name:"stream" e in
      let has sub =
        let n = String.length sub in
        let rec go i =
          i + n <= String.length out && (String.sub out i n = sub || go (i + 1))
        in
        go 0
      in
      Alcotest.(check bool)
        "header" true
        (has "stream:501:1: [error] expected \"]\" or \"ab\\n\", found 'x'");
      Alcotest.(check bool) "error line" true (has "501 | x");
      Alcotest.(check bool)
        "context start is a byte offset" true
        (has "* while parsing list, from byte 0");
      Alcotest.(check bool) "no opener corner" false (has "╭");
      Alcotest.(check bool) "no gutter bar" false (has "│");
      Alcotest.(check bool) "plain caret row" true (has "501 | x\n    | ^");
      let full = Parseff.render_error ~name:"stream" ~source:input e in
      let has_full sub =
        let n = String.length sub in
        let rec go i =
          i + n <= String.length full
          && (String.sub full i n = sub || go (i + 1))
        in
        go 0
      in
      Alcotest.(check bool) "source restores the opener" true (has_full "╭─╯");
      Alcotest.(check bool)
        "source restores the context position" true
        (has_full "* while parsing list, from 1:1")
  | Stdlib.Ok _ ->
      Alcotest.fail "expected an error"

let render_error ?(name = "input.json") input parser =
  match Parseff.parse input parser with
  | Stdlib.Ok _ ->
      "ok"
  | Stdlib.Error (e : Parseff.error) ->
      Parseff.render_error ~name e

let digits_parser () =
  ignore
    (Parseff.take_while ~at_least:1 ~label:"number" (fun c ->
         c >= '0' && c <= '9'
     )
    )

let test_fail_from_span () =
  let parser () =
    ignore (Parseff.consume "x = ");
    let start = Parseff.position () in
    digits_parser ();
    Parseff.fail ~from:start "number is too large"
  in
  check_render "span from"
    (lines
       [
         "input.json:1:5: [error] number is too large";
         "1 | x = 300;";
         "  |     ^^^";
       ]
    )
    (render_error "x = 300;" parser);
  match Parseff.parse "x = 300;" parser with
  | Error ({ kind = Failure "number is too large"; _ } as e)
    when e.range.start = 4 ->
      ()
  | _ ->
      Alcotest.fail "expected a Failure error positioned at the span start"

let test_fail_from_after_position_is_normalized () =
  let parser () = Parseff.fail ~from:5 "backwards" in
  check_render "span is never reversed"
    (lines [ "input.json:1:1: [error] backwards"; "1 | abcdef"; "  | ^^^^^" ])
    (render_error "abcdef" parser);
  let parser () =
    ignore (Parseff.consume "abc");
    Parseff.fail ~from:1 "short"
  in
  check_render "span covers from to position"
    (lines [ "input.json:1:2: [error] short"; "1 | abcdef"; "  |  ^^" ])
    (render_error "abcdef" parser)

let test_loose_note () =
  let parser () =
    Parseff.fail
      ~notes:[ { Parseff.at = None; text = "keys must be unique" } ]
      "duplicate key"
  in
  check_render "note line"
    (lines
       [
         "input.json:1:1: [error] duplicate key";
         "1 | abc";
         "  | ^";
         "  * note: keys must be unique";
       ]
    )
    (render_error "abc" parser)

let paren_note start =
  {
    Parseff.at = Some { Parseff.start; stop = start + 1 };
    text = "to match this";
  }

let test_secondary_underline_same_line () =
  let parser () =
    ignore (Parseff.consume "foo(1, 2");
    Parseff.fail ~notes:[ paren_note 3 ] "missing )"
  in
  check_render "underline under the error line"
    (lines
       [
         "input.json:1:9: [error] missing )";
         "1 | foo(1, 2";
         "  |         ^";
         "  |    - to match this";
       ]
    )
    (render_error "foo(1, 2" parser)

let test_secondary_underline_earlier_lines () =
  let input = "foo(\na,\nb,\nc,\nd" in
  let parser () =
    ignore (Parseff.consume input);
    Parseff.fail ~notes:[ paren_note 3 ] "missing )"
  in
  check_render "far line gets its own row and a gap"
    (lines
       [
         "input.json:5:2: [error] missing )";
         "1 | foo(";
         "  |    - to match this";
         "  | ...";
         "4 | c,";
         "5 | d";
         "  |  ^";
       ]
    )
    (render_error input parser);
  let input = "foo(\na,\nb" in
  let parser () =
    ignore (Parseff.consume input);
    Parseff.fail ~notes:[ paren_note 3 ] "missing )"
  in
  check_render "one hidden line is shown"
    (lines
       [
         "input.json:3:2: [error] missing )";
         "1 | foo(";
         "  |    - to match this";
         "2 | a,";
         "3 | b";
         "  |  ^";
       ]
    )
    (render_error input parser)

let test_error_from_and_notes () =
  let parser () =
    ignore (Parseff.consume "ab");
    Parseff.error ~from:0
      ~notes:[ { Parseff.at = None; text = "see the manual" } ]
      Boom
  in
  ( match Parseff.parse "abcd" parser with
  | Stdlib.Error e ->
      Alcotest.(check string)
        "user error"
        (lines
           [
             "1:1: [error] boom!";
             "1 | abcd";
             "  | ^^";
             "  * note: see the manual";
           ]
        )
        (Parseff.render_error ~custom:(function Boom -> "boom!" | _ -> "?") e)
  | Stdlib.Ok _ ->
      Alcotest.fail "expected an error"
  );
  match Parseff.parse "abcd" parser with
  | Error ({ kind = Custom Boom; _ } as e) when e.range.start = 0 ->
      ()
  | _ ->
      Alcotest.fail "expected a Custom error positioned at the span start"

let test_fail_frames () =
  let parser () =
    Parseff.context ~delimited:true "outer" (fun () ->
        ignore (Parseff.consume "a");
        Parseff.context "inner" (fun () ->
            ignore (Parseff.consume "b");
            Parseff.context "empty" (fun () -> Parseff.fail "boom")
        )
    )
  in
  check_render "fail goes through the contexts it unwinds"
    (lines
       [
         "input.json:1:3: [error] boom";
         "1 | abc";
         "  |   ^";
         "  * while parsing inner, from 1:2";
         "  * while parsing outer, from 1:1";
       ]
    )
    (render_error "abc" parser);
  Alcotest.(check string)
    "contexts" "outer@0!; inner@1" (contexts_of "abc" parser)

let id_re = Re.compile (Re.Posix.re "[a-z]+")
let digit_re = Re.compile (Re.Posix.re "[0-9]+")

let test_match_regex_label () =
  let parser () = ignore (Parseff.match_regex ~label:"identifier" id_re) in
  both_runners
    (fun s -> s)
    "x"
    (fun () -> Parseff.match_regex ~label:"identifier" id_re)
    "Ok x";
  both_runners show_unit "1" parser
    {|Error@0 Expected "expected identifier, found '1'"|};
  both_runners show_unit "" parser
    {|Error@0 Expected "expected identifier, found end of input"|};
  let both () =
    ignore
      (Parseff.or_
         (fun () -> Parseff.match_regex ~label:"number" digit_re)
         (fun () -> Parseff.match_regex ~label:"identifier" id_re)
         ()
      )
  in
  both_runners show_unit "!" both
    {|Error@0 Expected "expected identifier or number, found '!'"|};
  check_render "label is rendered"
    (lines
       [
         "input.json:1:1: [error] expected identifier or number, found '!'";
         "1 | !";
         "  | ^";
       ]
    )
    (render_error "!" both);
  both_runners show_unit "!"
    (fun () -> ignore (Parseff.match_regex id_re))
    {|Error@0 Expected "expected text matching the pattern, found '!'"|}

let test_note_on_dropped_line () =
  let input = String.make 2000 'a' ^ "!" in
  let chunked =
    let pos = ref 0 in
    Parseff.Source.of_function (fun buf off len ->
        let n = min (min 5 len) (String.length input - !pos) in
        Bytes.blit_string input !pos buf off n;
        pos := !pos + n;
        n
    )
  in
  let parser () =
    ignore (Parseff.many (fun () -> Parseff.char 'a') ());
    Parseff.fail ~notes:[ paren_note 0 ] "stray"
  in
  match Parseff.parse_source ~backtrack_window:64 chunked parser with
  | Stdlib.Error (e : Parseff.error) ->
      let out = Parseff.render_error e in
      Alcotest.(check bool)
        "falls back to a note line" true
        (List.mem "  * note: to match this" (String.split_on_char '\n' out))
  | Stdlib.Ok _ ->
      Alcotest.fail "expected an error"

let describe_kind (e : Parseff.error) =
  match e.kind with
  | Parseff.Expected { items = _, rest; found; _ } ->
      Printf.sprintf "expected %d more, found %s" (List.length rest)
        ( match found with
        | Parseff.Text t ->
            t
        | Parseff.End ->
            "end"
        | _ ->
            "other"
        )
  | Parseff.Failure m ->
      "message " ^ m
  | Parseff.Custom _ ->
      "user"
  | Parseff.Depth_limit_exceeded { limit } ->
      Printf.sprintf "depth %d" limit
  | Parseff.Backtrack_window_exceeded { limit; _ } ->
      Printf.sprintf "window %d" limit

let kind_of ?max_depth input parser =
  match Parseff.parse ?max_depth input parser with
  | Stdlib.Ok _ ->
      "ok"
  | Stdlib.Error e ->
      describe_kind e

let test_exhaustive_kind_match () =
  Alcotest.(check string)
    "expected" "expected 1 more, found x"
    (kind_of "x" (fun () ->
         ignore
           (Parseff.or_ Parseff.digit
              (fun () ->
                ignore (Parseff.char '1');
                0
              )
              ()
           )
     )
    );
  Alcotest.(check string)
    "message" "message boom"
    (kind_of "x" (fun () -> Parseff.fail "boom"));
  Alcotest.(check string)
    "user" "user"
    (kind_of "x" (fun () -> Parseff.error Unit_user));
  Alcotest.(check string)
    "depth" "depth 2"
    (kind_of ~max_depth:2 "[[[" (fun () ->
         let rec f () =
           Parseff.rec_ (fun () ->
               ignore (Parseff.char '[');
               f ()
           )
         in
         f ()
     )
    );
  let chunked =
    let pos = ref 0 in
    let input = String.make 500 'a' in
    Parseff.Source.of_function (fun buf off len ->
        let n = min (min 5 len) (String.length input - !pos) in
        Bytes.blit_string input !pos buf off n;
        pos := !pos + n;
        n
    )
  in
  match
    Parseff.parse_source ~backtrack_window:16 chunked (fun () ->
        ignore (Parseff.take_while (fun c -> c = 'a'))
    )
  with
  | Stdlib.Error e ->
      Alcotest.(check string) "window" "window 16" (describe_kind e)
  | Stdlib.Ok _ ->
      Alcotest.fail "expected the window to be exceeded"

let test_end_of_input_keeps_expected_set () =
  match Parseff.parse "[1, 2" ctx_json_parser with
  | Stdlib.Error
      {
        kind =
          Parseff.Expected
            {
              items = Parseff.Char a, [ Parseff.Char b ];
              found = Parseff.End;
              more = 0;
            };
        range = { start = 5; stop = 5 };
        _;
      } ->
      Alcotest.(check (list int))
        "comma and bracket"
        [ Char.code ','; Char.code ']' ]
        [ Uchar.to_int a; Uchar.to_int b ]
  | _ ->
      Alcotest.fail "expected the set of the array separator and closer"

let number_checked () =
  let n =
    int_of_string
      (Parseff.take_while ~at_least:1 ~label:"digit" (fun c ->
           c >= '0' && c <= '9'
       )
      )
  in
  if n > 255 then
    Parseff.error (Too_large n)
  else
    n

let test_user_errors_by_pattern () =
  ( match Parseff.parse "300" number_checked with
  | Stdlib.Error { kind = Parseff.Custom (Too_large n); range; _ } ->
      Alcotest.(check int) "payload" 300 n;
      Alcotest.(check int) "position" 3 range.start
  | _ ->
      Alcotest.fail "expected a user error"
  );
  ( match
      Parseff.parse "300" (fun () ->
          Parseff.expect "octet" (fun () -> number_checked ())
      )
    with
  | Stdlib.Error { kind = Parseff.Custom (Too_large 300); _ } ->
      ()
  | _ ->
      Alcotest.fail "expect must not relabel a user error"
  );
  ( match
      Parseff.parse "300" (fun () ->
          Parseff.or_ number_checked (fun () -> Parseff.fail "second") ()
      )
    with
  | Stdlib.Error { kind = Parseff.Failure "second"; _ } ->
      ()
  | _ ->
      Alcotest.fail "fail in the second branch must win"
  );
  ( match
      Parseff.parse "300" (fun () ->
          Parseff.or_ number_checked
            (fun () ->
              ignore (Parseff.char 'x');
              0
            )
            ()
      )
    with
  | Stdlib.Error { kind = Parseff.Custom (Too_large 300); _ } ->
      ()
  | _ ->
      Alcotest.fail "the furthest user error is reported"
  );
  match Parseff.parse "x" (fun () -> Parseff.fail "boom") with
  | Stdlib.Error { kind = Parseff.Failure "boom"; _ } ->
      ()
  | _ ->
      Alcotest.fail "fail gives a message"

let test_migration_pattern () =
  let migrate input =
    match Parseff.parse input number_checked with
    | Stdlib.Ok n ->
        Printf.sprintf "ok %d" n
    | Stdlib.Error e ->
        Parseff.error_message
          ~custom:(function
            | Too_large n -> Printf.sprintf "%d is too large" n | _ -> "?"
            )
          e
  in
  Alcotest.(check string) "ok" "ok 12" (migrate "12");
  Alcotest.(check string) "user" "300 is too large" (migrate "300");
  Alcotest.(check string)
    "expected" "expected digit, found end of input" (migrate "")

let test_diagnostics_carry_the_error () =
  let diagnostics = Parseff.Diagnostics.create () in
  let parser () =
    Parseff.warn diagnostics "careful";
    ignore (Parseff.consume "ab");
    Parseff.consume "c"
  in
  match Parseff.parse ~consume:`All ~diagnostics "abx" parser with
  | Stdlib.Error error -> (
      Alcotest.(check string)
        "message" "expected \"c\", found 'x'"
        (Parseff.error_message error);
      match Parseff.Diagnostics.to_list diagnostics with
      | [ { value = "careful"; pos = 0 } ] ->
          ()
      | _ ->
          Alcotest.fail "expected the diagnostic"
    )
  | _ ->
      Alcotest.fail "expected an error with its diagnostics"

let chunk_source ?(chunk = 100) input =
  let pos = ref 0 in
  Parseff.Source.of_chunks (fun () ->
      if !pos >= String.length input then
        None
      else begin
        let k = min chunk (String.length input - !pos) in
        let c = String.sub input !pos k in
        pos := !pos + k;
        Some c
      end
  )

let render_all e input =
  ignore (Parseff.render_error e);
  ignore (Parseff.render_error ~source:input e)

let test_backtrack_window_error_renders () =
  let long = String.make 5000 'a' in
  let check label input =
    match
      Parseff.parse_source ~backtrack_window:1000 (chunk_source input)
        (fun () -> ignore (Parseff.take 5000)
      )
    with
    | Error ({ kind = Backtrack_window_exceeded _; _ } as e) ->
        render_all e input
    | _ ->
        Alcotest.failf "%s: expected the window error" label
  in
  check "one long line" long;
  check "long second line" (String.make 100 'a' ^ "\n" ^ String.make 5000 'b');
  match
    Parseff.parse_source ~backtrack_window:1000 (chunk_source long) (fun () ->
        ignore (Parseff.take 5000);
        Parseff.fail
          ~notes:
            [
              {
                Parseff.at = Some { Parseff.start = 9000; stop = 9001 };
                text = "far";
              };
            ]
          "x"
    )
  with
  | Error e ->
      render_all e long
  | Ok _ ->
      Alcotest.fail "expected an error"

let test_notes_past_the_line_end () =
  let input = String.make 600 'a' in
  match
    Parseff.parse input (fun () ->
        ignore (Parseff.take 600);
        Parseff.fail
          ~notes:
            [
              {
                Parseff.at = Some { Parseff.start = 5000; stop = 5001 };
                text = "far";
              };
            ]
          "x"
    )
  with
  | Error e ->
      render_all e input
  | Ok _ ->
      Alcotest.fail "expected an error"

let lines_text =
  String.concat "" (List.init 400 (fun i -> Printf.sprintf "line %d\n" i))

let from_before_the_buffer_parser kind () =
  ignore (Parseff.consume "line 0\n");
  let from = Parseff.position () in
  Parseff.skip_while (fun _ -> true);
  match kind with
  | `Fail ->
      Parseff.fail ~from "unterminated"
  | `Error ->
      Parseff.error ~from Unterminated

let check_location_agrees label input (e : Parseff.error) =
  let at = Parseff.location_of_position input e.range.start in
  match e.location with
  | Some location ->
      Alcotest.(check (pair int int))
        (label ^ " line and column")
        (at.line, at.col)
        (location.line, location.col);
      Alcotest.(check int) (label ^ " offset") e.range.start location.offset
  | None ->
      Alcotest.failf "%s has no location" label

let test_from_older_than_the_buffer () =
  ( match Parseff.parse lines_text (from_before_the_buffer_parser `Fail) with
  | Error e ->
      Alcotest.(check int) "string mode keeps the offset" 7 e.range.start;
      check_location_agrees "string" lines_text e
  | Ok _ ->
      Alcotest.fail "expected an error"
  );
  let streaming kind =
    Parseff.parse_source ~backtrack_window:200
      (chunk_source ~chunk:64 lines_text)
      (from_before_the_buffer_parser kind)
  in
  let check label = function
    | Error (e : Parseff.error) -> (
        Alcotest.(check int) (label ^ " keeps the span start") 7 e.range.start;
        Alcotest.(check int) (label ^ " keeps the position") 7 e.range.start;
        Alcotest.(check bool)
          (label ^ " has no position")
          true (e.location = None);
        let header =
          List.hd (String.split_on_char '\n' (Parseff.render_error e))
        in
        Alcotest.(check bool)
          (label ^ " says byte, not a line")
          true
          (String.starts_with ~prefix:"byte 7: [error] " header);
        Alcotest.(check bool)
          (label ^ " shows no excerpt")
          true
          (not (String.contains (Parseff.render_error e) '|'));
        let exact =
          Parseff.parse lines_text (from_before_the_buffer_parser `Fail)
        in
        match exact with
        | Error (x : Parseff.error) when label = "fail" ->
            Alcotest.(check string)
              (label ^ " with the source gives the exact line")
              (Parseff.render_error ~source:lines_text x)
              (Parseff.render_error ~source:lines_text e)
        | _ ->
            ()
      )
    | Ok _ ->
        Alcotest.fail "expected an error"
  in
  check "fail" (streaming `Fail);
  check "error" (streaming `Error)

let test_look_ahead_probe_does_not_leak () =
  let parser () =
    ignore
      (Parseff.look_ahead (fun () ->
           Parseff.many (fun () -> Parseff.char 'a') ()
       )
      );
    ignore (Parseff.char 'b')
  in
  check_render "probe failures are forgotten"
    (lines
       [
         "input.json:1:1: [error] expected 'b', found 'aac'";
         "1 | aac";
         "  | ^^^";
       ]
    )
    (render_error "aac" parser);
  let guarded () =
    Parseff.or_
      (fun () ->
        ignore (Parseff.look_ahead (fun () -> Parseff.char 'a'));
        ignore (Parseff.char 'b')
      )
      (fun () -> ignore (Parseff.char 'c'))
      ()
  in
  check_render "a successful guard adds nothing"
    (lines
       [
         "input.json:1:1: [error] expected 'b' or 'c', found 'ad'";
         "1 | ad";
         "  | ^^";
       ]
    )
    (render_error "ad" guarded);
  check_render "a failing look_ahead is a failure"
    (lines
       [
         "input.json:1:2: [error] expected \"b\", found 'x'"; "1 | ax"; "  |  ^";
       ]
    )
    (render_error "ax" (fun () ->
         ignore (Parseff.look_ahead (fun () -> Parseff.consume "ab"))
     )
    );
  check_render "a failing look_ahead merges with the earlier group"
    (lines
       [
         "input.json:1:1: [error] expected 'q' or 'r', found 'z'";
         "1 | z";
         "  | ^";
       ]
    )
    (render_error "z" (fun () ->
         Parseff.or_
           (fun () -> ignore (Parseff.char 'q'))
           (fun () -> ignore (Parseff.look_ahead (fun () -> Parseff.char 'r')))
           ()
     )
    )

let test_multibyte_literal_mismatch () =
  check_render "two byte character"
    (lines
       [
         "input.json:1:1: [error] expected \"\xC3\xA9\", found '\xC3\xA8'";
         "1 | \xC3\xA8";
         "  | ^";
       ]
    )
    (render_error "\xC3\xA8" (fun () -> ignore (Parseff.consume "\xC3\xA9")));
  check_render "three byte character after a prefix"
    (lines
       [
         "input.json:1:2: [error] expected \"\xE2\x82\xAC\", found \
          '\xE2\x82\xAD'";
         "1 | x\xE2\x82\xAD";
         "  |  ^";
       ]
    )
    (render_error "x\xE2\x82\xAD" (fun () ->
         ignore (Parseff.consume "x\xE2\x82\xAC")
     )
    );
  check_render "input ends inside the character"
    (lines
       [
         "input.json:1:1: [error] expected \"\xC3\xA9\", found '\\xC3'";
         "1 | \\xC3";
         "  | ^^^^";
       ]
    )
    (render_error "\xC3" (fun () -> ignore (Parseff.consume "\xC3\xA9")))

let test_expect_with_commit_inside () =
  let with_commit body () =
    Parseff.or_
      (fun () -> ignore (Parseff.char 'a'))
      (fun () ->
        Parseff.expect "thing" (fun () ->
            Parseff.commit ();
            body ()
        )
      )
      ()
  in
  check_render "items before the commit are forgotten, the rest is relabeled"
    (lines
       [ "input.json:1:1: [error] expected thing, found 'z'"; "1 | z"; "  | ^" ]
    )
    (render_error "z"
       (with_commit (fun () ->
            ignore
              (Parseff.or_
                 (fun () -> Parseff.char 'x')
                 (fun () -> Parseff.char 'y')
                 ()
              )
        )
       )
    );
  check_render "a single failure is relabeled too"
    (lines
       [ "input.json:1:1: [error] expected thing, found 'z'"; "1 | z"; "  | ^" ]
    )
    (render_error "z" (with_commit (fun () -> ignore (Parseff.char 'x'))));
  check_render "without commit the earlier alternative stays"
    (lines
       [
         "input.json:1:1: [error] expected thing or 'a', found 'z'";
         "1 | z";
         "  | ^";
       ]
    )
    (render_error "z" (fun () ->
         Parseff.or_
           (fun () -> ignore (Parseff.char 'a'))
           (fun () ->
             Parseff.expect "thing" (fun () -> ignore (Parseff.char 'x'))
           )
           ()
     )
    )

let test_commit_without_frame_keeps_the_group () =
  check_render "nothing sealed, nothing forgotten"
    (lines
       [
         "input.json:1:3: [error] expected \"cd\", found 'XY'";
         "1 | abXY";
         "  |   ^^";
       ]
    )
    (render_error "abXY" (fun () ->
         ignore (Parseff.optional (fun () -> Parseff.consume "abcd") ());
         Parseff.commit ();
         ignore (Parseff.consume "q")
     )
    )

let test_expect_after_far_buffer_overflow () =
  let originals =
    List.init 50 (fun i () -> ignore (Parseff.char (Char.chr (77 + i))))
  in
  let parser () =
    Parseff.or_
      (fun () -> Parseff.one_of originals ())
      (fun () ->
        Parseff.expect "thing" (fun () ->
            for _ = 1 to 20 do
              ignore (Parseff.optional (fun () -> Parseff.char 'A') ())
            done;
            ignore (Parseff.char 'A')
        )
      )
      ()
  in
  match Parseff.parse "0" parser with
  | Error e ->
      let chars =
        List.init 50 (fun i -> Printf.sprintf "'%c'" (Char.chr (77 + i)))
      in
      let shown = List.filteri (fun i _ -> i < 7) chars in
      Alcotest.(check string)
        "originals kept, inner items replaced"
        ("expected thing, " ^ String.concat ", " shown
       ^ ", or 43 more alternatives, found '0'"
        )
        (Parseff.error_message e)
  | Ok _ ->
      Alcotest.fail "expected an error"

let test_window_markers_agree () =
  let digits n =
    String.init n (fun i -> Char.chr (Char.code '0' + (i mod 10)))
  in
  let check label input pos =
    match
      Parseff.parse input (fun () ->
          ignore (Parseff.take pos);
          ignore (Parseff.char '!')
      )
    with
    | Error e ->
        Alcotest.(check string)
          label
          (Parseff.render_error ~source:input e)
          (Parseff.render_error e)
    | Ok _ ->
        Alcotest.failf "%s: expected an error" label
  in
  check "long line" (digits 400) 279;
  check "line of 241 bytes" (digits 241 ^ "\nb") 0;
  check "line of 241 bytes, error late" (digits 241 ^ "\nb") 200;
  check "multi-byte characters at the cut"
    (String.concat "" (List.init 150 (fun _ -> "\xE2\x82\xAC")))
    3;
  check "multi-byte characters, error late"
    (String.concat "" (List.init 150 (fun _ -> "\xE2\x82\xAC")))
    300

let test_window_end_marker () =
  let input =
    String.init 400 (fun i -> Char.chr (Char.code 'a' + (i mod 26)))
  in
  match
    Parseff.parse input (fun () ->
        ignore (Parseff.take 279);
        ignore (Parseff.char '!')
    )
  with
  | Error e ->
      let row =
        List.nth (String.split_on_char '\n' (Parseff.render_error e)) 1
      in
      Alcotest.(check bool)
        "row ends with a marker" true
        (String.ends_with ~suffix:"..." row)
  | Ok _ ->
      Alcotest.fail "expected an error"

let test_lone_carriage_return () =
  match
    Parseff.parse "ab\r" (fun () ->
        ignore (Parseff.consume "ab\r");
        ignore (Parseff.char 'c')
    )
  with
  | Error e ->
      let expected =
        lines
          [
            "1:4: [error] expected 'c', found end of input";
            "1 | ab";
            "  |    ^";
          ]
      in
      Alcotest.(check string)
        "caret under the header column" expected (Parseff.render_error e);
      Alcotest.(check string)
        "same with the source" expected
        (Parseff.render_error ~source:"ab\r" e)
  | Ok _ ->
      Alcotest.fail "expected an error"

let test_found_with_data_left () =
  let parser () =
    ignore
      (Parseff.or_
         (fun () -> Parseff.take 5)
         (fun () -> String.make 1 (Parseff.char 'x'))
         ()
      )
  in
  check_render "bytes remain"
    (lines
       [
         "input.json:1:1: [error] expected 5 bytes or 'x', found 'abc'";
         "1 | abc";
         "  | ^^^";
       ]
    )
    (render_error "abc" parser);
  check_render "nothing remains"
    (lines
       [
         "input.json:1:1: [error] expected 5 bytes or 'x', found end of input";
         "1 |";
         "  | ^";
       ]
    )
    (render_error "" parser)

let test_c1_controls_are_escaped () =
  match Parseff.parse "\xC2\x85" (fun () -> ignore (Parseff.char 'a')) with
  | Error e ->
      Alcotest.(check string)
        "message" "expected 'a', found '\\u{0085}'" (Parseff.error_message e)
  | Ok _ ->
      Alcotest.fail "expected an error"

let test_built_in_failures_are_expected_items () =
  let message input parser =
    match Parseff.parse input parser with
    | Error e ->
        Parseff.error_message e
    | Ok _ ->
        "ok"
  in
  Alcotest.(check string)
    "regex without label"
    "expected text matching the pattern or \"x\", found '?'"
    (message "?" (fun () ->
         ignore
           (Parseff.or_
              (fun () -> Parseff.match_regex (Re.compile (Re.rep1 Re.digit)))
              (fun () -> Parseff.consume "x")
              ()
           )
     )
    );
  Alcotest.(check string)
    "int16 mismatch" "expected int16be 0x0203 or \"x\", found '\\x00'"
    (message "\x00\x01" (fun () ->
         ignore
           (Parseff.or_
              (fun () ->
                Parseff.BE.int16 0x0203;
                ""
              )
              (fun () -> Parseff.consume "x")
              ()
           )
     )
    );
  Alcotest.(check string)
    "invalid UTF-8" "expected valid UTF-8 or 'x', found '\\xFF'"
    (message "\xFF" (fun () ->
         ignore
           (Parseff.or_
              (fun () ->
                ignore (Parseff.Utf8.satisfy (fun _ -> true) ~label:"letter");
                'a'
              )
              (fun () -> Parseff.char 'x')
              ()
           )
     )
    );
  Alcotest.(check string)
    "empty one_of" "no alternative matched"
    (message "z" (Parseff.one_of []))

let source_of_chunks ?(chunk = 7) input =
  let pos = ref 0 in
  Parseff.Source.of_chunks (fun () ->
      if !pos >= String.length input then
        None
      else begin
        let n = min chunk (String.length input - !pos) in
        let s = String.sub input !pos n in
        pos := !pos + n;
        Some s
      end
  )

let numbered_lines n =
  String.concat "\n" (List.init n (fun i -> Printf.sprintf "line %d" (i + 1)))

let render_stream ?name ?source ?(window = 16) input parser =
  match
    Parseff.parse_source ~backtrack_window:window (source_of_chunks input)
      parser
  with
  | Stdlib.Ok _ ->
      "ok"
  | Stdlib.Error (e : Parseff.error) ->
      Parseff.render_error ?name ?source e

let message_of input parser =
  match Parseff.parse input parser with
  | Stdlib.Ok _ ->
      "ok"
  | Stdlib.Error (e : Parseff.error) ->
      Parseff.error_message e

let test_unknown_line_has_byte_header () =
  let input = numbered_lines 100 in
  let line_loop () =
    ignore
      (Parseff.many
         (fun () ->
           ignore (Parseff.take_while (fun c -> c <> '\n') ~label:"x");
           ignore (Parseff.char '\n')
         )
         ()
      );
    ignore (Parseff.take_while (fun _ -> true) ~label:"x")
  in
  let parser () =
    line_loop ();
    Parseff.fail ~from:3 "span starts long ago"
  in
  check_render "name and byte offset"
    "input: byte 3: [error] span starts long ago"
    (render_stream ~name:"input" input parser);
  check_render "no name" "byte 3: [error] span starts long ago"
    (render_stream input parser);
  check_render "~source gives the real line"
    (lines
       [ "input:1:4: [error] span starts long ago"; "1 | line 1"; "  |    ^^^" ]
    )
    (render_stream ~name:"input" ~source:input input parser)
  |> ignore;
  let small = "ab\ncd\nef" in
  let small_parser () =
    ignore (Parseff.take 6);
    ignore (Parseff.char 'q')
  in
  check_render "a known line is correct"
    "input:3:1: [error] expected 'q', found 'ef'"
    (List.hd
       (String.split_on_char '\n'
          (render_stream ~name:"input" ~window:4096 small small_parser)
       )
    )

let test_context_column_in_code_points () =
  let input = "\xCE\xBB\xCE\xBB\xCE\xBB[x" in
  let parser () =
    ignore (Parseff.take 6);
    Parseff.context ~delimited:true "arr" (fun () ->
        ignore (Parseff.char '[');
        ignore (Parseff.char 'q')
    )
  in
  let expected =
    lines
      [
        "input.json:1:5: [error] expected 'q', found 'x'";
        "1 | \xCE\xBB\xCE\xBB\xCE\xBB[x";
        "  |     ^";
        "  * while parsing arr, from 1:4";
      ]
  in
  check_render "code points" expected (render input parser);
  check_render "code points with ~source" expected
    (render ~source:input input parser)

let test_take_reports_what_is_left () =
  let take n () = ignore (Parseff.take n) in
  Alcotest.(check string)
    "fewer bytes" "expected 5 bytes, found only 3 (end of input)"
    (message_of "abc" (take 5));
  Alcotest.(check string)
    "nothing left" "expected 5 bytes, found end of input"
    (message_of "" (take 5));
  Alcotest.(check string)
    "a newline is not shown" "expected 5 bytes, found only 1 (end of input)"
    (message_of "\n" (take 5));
  Alcotest.(check string)
    "the rest holds a newline" "expected 5 bytes, found only 3 (end of input)"
    (message_of "ab\n" (take 5));
  Alcotest.(check string)
    "a huge count is exact"
    "expected 4611686018427387903 bytes, found only 2 (end of input)"
    (message_of "abc" (fun () ->
         ignore (Parseff.any_char ());
         ignore (Parseff.take max_int)
     )
    );
  check_render "rendered"
    (lines
       [
         "input.json:1:1: [error] expected 5 bytes, found only 3 (end of input)";
         "1 | abc";
         "  | ^^^";
       ]
    )
    (render_error "abc" (take 5));
  ( match Parseff.parse "abc" (take (-1)) with
  | _ ->
      Alcotest.fail "expected Invalid_argument"
  | exception Invalid_argument msg ->
      Alcotest.(check string)
        "negative count" "Parseff.take: count must be non-negative" msg
  );
  Alcotest.(check string)
    "streaming" "expected 5 bytes, found only 3 (end of input)"
    ( match Parseff.parse_source (source_of_chunks "abc") (take 5) with
    | Stdlib.Error (e : Parseff.error) ->
        Parseff.error_message e
    | Stdlib.Ok _ ->
        "ok"
    )

let test_int_mismatch_shows_the_value () =
  let check label expected input parser =
    Alcotest.(check string) label expected (message_of input parser)
  in
  check "BE.int16" "expected int16be 0x0001, found 0x0002" "\x00\x02" (fun () ->
      Parseff.BE.int16 1
  );
  check "LE.int16" "expected int16le 0x0001, found 0x0200" "\x00\x02" (fun () ->
      Parseff.LE.int16 1
  );
  check "BE.int32" "expected int32be 0x00000001, found 0x00000002"
    "\x00\x00\x00\x02" (fun () -> Parseff.BE.int32 1l
  );
  check "LE.int32" "expected int32le 0x00000001, found 0x02000000"
    "\x00\x00\x00\x02" (fun () -> Parseff.LE.int32 1l
  );
  check "BE.int64"
    "expected int64be 0x0000000000000001, found 0x0000000000000002"
    "\x00\x00\x00\x00\x00\x00\x00\x02" (fun () -> Parseff.BE.int64 1L
  );
  check "LE.int64"
    "expected int64le 0x0000000000000001, found 0x0200000000000000"
    "\x00\x00\x00\x00\x00\x00\x00\x02" (fun () -> Parseff.LE.int64 1L
  );
  check "short input" "expected 2 bytes, found only 1 (end of input)" "\x00"
    (fun () -> Parseff.BE.int16 1
  );
  check_render "rendered"
    (lines
       [
         "input.json:1:1: [error] expected int16be 0x0001, found 0x0002";
         "1 | \\x00\\x02";
         "  | ^^^^^^^^";
       ]
    )
    (render_error "\x00\x02" (fun () -> Parseff.BE.int16 1))

let test_quote_escapes () =
  let check label expected input parser =
    Alcotest.(check string) label expected (message_of input parser)
  in
  check "single quote expected" "expected '\\'', found 'x'" "x" (fun () ->
      ignore (Parseff.char '\'')
  );
  check "single quote found" "expected 'a', found '\\''" "'x" (fun () ->
      ignore (Parseff.char 'a')
  );
  check "raw byte expected" "expected '\\xE9', found 'x'" "x" (fun () ->
      ignore (Parseff.char '\xE9')
  );
  check "double quote literal" "expected \"it's\", found 'x'" "x" (fun () ->
      ignore (Parseff.consume "it's")
  )

let test_user_error_without_printer () =
  match Parseff.parse "x" (fun () -> Parseff.error Boom) with
  | Stdlib.Error e ->
      Alcotest.(check string)
        "message" "custom error (no ~custom printer given)"
        (Parseff.error_message e);
      check_render "render"
        (lines
           [
             "input:1:1: [error] custom error (no ~custom printer given)";
             "1 | x";
             "  | ^";
           ]
        )
        (Parseff.render_error ~name:"input" e)
  | Stdlib.Ok _ ->
      Alcotest.fail "expected an error"

let test_expected_list_is_capped () =
  let alternatives n =
    List.init n (fun i () ->
        ignore (Parseff.consume (Printf.sprintf "w%02d" (i + 1)))
    )
  in
  let shown k =
    String.concat ", "
      (List.init k (fun i -> Printf.sprintf "\"w%02d\"" (i + 1)))
  in
  Alcotest.(check string)
    "eight items are all listed"
    ("expected " ^ shown 7 ^ ", or \"w08\", found 'zzz'")
    (message_of "zzz" (Parseff.one_of (alternatives 8)));
  Alcotest.(check string)
    "one more is singular"
    ("expected " ^ shown 8 ^ ", or 1 more alternative, found 'zzz'")
    (message_of "zzz" (Parseff.one_of (alternatives 9)));
  Alcotest.(check string)
    "several more are plural"
    ("expected " ^ shown 8 ^ ", or 4 more alternatives, found 'zzz'")
    (message_of "zzz" (Parseff.one_of (alternatives 12)));
  check_render "the header follows"
    (lines
       [
         "input.json:1:1: [error] expected " ^ shown 8
         ^ ", or 1 more alternative, found 'zzz'";
         "1 | zzz";
         "  | ^^^";
       ]
    )
    (render_error "zzz" (Parseff.one_of (alternatives 9)))

let test_excerpt_escapes () =
  let expect_char = fun () -> ignore (Parseff.char 'a') in
  check_render "escape byte"
    (lines
       [
         "input.json:1:1: [error] expected 'a', found '\\x1B'";
         "1 | \\x1B[0m";
         "  | ^^^^";
       ]
    )
    (render_error "\x1b[0m" expect_char);
  check_render "invalid byte before the error"
    (lines
       [
         "input.json:1:4: [error] expected 'q', found 'cd'";
         "1 | ab\\xFFcd";
         "  |       ^^";
       ]
    )
    (render_error "ab\xFFcd" (fun () ->
         ignore (Parseff.take 3);
         ignore (Parseff.char 'q')
     )
    );
  check_render "invalid byte under the caret"
    (lines
       [
         "input.json:1:3: [error] expected 'q', found '\\xFF'";
         "1 | ab\\xFFcd";
         "  |   ^^^^";
       ]
    )
    (render_error "ab\xFFcd" (fun () ->
         ignore (Parseff.take 2);
         ignore (Parseff.char 'q')
     )
    );
  check_render "C1 control"
    (lines
       [
         "input.json:1:1: [error] expected 'a', found '\\u{0085}'";
         "1 | \\u{0085}";
         "  | ^^^^^^^^";
       ]
    )
    (render_error "\xC2\x85" expect_char);
  check_render "lone carriage return"
    (lines
       [
         "input.json:1:5: [error] expected 'q', found 'd'";
         "1 | ab\\rcd";
         "  |      ^";
       ]
    )
    (render_error "ab\rcd" (fun () ->
         ignore (Parseff.take 4);
         ignore (Parseff.char 'q')
     )
    );
  check_render "corner bar after an escape"
    (lines
       [
         "input.json:3:3: [error] expected ']', found 'x'";
         "1 |   \\x01[";
         "  | ╭─────╯";
         "2 | │   1,";
         "3 | │   x";
         "  | ╰───^";
         "  * while parsing list, from 1:2";
       ]
    )
    (render_error "\x01[\n  1,\n  x" (fun () ->
         ignore (Parseff.take 1);
         Parseff.context ~delimited:true "list" (fun () ->
             ignore (Parseff.char '[');
             ignore (Parseff.consume "\n  1,\n  ");
             ignore (Parseff.char ']')
         )
     )
    )

let test_invisible_code_points_are_escaped () =
  let found input = message_of input (fun () -> ignore (Parseff.char 'a')) in
  let expect label code input =
    Alcotest.(check string)
      label
      (Printf.sprintf "expected 'a', found '\\u{%s}'" code)
      (found input)
  in
  expect "no-break space" "00A0" "\xC2\xA0x";
  expect "zero width space" "200B" "\xE2\x80\x8Bx";
  expect "byte order mark" "FEFF" "\xEF\xBB\xBFx";
  expect "line separator" "2028" "\xE2\x80\xA8x";
  expect "paragraph separator" "2029" "\xE2\x80\xA9x";
  expect "combining mark first" "0301" "\xCC\x81x";
  let expected input =
    message_of "x" (fun () -> ignore (Parseff.consume input))
  in
  Alcotest.(check string)
    "combining mark first in a literal" "expected \"\\u{0301}a\", found 'x'"
    (expected "\xCC\x81a");
  Alcotest.(check string)
    "combining mark inside a literal stays" "expected \"e\xCC\x81\", found 'x'"
    (expected "e\xCC\x81");
  Alcotest.(check string)
    "Utf8.char" "expected '\\u{200B}', found 'x'"
    (message_of "x" (fun () -> ignore (Parseff.Utf8.char (Uchar.of_int 0x200B))))

let test_no_row_has_trailing_spaces () =
  let outputs =
    [
      render_error "a\n\nx" (fun () ->
          ignore (Parseff.take 3);
          ignore (Parseff.char 'q')
      );
      render_error "" (fun () -> ignore (Parseff.char 'q'));
      render_error "[\n\n1 2]" ctx_json_parser;
      render_error "abc" (fun () ->
          ignore (Parseff.take 1);
          Parseff.fail ""
      );
    ]
  in
  List.iter
    (fun out ->
      List.iter
        (fun row ->
          Alcotest.(check bool)
            ("no trailing space in " ^ String.escaped row)
            false
            (String.length row > 0 && row.[String.length row - 1] = ' ')
        )
        (String.split_on_char '\n' out)
    )
    outputs;
  check_render "empty source row"
    (lines
       [
         "input.json:3:1: [error] expected 'q', found 'x'";
         "2 |";
         "3 | x";
         "  | ^";
       ]
    )
    (render_error "a\n\nx" (fun () ->
         ignore (Parseff.take 3);
         ignore (Parseff.char 'q')
     )
    );
  check_render "empty message"
    (lines [ "input.json:1:2: [error]"; "1 | abc"; "  |  ^" ])
    (render_error "abc" (fun () ->
         ignore (Parseff.take 1);
         Parseff.fail ""
     )
    )

let test_truncated_found_keeps_quotes_closed () =
  Alcotest.(check string)
    "dots outside the quotes"
    ("expected 'q', found '" ^ String.make 32 'x' ^ "'...")
    (message_of (String.make 40 'x') (fun () -> ignore (Parseff.char 'q')))

let test_no_excerpt_rows_are_indented_by_two () =
  let input = numbered_lines 100 in
  let line_loop () =
    ignore
      (Parseff.many
         (fun () ->
           ignore (Parseff.take_while (fun c -> c <> '\n') ~label:"x");
           ignore (Parseff.char '\n')
         )
         ()
      );
    ignore (Parseff.take_while (fun _ -> true) ~label:"x")
  in
  check_render "note"
    (lines [ "input: byte 3: [error] span starts long ago"; "  * note: hint" ])
    (render_stream ~name:"input" input (fun () ->
         line_loop ();
         Parseff.fail ~from:3
           ~notes:[ { Parseff.at = None; text = "hint" } ]
           "span starts long ago"
     )
    );
  check_render "context"
    (lines
       [
         "input: byte 3: [error] span starts long ago";
         "  * while parsing block, from byte 0";
       ]
    )
    (render_stream ~name:"input" input (fun () ->
         Parseff.context "block" (fun () ->
             line_loop ();
             Parseff.fail ~from:3 "span starts long ago"
         )
     )
    )

let test_limits_keep_frames_and_give_help () =
  let rec nest () =
    Parseff.rec_ (fun () ->
        Parseff.context "nest" (fun () ->
            ignore (Parseff.char '[');
            nest ()
        )
    )
  in
  ( match Parseff.parse ~max_depth:3 "[[[[[[" nest with
  | Stdlib.Error (e : Parseff.error) ->
      check_render "depth"
        (lines
           [
             "1:4: [error] maximum nesting depth 3 exceeded";
             "1 | [[[[[[";
             "  |    ^";
             "  * while parsing nest, from 1:3";
             "  * while parsing nest, from 1:2";
             "  * while parsing nest, from 1:1";
             "  * help: raise ~max_depth";
           ]
        )
        (Parseff.render_error e)
  | Stdlib.Ok _ ->
      Alcotest.fail "expected an error"
  );
  let input = numbered_lines 100 in
  let parser () =
    ignore (Parseff.take 7);
    Parseff.context ~delimited:true "block" (fun () ->
        ignore (Parseff.take 7);
        ignore (Parseff.take 100000)
    )
  in
  check_render "backtrack window"
    (lines
       [
         "input:3:1: [error] backtrack window of 16 bytes exceeded";
         "3 | line 3";
         "  | ^";
         "  * while parsing block, from byte 7";
         "  * help: call commit or cut earlier, or raise ~backtrack_window";
       ]
    )
    (render_stream ~name:"input" input parser)

let test_one_hidden_frame_is_printed () =
  let rec nest n () =
    ignore (Parseff.char 'a');
    if n = 0 then
      ignore (Parseff.char 'x')
    else
      Parseff.context (Printf.sprintf "level%d" n) (nest (n - 1))
  in
  let out = render (String.make 7 'a') (nest 6) in
  Alcotest.(check bool)
    "no 'and 1 more' row" false
    (List.mem "  * ... and 1 more" (String.split_on_char '\n' out));
  Alcotest.(check bool)
    "the sixth context is printed" true
    (List.mem "  * while parsing level6, from 1:2"
       (String.split_on_char '\n' out)
    )

let test_regex_label_without_label () =
  Alcotest.(check string)
    "unlabeled regex" "expected text matching the pattern, found '!'"
    (message_of "!" (fun () ->
         ignore (Parseff.match_regex (Re.compile (Re.rep1 Re.digit)))
     )
    )

let test_built_in_labels () =
  let message input parser = message_of input parser in
  Alcotest.(check (list string))
    "labels"
    [
      "expected digit, found 'x'";
      "expected letter, found '1'";
      "expected alphanumeric, found '!'";
      "expected whitespace, found 'x'";
      "expected any character, found end of input";
    ]
    [
      message "x" (fun () -> ignore (Parseff.digit ()));
      message "1" (fun () -> ignore (Parseff.letter ()));
      message "!" (fun () -> ignore (Parseff.alphanum ()));
      message "x" (fun () -> ignore (Parseff.whitespace ~at_least:1 ()));
      message "" (fun () -> ignore (Parseff.any_char ()));
    ]

let test_gap_row_is_in_the_gutter () =
  let input =
    "foo(\n" ^ String.concat "" (List.init 10 (fun _ -> "a,\n")) ^ "d"
  in
  let parser () =
    ignore (Parseff.consume input);
    Parseff.fail ~notes:[ paren_note 3 ] "missing )"
  in
  check_render "gap outside a bar"
    (lines
       [
         "input.json:12:2: [error] missing )";
         " 1 | foo(";
         "   |    - to match this";
         "   | ...";
         "11 | a,";
         "12 | d";
         "   |  ^";
       ]
    )
    (render_error input parser)

let test_empty_one_of_adds_no_item () =
  Alcotest.(check string)
    "alone" "no alternative matched"
    (message_of "x" (Parseff.one_of []));
  check_render "alone, rendered"
    (lines
       [ "input.json:1:1: [error] no alternative matched"; "1 | x"; "  | ^" ]
    )
    (render_error "x" (Parseff.one_of []));
  ( match Parseff.parse "x" (Parseff.one_of []) with
  | Stdlib.Error { kind = Failure m; _ } ->
      Alcotest.(check string) "kind" "no alternative matched" m
  | _ ->
      Alcotest.fail "expected a Failure"
  );
  Alcotest.(check string)
    "next to another failure" "expected \"a\", found 'x'"
    (message_of "x" (fun () ->
         ignore
           (Parseff.or_
              (fun () -> Parseff.one_of [] ())
              (fun () -> Parseff.consume "a")
              ()
           )
     )
    );
  Alcotest.(check string)
    "inside one_of" "expected 'b' or \"a\", found 'x'"
    (message_of "x" (fun () ->
         ignore
           (Parseff.one_of
              [
                Parseff.one_of [];
                (fun () -> ignore (Parseff.consume "a"));
                (fun () -> ignore (Parseff.char 'b'));
              ]
              ()
           )
     )
    );
  Alcotest.(check string)
    "still backtracks" "ok"
    ( match
        Parseff.parse "x" (fun () ->
            Parseff.or_ (fun () -> Parseff.one_of [] ()) (fun () -> "ok") ()
        )
      with
    | Stdlib.Ok s ->
        s
    | Stdlib.Error _ ->
        "error"
    )

let test_position_is_known_or_absent () =
  ( match
      Parseff.parse "ab\n\xCE\xBB!" (fun () ->
          ignore (Parseff.take 5);
          ignore (Parseff.char 'q')
      )
    with
  | Stdlib.Error (e : Parseff.error) -> (
      match e.location with
      | Some location ->
          Alcotest.(check (pair int int))
            "line and column" (2, 3)
            (location.line, location.col);
          Alcotest.(check string)
            "the header column counts characters" "2:2: [error]"
            (String.sub (Parseff.render_error e) 0 12)
      | None ->
          Alcotest.fail "expected a position"
    )
  | Stdlib.Ok _ ->
      Alcotest.fail "expected an error"
  );
  ( match
      Parseff.parse_source ~backtrack_window:200
        (chunk_source ~chunk:64 lines_text)
        (from_before_the_buffer_parser `Fail)
    with
  | Stdlib.Error (e : Parseff.error) ->
      Alcotest.(check bool)
        "past the buffer has no position" true (e.location = None)
  | Stdlib.Ok _ ->
      Alcotest.fail "expected an error"
  );
  match
    Parseff.parse_source ~backtrack_window:4096 (source_of_chunks "ab\ncd")
      (fun () ->
        ignore (Parseff.take 4);
        ignore (Parseff.char 'q')
    )
  with
  | Stdlib.Error (e : Parseff.error) -> (
      match e.location with
      | Some location ->
          Alcotest.(check (pair int int))
            "a known line is exact" (2, 2)
            (location.line, location.col)
      | None ->
          Alcotest.fail "expected a position"
    )
  | Stdlib.Ok _ ->
      Alcotest.fail "expected an error"

let test_overflow_count_is_an_int () =
  let alternatives =
    List.init 100 (fun i () ->
        ignore (Parseff.consume (Printf.sprintf "item%03d" i))
    )
  in
  match Parseff.parse "zzz" (Parseff.one_of alternatives) with
  | Stdlib.Error (e : Parseff.error) -> (
      match e.kind with
      | Expected { items = _, rest; more; _ } ->
          Alcotest.(check (pair int int))
            "items and more" (64, 36)
            (List.length rest + 1, more);
          Alcotest.(check bool)
            "no item is a count label" false
            (List.exists (function Parseff.Label _ -> true | _ -> false) rest)
      | _ ->
          Alcotest.fail "expected Expected"
    )
  | Stdlib.Ok _ ->
      Alcotest.fail "expected an error"

let test_mixed_int_widths_fall_back_to_text () =
  let message ?(input = "\x00\x00\x00\x02") parser = message_of input parser in
  Alcotest.(check string)
    "different widths"
    "expected int16be 0x0001 or int32be 0x00000001, found '\\x00'"
    (message (fun () ->
         ignore
           (Parseff.or_
              (fun () -> Parseff.BE.int16 1)
              (fun () -> Parseff.BE.int32 1l)
              ()
           )
     )
    );
  Alcotest.(check string)
    "different endianness"
    "expected int16be 0x0001 or int16le 0x0001, found '\\x00'"
    (message ~input:"\x00\x02\x00\x00" (fun () ->
         ignore
           (Parseff.or_
              (fun () -> Parseff.BE.int16 1)
              (fun () -> Parseff.LE.int16 1)
              ()
           )
     )
    );
  Alcotest.(check string)
    "same width and value"
    "expected int16be 0x0001 or int16be 0x0003, found 0x0000"
    (message (fun () ->
         ignore
           (Parseff.or_
              (fun () -> Parseff.BE.int16 1)
              (fun () -> Parseff.BE.int16 3)
              ()
           )
     )
    )

let test_source_and_user_text_keep_trailing_spaces () =
  check_render "source line ending in spaces"
    (lines
       [
         "input.json:1:6: [error] expected 'q', found end of input";
         "1 | ab   ";
         "  |      ^";
       ]
    )
    (render_error "ab   " (fun () ->
         ignore (Parseff.take 5);
         ignore (Parseff.char 'q')
     )
    );
  check_render "found spaces"
    (lines
       [
         "input.json:1:3: [error] expected 'q', found ' '";
         "1 | ab   ";
         "  |   ^";
       ]
    )
    (render_error "ab   " (fun () ->
         ignore (Parseff.take 2);
         ignore (Parseff.char 'q')
     )
    );
  check_render "user text"
    (lines [ "input.json:1:1: [error] msg "; "1 | x"; "  | ^"; "  * note: n " ])
    (render_error "x" (fun () ->
         Parseff.fail ~notes:[ { Parseff.at = None; text = "n " } ] "msg "
     )
    )

let test_excerpt_escapes_invisible_code_points () =
  let expect_char = fun () -> ignore (Parseff.char 'a') in
  check_render "no-break space"
    (lines
       [
         "input.json:1:1: [error] expected 'a', found '\\u{00A0}'";
         "1 | \\u{00A0}x";
         "  | ^^^^^^^^";
       ]
    )
    (render_error "\xC2\xA0x" expect_char);
  check_render "zero width space before the error"
    (lines
       [
         "input.json:1:2: [error] expected 'q', found 'x'";
         "1 | \\u{200B}x";
         "  |         ^";
       ]
    )
    (render_error "\xE2\x80\x8Bx" (fun () ->
         ignore (Parseff.take 3);
         ignore (Parseff.char 'q')
     )
    );
  check_render "combining mark at the start of the row"
    (lines
       [
         "input.json:1:1: [error] expected 'a', found '\\u{0301}'";
         "1 | \\u{0301}x";
         "  | ^^^^^^^^";
       ]
    )
    (render_error "\xCC\x81x" expect_char);
  check_render "combining mark after a base character stays"
    (lines
       [
         "input.json:1:3: [error] expected 'q', found 'x'";
         "1 | e\xCC\x81x";
         "  |   ^";
       ]
    )
    (render_error "e\xCC\x81x" (fun () ->
         ignore (Parseff.take 3);
         ignore (Parseff.char 'q')
     )
    )

let test_window_error_points_at_the_parser () =
  let input = numbered_lines 100 in
  let parser () =
    ignore (Parseff.take 7);
    Parseff.context ~delimited:true "block" (fun () ->
        ignore (Parseff.take 7);
        ignore (Parseff.take 100000)
    )
  in
  match
    Parseff.parse_source ~backtrack_window:16 (source_of_chunks input) parser
  with
  | Stdlib.Error (e : Parseff.error) ->
      Alcotest.(check int) "position is the parser's" 14 e.range.start;
      Alcotest.(check bool) "the line is known" true (e.location <> None);
      Alcotest.(check bool)
        "no byte header" false
        (String.starts_with ~prefix:"byte" (Parseff.render_error e))
  | Stdlib.Ok _ ->
      Alcotest.fail "expected an error"

let test_duplicates_do_not_hide_alternatives () =
  let parser () =
    for _ = 1 to 70 do
      ignore (Parseff.optional (fun () -> Parseff.char 'a') ())
    done;
    Parseff.one_of
      [
        (fun () -> Parseff.char 'x');
        (fun () -> Parseff.char 'y');
        (fun () -> Parseff.char 'z');
      ]
      ()
  in
  Alcotest.(check string)
    "every distinct alternative is listed"
    "expected 'a', 'x', 'y', or 'z', found '!'" (message_of "!" parser)

let test_expect_relabels_after_duplicates () =
  let parser () =
    for _ = 1 to 70 do
      ignore (Parseff.optional (fun () -> Parseff.char 'a') ())
    done;
    Parseff.expect "thing" (fun () ->
        Parseff.one_of
          [
            (fun () -> Parseff.char 'x');
            (fun () -> Parseff.char 'y');
            (fun () -> Parseff.char 'z');
          ]
          ()
    )
  in
  Alcotest.(check string)
    "expect still relabels" "expected thing or 'a', found '!'"
    (message_of "!" parser)

let test_char_and_one_byte_literal_print_once () =
  Alcotest.(check string)
    "one item" "expected ',', found '!'"
    (message_of "!" (fun () ->
         Parseff.one_of
           [
             (fun () -> ignore (Parseff.char ','));
             (fun () -> ignore (Parseff.consume ","));
           ]
           ()
     )
    )

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
          test_case "item buffer overflow counts distinct" `Quick
            test_item_buffer_overflow_counts_distinct;
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
          test_case "no context, no contexts" `Quick
            test_context_chain_without_context;
          test_case "chain after a context closes" `Quick
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
          test_case "success leaves outer contexts" `Quick
            test_cut_does_not_leave_context_on_success;
          test_case "outer contexts sealed on failure only" `Quick
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
          test_case "C opener mid-line" `Quick test_render_opener_mid_line;
          test_case "C opener after a tab" `Quick test_render_opener_tab;
          test_case "C opener after a multi-byte char" `Quick
            test_render_opener_utf8;
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
          test_case "context cap" `Quick test_render_context_cap;
          test_case "invalid UTF-8 and control bytes" `Quick
            test_render_invalid_utf8_and_controls;
          test_case "window cut" `Quick test_render_window_cut;
          test_case "source" `Quick test_render_source;
          test_case "other kinds" `Quick test_render_other_kinds;
          test_case "streaming opener dropped" `Quick
            test_render_streaming_opener_dropped;
        ]
      );
      ( "error extras",
        [
          test_case "fail ~from span" `Quick test_fail_from_span;
          test_case "span is normalized" `Quick
            test_fail_from_after_position_is_normalized;
          test_case "note line" `Quick test_loose_note;
          test_case "underline on the error line" `Quick
            test_secondary_underline_same_line;
          test_case "underline on earlier lines" `Quick
            test_secondary_underline_earlier_lines;
          test_case "error ~from ~notes" `Quick test_error_from_and_notes;
          test_case "fail contexts" `Quick test_fail_frames;
          test_case "match_regex label" `Quick test_match_regex_label;
          test_case "note on a dropped line" `Quick test_note_on_dropped_line;
        ]
      );
      ( "public errors",
        [
          test_case "exhaustive kind match" `Quick test_exhaustive_kind_match;
          test_case "end of input keeps the expected set" `Quick
            test_end_of_input_keeps_expected_set;
          test_case "user errors by pattern" `Quick test_user_errors_by_pattern;
          test_case "migration pattern" `Quick test_migration_pattern;
          test_case "diagnostics carry the error" `Quick
            test_diagnostics_carry_the_error;
        ]
      );
      ( "review fixes",
        [
          test_case "window error builds" `Quick
            test_backtrack_window_error_renders;
          test_case "notes past the line end" `Quick
            test_notes_past_the_line_end;
          test_case "from older than the buffer" `Quick
            test_from_older_than_the_buffer;
          test_case "look_ahead probes are forgotten" `Quick
            test_look_ahead_probe_does_not_leak;
          test_case "multi-byte literal mismatch" `Quick
            test_multibyte_literal_mismatch;
          test_case "expect with commit inside" `Quick
            test_expect_with_commit_inside;
          test_case "commit without a frame" `Quick
            test_commit_without_frame_keeps_the_group;
          test_case "expect after far buffer overflow" `Quick
            test_expect_after_far_buffer_overflow;
          test_case "window markers agree" `Quick test_window_markers_agree;
          test_case "window end marker" `Quick test_window_end_marker;
          test_case "lone carriage return" `Quick test_lone_carriage_return;
          test_case "found with data left" `Quick test_found_with_data_left;
          test_case "C1 controls" `Quick test_c1_controls_are_escaped;
          test_case "built-in failures are expected items" `Quick
            test_built_in_failures_are_expected_items;
        ]
      );
      ( "output polish",
        [
          test_case "unknown line has a byte header" `Quick
            test_unknown_line_has_byte_header;
          test_case "context column in code points" `Quick
            test_context_column_in_code_points;
          test_case "take reports what is left" `Quick
            test_take_reports_what_is_left;
          test_case "int mismatch shows the value" `Quick
            test_int_mismatch_shows_the_value;
          test_case "quote escapes" `Quick test_quote_escapes;
          test_case "user error without printer" `Quick
            test_user_error_without_printer;
          test_case "expected list is capped" `Quick
            test_expected_list_is_capped;
          test_case "excerpt escapes" `Quick test_excerpt_escapes;
          test_case "invisible code points" `Quick
            test_invisible_code_points_are_escaped;
          test_case "no trailing spaces" `Quick test_no_row_has_trailing_spaces;
          test_case "truncated found" `Quick
            test_truncated_found_keeps_quotes_closed;
          test_case "no-excerpt rows" `Quick
            test_no_excerpt_rows_are_indented_by_two;
          test_case "limits keep contexts and give help" `Quick
            test_limits_keep_frames_and_give_help;
          test_case "one hidden context is printed" `Quick
            test_one_hidden_frame_is_printed;
          test_case "regex label" `Quick test_regex_label_without_label;
          test_case "built-in labels" `Quick test_built_in_labels;
          test_case "gap row in the gutter" `Quick test_gap_row_is_in_the_gutter;
          test_case "empty one_of adds no item" `Quick
            test_empty_one_of_adds_no_item;
          test_case "position is known or absent" `Quick
            test_position_is_known_or_absent;
          test_case "overflow count is an int" `Quick
            test_overflow_count_is_an_int;
          test_case "mixed int widths fall back to text" `Quick
            test_mixed_int_widths_fall_back_to_text;
          test_case "trailing spaces of source and user text" `Quick
            test_source_and_user_text_keep_trailing_spaces;
          test_case "excerpt escapes invisible code points" `Quick
            test_excerpt_escapes_invisible_code_points;
          test_case "window error points at the parser" `Quick
            test_window_error_points_at_the_parser;
          test_case "duplicates do not hide alternatives" `Quick
            test_duplicates_do_not_hide_alternatives;
          test_case "expect relabels after duplicates" `Quick
            test_expect_relabels_after_duplicates;
          test_case "char and one-byte literal print once" `Quick
            test_char_and_one_byte_literal_print_once;
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
