let parse_with_pos input parser =
  Parseff.parse input (fun () ->
      let value = parser () in
      (value, Parseff.position ())
  )

let test_consume_success () =
  match parse_with_pos "hello" (fun () -> Parseff.consume "hello") with
  | Ok (s, pos) ->
      Alcotest.(check string) "matched string" "hello" s;
      Alcotest.(check int) "final position" 5 pos
  | Error _ ->
      Alcotest.fail "Expected success"

let test_consume_failure () =
  match Parseff.parse "hello" (fun () -> Parseff.consume "world") with
  | Ok _ ->
      Alcotest.fail "Expected failure"
  | Error { pos; error = `Expected expected } ->
      Alcotest.(check int) "error position" 0 pos;
      Alcotest.(check string) "error message" "expected \"world\"" expected
  | Error _ ->
      Alcotest.fail "Unexpected error type"

let test_char_success () =
  match parse_with_pos "a" (fun () -> Parseff.char 'a') with
  | Ok (c, pos) ->
      Alcotest.(check char) "matched char" 'a' c;
      Alcotest.(check int) "final position" 1 pos
  | Error _ ->
      Alcotest.fail "Expected success"

let test_satisfy () =
  let is_digit c = c >= '0' && c <= '9' in
  match
    Parseff.parse "7" (fun () -> Parseff.satisfy is_digit ~label:"digit")
  with
  | Ok c ->
      Alcotest.(check char) "matched digit" '7' c
  | Error _ ->
      Alcotest.fail "Expected success"

let test_digit () =
  match Parseff.parse "9" Parseff.digit with
  | Ok n ->
      Alcotest.(check int) "parsed digit" 9 n
  | Error _ ->
      Alcotest.fail "Expected success"

let test_sequence () =
  let parser () =
    let a = Parseff.consume "hello" in
    let _ = Parseff.consume " " in
    let b = Parseff.consume "world" in
    (a, b)
  in
  match parse_with_pos "hello world" parser with
  | Ok ((a, b), pos) ->
      Alcotest.(check string) "first part" "hello" a;
      Alcotest.(check string) "second part" "world" b;
      Alcotest.(check int) "final position" 11 pos
  | Error _ ->
      Alcotest.fail "Expected success"

let test_alternation_left () =
  let parser () =
    Parseff.or_
      (fun () -> Parseff.consume "foo")
      (fun () -> Parseff.consume "bar")
      ()
  in
  match Parseff.parse "foo" parser with
  | Ok s ->
      Alcotest.(check string) "matched left" "foo" s
  | Error _ ->
      Alcotest.fail "Expected success"

let test_alternation_right () =
  let parser () =
    Parseff.or_
      (fun () -> Parseff.consume "foo")
      (fun () -> Parseff.consume "bar")
      ()
  in
  match Parseff.parse "bar" parser with
  | Ok s ->
      Alcotest.(check string) "matched right" "bar" s
  | Error _ ->
      Alcotest.fail "Expected success"

let test_alternation_failure () =
  let parser () =
    Parseff.or_
      (fun () -> Parseff.consume "foo")
      (fun () -> Parseff.consume "bar")
      ()
  in
  match Parseff.parse "baz" parser with
  | Ok _ ->
      Alcotest.fail "Expected failure"
  | Error { pos; error = `Expected expected } ->
      Alcotest.(check int) "error position" 0 pos;
      Alcotest.(check string)
        "error message" "expected \"foo\" or expected \"bar\"" expected
  | Error _ ->
      Alcotest.fail "Expected `Expected error variant"

let test_one_of_failure () =
  let parser =
    Parseff.one_of
      [
        (fun () -> Parseff.consume "if");
        (fun () -> Parseff.consume "else");
        (fun () -> Parseff.consume "while");
      ]
  in
  match Parseff.parse "xyz" parser with
  | Ok _ ->
      Alcotest.fail "Expected failure"
  | Error { pos; error = `Expected expected } ->
      (* Parse_error from "if" branch (which could check but mismatched) is
         preferred over Unexpected_eof from "else"/"while" branches (which were
         too short to even check) at the same position. *)
      Alcotest.(check int) "error position" 0 pos;
      Alcotest.(check string) "error message" "expected \"if\"" expected
  | Error _ ->
      Alcotest.fail "Expected `Expected"

let test_many_empty () =
  match Parseff.parse "" (Parseff.many Parseff.digit) with
  | Ok lst ->
      Alcotest.(check int) "empty list" 0 (List.length lst)
  | Error _ ->
      Alcotest.fail "Expected success"

let test_many_several () =
  match parse_with_pos "123" (Parseff.many Parseff.digit) with
  | Ok (lst, pos) ->
      Alcotest.(check (list int)) "digits" [ 1; 2; 3 ] lst;
      Alcotest.(check int) "final position" 3 pos
  | Error _ ->
      Alcotest.fail "Expected success"

let test_many1_success () =
  match Parseff.parse "456" (Parseff.many ~at_least:1 Parseff.digit) with
  | Ok lst ->
      Alcotest.(check (list int)) "digits" [ 4; 5; 6 ] lst
  | Error _ ->
      Alcotest.fail "Expected success"

let test_many1_failure () =
  match Parseff.parse "abc" (Parseff.many ~at_least:1 Parseff.digit) with
  | Ok _ ->
      Alcotest.fail "Expected failure"
  | Error { pos; error = `Expected expected } ->
      Alcotest.(check int) "error position" 0 pos;
      Alcotest.(check string) "error message" "expected digit" expected
  | Error _ ->
      Alcotest.fail "Unexpected error type"

let test_optional_some () =
  match Parseff.parse "x" (Parseff.optional Parseff.letter) with
  | Ok (Some c) ->
      Alcotest.(check char) "letter" 'x' c
  | Ok None ->
      Alcotest.fail "Expected Some"
  | Error _ ->
      Alcotest.fail "Expected success"

let test_optional_none () =
  match parse_with_pos "123" (Parseff.optional Parseff.letter) with
  | Ok (None, pos) ->
      Alcotest.(check int) "position unchanged" 0 pos
  | Ok (Some _, _) ->
      Alcotest.fail "Expected None"
  | Error _ ->
      Alcotest.fail "Expected success"

let test_end_of_input_success () =
  match Parseff.parse "" Parseff.end_of_input with
  | Ok () ->
      ()
  | Error _ ->
      Alcotest.fail "Expected success"

let test_end_of_input_failure () =
  match Parseff.parse "x" Parseff.end_of_input with
  | Ok _ ->
      Alcotest.fail "Expected failure"
  | Error { pos; error = `Expected expected } ->
      Alcotest.(check int) "error position" 0 pos;
      Alcotest.(check string) "error message" "expected end of input" expected
  | Error _ ->
      Alcotest.fail "Unexpected error type"

let test_look_ahead_success () =
  let parser () =
    let _ = Parseff.look_ahead Parseff.digit in
    let d = Parseff.digit () in
    d
  in
  match Parseff.parse "5" parser with
  | Ok n ->
      Alcotest.(check int) "digit" 5 n
  | Error _ ->
      Alcotest.fail "Expected success"

let test_look_ahead_no_consume () =
  let parser () =
    let _ = Parseff.look_ahead (fun () -> Parseff.consume "hello") in
    Parseff.consume "hello"
  in
  match Parseff.parse "hello" parser with
  | Ok s ->
      Alcotest.(check string) "matched" "hello" s
  | Error _ ->
      Alcotest.fail "Expected success"

let test_commit_noop_outside_frame () =
  match
    parse_with_pos "a" (fun () ->
        Parseff.commit ();
        Parseff.char 'a'
    )
  with
  | Ok (c, pos) ->
      Alcotest.(check char) "char" 'a' c;
      Alcotest.(check int) "position" 1 pos
  | Error _ ->
      Alcotest.fail "Expected success"

let test_commit_blocks_or_backtracking () =
  let parser () =
    Parseff.or_
      (fun () ->
        let _ = Parseff.consume "ab" in
        Parseff.commit ();
        let _ = Parseff.char 'x' in
        "left"
      )
      (fun () -> Parseff.consume "abcd")
      ()
  in
  match Parseff.parse "abcd" parser with
  | Ok _ ->
      Alcotest.fail "Expected committed branch failure"
  | Error { pos; error = `Expected msg } ->
      Alcotest.(check int) "error position" 2 pos;
      Alcotest.(check string) "error message" "expected 'x'" msg
  | Error _ ->
      Alcotest.fail "Unexpected error type"

let test_commit_stops_many_from_succeeding () =
  let item () =
    let _ = Parseff.char 'a' in
    let _ = Parseff.char ',' in
    Parseff.commit ();
    Parseff.char 'b'
  in
  match Parseff.parse "a,ba,c" (Parseff.many item) with
  | Ok _ ->
      Alcotest.fail "Expected committed repetition failure"
  | Error { pos; error = `Expected msg } ->
      Alcotest.(check int) "error position" 5 pos;
      Alcotest.(check string) "error message" "expected 'b'" msg
  | Error _ ->
      Alcotest.fail "Unexpected error type"

let test_commit_does_not_escape_look_ahead () =
  let parser () =
    Parseff.or_
      (fun () ->
        let _ =
          Parseff.look_ahead (fun () ->
              let _ = Parseff.consume "ab" in
              Parseff.commit ();
              Parseff.char 'x'
          )
        in
        Parseff.consume "abcd"
      )
      (fun () -> Parseff.consume "abcd")
      ()
  in
  match Parseff.parse "abcd" parser with
  | Ok s ->
      Alcotest.(check string) "fallback branch" "abcd" s
  | Error _ ->
      Alcotest.fail "Expected success"

let test_sep_by_empty () =
  match
    Parseff.parse "" (Parseff.sep_by Parseff.digit (fun () -> Parseff.char ','))
  with
  | Ok lst ->
      Alcotest.(check (list int)) "empty" [] lst
  | Error _ ->
      Alcotest.fail "Expected success"

let test_sep_by_one () =
  match
    Parseff.parse "5" (Parseff.sep_by Parseff.digit (fun () -> Parseff.char ','))
  with
  | Ok lst ->
      Alcotest.(check (list int)) "one element" [ 5 ] lst
  | Error _ ->
      Alcotest.fail "Expected success"

let test_sep_by_several () =
  match
    Parseff.parse "1,2,3"
      (Parseff.sep_by Parseff.digit (fun () -> Parseff.char ','))
  with
  | Ok lst ->
      Alcotest.(check (list int)) "three elements" [ 1; 2; 3 ] lst
  | Error _ ->
      Alcotest.fail "Expected success"

let test_between () =
  let parser =
    Parseff.between
      (fun () -> Parseff.char '(')
      (fun () -> Parseff.char ')')
      Parseff.digit
  in
  match parse_with_pos "(7)" parser with
  | Ok (n, pos) ->
      Alcotest.(check int) "parsed value" 7 n;
      Alcotest.(check int) "final position" 3 pos
  | Error _ ->
      Alcotest.fail "Expected success"

let test_between_failure_missing_close () =
  let parser =
    Parseff.between
      (fun () -> Parseff.char '(')
      (fun () -> Parseff.char ')')
      Parseff.digit
  in
  match Parseff.parse "(7" parser with
  | Ok _ ->
      Alcotest.fail "Expected failure"
  | Error { pos; error = `Unexpected_end_of_input } ->
      Alcotest.(check int) "error position" 2 pos
  | Error _ ->
      Alcotest.fail "Expected `Unexpected_end_of_input"

let test_end_by () =
  let parser = Parseff.end_by Parseff.digit (fun () -> Parseff.char ',') in
  match parse_with_pos "1,2,3," parser with
  | Ok (lst, pos) ->
      Alcotest.(check (list int)) "parsed list" [ 1; 2; 3 ] lst;
      Alcotest.(check int) "final position" 6 pos
  | Error _ ->
      Alcotest.fail "Expected success"

let test_end_by_empty () =
  let parser = Parseff.end_by Parseff.digit (fun () -> Parseff.char ',') in
  match parse_with_pos "" parser with
  | Ok (lst, pos) ->
      Alcotest.(check (list int)) "empty" [] lst;
      Alcotest.(check int) "final position" 0 pos
  | Error _ ->
      Alcotest.fail "Expected success"

let test_end_by1_failure () =
  let parser =
    Parseff.end_by ~at_least:1 Parseff.digit (fun () -> Parseff.char ',')
  in
  match Parseff.parse "" parser with
  | Ok _ ->
      Alcotest.fail "Expected failure"
  | Error { pos; error = `Unexpected_end_of_input } ->
      Alcotest.(check int) "error position" 0 pos
  | Error _ ->
      Alcotest.fail "Expected `Unexpected_end_of_input"

let test_end_by1_success () =
  let parser =
    Parseff.end_by ~at_least:1 Parseff.digit (fun () -> Parseff.char ',')
  in
  match parse_with_pos "1,2,3," parser with
  | Ok (lst, pos) ->
      Alcotest.(check (list int)) "parsed list" [ 1; 2; 3 ] lst;
      Alcotest.(check int) "final position" 6 pos
  | Error _ ->
      Alcotest.fail "Expected success"

let test_fold_left_left_assoc () =
  let num = Parseff.digit in
  let sub_op () =
    let _ = Parseff.char '-' in
    ( - )
  in
  match parse_with_pos "9-3-1" (Parseff.fold_left num sub_op) with
  | Ok (value, pos) ->
      Alcotest.(check int) "left associative" 5 value;
      Alcotest.(check int) "final position" 5 pos
  | Error _ ->
      Alcotest.fail "Expected success"

let test_fold_right_right_assoc () =
  let num = Parseff.digit in
  let sub_op () =
    let _ = Parseff.char '-' in
    ( - )
  in
  match parse_with_pos "9-3-1" (Parseff.fold_right num sub_op) with
  | Ok (value, pos) ->
      Alcotest.(check int) "right associative" 7 value;
      Alcotest.(check int) "final position" 5 pos
  | Error _ ->
      Alcotest.fail "Expected success"

let test_fold_left_otherwise () =
  let parser =
    Parseff.fold_left Parseff.digit
      (fun () ->
        let _ = Parseff.char '+' in
        ( + )
      )
      ~otherwise:42
  in
  match parse_with_pos "" parser with
  | Ok (value, pos) ->
      Alcotest.(check int) "otherwise value" 42 value;
      Alcotest.(check int) "final position" 0 pos
  | Error _ ->
      Alcotest.fail "Expected success"

let test_fold_left_non_otherwise () =
  let parser =
    Parseff.fold_left Parseff.digit
      (fun () ->
        let _ = Parseff.char '+' in
        ( + )
      )
      ~otherwise:42
  in
  match parse_with_pos "1+2+3" parser with
  | Ok (value, pos) ->
      Alcotest.(check int) "combined value" 6 value;
      Alcotest.(check int) "final position" 5 pos
  | Error _ ->
      Alcotest.fail "Expected success"

let test_fold_right_otherwise () =
  let parser =
    Parseff.fold_right Parseff.digit
      (fun () ->
        let _ = Parseff.char '+' in
        ( + )
      )
      ~otherwise:42
  in
  match parse_with_pos "" parser with
  | Ok (value, pos) ->
      Alcotest.(check int) "otherwise value" 42 value;
      Alcotest.(check int) "final position" 0 pos
  | Error _ ->
      Alcotest.fail "Expected success"

let test_fold_right_non_otherwise () =
  let parser =
    Parseff.fold_right Parseff.digit
      (fun () ->
        let _ = Parseff.char '^' in
        fun l r -> int_of_float (float_of_int l ** float_of_int r)
      )
      ~otherwise:42
  in
  match parse_with_pos "2^3^2" parser with
  | Ok (value, pos) ->
      Alcotest.(check int) "combined value" 512 value;
      Alcotest.(check int) "final position" 5 pos
  | Error _ ->
      Alcotest.fail "Expected success"

let test_fold_left_requires_one () =
  let parser =
    Parseff.fold_left Parseff.digit (fun () ->
        let _ = Parseff.char '+' in
        ( + )
    )
  in
  match Parseff.parse "" parser with
  | Ok _ ->
      Alcotest.fail "Expected failure"
  | Error { pos; error = `Unexpected_end_of_input } ->
      Alcotest.(check int) "error position" 0 pos
  | Error _ ->
      Alcotest.fail "Expected `Unexpected_end_of_input"

let test_fold_right_requires_one () =
  let parser =
    Parseff.fold_right Parseff.digit (fun () ->
        let _ = Parseff.char '+' in
        ( + )
    )
  in
  match Parseff.parse "" parser with
  | Ok _ ->
      Alcotest.fail "Expected failure"
  | Error { pos; error = `Unexpected_end_of_input } ->
      Alcotest.(check int) "error position" 0 pos
  | Error _ ->
      Alcotest.fail "Expected `Unexpected_end_of_input"

let test_count () =
  match Parseff.parse "abc" (Parseff.count 3 Parseff.letter) with
  | Ok lst ->
      Alcotest.(check (list char)) "three letters" [ 'a'; 'b'; 'c' ] lst
  | Error _ ->
      Alcotest.fail "Expected success"

let test_count_insufficient () =
  match Parseff.parse "ab" (Parseff.count 3 Parseff.letter) with
  | Ok _ ->
      Alcotest.fail "Expected failure"
  | Error { pos; error = `Unexpected_end_of_input } ->
      Alcotest.(check int) "error position" 2 pos
  | Error _ ->
      Alcotest.fail "Expected `Unexpected_end_of_input"

let test_parse_non_enforcing () =
  match Parseff.parse "abcXYZ" (fun () -> Parseff.consume "abc") with
  | Ok s ->
      Alcotest.(check string) "parsed prefix" "abc" s
  | Error _ ->
      Alcotest.fail "Expected success"

let test_parse_until_end_requires_eof () =
  let outcome =
    Parseff.parse_until_end "abcXYZ" (fun () -> Parseff.consume "abc")
  in
  match outcome with
  | Ok _ ->
      Alcotest.fail "Expected failure"
  | Error { pos; error = `Expected _; diagnostics } ->
      Alcotest.(check int) "error position" 3 pos;
      Alcotest.(check int) "no diagnostics" 0 (List.length diagnostics)
  | Error _ ->
      Alcotest.fail "Unexpected error type"

let test_parse_until_end_collects_diagnostics () =
  let parser () =
    Parseff.warn "start";
    let _ = Parseff.consume "ok" in
    Parseff.warn_at ~pos:0 "forced";
    42
  in
  let outcome = Parseff.parse_until_end "ok" parser in
  match outcome with
  | Ok (n, diagnostics) ->
      Alcotest.(check int) "value" 42 n;
      let got =
        List.map
          (fun ({ pos; diagnostic } : string Parseff.diagnostic) ->
            (pos, diagnostic)
          )
          diagnostics
      in
      Alcotest.(check (list (pair int string)))
        "diagnostics"
        [ (0, "start"); (0, "forced") ]
        got
  | Error _ ->
      Alcotest.fail "Expected success"

let test_diagnostics_rollback_on_or () =
  let parser () =
    Parseff.or_
      (fun () ->
        Parseff.warn "left";
        Parseff.consume "x"
      )
      (fun () ->
        Parseff.warn "right";
        Parseff.consume "b"
      )
      ()
  in
  let outcome = Parseff.parse_until_end "b" parser in
  match outcome with
  | Ok (_, diagnostics) ->
      let got =
        List.map
          (fun ({ diagnostic; _ } : string Parseff.diagnostic) -> diagnostic)
          diagnostics
      in
      Alcotest.(check (list string)) "only right diagnostic" [ "right" ] got
  | Error _ ->
      Alcotest.fail "Expected success"

let test_diagnostics_rollback_on_look_ahead () =
  let parser () =
    let _ =
      Parseff.look_ahead (fun () ->
          Parseff.warn "peek";
          Parseff.consume "a"
      )
    in
    Parseff.consume "a"
  in
  let outcome = Parseff.parse_until_end "a" parser in
  match outcome with
  | Ok (_, diagnostics) ->
      Alcotest.(check int) "no leaked diagnostics" 0 (List.length diagnostics)
  | Error _ ->
      Alcotest.fail "Expected success"

let test_diagnostics_rollback_on_many_stop () =
  let parser () =
    let ds =
      Parseff.many
        (fun () ->
          Parseff.warn "iter";
          Parseff.digit ()
        )
        ()
    in
    let _ = Parseff.char 'a' in
    ds
  in
  let outcome = Parseff.parse_until_end "12a" parser in
  match outcome with
  | Ok (ds, diagnostics) ->
      Alcotest.(check (list int)) "digits" [ 1; 2 ] ds;
      Alcotest.(check int)
        "only successful iterations" 2 (List.length diagnostics)
  | Error _ ->
      Alcotest.fail "Expected success"

let test_error_contains_diagnostics () =
  let outcome =
    Parseff.parse_until_end "y" (fun () ->
        Parseff.warn "before-failure";
        Parseff.consume "x"
    )
  in
  match outcome with
  | Ok _ ->
      Alcotest.fail "Expected failure"
  | Error { pos; error = `Expected _; diagnostics } ->
      Alcotest.(check int) "error position" 0 pos;
      let got =
        List.map
          (fun ({ diagnostic; _ } : string Parseff.diagnostic) -> diagnostic)
          diagnostics
      in
      Alcotest.(check (list string))
        "diagnostics preserved" [ "before-failure" ] got
  | Error _ ->
      Alcotest.fail "Unexpected error type"

let test_parse_until_end_preserves_end_of_input_semantics () =
  let ok_outcome =
    Parseff.parse_until_end "a" (fun () ->
        let _ = Parseff.char 'a' in
        Parseff.end_of_input ();
        "ok"
    )
  in
  ( match ok_outcome with
  | Ok (s, _) ->
      Alcotest.(check string) "explicit eof still works" "ok" s
  | Error _ ->
      Alcotest.fail "Expected success"
  );
  let fail_outcome =
    Parseff.parse_until_end "ab" (fun () ->
        let _ = Parseff.char 'a' in
        Parseff.end_of_input ();
        "unreachable"
    )
  in
  match fail_outcome with
  | Ok _ ->
      Alcotest.fail "Expected failure"
  | Error { pos; error = `Expected _; _ } ->
      Alcotest.(check int) "fails at explicit eof position" 1 pos
  | Error _ ->
      Alcotest.fail "Unexpected error type"

let test_satisfy_failure () =
  match
    Parseff.parse "a" (fun () ->
        Parseff.satisfy (fun c -> c >= '0' && c <= '9') ~label:"digit"
    )
  with
  | Ok _ ->
      Alcotest.fail "Expected failure"
  | Error { pos; error = `Expected expected } ->
      Alcotest.(check int) "error position" 0 pos;
      Alcotest.(check string) "error message" "expected digit" expected
  | Error _ ->
      Alcotest.fail "Unexpected error type"

let test_satisfy_eof () =
  match
    Parseff.parse "" (fun () ->
        Parseff.satisfy (fun c -> c >= '0' && c <= '9') ~label:"digit"
    )
  with
  | Ok _ ->
      Alcotest.fail "Expected failure"
  | Error { pos; error = `Unexpected_end_of_input } ->
      Alcotest.(check int) "error position" 0 pos
  | Error _ ->
      Alcotest.fail "Expected `Unexpected_end_of_input"

let test_char_failure () =
  match Parseff.parse "b" (fun () -> Parseff.char 'a') with
  | Ok _ ->
      Alcotest.fail "Expected failure"
  | Error { pos; error = `Expected expected } ->
      Alcotest.(check int) "error position" 0 pos;
      Alcotest.(check string) "error message" "expected 'a'" expected
  | Error _ ->
      Alcotest.fail "Unexpected error type"

let test_look_ahead_failure () =
  match
    Parseff.parse "abc" (fun () ->
        Parseff.look_ahead (fun () -> Parseff.consume "xyz")
    )
  with
  | Ok _ ->
      Alcotest.fail "Expected failure"
  | Error { pos; error = `Expected expected } ->
      Alcotest.(check int) "error position" 0 pos;
      Alcotest.(check string) "error message" "expected \"xyz\"" expected
  | Error _ ->
      Alcotest.fail "Unexpected error type"

let test_one_of_success () =
  let parser =
    Parseff.one_of
      [
        (fun () -> Parseff.consume "if");
        (fun () -> Parseff.consume "else");
        (fun () -> Parseff.consume "while");
      ]
  in
  match Parseff.parse "else" parser with
  | Ok s ->
      Alcotest.(check string) "matched second" "else" s
  | Error _ ->
      Alcotest.fail "Expected success"

let test_warn_at_nonzero_pos () =
  let outcome =
    Parseff.parse_until_end "ab" (fun () ->
        let _ = Parseff.consume "ab" in
        Parseff.warn_at ~pos:5 "at-five";
        42
    )
  in
  match outcome with
  | Ok (n, diagnostics) ->
      Alcotest.(check int) "value" 42 n;
      let got =
        List.map
          (fun ({ pos; diagnostic } : string Parseff.diagnostic) ->
            (pos, diagnostic)
          )
          diagnostics
      in
      Alcotest.(check (list (pair int string)))
        "diagnostics"
        [ (5, "at-five") ]
        got
  | Error _ ->
      Alcotest.fail "Expected success"

let test_letter_success () =
  match Parseff.parse "a" Parseff.letter with
  | Ok c ->
      Alcotest.(check char) "lowercase" 'a' c
  | Error _ ->
      Alcotest.fail "Expected success"

let test_letter_failure () =
  match Parseff.parse "1" Parseff.letter with
  | Ok _ ->
      Alcotest.fail "Expected failure"
  | Error { pos; error = `Expected expected } ->
      Alcotest.(check int) "error position" 0 pos;
      Alcotest.(check string) "error message" "expected letter" expected
  | Error _ ->
      Alcotest.fail "Unexpected error type"

let test_skip_while () =
  match
    parse_with_pos "   hello" (fun () ->
        Parseff.skip_while (fun c -> c = ' ');
        Parseff.consume "hello"
    )
  with
  | Ok (s, pos) ->
      Alcotest.(check string) "matched" "hello" s;
      Alcotest.(check int) "final position" 8 pos
  | Error _ ->
      Alcotest.fail "Expected success"

(* --- expect --- *)

let test_expect_relabels_parse_error () =
  match
    Parseff.parse "abc" (fun () -> Parseff.expect "a digit (0-9)" Parseff.digit)
  with
  | Ok _ ->
      Alcotest.fail "Expected failure"
  | Error { pos; error = `Expected expected } ->
      Alcotest.(check int) "error position" 0 pos;
      Alcotest.(check string) "error message" "a digit (0-9)" expected
  | Error _ ->
      Alcotest.fail "Unexpected error type"

let test_expect_success () =
  match
    Parseff.parse "5" (fun () -> Parseff.expect "a digit (0-9)" Parseff.digit)
  with
  | Ok n ->
      Alcotest.(check int) "digit value" 5 n
  | Error _ ->
      Alcotest.fail "Expected success"

let test_expect_user_error_passthrough () =
  match
    Parseff.parse "5" (fun () ->
        Parseff.expect "a small number" (fun () ->
            let n = Parseff.digit () in
            if n > 3 then
              Parseff.error (`Too_big n)
            else
              n
        )
    )
  with
  | Ok _ ->
      Alcotest.fail "Expected failure"
  | Error { pos; error = `Too_big n } ->
      Alcotest.(check int) "error position" 1 pos;
      Alcotest.(check int) "user error value" 5 n
  | Error { error = `Expected _; _ } ->
      Alcotest.fail "User error was swallowed by expect"
  | Error _ ->
      Alcotest.fail "Unexpected error type"

let test_expect_relabels_or_error () =
  match
    Parseff.parse "xyz" (fun () ->
        Parseff.expect "a keyword"
          (Parseff.one_of
             [
               (fun () -> Parseff.consume "if");
               (fun () -> Parseff.consume "else");
             ]
          )
    )
  with
  | Ok _ ->
      Alcotest.fail "Expected failure"
  | Error { pos; error = `Expected expected } ->
      Alcotest.(check int) "error position" 0 pos;
      Alcotest.(check string) "error message" "a keyword" expected
  | Error _ ->
      Alcotest.fail "Unexpected error type"

(* --- one_of_labeled --- *)

let test_one_of_labeled_success () =
  match
    Parseff.parse "hello"
      (Parseff.one_of_labeled
         [
           ("number", fun () -> Parseff.consume "123");
           ("greeting", fun () -> Parseff.consume "hello");
         ]
      )
  with
  | Ok s ->
      Alcotest.(check string) "matched second" "hello" s
  | Error _ ->
      Alcotest.fail "Expected success"

let test_one_of_labeled_failure () =
  match
    Parseff.parse "xyz"
      (Parseff.one_of_labeled
         [
           ("number", fun () -> Parseff.consume "123");
           ("greeting", fun () -> Parseff.consume "hello");
         ]
      )
  with
  | Ok _ ->
      Alcotest.fail "Expected failure"
  | Error { pos; error = `Expected expected } ->
      Alcotest.(check int) "error position" 0 pos;
      Alcotest.(check string)
        "error message" "expected one of: number, greeting" expected
  | Error _ ->
      Alcotest.fail "Unexpected error type"

let test_one_of_labeled_user_error_passthrough () =
  match
    Parseff.parse "5"
      (Parseff.one_of_labeled
         [
           ( "validated",
             fun () ->
               let n = Parseff.digit () in
               if n > 3 then
                 Parseff.error (`Too_big n)
               else
                 n
           );
         ]
      )
  with
  | Ok _ ->
      Alcotest.fail "Expected failure"
  | Error { error = `Too_big n; _ } ->
      Alcotest.(check int) "user error value" 5 n
  | Error { error = `Expected _; _ } ->
      Alcotest.fail "User error was swallowed by one_of_labeled"
  | Error _ ->
      Alcotest.fail "Unexpected error type"

(* --- error --- *)

let test_error_custom_type () =
  match
    Parseff.parse "abc" (fun () ->
        let _ = Parseff.consume "abc" in
        Parseff.error (`Custom_error "bad")
    )
  with
  | Ok _ ->
      Alcotest.fail "Expected failure"
  | Error { pos; error = `Custom_error msg } ->
      Alcotest.(check int) "error position" 3 pos;
      Alcotest.(check string) "error message" "bad" msg
  | Error { error = `Expected _; _ } ->
      Alcotest.fail "error should not produce `Expected"
  | Error _ ->
      Alcotest.fail "Unexpected error type"

let test_error_distinct_from_fail () =
  let fail_result = Parseff.parse "" (fun () -> Parseff.fail "boom") in
  let error_result =
    Parseff.parse "" (fun () -> Parseff.error (`Boom "boom"))
  in
  ( match fail_result with
  | Error { error = `Failure msg; _ } ->
      Alcotest.(check string) "fail message" "boom" msg
  | _ ->
      Alcotest.fail "Expected `Failure from fail"
  );
  match error_result with
  | Error { error = `Boom msg; _ } ->
      Alcotest.(check string) "error message" "boom" msg
  | _ ->
      Alcotest.fail "Expected `Boom from error"

(* --- fail / Failure semantics --- *)

(* fail produces `Failure, not `Expected *)
let test_fail_produces_failure () =
  match Parseff.parse "abc" (fun () -> Parseff.fail "bad value") with
  | Error { error = `Failure msg; _ } ->
      Alcotest.(check string) "failure message" "bad value" msg
  | Error { error = `Expected _; _ } ->
      Alcotest.fail "fail should not produce `Expected"
  | _ ->
      Alcotest.fail "Expected `Failure error"

(* Failure punches through or_ *)
let test_failure_punches_through_or () =
  match
    Parseff.parse "abc" (fun () ->
        Parseff.or_
          (fun () ->
            let _ = Parseff.consume "abc" in
            Parseff.fail "validation failed"
          )
          (fun () -> "fallback")
          ()
    )
  with
  | Error { error = `Failure msg; _ } ->
      Alcotest.(check string) "failure message" "validation failed" msg
  | Ok v ->
      Alcotest.fail
        (Printf.sprintf
           "Expected Failure but got Ok %S — or_ should not catch Failure" v
        )
  | Error _ ->
      Alcotest.fail "Expected `Failure error"

(* Expected IS caught by or_ (backtracking still works for Expected) *)
let test_expected_caught_by_or () =
  match
    Parseff.parse "abc" (fun () ->
        Parseff.or_
          (fun () -> Parseff.consume "xyz")
          (fun () -> Parseff.consume "abc")
          ()
    )
  with
  | Ok s ->
      Alcotest.(check string) "or_ backtracks on Expected" "abc" s
  | Error _ ->
      Alcotest.fail "Expected success from or_ backtracking"

(* Failure punches through many *)
let test_failure_punches_through_many () =
  let counter = ref 0 in
  match
    Parseff.parse "aaab" (fun () ->
        Parseff.many
          (fun () ->
            incr counter;
            if !counter > 2 then
              Parseff.fail "too many"
            else
              Parseff.consume "a"
          )
          ()
    )
  with
  | Error { error = `Failure msg; _ } ->
      Alcotest.(check string) "failure message" "too many" msg
  | Ok _ ->
      Alcotest.fail
        "Expected Failure but got Ok — many should not catch Failure"
  | Error _ ->
      Alcotest.fail "Expected `Failure error"

(* Failure punches through optional *)
let test_failure_punches_through_optional () =
  match
    Parseff.parse "abc" (fun () ->
        Parseff.optional
          (fun () ->
            let _ = Parseff.consume "abc" in
            Parseff.fail "nope"
          )
          ()
    )
  with
  | Error { error = `Failure msg; _ } ->
      Alcotest.(check string) "failure message" "nope" msg
  | Ok _ ->
      Alcotest.fail
        "Expected Failure but got Ok — optional should not catch Failure"
  | Error _ ->
      Alcotest.fail "Expected `Failure error"

(* Failure punches through look_ahead *)
let test_failure_punches_through_look_ahead () =
  match
    Parseff.parse "abc" (fun () ->
        Parseff.look_ahead (fun () ->
            let _ = Parseff.consume "abc" in
            Parseff.fail "look_ahead fail"
        )
    )
  with
  | Error { error = `Failure msg; _ } ->
      Alcotest.(check string) "failure message" "look_ahead fail" msg
  | Ok _ ->
      Alcotest.fail
        "Expected Failure but got Ok — look_ahead should not catch Failure"
  | Error _ ->
      Alcotest.fail "Expected `Failure error"

(* Failure punches through expect *)
let test_failure_punches_through_expect () =
  match
    Parseff.parse "abc" (fun () ->
        Parseff.expect "a number" (fun () ->
            let _ = Parseff.consume "abc" in
            Parseff.fail "not a valid number"
        )
    )
  with
  | Error { error = `Failure msg; _ } ->
      Alcotest.(check string) "failure message" "not a valid number" msg
  | Error { error = `Expected _; _ } ->
      Alcotest.fail "expect should not catch Failure"
  | Ok _ ->
      Alcotest.fail "Expected Failure error"
  | Error _ ->
      Alcotest.fail "Unexpected error type"

(* Failure punches through one_of_labeled *)
let test_failure_punches_through_one_of_labeled () =
  match
    Parseff.parse "abc" (fun () ->
        Parseff.one_of_labeled
          [
            ( "first",
              fun () ->
                let _ = Parseff.consume "abc" in
                Parseff.fail "bad value"
            );
            ("second", fun () -> Parseff.consume "xyz");
          ]
          ()
    )
  with
  | Error { error = `Failure msg; _ } ->
      Alcotest.(check string) "failure message" "bad value" msg
  | Error { error = `Expected _; _ } ->
      Alcotest.fail "one_of_labeled should not catch Failure"
  | Ok _ ->
      Alcotest.fail "Expected Failure error"
  | Error _ ->
      Alcotest.fail "Unexpected error type"

(* Internal combinators still produce Expected *)
let test_consume_produces_expected () =
  match Parseff.parse "abc" (fun () -> Parseff.consume "xyz") with
  | Error { error = `Expected _; _ } ->
      ()
  | _ ->
      Alcotest.fail "consume mismatch should produce `Expected"

let test_satisfy_produces_expected () =
  match
    Parseff.parse "a" (fun () ->
        Parseff.satisfy (fun c -> c >= '0' && c <= '9') ~label:"digit"
    )
  with
  | Error { error = `Expected msg; _ } ->
      Alcotest.(check string) "expected message" "expected digit" msg
  | _ ->
      Alcotest.fail "satisfy mismatch should produce `Expected"

let test_one_of_produces_expected () =
  match
    Parseff.parse "c" (fun () ->
        Parseff.one_of
          [ (fun () -> Parseff.consume "a"); (fun () -> Parseff.consume "b") ]
          ()
    )
  with
  | Error { error = `Expected _; _ } ->
      ()
  | _ ->
      Alcotest.fail "one_of failure should produce `Expected"

let test_take_while_at_least_produces_expected () =
  match
    Parseff.parse "abc" (fun () ->
        Parseff.take_while ~at_least:1
          (fun c -> c >= '0' && c <= '9')
          ~label:"digit"
    )
  with
  | Error { error = `Expected msg; _ } ->
      Alcotest.(check string) "expected message" "digit" msg
  | _ ->
      Alcotest.fail "take_while ~at_least failure should produce `Expected"

(* Failure works with parse_source (streaming) *)
let test_failure_with_streaming () =
  let src = Parseff.Source.of_string "abc" in
  match
    Parseff.parse_source src (fun () ->
        let _ = Parseff.consume "abc" in
        Parseff.fail "stream fail"
    )
  with
  | Error { error = `Failure msg; _ } ->
      Alcotest.(check string) "failure message" "stream fail" msg
  | Ok _ ->
      Alcotest.fail "Expected Failure error"
  | Error _ ->
      Alcotest.fail "Unexpected error type"

(* Failure punches through or_ in streaming mode *)
let test_failure_punches_through_or_streaming () =
  let src = Parseff.Source.of_string "abc" in
  match
    Parseff.parse_source src (fun () ->
        Parseff.or_
          (fun () ->
            let _ = Parseff.consume "abc" in
            Parseff.fail "stream validation failed"
          )
          (fun () -> "fallback")
          ()
    )
  with
  | Error { error = `Failure msg; _ } ->
      Alcotest.(check string) "failure message" "stream validation failed" msg
  | Ok _ ->
      Alcotest.fail
        "Expected Failure — or_ should not catch Failure in streaming mode"
  | Error _ ->
      Alcotest.fail "Unexpected error type"

(* --- catch --- *)

(* catch recovers from Failure *)
let test_catch_recovers_failure () =
  match
    Parseff.parse "300" (fun () ->
        Parseff.catch
          (fun () ->
            let s =
              Parseff.take_while ~at_least:1
                (fun c -> c >= '0' && c <= '9')
                ~label:"digit"
            in
            let n = int_of_string s in
            if n > 255 then
              Parseff.fail "out of range"
            else
              n
          )
          (fun _msg -> -1)
    )
  with
  | Ok n ->
      Alcotest.(check int) "catch returns handler value" (-1) n
  | Error _ ->
      Alcotest.fail "Expected Ok from catch recovery"

(* catch passes through on success *)
let test_catch_passthrough_on_success () =
  match
    Parseff.parse "42" (fun () ->
        Parseff.catch
          (fun () ->
            let s =
              Parseff.take_while ~at_least:1
                (fun c -> c >= '0' && c <= '9')
                ~label:"digit"
            in
            int_of_string s
          )
          (fun _msg -> -1)
    )
  with
  | Ok n ->
      Alcotest.(check int) "catch passthrough" 42 n
  | Error _ ->
      Alcotest.fail "Expected Ok"

(* catch does NOT catch Expected errors *)
let test_catch_does_not_catch_expected () =
  match
    Parseff.parse "abc" (fun () ->
        Parseff.catch (fun () -> Parseff.consume "xyz") (fun _msg -> "recovered")
    )
  with
  | Error { error = `Expected _; _ } ->
      ()
  | Ok _ ->
      Alcotest.fail "catch should not intercept Expected errors"
  | Error _ ->
      Alcotest.fail "Unexpected error type"

(* catch makes Failure recoverable inside or_ *)
let test_catch_enables_backtracking () =
  match
    Parseff.parse "abc" (fun () ->
        Parseff.or_
          (fun () ->
            Parseff.catch
              (fun () ->
                let _ = Parseff.consume "abc" in
                Parseff.fail "bad value"
              )
              (fun _msg -> "caught")
          )
          (fun () -> "fallback")
          ()
    )
  with
  | Ok s ->
      Alcotest.(check string) "catch inside or_" "caught" s
  | Error _ ->
      Alcotest.fail "Expected Ok from catch inside or_"

(* catch handler receives the message *)
let test_catch_receives_message () =
  match
    Parseff.parse "" (fun () ->
        Parseff.catch
          (fun () -> Parseff.fail "specific message")
          (fun msg -> msg)
    )
  with
  | Ok s ->
      Alcotest.(check string) "handler receives message" "specific message" s
  | Error _ ->
      Alcotest.fail "Expected Ok"

(* catch works with streaming *)
let test_catch_streaming () =
  let src = Parseff.Source.of_string "abc" in
  match
    Parseff.parse_source src (fun () ->
        Parseff.catch
          (fun () ->
            let _ = Parseff.consume "abc" in
            Parseff.fail "stream fail"
          )
          (fun msg -> msg)
    )
  with
  | Ok s ->
      Alcotest.(check string) "catch in streaming" "stream fail" s
  | Error _ ->
      Alcotest.fail "Expected Ok from catch in streaming"

(* --- whitespace / whitespace1 --- *)

let test_whitespace () =
  match Parseff.parse "  \t\nX" (fun () -> Parseff.whitespace ()) with
  | Ok s ->
      Alcotest.(check string) "whitespace" "  \t\n" s
  | Error _ ->
      Alcotest.fail "Expected success"

let test_whitespace_empty () =
  match Parseff.parse "abc" (fun () -> Parseff.whitespace ()) with
  | Ok s ->
      Alcotest.(check string) "no whitespace" "" s
  | Error _ ->
      Alcotest.fail "Expected success"

let test_whitespace1_success () =
  match Parseff.parse "  \tX" (fun () -> Parseff.whitespace ~at_least:1 ()) with
  | Ok s ->
      Alcotest.(check string) "whitespace" "  \t" s
  | Error _ ->
      Alcotest.fail "Expected success"

let test_whitespace1_failure () =
  match Parseff.parse "abc" (fun () -> Parseff.whitespace ~at_least:1 ()) with
  | Ok _ ->
      Alcotest.fail "Expected failure"
  | Error { pos; error = `Expected expected } ->
      Alcotest.(check int) "error position" 0 pos;
      Alcotest.(check string) "error message" "whitespace" expected
  | Error _ ->
      Alcotest.fail "Unexpected error type"

(* --- alphanum --- *)

let test_alphanum_letter () =
  match Parseff.parse "a" Parseff.alphanum with
  | Ok c ->
      Alcotest.(check char) "letter" 'a' c
  | Error _ ->
      Alcotest.fail "Expected success"

let test_alphanum_digit () =
  match Parseff.parse "5" Parseff.alphanum with
  | Ok c ->
      Alcotest.(check char) "digit" '5' c
  | Error _ ->
      Alcotest.fail "Expected success"

let test_alphanum_failure () =
  match Parseff.parse "!" Parseff.alphanum with
  | Ok _ ->
      Alcotest.fail "Expected failure"
  | Error { pos; error = `Expected expected } ->
      Alcotest.(check int) "error position" 0 pos;
      Alcotest.(check string) "error message" "expected alphanumeric" expected
  | Error _ ->
      Alcotest.fail "Unexpected error type"

(* --- match_regex --- *)

let test_match_regex_success () =
  let re = Re.compile (Re.Posix.re "[0-9]+") in
  match Parseff.parse "12345abc" (fun () -> Parseff.match_regex re) with
  | Ok s ->
      Alcotest.(check string) "matched digits" "12345" s
  | Error _ ->
      Alcotest.fail "Expected success"

let test_match_regex_failure () =
  let re = Re.compile (Re.Posix.re "[0-9]+") in
  match Parseff.parse "abc" (fun () -> Parseff.match_regex re) with
  | Ok _ ->
      Alcotest.fail "Expected failure"
  | Error { pos; error = `Expected expected } ->
      Alcotest.(check int) "error position" 0 pos;
      Alcotest.(check string) "error message" "regex match failed" expected
  | Error _ ->
      Alcotest.fail "Unexpected error type"

let test_match_regex_anchored () =
  let re = Re.compile (Re.Posix.re "[0-9]+") in
  match
    parse_with_pos "abc123" (fun () ->
        let _ = Parseff.consume "abc" in
        Parseff.match_regex re
    )
  with
  | Ok (s, pos) ->
      Alcotest.(check string) "matched from current pos" "123" s;
      Alcotest.(check int) "final position" 6 pos
  | Error _ ->
      Alcotest.fail "Expected success"

(* --- sep_by_take --- *)

let test_sep_by_take_empty () =
  let is_alpha c = c >= 'a' && c <= 'z' in
  match
    Parseff.parse "" (fun () ->
        Parseff.sep_by_take Parseff.is_whitespace ',' is_alpha
    )
  with
  | Ok lst ->
      Alcotest.(check (list string)) "empty" [] lst
  | Error _ ->
      Alcotest.fail "Expected success"

let test_sep_by_take_single () =
  let is_alpha c = c >= 'a' && c <= 'z' in
  match
    Parseff.parse "hello" (fun () ->
        Parseff.sep_by_take Parseff.is_whitespace ',' is_alpha
    )
  with
  | Ok lst ->
      Alcotest.(check (list string)) "single" [ "hello" ] lst
  | Error _ ->
      Alcotest.fail "Expected success"

let test_sep_by_take_several () =
  let is_alpha c = c >= 'a' && c <= 'z' in
  match
    Parseff.parse "a , b , c" (fun () ->
        Parseff.sep_by_take Parseff.is_whitespace ',' is_alpha
    )
  with
  | Ok lst ->
      Alcotest.(check (list string)) "three items" [ "a"; "b"; "c" ] lst
  | Error _ ->
      Alcotest.fail "Expected success"

(* --- sep_by_take_span --- *)

let test_sep_by_take_span_empty () =
  let is_alpha c = c >= 'a' && c <= 'z' in
  match
    Parseff.parse "" (fun () ->
        Parseff.sep_by_take_span Parseff.is_whitespace ',' is_alpha
    )
  with
  | Ok lst ->
      Alcotest.(check int) "empty" 0 (List.length lst)
  | Error _ ->
      Alcotest.fail "Expected success"

let test_sep_by_take_span_several () =
  let is_alpha c = c >= 'a' && c <= 'z' in
  match
    Parseff.parse "aa , bb , cc" (fun () ->
        Parseff.sep_by_take_span Parseff.is_whitespace ',' is_alpha
    )
  with
  | Ok spans ->
      let strs = List.map Parseff.span_to_string spans in
      Alcotest.(check (list string)) "three items" [ "aa"; "bb"; "cc" ] strs;
      let offsets = List.map (fun (s : Parseff.span) -> s.off) spans in
      Alcotest.(check (list int)) "offsets" [ 0; 5; 10 ] offsets;
      let lengths = List.map (fun (s : Parseff.span) -> s.len) spans in
      Alcotest.(check (list int)) "lengths" [ 2; 2; 2 ] lengths
  | Error _ ->
      Alcotest.fail "Expected success"

(* --- fused_sep_take --- *)

let test_fused_sep_take_success () =
  let is_alpha c = c >= 'a' && c <= 'z' in
  match
    Parseff.parse ", hello" (fun () ->
        Parseff.fused_sep_take Parseff.is_whitespace ',' is_alpha
    )
  with
  | Ok s ->
      Alcotest.(check string) "taken value" "hello" s
  | Error _ ->
      Alcotest.fail "Expected success"

let test_fused_sep_take_missing_sep () =
  let is_alpha c = c >= 'a' && c <= 'z' in
  match
    Parseff.parse "abc" (fun () ->
        Parseff.fused_sep_take Parseff.is_whitespace ',' is_alpha
    )
  with
  | Ok _ ->
      Alcotest.fail "Expected failure"
  | Error { pos; error = `Expected expected } ->
      Alcotest.(check int) "error position" 0 pos;
      Alcotest.(check string) "error message" "expected ','" expected
  | Error _ ->
      Alcotest.fail "Unexpected error type"

let test_fused_sep_take_missing_value () =
  let is_alpha c = c >= 'a' && c <= 'z' in
  match
    Parseff.parse ", " (fun () ->
        Parseff.fused_sep_take Parseff.is_whitespace ',' is_alpha
    )
  with
  | Ok _ ->
      Alcotest.fail "Expected failure"
  | Error { pos; error = `Unexpected_end_of_input } ->
      (* After ",_" whitespace is skipped, pos is at EOF *)
      Alcotest.(check int) "error position" 2 pos
  | Error _ ->
      Alcotest.fail "Expected `Unexpected_end_of_input"

(* --- take_while_span --- *)

let test_take_while_span () =
  match
    parse_with_pos "aaabbb" (fun () ->
        let sp = Parseff.take_while_span (fun c -> c = 'a') in
        (Parseff.span_to_string sp, sp.off, sp.len)
    )
  with
  | Ok ((s, off, len), pos) ->
      Alcotest.(check string) "span string" "aaa" s;
      Alcotest.(check int) "span offset" 0 off;
      Alcotest.(check int) "span length" 3 len;
      Alcotest.(check int) "parser position" 3 pos
  | Error _ ->
      Alcotest.fail "Expected success"

let test_take_while_span_empty () =
  match
    Parseff.parse "xyz" (fun () ->
        let sp = Parseff.take_while_span (fun c -> c = 'a') in
        (Parseff.span_to_string sp, sp.len)
    )
  with
  | Ok (s, len) ->
      Alcotest.(check string) "empty span" "" s;
      Alcotest.(check int) "zero length" 0 len
  | Error _ ->
      Alcotest.fail "Expected success"

let check_location msg expected actual =
  Alcotest.(check int)
    (msg ^ " offset") expected.Parseff.offset actual.Parseff.offset;
  Alcotest.(check int) (msg ^ " line") expected.Parseff.line actual.Parseff.line;
  Alcotest.(check int) (msg ^ " col") expected.Parseff.col actual.Parseff.col

let test_location_at_start () =
  match Parseff.parse "hello" (fun () -> Parseff.location ()) with
  | Ok loc ->
      check_location "start" { offset = 0; line = 1; col = 1 } loc
  | Error _ ->
      Alcotest.fail "Expected success"

let test_location_after_consume () =
  let parser () =
    let _ = Parseff.consume "hello" in
    Parseff.location ()
  in
  match Parseff.parse "hello world" parser with
  | Ok loc ->
      check_location "after consume" { offset = 5; line = 1; col = 6 } loc
  | Error _ ->
      Alcotest.fail "Expected success"

let test_location_multiline () =
  let input = "aaa\nbbb\nccc" in
  let parser () =
    let _ = Parseff.consume "aaa\nbbb\nc" in
    Parseff.location ()
  in
  match Parseff.parse input parser with
  | Ok loc ->
      check_location "multiline" { offset = 9; line = 3; col = 2 } loc
  | Error _ ->
      Alcotest.fail "Expected success"

let test_location_column_resets () =
  let input = "abc\nde" in
  let parser () =
    let _ = Parseff.consume "abc\nd" in
    Parseff.location ()
  in
  match Parseff.parse input parser with
  | Ok loc ->
      check_location "col reset" { offset = 5; line = 2; col = 2 } loc
  | Error _ ->
      Alcotest.fail "Expected success"

let test_location_multiple_calls () =
  let input = "aa\nbb\ncc" in
  let parser () =
    let _ = Parseff.consume "aa" in
    let loc1 = Parseff.location () in
    let _ = Parseff.consume "\nbb\nc" in
    let loc2 = Parseff.location () in
    (loc1, loc2)
  in
  match Parseff.parse input parser with
  | Ok (loc1, loc2) ->
      check_location "first" { offset = 2; line = 1; col = 3 } loc1;
      check_location "second" { offset = 7; line = 3; col = 2 } loc2
  | Error _ ->
      Alcotest.fail "Expected success"

let test_location_at_newline () =
  let input = "ab\ncd" in
  let parser () =
    let _ = Parseff.consume "ab" in
    let loc1 = Parseff.location () in
    let _ = Parseff.consume "\n" in
    let loc2 = Parseff.location () in
    (loc1, loc2)
  in
  match Parseff.parse input parser with
  | Ok (loc1, loc2) ->
      check_location "before newline" { offset = 2; line = 1; col = 3 } loc1;
      check_location "after newline" { offset = 3; line = 2; col = 1 } loc2
  | Error _ ->
      Alcotest.fail "Expected success"

let test_location_backtracking () =
  let input = "aaa\nbbb" in
  let parser () =
    Parseff.or_
      (fun () ->
        let _ = Parseff.consume "aaa\nbbb\nccc" in
        ()
      )
      (fun () ->
        let _ = Parseff.consume "aaa\nb" in
        ()
      )
      ();
    Parseff.location ()
  in
  match Parseff.parse input parser with
  | Ok loc ->
      check_location "after backtrack" { offset = 5; line = 2; col = 2 } loc
  | Error _ ->
      Alcotest.fail "Expected success"

let test_location_empty_input () =
  match Parseff.parse "" (fun () -> Parseff.location ()) with
  | Ok loc ->
      check_location "empty" { offset = 0; line = 1; col = 1 } loc
  | Error _ ->
      Alcotest.fail "Expected success"

let test_location_of_position_basic () =
  let input = "aaa\nbbb\nccc" in
  let loc = Parseff.location_of_position input 0 in
  check_location "start" { offset = 0; line = 1; col = 1 } loc;
  let loc = Parseff.location_of_position input 3 in
  check_location "end of line 1" { offset = 3; line = 1; col = 4 } loc;
  let loc = Parseff.location_of_position input 4 in
  check_location "start of line 2" { offset = 4; line = 2; col = 1 } loc;
  let loc = Parseff.location_of_position input 8 in
  check_location "start of line 3" { offset = 8; line = 3; col = 1 } loc;
  let loc = Parseff.location_of_position input 10 in
  check_location "end of input" { offset = 10; line = 3; col = 3 } loc

let test_location_of_position_empty () =
  let loc = Parseff.location_of_position "" 0 in
  check_location "empty" { offset = 0; line = 1; col = 1 } loc

let test_location_of_position_single_line () =
  let input = "hello" in
  let loc = Parseff.location_of_position input 3 in
  check_location "mid" { offset = 3; line = 1; col = 4 } loc

let test_location_with_position () =
  let input = "aa\nbb" in
  let parser () =
    let _ = Parseff.consume "aa\nb" in
    let pos = Parseff.position () in
    let loc = Parseff.location () in
    (pos, loc)
  in
  match Parseff.parse input parser with
  | Ok (pos, loc) ->
      Alcotest.(check int) "position matches" pos loc.offset;
      check_location "loc" { offset = 4; line = 2; col = 2 } loc
  | Error _ ->
      Alcotest.fail "Expected success"

let () =
  let open Alcotest in
  run "Parseff Primitives"
    [
      ( "basic",
        [
          test_case "consume success" `Quick test_consume_success;
          test_case "consume failure" `Quick test_consume_failure;
          test_case "char success" `Quick test_char_success;
          test_case "char failure" `Quick test_char_failure;
          test_case "satisfy" `Quick test_satisfy;
          test_case "satisfy failure" `Quick test_satisfy_failure;
          test_case "satisfy eof" `Quick test_satisfy_eof;
          test_case "digit" `Quick test_digit;
          test_case "letter success" `Quick test_letter_success;
          test_case "letter failure" `Quick test_letter_failure;
          test_case "alphanum letter" `Quick test_alphanum_letter;
          test_case "alphanum digit" `Quick test_alphanum_digit;
          test_case "alphanum failure" `Quick test_alphanum_failure;
          test_case "any_char" `Quick (fun () ->
              match Parseff.parse "x" Parseff.any_char with
              | Ok c ->
                  Alcotest.(check char) "any" 'x' c
              | Error _ ->
                  Alcotest.fail "Expected success"
          );
        ]
      );
      ( "sequencing",
        [
          test_case "sequence" `Quick test_sequence;
          test_case "alternation left" `Quick test_alternation_left;
          test_case "alternation right" `Quick test_alternation_right;
          test_case "alternation failure" `Quick test_alternation_failure;
          test_case "one_of success" `Quick test_one_of_success;
          test_case "one_of failure" `Quick test_one_of_failure;
        ]
      );
      ( "repetition",
        [
          test_case "many empty" `Quick test_many_empty;
          test_case "many several" `Quick test_many_several;
          test_case "many1 success" `Quick test_many1_success;
          test_case "many1 failure" `Quick test_many1_failure;
          test_case "optional some" `Quick test_optional_some;
          test_case "optional none" `Quick test_optional_none;
          test_case "count" `Quick test_count;
          test_case "count insufficient" `Quick test_count_insufficient;
        ]
      );
      ( "separators",
        [
          test_case "sep_by empty" `Quick test_sep_by_empty;
          test_case "sep_by one" `Quick test_sep_by_one;
          test_case "sep_by several" `Quick test_sep_by_several;
          test_case "between" `Quick test_between;
          test_case "between missing close" `Quick
            test_between_failure_missing_close;
          test_case "end_by" `Quick test_end_by;
          test_case "end_by empty" `Quick test_end_by_empty;
          test_case "end_by1 failure" `Quick test_end_by1_failure;
          test_case "end_by1 success" `Quick test_end_by1_success;
          test_case "fold_left left assoc" `Quick test_fold_left_left_assoc;
          test_case "fold_right right assoc" `Quick test_fold_right_right_assoc;
          test_case "fold_left otherwise" `Quick test_fold_left_otherwise;
          test_case "fold_left non-otherwise" `Quick
            test_fold_left_non_otherwise;
          test_case "fold_right otherwise" `Quick test_fold_right_otherwise;
          test_case "fold_right non-otherwise" `Quick
            test_fold_right_non_otherwise;
          test_case "fold_left requires one" `Quick test_fold_left_requires_one;
          test_case "fold_right requires one" `Quick
            test_fold_right_requires_one;
        ]
      );
      ( "special",
        [
          test_case "end_of_input success" `Quick test_end_of_input_success;
          test_case "end_of_input failure" `Quick test_end_of_input_failure;
          test_case "look_ahead success" `Quick test_look_ahead_success;
          test_case "look_ahead no consume" `Quick test_look_ahead_no_consume;
          test_case "look_ahead failure" `Quick test_look_ahead_failure;
          test_case "commit noop outside frame" `Quick
            test_commit_noop_outside_frame;
          test_case "commit blocks alternation" `Quick
            test_commit_blocks_or_backtracking;
          test_case "commit stops many" `Quick
            test_commit_stops_many_from_succeeding;
          test_case "commit stays inside look_ahead" `Quick
            test_commit_does_not_escape_look_ahead;
          test_case "skip_while" `Quick test_skip_while;
        ]
      );
      ( "expect",
        [
          test_case "relabels parse error" `Quick
            test_expect_relabels_parse_error;
          test_case "success passthrough" `Quick test_expect_success;
          test_case "user error passthrough" `Quick
            test_expect_user_error_passthrough;
          test_case "relabels or_ error" `Quick test_expect_relabels_or_error;
        ]
      );
      ( "one_of_labeled",
        [
          test_case "success" `Quick test_one_of_labeled_success;
          test_case "failure message" `Quick test_one_of_labeled_failure;
          test_case "user error passthrough" `Quick
            test_one_of_labeled_user_error_passthrough;
        ]
      );
      ( "error combinator",
        [
          test_case "custom error type" `Quick test_error_custom_type;
          test_case "distinct from fail" `Quick test_error_distinct_from_fail;
        ]
      );
      ( "fail / Failure semantics",
        [
          test_case "fail produces Failure" `Quick test_fail_produces_failure;
          test_case "Failure punches through or_" `Quick
            test_failure_punches_through_or;
          test_case "Expected caught by or_" `Quick test_expected_caught_by_or;
          test_case "Failure punches through many" `Quick
            test_failure_punches_through_many;
          test_case "Failure punches through optional" `Quick
            test_failure_punches_through_optional;
          test_case "Failure punches through look_ahead" `Quick
            test_failure_punches_through_look_ahead;
          test_case "Failure punches through expect" `Quick
            test_failure_punches_through_expect;
          test_case "Failure punches through one_of_labeled" `Quick
            test_failure_punches_through_one_of_labeled;
          test_case "consume produces Expected" `Quick
            test_consume_produces_expected;
          test_case "satisfy produces Expected" `Quick
            test_satisfy_produces_expected;
          test_case "one_of produces Expected" `Quick
            test_one_of_produces_expected;
          test_case "take_while at_least produces Expected" `Quick
            test_take_while_at_least_produces_expected;
          test_case "Failure with streaming" `Quick test_failure_with_streaming;
          test_case "Failure punches through or_ streaming" `Quick
            test_failure_punches_through_or_streaming;
        ]
      );
      ( "catch",
        [
          test_case "recovers from Failure" `Quick test_catch_recovers_failure;
          test_case "passthrough on success" `Quick
            test_catch_passthrough_on_success;
          test_case "does not catch Expected" `Quick
            test_catch_does_not_catch_expected;
          test_case "enables backtracking" `Quick
            test_catch_enables_backtracking;
          test_case "handler receives message" `Quick
            test_catch_receives_message;
          test_case "works with streaming" `Quick test_catch_streaming;
        ]
      );
      ( "whitespace",
        [
          test_case "whitespace" `Quick test_whitespace;
          test_case "whitespace empty" `Quick test_whitespace_empty;
          test_case "whitespace1 success" `Quick test_whitespace1_success;
          test_case "whitespace1 failure" `Quick test_whitespace1_failure;
        ]
      );
      ( "match_regex",
        [
          test_case "success" `Quick test_match_regex_success;
          test_case "failure" `Quick test_match_regex_failure;
          test_case "anchored at position" `Quick test_match_regex_anchored;
        ]
      );
      ( "fused operations",
        [
          test_case "sep_by_take empty" `Quick test_sep_by_take_empty;
          test_case "sep_by_take single" `Quick test_sep_by_take_single;
          test_case "sep_by_take several" `Quick test_sep_by_take_several;
          test_case "sep_by_take_span empty" `Quick test_sep_by_take_span_empty;
          test_case "sep_by_take_span several" `Quick
            test_sep_by_take_span_several;
          test_case "fused_sep_take success" `Quick test_fused_sep_take_success;
          test_case "fused_sep_take missing sep" `Quick
            test_fused_sep_take_missing_sep;
          test_case "fused_sep_take missing value" `Quick
            test_fused_sep_take_missing_value;
          test_case "take_while_span" `Quick test_take_while_span;
          test_case "take_while_span empty" `Quick test_take_while_span_empty;
        ]
      );
      ( "diagnostics",
        [
          test_case "parse non-enforcing" `Quick test_parse_non_enforcing;
          test_case "parse_until_end requires eof" `Quick
            test_parse_until_end_requires_eof;
          test_case "collect diagnostics" `Quick
            test_parse_until_end_collects_diagnostics;
          test_case "rollback on or" `Quick test_diagnostics_rollback_on_or;
          test_case "rollback on look_ahead" `Quick
            test_diagnostics_rollback_on_look_ahead;
          test_case "rollback on many stop" `Quick
            test_diagnostics_rollback_on_many_stop;
          test_case "error includes diagnostics" `Quick
            test_error_contains_diagnostics;
          test_case "preserves end_of_input semantics" `Quick
            test_parse_until_end_preserves_end_of_input_semantics;
          test_case "warn_at nonzero pos" `Quick test_warn_at_nonzero_pos;
        ]
      );
      ( "location",
        [
          test_case "at start" `Quick test_location_at_start;
          test_case "after consume" `Quick test_location_after_consume;
          test_case "multiline" `Quick test_location_multiline;
          test_case "column resets after newline" `Quick
            test_location_column_resets;
          test_case "multiple calls" `Quick test_location_multiple_calls;
          test_case "at newline boundary" `Quick test_location_at_newline;
          test_case "after backtracking" `Quick test_location_backtracking;
          test_case "empty input" `Quick test_location_empty_input;
          test_case "with position" `Quick test_location_with_position;
        ]
      );
      ( "location_of_position",
        [
          test_case "basic" `Quick test_location_of_position_basic;
          test_case "empty input" `Quick test_location_of_position_empty;
          test_case "single line" `Quick test_location_of_position_single_line;
        ]
      );
    ]
