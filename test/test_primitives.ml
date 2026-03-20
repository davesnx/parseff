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
      Alcotest.(check string) "error message" "expected \"bar\"" expected
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
  | Error { pos; error = `Unexpected_end_of_input } ->
      (* "xyz" is shorter than "while", so last branch fails at EOF *)
      Alcotest.(check int) "error position" 0 pos
  | Error _ ->
      Alcotest.fail "Expected `Unexpected_end_of_input"

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

let test_chainl1_left_assoc () =
  let num = Parseff.digit in
  let sub_op () =
    let _ = Parseff.char '-' in
    ( - )
  in
  match parse_with_pos "9-3-1" (Parseff.chainl num sub_op) with
  | Ok (value, pos) ->
      Alcotest.(check int) "left associative" 5 value;
      Alcotest.(check int) "final position" 5 pos
  | Error _ ->
      Alcotest.fail "Expected success"

let test_chainr1_right_assoc () =
  let num = Parseff.digit in
  let sub_op () =
    let _ = Parseff.char '-' in
    ( - )
  in
  match parse_with_pos "9-3-1" (Parseff.chainr num sub_op) with
  | Ok (value, pos) ->
      Alcotest.(check int) "right associative" 7 value;
      Alcotest.(check int) "final position" 5 pos
  | Error _ ->
      Alcotest.fail "Expected success"

let test_chainl_default () =
  let parser =
    Parseff.chainl Parseff.digit
      (fun () ->
        let _ = Parseff.char '+' in
        ( + )
      )
      ~default:42
  in
  match parse_with_pos "" parser with
  | Ok (value, pos) ->
      Alcotest.(check int) "default value" 42 value;
      Alcotest.(check int) "final position" 0 pos
  | Error _ ->
      Alcotest.fail "Expected success"

let test_chainl_non_default () =
  let parser =
    Parseff.chainl Parseff.digit
      (fun () ->
        let _ = Parseff.char '+' in
        ( + )
      )
      ~default:42
  in
  match parse_with_pos "1+2+3" parser with
  | Ok (value, pos) ->
      Alcotest.(check int) "combined value" 6 value;
      Alcotest.(check int) "final position" 5 pos
  | Error _ ->
      Alcotest.fail "Expected success"

let test_chainr_default () =
  let parser =
    Parseff.chainr Parseff.digit
      (fun () ->
        let _ = Parseff.char '+' in
        ( + )
      )
      ~default:42
  in
  match parse_with_pos "" parser with
  | Ok (value, pos) ->
      Alcotest.(check int) "default value" 42 value;
      Alcotest.(check int) "final position" 0 pos
  | Error _ ->
      Alcotest.fail "Expected success"

let test_chainr_non_default () =
  let parser =
    Parseff.chainr Parseff.digit
      (fun () ->
        let _ = Parseff.char '^' in
        fun l r -> int_of_float (float_of_int l ** float_of_int r)
      )
      ~default:42
  in
  match parse_with_pos "2^3^2" parser with
  | Ok (value, pos) ->
      Alcotest.(check int) "combined value" 512 value;
      Alcotest.(check int) "final position" 5 pos
  | Error _ ->
      Alcotest.fail "Expected success"

let test_chainl1_requires_one () =
  let parser =
    Parseff.chainl Parseff.digit (fun () ->
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

let test_chainr1_requires_one () =
  let parser =
    Parseff.chainr Parseff.digit (fun () ->
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

(* {{{ Phase 2: Missing test dimensions *)

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
      Alcotest.(check string) "error message" "expected a" expected
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

(* }}} *)

(* {{{ Phase 3: Tests for previously untested combinators *)

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
  | Error { error = `Expected msg; _ } ->
      Alcotest.(check string) "fail message" "boom" msg
  | _ ->
      Alcotest.fail "Expected `Expected from fail"
  );
  match error_result with
  | Error { error = `Boom msg; _ } ->
      Alcotest.(check string) "error message" "boom" msg
  | _ ->
      Alcotest.fail "Expected `Boom from error"

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

(* }}} *)

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
          test_case "chainl1 left assoc" `Quick test_chainl1_left_assoc;
          test_case "chainr1 right assoc" `Quick test_chainr1_right_assoc;
          test_case "chainl default" `Quick test_chainl_default;
          test_case "chainl non-default" `Quick test_chainl_non_default;
          test_case "chainr default" `Quick test_chainr_default;
          test_case "chainr non-default" `Quick test_chainr_non_default;
          test_case "chainl1 requires one" `Quick test_chainl1_requires_one;
          test_case "chainr1 requires one" `Quick test_chainr1_requires_one;
        ]
      );
      ( "special",
        [
          test_case "end_of_input success" `Quick test_end_of_input_success;
          test_case "end_of_input failure" `Quick test_end_of_input_failure;
          test_case "look_ahead success" `Quick test_look_ahead_success;
          test_case "look_ahead no consume" `Quick test_look_ahead_no_consume;
          test_case "look_ahead failure" `Quick test_look_ahead_failure;
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
    ]
