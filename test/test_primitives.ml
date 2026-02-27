let parse_with_pos input parser =
  Parseff.parse input (fun () ->
      let value = parser () in
      (value, Parseff.position ()))

let test_consume_success () =
  match parse_with_pos "hello" (fun () -> Parseff.consume "hello") with
  | Ok (s, pos) ->
      Alcotest.(check string) "matched string" "hello" s;
      Alcotest.(check int) "final position" 5 pos
  | Error _ -> Alcotest.fail "Expected success"

let test_consume_failure () =
  match Parseff.parse "hello" (fun () -> Parseff.consume "world") with
  | Ok _ -> Alcotest.fail "Expected failure"
  | Error { pos; error = `Expected expected } ->
      Alcotest.(check int) "error position" 0 pos;
      Alcotest.(check string) "error message" "expected \"world\"" expected
  | Error _ -> Alcotest.fail "Unexpected error type"

let test_char_success () =
  match parse_with_pos "a" (fun () -> Parseff.char 'a') with
  | Ok (c, pos) ->
      Alcotest.(check char) "matched char" 'a' c;
      Alcotest.(check int) "final position" 1 pos
  | Error _ -> Alcotest.fail "Expected success"

let test_satisfy () =
  let is_digit c = c >= '0' && c <= '9' in
  match
    Parseff.parse "7" (fun () -> Parseff.satisfy is_digit ~label:"digit")
  with
  | Ok c -> Alcotest.(check char) "matched digit" '7' c
  | Error _ -> Alcotest.fail "Expected success"

let test_digit () =
  match Parseff.parse "9" Parseff.digit with
  | Ok n -> Alcotest.(check int) "parsed digit" 9 n
  | Error _ -> Alcotest.fail "Expected success"

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
  | Error _ -> Alcotest.fail "Expected success"

let test_alternation_left () =
  let parser () =
    Parseff.or_
      (fun () -> Parseff.consume "foo")
      (fun () -> Parseff.consume "bar")
      ()
  in
  match Parseff.parse "foo" parser with
  | Ok s -> Alcotest.(check string) "matched left" "foo" s
  | Error _ -> Alcotest.fail "Expected success"

let test_alternation_right () =
  let parser () =
    Parseff.or_
      (fun () -> Parseff.consume "foo")
      (fun () -> Parseff.consume "bar")
      ()
  in
  match Parseff.parse "bar" parser with
  | Ok s -> Alcotest.(check string) "matched right" "bar" s
  | Error _ -> Alcotest.fail "Expected success"

let test_alternation_failure () =
  let parser () =
    Parseff.or_
      (fun () -> Parseff.consume "foo")
      (fun () -> Parseff.consume "bar")
      ()
  in
  match Parseff.parse "baz" parser with
  | Ok _ -> Alcotest.fail "Expected failure"
  | Error { error = `Expected _; _ } -> ()
  | Error _ -> Alcotest.fail "Expected `Expected error variant"

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
  | Ok _ -> Alcotest.fail "Expected failure"
  | Error { error = `Expected _; _ } -> ()
  | Error _ -> Alcotest.fail "Expected `Expected error variant from one_of"

let test_many_empty () =
  match Parseff.parse "" (Parseff.many Parseff.digit) with
  | Ok lst -> Alcotest.(check int) "empty list" 0 (List.length lst)
  | Error _ -> Alcotest.fail "Expected success"

let test_many_several () =
  match parse_with_pos "123" (Parseff.many Parseff.digit) with
  | Ok (lst, pos) ->
      Alcotest.(check (list int)) "digits" [ 1; 2; 3 ] lst;
      Alcotest.(check int) "final position" 3 pos
  | Error _ -> Alcotest.fail "Expected success"

let test_many1_success () =
  match Parseff.parse "456" (Parseff.many1 Parseff.digit) with
  | Ok lst -> Alcotest.(check (list int)) "digits" [ 4; 5; 6 ] lst
  | Error _ -> Alcotest.fail "Expected success"

let test_many1_failure () =
  match Parseff.parse "abc" (Parseff.many1 Parseff.digit) with
  | Ok _ -> Alcotest.fail "Expected failure"
  | Error _ -> ()

let test_optional_some () =
  match Parseff.parse "x" (Parseff.optional Parseff.letter) with
  | Ok (Some c) -> Alcotest.(check char) "letter" 'x' c
  | Ok None -> Alcotest.fail "Expected Some"
  | Error _ -> Alcotest.fail "Expected success"

let test_optional_none () =
  match parse_with_pos "123" (Parseff.optional Parseff.letter) with
  | Ok (None, pos) -> Alcotest.(check int) "position unchanged" 0 pos
  | Ok (Some _, _) -> Alcotest.fail "Expected None"
  | Error _ -> Alcotest.fail "Expected success"

let test_end_of_input_success () =
  match Parseff.parse "" Parseff.end_of_input with
  | Ok () -> ()
  | Error _ -> Alcotest.fail "Expected success"

let test_end_of_input_failure () =
  match Parseff.parse "x" Parseff.end_of_input with
  | Ok _ -> Alcotest.fail "Expected failure"
  | Error _ -> ()

let test_look_ahead_success () =
  let parser () =
    let _ = Parseff.look_ahead Parseff.digit in
    let d = Parseff.digit () in
    d
  in
  match Parseff.parse "5" parser with
  | Ok n -> Alcotest.(check int) "digit" 5 n
  | Error _ -> Alcotest.fail "Expected success"

let test_look_ahead_no_consume () =
  let parser () =
    let _ = Parseff.look_ahead (fun () -> Parseff.consume "hello") in
    Parseff.consume "hello"
  in
  match Parseff.parse "hello" parser with
  | Ok s -> Alcotest.(check string) "matched" "hello" s
  | Error _ -> Alcotest.fail "Expected success"

let test_sep_by_empty () =
  match
    Parseff.parse "" (Parseff.sep_by Parseff.digit (fun () -> Parseff.char ','))
  with
  | Ok lst -> Alcotest.(check (list int)) "empty" [] lst
  | Error _ -> Alcotest.fail "Expected success"

let test_sep_by_one () =
  match
    Parseff.parse "5"
      (Parseff.sep_by Parseff.digit (fun () -> Parseff.char ','))
  with
  | Ok lst -> Alcotest.(check (list int)) "one element" [ 5 ] lst
  | Error _ -> Alcotest.fail "Expected success"

let test_sep_by_several () =
  match
    Parseff.parse "1,2,3"
      (Parseff.sep_by Parseff.digit (fun () -> Parseff.char ','))
  with
  | Ok lst -> Alcotest.(check (list int)) "three elements" [ 1; 2; 3 ] lst
  | Error _ -> Alcotest.fail "Expected success"

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
  | Error _ -> Alcotest.fail "Expected success"

let test_between_failure_missing_close () =
  let parser =
    Parseff.between
      (fun () -> Parseff.char '(')
      (fun () -> Parseff.char ')')
      Parseff.digit
  in
  match Parseff.parse "(7" parser with
  | Ok _ -> Alcotest.fail "Expected failure"
  | Error { pos; error = `Expected expected } ->
      Alcotest.(check int) "error position" 2 pos;
      Alcotest.(check bool)
        "error message present" true
        (String.length expected > 0)
  | Error _ -> Alcotest.fail "Unexpected error type"

let test_end_by () =
  let parser = Parseff.end_by Parseff.digit (fun () -> Parseff.char ',') in
  match parse_with_pos "1,2,3," parser with
  | Ok (lst, pos) ->
      Alcotest.(check (list int)) "parsed list" [ 1; 2; 3 ] lst;
      Alcotest.(check int) "final position" 6 pos
  | Error _ -> Alcotest.fail "Expected success"

let test_end_by_empty () =
  let parser = Parseff.end_by Parseff.digit (fun () -> Parseff.char ',') in
  match parse_with_pos "" parser with
  | Ok (lst, pos) ->
      Alcotest.(check (list int)) "empty" [] lst;
      Alcotest.(check int) "final position" 0 pos
  | Error _ -> Alcotest.fail "Expected success"

let test_end_by1_failure () =
  let parser = Parseff.end_by1 Parseff.digit (fun () -> Parseff.char ',') in
  match Parseff.parse "" parser with
  | Ok _ -> Alcotest.fail "Expected failure"
  | Error _ -> ()

let test_end_by1_success () =
  let parser = Parseff.end_by1 Parseff.digit (fun () -> Parseff.char ',') in
  match parse_with_pos "1,2,3," parser with
  | Ok (lst, pos) ->
      Alcotest.(check (list int)) "parsed list" [ 1; 2; 3 ] lst;
      Alcotest.(check int) "final position" 6 pos
  | Error _ -> Alcotest.fail "Expected success"

let test_chainl1_left_assoc () =
  let num = Parseff.digit in
  let sub_op () =
    let _ = Parseff.char '-' in
    ( - )
  in
  match parse_with_pos "9-3-1" (Parseff.chainl1 num sub_op) with
  | Ok (value, pos) ->
      Alcotest.(check int) "left associative" 5 value;
      Alcotest.(check int) "final position" 5 pos
  | Error _ -> Alcotest.fail "Expected success"

let test_chainr1_right_assoc () =
  let num = Parseff.digit in
  let sub_op () =
    let _ = Parseff.char '-' in
    ( - )
  in
  match parse_with_pos "9-3-1" (Parseff.chainr1 num sub_op) with
  | Ok (value, pos) ->
      Alcotest.(check int) "right associative" 7 value;
      Alcotest.(check int) "final position" 5 pos
  | Error _ -> Alcotest.fail "Expected success"

let test_chainl_default () =
  let parser =
    Parseff.chainl Parseff.digit
      (fun () ->
        let _ = Parseff.char '+' in
        ( + ))
      42
  in
  match parse_with_pos "" parser with
  | Ok (value, pos) ->
      Alcotest.(check int) "default value" 42 value;
      Alcotest.(check int) "final position" 0 pos
  | Error _ -> Alcotest.fail "Expected success"

let test_chainl_non_default () =
  let parser =
    Parseff.chainl Parseff.digit
      (fun () ->
        let _ = Parseff.char '+' in
        ( + ))
      42
  in
  match parse_with_pos "1+2+3" parser with
  | Ok (value, pos) ->
      Alcotest.(check int) "combined value" 6 value;
      Alcotest.(check int) "final position" 5 pos
  | Error _ -> Alcotest.fail "Expected success"

let test_chainr_default () =
  let parser =
    Parseff.chainr Parseff.digit
      (fun () ->
        let _ = Parseff.char '+' in
        ( + ))
      42
  in
  match parse_with_pos "" parser with
  | Ok (value, pos) ->
      Alcotest.(check int) "default value" 42 value;
      Alcotest.(check int) "final position" 0 pos
  | Error _ -> Alcotest.fail "Expected success"

let test_chainr_non_default () =
  let parser =
    Parseff.chainr Parseff.digit
      (fun () ->
        let _ = Parseff.char '^' in
        fun l r -> int_of_float (float_of_int l ** float_of_int r))
      42
  in
  match parse_with_pos "2^3^2" parser with
  | Ok (value, pos) ->
      Alcotest.(check int) "combined value" 512 value;
      Alcotest.(check int) "final position" 5 pos
  | Error _ -> Alcotest.fail "Expected success"

let test_chainl1_requires_one () =
  let parser =
    Parseff.chainl1 Parseff.digit (fun () ->
        let _ = Parseff.char '+' in
        ( + ))
  in
  match Parseff.parse "" parser with
  | Ok _ -> Alcotest.fail "Expected failure"
  | Error _ -> ()

let test_chainr1_requires_one () =
  let parser =
    Parseff.chainr1 Parseff.digit (fun () ->
        let _ = Parseff.char '+' in
        ( + ))
  in
  match Parseff.parse "" parser with
  | Ok _ -> Alcotest.fail "Expected failure"
  | Error _ -> ()

let test_count () =
  match Parseff.parse "abc" (Parseff.count 3 Parseff.letter) with
  | Ok lst -> Alcotest.(check (list char)) "three letters" [ 'a'; 'b'; 'c' ] lst
  | Error _ -> Alcotest.fail "Expected success"

let test_count_insufficient () =
  match Parseff.parse "ab" (Parseff.count 3 Parseff.letter) with
  | Ok _ -> Alcotest.fail "Expected failure"
  | Error _ -> ()

let () =
  let open Alcotest in
  run "Parseff Primitives"
    [
      ( "basic",
        [
          test_case "consume success" `Quick test_consume_success;
          test_case "consume failure" `Quick test_consume_failure;
          test_case "char success" `Quick test_char_success;
          test_case "satisfy" `Quick test_satisfy;
          test_case "digit" `Quick test_digit;
        ] );
      ( "sequencing",
        [
          test_case "sequence" `Quick test_sequence;
          test_case "alternation left" `Quick test_alternation_left;
          test_case "alternation right" `Quick test_alternation_right;
          test_case "alternation failure" `Quick test_alternation_failure;
          test_case "one_of failure" `Quick test_one_of_failure;
        ] );
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
        ] );
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
        ] );
      ( "special",
        [
          test_case "end_of_input success" `Quick test_end_of_input_success;
          test_case "end_of_input failure" `Quick test_end_of_input_failure;
          test_case "look_ahead success" `Quick test_look_ahead_success;
          test_case "look_ahead no consume" `Quick test_look_ahead_no_consume;
        ] );
    ]
