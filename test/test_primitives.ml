open Parseff

let test_consume_success () =
  match run "hello" (fun () -> consume "hello") with
  | Ok (s, pos) ->
      Alcotest.(check string) "matched string" "hello" s;
      Alcotest.(check int) "final position" 5 pos
  | Error _ -> Alcotest.fail "Expected success"

let test_consume_failure () =
  match run "hello" (fun () -> consume "world") with
  | Ok _ -> Alcotest.fail "Expected failure"
  | Error { pos; expected } ->
      Alcotest.(check int) "error position" 0 pos;
      Alcotest.(check string) "error message" "expected \"world\"" expected

let test_char_success () =
  match run "a" (fun () -> char 'a') with
  | Ok (c, pos) ->
      Alcotest.(check char) "matched char" 'a' c;
      Alcotest.(check int) "final position" 1 pos
  | Error _ -> Alcotest.fail "Expected success"

let test_satisfy () =
  let is_digit c = c >= '0' && c <= '9' in
  match run "7" (fun () -> satisfy is_digit "digit") with
  | Ok (c, _) -> Alcotest.(check char) "matched digit" '7' c
  | Error _ -> Alcotest.fail "Expected success"

let test_digit () =
  match run "9" digit with
  | Ok (n, _) -> Alcotest.(check int) "parsed digit" 9 n
  | Error _ -> Alcotest.fail "Expected success"

let test_sequence () =
  let parser () =
    let a = consume "hello" in
    let _ = consume " " in
    let b = consume "world" in
    (a, b)
  in
  match run "hello world" parser with
  | Ok ((a, b), pos) ->
      Alcotest.(check string) "first part" "hello" a;
      Alcotest.(check string) "second part" "world" b;
      Alcotest.(check int) "final position" 11 pos
  | Error _ -> Alcotest.fail "Expected success"

let test_alternation_left () =
  let parser () = ((fun () -> consume "foo") <|> fun () -> consume "bar") () in
  match run "foo" parser with
  | Ok (s, _) -> Alcotest.(check string) "matched left" "foo" s
  | Error _ -> Alcotest.fail "Expected success"

let test_alternation_right () =
  let parser () = ((fun () -> consume "foo") <|> fun () -> consume "bar") () in
  match run "bar" parser with
  | Ok (s, _) -> Alcotest.(check string) "matched right" "bar" s
  | Error _ -> Alcotest.fail "Expected success"

let test_alternation_failure () =
  let parser () = ((fun () -> consume "foo") <|> fun () -> consume "bar") () in
  match run "baz" parser with
  | Ok _ -> Alcotest.fail "Expected failure"
  | Error _ -> ()

let test_many_empty () =
  match run "" (many digit) with
  | Ok (lst, _) -> Alcotest.(check int) "empty list" 0 (List.length lst)
  | Error _ -> Alcotest.fail "Expected success"

let test_many_several () =
  match run "123" (many digit) with
  | Ok (lst, pos) ->
      Alcotest.(check (list int)) "digits" [ 1; 2; 3 ] lst;
      Alcotest.(check int) "final position" 3 pos
  | Error _ -> Alcotest.fail "Expected success"

let test_many1_success () =
  match run "456" (many1 digit) with
  | Ok (lst, _) -> Alcotest.(check (list int)) "digits" [ 4; 5; 6 ] lst
  | Error _ -> Alcotest.fail "Expected success"

let test_many1_failure () =
  match run "abc" (many1 digit) with
  | Ok _ -> Alcotest.fail "Expected failure"
  | Error _ -> ()

let test_optional_some () =
  match run "x" (optional letter) with
  | Ok (Some c, _) -> Alcotest.(check char) "letter" 'x' c
  | Ok (None, _) -> Alcotest.fail "Expected Some"
  | Error _ -> Alcotest.fail "Expected success"

let test_optional_none () =
  match run "123" (optional letter) with
  | Ok (None, pos) -> Alcotest.(check int) "position unchanged" 0 pos
  | Ok (Some _, _) -> Alcotest.fail "Expected None"
  | Error _ -> Alcotest.fail "Expected success"

let test_end_of_input_success () =
  match run "" end_of_input with
  | Ok ((), _) -> ()
  | Error _ -> Alcotest.fail "Expected success"

let test_end_of_input_failure () =
  match run "x" end_of_input with
  | Ok _ -> Alcotest.fail "Expected failure"
  | Error _ -> ()

let test_look_ahead_success () =
  let parser () =
    let _ = look_ahead digit in
    let d = digit () in
    d
  in
  match run "5" parser with
  | Ok (n, _) -> Alcotest.(check int) "digit" 5 n
  | Error _ -> Alcotest.fail "Expected success"

let test_look_ahead_no_consume () =
  let parser () =
    let _ = look_ahead (fun () -> consume "hello") in
    consume "hello"
  in
  match run "hello" parser with
  | Ok (s, _) -> Alcotest.(check string) "matched" "hello" s
  | Error _ -> Alcotest.fail "Expected success"

let test_sep_by_empty () =
  match run "" (sep_by digit (fun () -> char ',')) with
  | Ok (lst, _) -> Alcotest.(check (list int)) "empty" [] lst
  | Error _ -> Alcotest.fail "Expected success"

let test_sep_by_one () =
  match run "5" (sep_by digit (fun () -> char ',')) with
  | Ok (lst, _) -> Alcotest.(check (list int)) "one element" [ 5 ] lst
  | Error _ -> Alcotest.fail "Expected success"

let test_sep_by_several () =
  match run "1,2,3" (sep_by digit (fun () -> char ',')) with
  | Ok (lst, _) -> Alcotest.(check (list int)) "three elements" [ 1; 2; 3 ] lst
  | Error _ -> Alcotest.fail "Expected success"

let test_count () =
  match run "abc" (count 3 letter) with
  | Ok (lst, _) -> Alcotest.(check (list char)) "three letters" [ 'a'; 'b'; 'c' ] lst
  | Error _ -> Alcotest.fail "Expected success"

let test_count_insufficient () =
  match run "ab" (count 3 letter) with
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
        ] );
      ( "special",
        [
          test_case "end_of_input success" `Quick test_end_of_input_success;
          test_case "end_of_input failure" `Quick test_end_of_input_failure;
          test_case "look_ahead success" `Quick test_look_ahead_success;
          test_case "look_ahead no consume" `Quick test_look_ahead_no_consume;
        ] );
    ]
