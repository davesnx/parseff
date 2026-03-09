let number () =
  let digits = Parseff.many1 Parseff.digit () in
  let n = List.fold_left (fun acc d -> (acc * 10) + d) 0 digits in
  if n >= 0 && n <= 255 then
    n
  else
    Parseff.fail (Printf.sprintf "number out of range: %d" n)

let ip_address () =
  let a = number () in
  let _ = Parseff.consume "." in
  let b = number () in
  let _ = Parseff.consume "." in
  let c = number () in
  let _ = Parseff.consume "." in
  let d = number () in
  Parseff.end_of_input ();
  (a, b, c, d)

let test_valid_ip1 () =
  match Parseff.parse "1.2.3.4" ip_address with
  | Ok (a, b, c, d) ->
      Alcotest.(check int) "a" 1 a;
      Alcotest.(check int) "b" 2 b;
      Alcotest.(check int) "c" 3 c;
      Alcotest.(check int) "d" 4 d
  | Error _ ->
      Alcotest.fail "Expected success"

let test_valid_ip2 () =
  match Parseff.parse "255.255.255.255" ip_address with
  | Ok (a, b, c, d) ->
      Alcotest.(check int) "a" 255 a;
      Alcotest.(check int) "b" 255 b;
      Alcotest.(check int) "c" 255 c;
      Alcotest.(check int) "d" 255 d
  | Error _ ->
      Alcotest.fail "Expected success"

let test_valid_ip3 () =
  match Parseff.parse "192.168.1.100" ip_address with
  | Ok (a, b, c, d) ->
      Alcotest.(check int) "a" 192 a;
      Alcotest.(check int) "b" 168 b;
      Alcotest.(check int) "c" 1 c;
      Alcotest.(check int) "d" 100 d
  | Error _ ->
      Alcotest.fail "Expected success"

let test_invalid_out_of_range () =
  match Parseff.parse "1.2.3.256" ip_address with
  | Ok _ ->
      Alcotest.fail "Expected failure for out of range"
  | Error { pos; error = `Expected expected } ->
      Alcotest.(check int) "error position" 9 pos;
      Alcotest.(check string)
        "error message" "number out of range: 256" expected
  | Error _ ->
      Alcotest.fail "Unexpected error type"

let test_invalid_incomplete () =
  match Parseff.parse "1.2.3" ip_address with
  | Ok _ ->
      Alcotest.fail "Expected failure for incomplete IP"
  | Error { pos; error = `Unexpected_end_of_input } ->
      Alcotest.(check int) "error position" 5 pos
  | Error _ ->
      Alcotest.fail "Expected `Unexpected_end_of_input"

let test_invalid_trailing () =
  match Parseff.parse "1.2.3.4 extra" ip_address with
  | Ok _ ->
      Alcotest.fail "Expected failure for trailing data"
  | Error { pos; error = `Expected expected } ->
      Alcotest.(check int) "error position" 7 pos;
      Alcotest.(check string) "error message" "expected end of input" expected
  | Error _ ->
      Alcotest.fail "Unexpected error type"

let () =
  let open Alcotest in
  run "IP Address Parser"
    [
      ( "valid",
        [
          test_case "1.2.3.4" `Quick test_valid_ip1;
          test_case "255.255.255.255" `Quick test_valid_ip2;
          test_case "192.168.1.100" `Quick test_valid_ip3;
        ]
      );
      ( "invalid",
        [
          test_case "out of range" `Quick test_invalid_out_of_range;
          test_case "incomplete" `Quick test_invalid_incomplete;
          test_case "trailing data" `Quick test_invalid_trailing;
        ]
      );
    ]
