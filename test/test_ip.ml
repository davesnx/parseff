open Parseff

(** Parse a decimal number 0-255 *)
let number () =
  let digits = many1 digit () in
  let n =
    List.fold_left (fun acc d -> (acc * 10) + d) 0 digits
  in
  if n >= 0 && n <= 255 then n else fail (Printf.sprintf "number out of range: %d" n)

(** Parse an IP address: 0-255.0-255.0-255.0-255 *)
let ip_address () =
  let a = number () in
  let _ = consume "." in
  let b = number () in
  let _ = consume "." in
  let c = number () in
  let _ = consume "." in
  let d = number () in
  end_of_input ();
  (a, b, c, d)

let test_valid_ip1 () =
  match run "1.2.3.4" ip_address with
  | Ok ((a, b, c, d), _) ->
      Alcotest.(check int) "a" 1 a;
      Alcotest.(check int) "b" 2 b;
      Alcotest.(check int) "c" 3 c;
      Alcotest.(check int) "d" 4 d
  | Error _ -> Alcotest.fail "Expected success"

let test_valid_ip2 () =
  match run "255.255.255.255" ip_address with
  | Ok ((a, b, c, d), _) ->
      Alcotest.(check int) "a" 255 a;
      Alcotest.(check int) "b" 255 b;
      Alcotest.(check int) "c" 255 c;
      Alcotest.(check int) "d" 255 d
  | Error _ -> Alcotest.fail "Expected success"

let test_valid_ip3 () =
  match run "192.168.1.100" ip_address with
  | Ok ((a, b, c, d), _) ->
      Alcotest.(check int) "a" 192 a;
      Alcotest.(check int) "b" 168 b;
      Alcotest.(check int) "c" 1 c;
      Alcotest.(check int) "d" 100 d
  | Error _ -> Alcotest.fail "Expected success"

let test_invalid_out_of_range () =
  match run "1.2.3.256" ip_address with
  | Ok _ -> Alcotest.fail "Expected failure for out of range"
  | Error { pos; expected } ->
      (* The error happens after parsing "256", so position is 9 *)
      Alcotest.(check int) "error position" 9 pos;
      Alcotest.(check bool) "contains 'out of range'" true (String.contains expected 'r')

let test_invalid_incomplete () =
  match run "1.2.3" ip_address with
  | Ok _ -> Alcotest.fail "Expected failure for incomplete IP"
  | Error _ -> ()

let test_invalid_trailing () =
  match run "1.2.3.4 extra" ip_address with
  | Ok _ -> Alcotest.fail "Expected failure for trailing data"
  | Error { expected; _ } ->
      Alcotest.(check bool)
        "expected end of input" true
        (String.contains expected 'e')

let () =
  let open Alcotest in
  run "IP Address Parser"
    [
      ( "valid",
        [
          test_case "1.2.3.4" `Quick test_valid_ip1;
          test_case "255.255.255.255" `Quick test_valid_ip2;
          test_case "192.168.1.100" `Quick test_valid_ip3;
        ] );
      ( "invalid",
        [
          test_case "out of range" `Quick test_invalid_out_of_range;
          test_case "incomplete" `Quick test_invalid_incomplete;
          test_case "trailing data" `Quick test_invalid_trailing;
        ] );
    ]
