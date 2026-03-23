let check_parseff_error exn =
  let msg = Printexc.to_string exn in
  Alcotest.(check bool)
    "starts with Parseff:" true
    (String.length msg >= 7 && String.sub msg 0 7 = "Parseff");
  Alcotest.(check bool)
    "mentions parse context" true
    (Re.(execp (compile (str "parse context"))) msg)

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

let () =
  let open Alcotest in
  run "Errors"
    [
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
