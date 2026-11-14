let chain n = String.concat "^" (List.init n (fun _ -> "1"))

let parser () =
  Parseff.fold_right Parseff.digit
    (fun () ->
      let _ = Parseff.char '^' in
      ( + )
    )
    ()

let parse n =
  match Parseff.parse (chain n) parser with
  | r ->
      r
  | exception Stack_overflow ->
      Alcotest.failf "Stack_overflow on a chain of %d operands" n

let limit () =
  match parse ((Gc.get ()).stack_limit / 8) with
  | Error { kind = Depth_limit_exceeded { limit }; _ } ->
      limit
  | Ok _ ->
      Alcotest.fail "Expected Depth_limit_exceeded, got Ok"
  | Error e ->
      Alcotest.fail (Parseff.error_message e)

let test_over_limit () =
  let limit = limit () in
  Alcotest.(check bool) "limit above 10000" true (limit > 10_000);
  match parse (limit + 1) with
  | Error ({ kind = Depth_limit_exceeded { limit = l }; _ } as e) ->
      Alcotest.(check int) "reported limit" limit l;
      Alcotest.(check int) "error position" ((2 * limit) + 1) e.range.start;
      Alcotest.(check string)
        "message"
        (Printf.sprintf "maximum nesting depth %d exceeded" limit)
        (Parseff.error_message e);
      let lines = String.split_on_char '\n' (Parseff.render_error e) in
      Alcotest.(check string)
        "help" "  * help: raise the stack limit with OCAMLRUNPARAM=l"
        (List.nth lines (List.length lines - 1))
  | Ok _ ->
      Alcotest.fail "Expected Depth_limit_exceeded, got Ok"
  | Error e ->
      Alcotest.fail (Parseff.error_message e)

let test_at_limit () =
  let limit = limit () in
  match parse limit with
  | Ok v ->
      Alcotest.(check int) "sum" limit v
  | Error e ->
      Alcotest.fail (Parseff.error_message e)

let test_parse_after_limit () =
  ignore (limit ());
  match parse 3 with
  | Ok v ->
      Alcotest.(check int) "sum" 3 v
  | Error e ->
      Alcotest.fail (Parseff.error_message e)

let () =
  Alcotest.run "Parseff fold_right limit"
    [
      ( "fold_right",
        [
          Alcotest.test_case "over the limit" `Quick test_over_limit;
          Alcotest.test_case "at the limit" `Quick test_at_limit;
          Alcotest.test_case "parse after the limit" `Quick
            test_parse_after_limit;
        ]
      );
    ]
