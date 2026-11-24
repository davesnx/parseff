let raises name msg f =
  Alcotest.check_raises name (Invalid_argument msg) (fun () -> ignore (f ()))

let in_parse p () = Parseff.parse ~consume:`Partial "abc" p
let digit () = Parseff.digit ()
let comma () = Parseff.char ','

let test_negative_at_least () =
  let check name f =
    raises name
      ("Parseff." ^ name ^ ": at_least must be at least 0")
      (in_parse f)
  in
  check "take_while" (fun () ->
      Parseff.take_while ~at_least:(-1) (fun _ -> true)
  );
  check "take_while" (fun () ->
      Parseff.take_while ~at_least:(-5) ~at_most:0 (fun _ -> true)
  );
  check "many" (fun () -> Parseff.many ~at_least:(-1) digit);
  check "many" (fun () -> Parseff.many ~at_least:(-1) ~at_most:0 digit);
  check "sep_by" (fun () -> Parseff.sep_by ~at_least:(-1) digit comma);
  check "end_by" (fun () -> Parseff.end_by ~at_least:(-1) digit comma);
  check "many_iter" (fun () ->
      (Parseff.many_iter [@alert "-unstable"]) ~at_least:(-1) digit ignore
  );
  check "sep_by_iter" (fun () ->
      (Parseff.sep_by_iter [@alert "-unstable"]) ~at_least:(-1) digit comma
        ignore
  );
  check "whitespace" (fun () -> Parseff.whitespace ~at_least:(-1) ());
  check "Utf8.take_while" (fun () ->
      Parseff.Utf8.take_while ~at_least:(-1) (fun _ -> true)
  );
  check "Utf8.whitespace" (fun () -> Parseff.Utf8.whitespace ~at_least:(-1) ())

let test_zero_at_least () =
  Alcotest.(check bool)
    "take_while" true
    (Parseff.parse "" (fun () -> Parseff.take_while ~at_least:0 (fun _ -> true))
    = Ok ""
    )

let test_max_depth () =
  let p () = Parseff.rec_ (fun () -> ()) in
  let src () = Parseff.Source.of_string "" in
  List.iter
    (fun d ->
      raises "parse" "Parseff.parse: max_depth must be at least 1" (fun () ->
          Parseff.parse ~max_depth:d "" p
      );
      raises "Source.parse" "Parseff.Source.parse: max_depth must be at least 1"
        (fun () -> Parseff.Source.parse ~max_depth:d (src ()) p
      );
      raises "Source.parse_each"
        "Parseff.Source.parse_each: max_depth must be at least 1" (fun () ->
          Parseff.Source.parse_each ~max_depth:d (src ()) p
      );
      raises "Push.parse" "Parseff.Push.parse: max_depth must be at least 1"
        (fun () -> Parseff.Push.parse ~max_depth:d p
      );
      raises "Push.parse_each"
        "Parseff.Push.parse_each: max_depth must be at least 1" (fun () ->
          Parseff.Push.parse_each ~max_depth:d ~on_value:(fun () -> `Continue) p
      )
    )
    [ 0; -1 ];
  Alcotest.(check bool)
    "max_depth 1" true
    (Parseff.parse ~max_depth:1 "" p = Ok ())

type Parseff.custom_error += Bad

let test_negative_range () =
  let check name from upto =
    raises (name ^ " from")
      ("Parseff." ^ name ^ ": from must be at least 0")
      (in_parse from);
    raises (name ^ " upto")
      ("Parseff." ^ name ^ ": upto must be at least 0")
      (in_parse upto)
  in
  check "fail"
    (fun () -> Parseff.fail ~from:(-5) "bad")
    (fun () -> Parseff.fail ~upto:(-1) "bad");
  check "error"
    (fun () -> Parseff.error ~from:(-5) Bad)
    (fun () -> Parseff.error ~upto:(-1) Bad);
  check "warn"
    (fun () -> Parseff.warn ~from:(-5) Bad)
    (fun () -> Parseff.warn ~upto:(-1) Bad)

let test_zero_range () =
  match
    Parseff.parse "ab" (fun () ->
        let _ = Parseff.char 'a' in
        Parseff.fail ~from:0 "bad"
    )
  with
  | Error e ->
      Alcotest.(check (pair int int))
        "range" (0, 1)
        (e.range.start, e.range.stop)
  | Ok () ->
      Alcotest.fail "expected Error"

let test_negative_count () =
  raises "count" "Parseff.count: n must be at least 0"
    (in_parse (fun () -> Parseff.count (-1) digit));
  Alcotest.(check bool)
    "count 0" true
    (Parseff.parse "" (fun () -> Parseff.count 0 digit) = Ok [])

let test_location_of_position () =
  let msg =
    "Parseff.location_of_position: pos is not between 0 and the input length"
  in
  raises "negative" msg (fun () -> Parseff.location_of_position "abc" (-1));
  raises "past the end" msg (fun () -> Parseff.location_of_position "abc" 4);
  raises "empty input" msg (fun () -> Parseff.location_of_position "" 1);
  let l = Parseff.location_of_position "ab\nc" 4 in
  Alcotest.(check (pair int int)) "end of input" (2, 2) (l.line, l.byte_col)

let skip pairs input =
  Parseff.parse ~consume:`Partial input (fun () ->
      Parseff.skip_nested pairs (fun c -> c = ',');
      Parseff.position ()
  )

let test_skip_nested_symmetric () =
  let check name input expected =
    Alcotest.(check bool) name true (skip [ ('"', '"') ] input = Ok expected)
  in
  check "quoted separator" "\"a,b\", c" 5;
  check "two quoted runs" "\"a\"\"b,\" x, y" 9;
  check "unclosed quote" "\"a, b" 5;
  Alcotest.(check bool)
    "with asymmetric pairs" true
    (skip [ ('(', ')'); ('|', '|') ] "(|a, b|), c" = Ok 8)

let () =
  let open Alcotest in
  run "Param_validation"
    [
      ( "at_least",
        [
          test_case "negative raises" `Quick test_negative_at_least;
          test_case "zero accepted" `Quick test_zero_at_least;
        ]
      );
      ("max_depth", [ test_case "non-positive raises" `Quick test_max_depth ]);
      ( "range",
        [
          test_case "negative raises" `Quick test_negative_range;
          test_case "zero accepted" `Quick test_zero_range;
        ]
      );
      ("count", [ test_case "negative raises" `Quick test_negative_count ]);
      ( "location_of_position",
        [ test_case "out of range raises" `Quick test_location_of_position ]
      );
      ( "skip_nested",
        [ test_case "symmetric pair closes" `Quick test_skip_nested_symmetric ]
      );
    ]
