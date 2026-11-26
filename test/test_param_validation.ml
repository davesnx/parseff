let raises name msg f =
  Alcotest.check_raises name (Invalid_argument msg) (fun () ->
      let _ = f () in
      ()
  )

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
  check "Iter.many" (fun () -> Parseff.Iter.many ~at_least:(-1) digit ignore);
  check "Iter.sep_by" (fun () ->
      Parseff.Iter.sep_by ~at_least:(-1) digit comma ignore
  );
  check "whitespace" (fun () -> Parseff.whitespace ~at_least:(-1) ());
  check "Utf8.take_while" (fun () ->
      Parseff.Utf8.take_while ~at_least:(-1) (fun _ -> true)
  );
  check "Utf8.whitespace" (fun () -> Parseff.Utf8.whitespace ~at_least:(-1) ());
  check "Slice.take_while" (fun () ->
      Parseff.Slice.take_while ~at_least:(-1) (fun _ -> true)
  );
  check "Slice.take_while" (fun () ->
      Parseff.Slice.take_while ~at_least:(-1)
        ~set:(Parseff.Charset.of_predicate (fun _ -> true))
        (fun _ -> true)
  );
  check "skip_while" (fun () ->
      Parseff.skip_while ~at_least:(-1) (fun _ -> true)
  );
  check "skip_while" (fun () ->
      Parseff.skip_while ~at_least:(-1)
        ~set:(Parseff.Charset.of_predicate (fun _ -> true))
        (fun _ -> true)
  );
  check "Utf8.skip_while" (fun () ->
      Parseff.Utf8.skip_while ~at_least:(-1) (fun _ -> true)
  );
  check "Utf8.Slice.take_while" (fun () ->
      Parseff.Utf8.Slice.take_while ~at_least:(-1) (fun _ -> true)
  )

let test_negative_at_least_outside_parse () =
  let check name f =
    raises name ("Parseff." ^ name ^ ": at_least must be at least 0") f
  in
  let any _ = true in
  check "take_while" (fun () -> Parseff.take_while ~at_least:(-1) any);
  check "skip_while" (fun () -> Parseff.skip_while ~at_least:(-1) any);
  check "Slice.take_while" (fun () ->
      Parseff.Slice.take_while ~at_least:(-1) any
  );
  check "Utf8.skip_while" (fun () -> Parseff.Utf8.skip_while ~at_least:(-1) any);
  check "Utf8.Slice.take_while" (fun () ->
      Parseff.Utf8.Slice.take_while ~at_least:(-1) any
  )

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

let test_invalid_at_most () =
  let set = Parseff.Charset.of_predicate (fun _ -> true) in
  let any _ = true in
  let check name f =
    List.iter
      (fun (at_least, at_most) ->
        raises
          (Printf.sprintf "%s ~at_least:%d ~at_most:%d" name at_least at_most)
          ("Parseff." ^ name
         ^ ": at_most must be at least 0 and at least at_least"
          )
          (in_parse (fun () -> f at_least at_most))
      )
      [ (0, -1); (2, 1); (0, min_int) ]
  in
  check "take_while" (fun at_least at_most ->
      Parseff.take_while ~at_least ~at_most any
  );
  check "take_while" (fun at_least at_most ->
      Parseff.take_while ~at_least ~at_most ~set any
  );
  check "Slice.take_while" (fun at_least at_most ->
      Parseff.Slice.take_while ~at_least ~at_most any
  );
  check "Slice.take_while" (fun at_least at_most ->
      Parseff.Slice.take_while ~at_least ~at_most ~set any
  );
  check "skip_while" (fun at_least at_most ->
      Parseff.skip_while ~at_least ~at_most any
  );
  check "skip_while" (fun at_least at_most ->
      Parseff.skip_while ~at_least ~at_most ~set any
  );
  check "Utf8.take_while" (fun at_least at_most ->
      Parseff.Utf8.take_while ~at_least ~at_most (fun _ -> true)
  );
  check "Utf8.Slice.take_while" (fun at_least at_most ->
      Parseff.Utf8.Slice.take_while ~at_least ~at_most (fun _ -> true)
  );
  check "Utf8.skip_while" (fun at_least at_most ->
      Parseff.Utf8.skip_while ~at_least ~at_most (fun _ -> true)
  );
  check "many" (fun at_least at_most -> Parseff.many ~at_least ~at_most digit);
  check "sep_by" (fun at_least at_most ->
      Parseff.sep_by ~at_least ~at_most digit comma
  );
  check "end_by" (fun at_least at_most ->
      Parseff.end_by ~at_least ~at_most digit comma
  );
  List.iter
    (fun at_most ->
      raises "take_until" "Parseff.take_until: at_most must be at least 0"
        (in_parse (fun () -> Parseff.take_until ~at_most "c"));
      raises "skip_until" "Parseff.skip_until: at_most must be at least 0"
        (in_parse (fun () -> Parseff.skip_until ~at_most "c"))
    )
    [ -1; min_int ]

let test_at_most_equal_at_least () =
  let ok name expected p =
    Alcotest.(check bool)
      name true
      (Parseff.parse ~consume:`Partial "abc" p = Ok expected)
  in
  ok "take_while" 0 (fun () ->
      String.length (Parseff.take_while ~at_least:0 ~at_most:0 (fun _ -> false))
  );
  ok "many" 0 (fun () -> List.length (Parseff.many ~at_least:0 ~at_most:0 digit));
  ok "sep_by" 0 (fun () ->
      List.length (Parseff.sep_by ~at_least:0 ~at_most:0 digit comma)
  );
  ok "end_by" 0 (fun () ->
      List.length (Parseff.end_by ~at_least:0 ~at_most:0 digit comma)
  )

let () =
  let open Alcotest in
  run "Param_validation"
    [
      ( "at_least",
        [
          test_case "negative raises" `Quick test_negative_at_least;
          test_case "negative raises outside a parse" `Quick
            test_negative_at_least_outside_parse;
          test_case "zero accepted" `Quick test_zero_at_least;
        ]
      );
      ( "at_most",
        [
          test_case "invalid raises" `Quick test_invalid_at_most;
          test_case "equal to at_least accepted" `Quick
            test_at_most_equal_at_least;
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
