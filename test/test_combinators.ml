type Parseff.custom_error += Cause_e

let show_error (e : Parseff.error) =
  match e.kind with
  | Custom Cause_e ->
      "E"
  | Expected _ ->
      Printf.sprintf "Expected %S" (Parseff.error_message e)
  | Failed s ->
      Printf.sprintf "Failed %S" s
  | _ ->
      "other"

let show show_v = function
  | Ok v ->
      "Ok " ^ show_v v
  | Error e ->
      Printf.sprintf "Error@%d %s" e.Parseff.range.start (show_error e)

let chunked_source input =
  let pos = ref 0 in
  Parseff.Source.of_function (fun buf off len ->
      let n = min (min 1 len) (String.length input - !pos) in
      Bytes.blit_string input !pos buf off n;
      pos := !pos + n;
      n
  )

let both show_v input parser expected =
  let with_pos () =
    let v = parser () in
    (v, Parseff.position ())
  in
  let show_vp (v, p) = Printf.sprintf "(%s, %d)" (show_v v) p in
  Alcotest.(check string)
    "parse" expected
    (show show_vp (Parseff.parse ~consume:`Partial input with_pos));
  Alcotest.(check string)
    "parse_source" expected
    (show show_vp
       (Parseff.parse_source ~consume:`Partial (chunked_source input) with_pos)
    )

let show_string s = Printf.sprintf "%S" s
let show_ints l = "[" ^ String.concat ";" (List.map string_of_int l) ^ "]"

let test_one_of_labeled_custom_error () =
  let r =
    Parseff.parse "x" (fun () ->
        Parseff.one_of_labeled
          [
            ( "a",
              fun () ->
                ignore (Parseff.consume "x");
                Parseff.error Cause_e
            );
            ( "b",
              fun () ->
                ignore (Parseff.consume "y");
                ""
            );
          ]
          ()
    )
  in
  Alcotest.(check string) "custom error kept" "Error@1 E" (show show_string r)

let test_expect_custom_error_through_or () =
  let r =
    Parseff.parse "x" (fun () ->
        Parseff.expect "thing" (fun () ->
            Parseff.or_
              (fun () ->
                ignore (Parseff.consume "x");
                Parseff.error Cause_e
              )
              (fun () ->
                ignore (Parseff.consume "y");
                ""
              )
              ()
        )
    )
  in
  Alcotest.(check string) "custom error kept" "Error@1 E" (show show_string r)

let test_many_non_consuming () =
  both show_ints "abc"
    (fun () ->
      [ List.length (Parseff.many (fun () -> Parseff.peek_char ()) ()) ]
    )
    "Ok ([0], 0)"

let test_many_optional_digit () =
  both show_ints "abc"
    (fun () ->
      [
        List.length
          (Parseff.many (fun () -> Parseff.optional Parseff.digit ()) ());
      ]
    )
    "Ok ([0], 0)"

let test_many_stops_after_consuming () =
  both show_ints "12a"
    (fun () ->
      Parseff.many
        (fun () ->
          match Parseff.optional Parseff.digit () with
          | Some d ->
              Char.code d - Char.code '0'
          | None ->
              -1
        )
        ()
    )
    "Ok ([1;2], 2)"

let test_many_at_least_non_consuming () =
  let r =
    Parseff.parse ~consume:`Partial "abc" (fun () ->
        List.length
          (Parseff.many ~at_least:1 (fun () -> Parseff.peek_char ()) ())
    )
  in
  Alcotest.(check string) "required element kept" "Ok 1" (show string_of_int r)

let test_sep_by_non_consuming () =
  both show_ints "abc"
    (fun () ->
      [
        List.length
          (Parseff.sep_by (fun () -> Parseff.peek_char ()) (fun () -> ()) ());
      ]
    )
    "Ok ([1], 0)"

let test_fold_left_non_consuming () =
  both string_of_int "abc"
    (fun () -> Parseff.fold_left (fun () -> 1) (fun () -> ( + )) ())
    "Ok (1, 0)"

let bounded p =
  let calls = ref 0 in
  fun () ->
    incr calls;
    if !calls > 1000 then Alcotest.fail "the loop did not stop";
    p ()

let tree a b = "(" ^ a ^ "^" ^ b ^ ")"

let digit_or_dash () =
  match Parseff.optional Parseff.digit () with
  | Some d ->
      String.make 1 d
  | None ->
      "-"

let test_fold_right_non_consuming () =
  both string_of_int "abc"
    (fun () -> Parseff.fold_right (bounded (fun () -> 1)) (fun () -> ( + )) ())
    "Ok (1, 0)";
  both string_of_int "abc"
    (fun () ->
      Parseff.fold_right
        (bounded (fun () -> 1))
        (fun () -> ( + ))
        ~otherwise:0 ()
    )
    "Ok (1, 0)"

let test_fold_right_op_non_consuming () =
  both show_string "12a"
    (fun () -> Parseff.fold_right (bounded digit_or_dash) (fun () -> tree) ())
    {|Ok ("(1^2)", 2)|}

let test_fold_right_element_non_consuming () =
  both show_string "1^^"
    (fun () ->
      Parseff.fold_right (bounded digit_or_dash)
        (fun () ->
          ignore (Parseff.optional (fun () -> Parseff.char '^') ());
          tree
        )
        ()
    )
    {|Ok ("(1^(-^-))", 3)|}

let test_fold_right_right_assoc () =
  both show_string "1^2^3"
    (fun () ->
      Parseff.fold_right
        (fun () -> String.make 1 (Parseff.digit ()))
        (fun () ->
          ignore (Parseff.char '^');
          tree
        )
        ()
    )
    {|Ok ("(1^(2^3))", 5)|}

let test_fold_right_dropped_pair_rolled_back () =
  let d = Parseff.Diagnostics.create () in
  let cell = Parseff.Ref.create 0 in
  let r =
    Parseff.parse ~consume:`Partial ~diagnostics:d "12" (fun () ->
        let v =
          Parseff.fold_right (bounded digit_or_dash)
            (fun () ->
              Parseff.warn d "op";
              Parseff.Ref.set cell (Parseff.Ref.get cell + 1);
              tree
            )
            ()
        in
        Printf.sprintf "%s %d" v (Parseff.Ref.get cell)
    )
  in
  Alcotest.(check string) "value" {|Ok "(1^2) 1"|} (show show_string r);
  Alcotest.(check int) "diagnostics" 1 (Parseff.Diagnostics.length d)

let test_take_max_int () =
  let r =
    Parseff.parse "abc" (fun () ->
        ignore (Parseff.any_char ());
        Parseff.take max_int
    )
  in
  Alcotest.(check string)
    "short input"
    {|Error@1 Expected "expected 4611686018427387903 bytes, found only 2 (end of input)"|}
    (show show_string r)

let test_take_max_int_streaming () =
  let r =
    Parseff.parse_source (chunked_source "abc") (fun () ->
        ignore (Parseff.any_char ());
        Parseff.take max_int
    )
  in
  match r with
  | Error ({ kind = Expected { found = Short _; _ }; _ } as e) ->
      Alcotest.(check string)
        "expected items"
        "expected 4611686018427387903 bytes, found only 2 (end of input)"
        (Parseff.error_message e);
      ()
  | Error { kind = Backtrack_window_exceeded _; _ } ->
      ()
  | _ ->
      Alcotest.fail "expected end of input or window error"

let test_take_while_huge_at_least () =
  let huge = "4611686018427387903" in
  ( match
      Parseff.parse "abc" (fun () ->
          Parseff.take_while ~at_least:max_int (fun _ -> true)
      )
    with
  | Error ({ kind = Expected { found = End; _ }; _ } as e) ->
      Alcotest.(check int) "position" 3 e.range.start;
      Alcotest.(check string)
        "message"
        ("expected at least " ^ huge ^ " characters, found end of input")
        (Parseff.error_message e)
  | _ ->
      Alcotest.fail "expected an end of input error"
  );
  ( match
      Parseff.parse "aab" (fun () ->
          Parseff.take_while ~at_least:max_int (fun c -> c = 'a')
      )
    with
  | Error ({ kind = Expected { found = Text "b"; _ }; _ } as e) ->
      Alcotest.(check int) "position" 2 e.range.start;
      Alcotest.(check string)
        "message"
        ("expected at least " ^ huge ^ " characters, found 'b'")
        (Parseff.error_message e)
  | _ ->
      Alcotest.fail "expected a mismatch"
  );
  match
    Parseff.parse "aab" (fun () ->
        Parseff.Utf8.take_while ~at_least:max_int (fun u ->
            Uchar.to_int u = Char.code 'a'
        )
    )
  with
  | Error e ->
      Alcotest.(check string)
        "utf8 message"
        ("expected at least " ^ huge ^ " characters, found 'b'")
        (Parseff.error_message e)
  | Ok _ ->
      Alcotest.fail "expected a failure"

let test_consume_prefix_mismatch () =
  let r = Parseff.parse "x" (fun () -> Parseff.consume "true") in
  Alcotest.(check string)
    "mismatch" {|Error@0 Expected "expected \"true\", found 'x'"|}
    (show show_string r);
  let r = Parseff.parse "tr" (fun () -> Parseff.consume "true") in
  Alcotest.(check string)
    "proper prefix" {|Error@2 Expected "expected \"ue\", found end of input"|}
    (show show_string r)

let test_consume_or_mismatch () =
  let p () =
    Parseff.or_
      (fun () -> Parseff.consume "true")
      (fun () -> Parseff.consume "false")
      ()
  in
  let expected =
    {|Error@0 Expected "expected \"false\" or \"true\", found 'no'"|}
  in
  Alcotest.(check string)
    "parse" expected
    (show show_string (Parseff.parse "no" p));
  Alcotest.(check string)
    "parse_source" expected
    (show show_string (Parseff.parse_source (chunked_source "no") p))

let test_consume_streaming_slow_source () =
  let r =
    Parseff.parse_source (chunked_source "true") (fun () ->
        Parseff.consume "true"
    )
  in
  Alcotest.(check string) "slow source" "Ok \"true\"" (show show_string r)

let () =
  let open Alcotest in
  run "Combinators"
    [
      ( "error relabeling",
        [
          test_case "one_of_labeled keeps custom error" `Quick
            test_one_of_labeled_custom_error;
          test_case "expect keeps custom error through or_" `Quick
            test_expect_custom_error_through_or;
        ]
      );
      ( "non-consuming loops",
        [
          test_case "many peek_char" `Quick test_many_non_consuming;
          test_case "many optional digit" `Quick test_many_optional_digit;
          test_case "many stops after consuming" `Quick
            test_many_stops_after_consuming;
          test_case "many at_least" `Quick test_many_at_least_non_consuming;
          test_case "sep_by" `Quick test_sep_by_non_consuming;
          test_case "fold_left" `Quick test_fold_left_non_consuming;
          test_case "fold_right" `Quick test_fold_right_non_consuming;
          test_case "fold_right op" `Quick test_fold_right_op_non_consuming;
          test_case "fold_right element" `Quick
            test_fold_right_element_non_consuming;
          test_case "fold_right right assoc" `Quick test_fold_right_right_assoc;
          test_case "fold_right dropped pair" `Quick
            test_fold_right_dropped_pair_rolled_back;
        ]
      );
      ( "take",
        [
          test_case "max_int" `Quick test_take_max_int;
          test_case "max_int streaming" `Quick test_take_max_int_streaming;
          test_case "huge at_least" `Quick test_take_while_huge_at_least;
        ]
      );
      ( "consume",
        [
          test_case "prefix mismatch" `Quick test_consume_prefix_mismatch;
          test_case "or_ mismatch" `Quick test_consume_or_mismatch;
          test_case "slow source" `Quick test_consume_streaming_slow_source;
        ]
      );
    ]
