let show_error = function
  | `E ->
      "E"
  | `Expected s ->
      Printf.sprintf "Expected %S" s
  | `Unexpected_end_of_input ->
      "Unexpected_end_of_input"
  | `Message s ->
      Printf.sprintf "Message %S" s
  | _ ->
      "other"

let show show_v = function
  | Legacy.Ok v ->
      "Ok " ^ show_v v
  | Legacy.Error { pos; error } ->
      Printf.sprintf "Error@%d %s" pos (show_error error)

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
    (show show_vp (Legacy.parse input with_pos));
  Alcotest.(check string)
    "parse_source" expected
    (show show_vp (Legacy.parse_source (chunked_source input) with_pos))

let show_string s = Printf.sprintf "%S" s
let show_ints l = "[" ^ String.concat ";" (List.map string_of_int l) ^ "]"

let test_one_of_labeled_user_error () =
  let r =
    Legacy.parse "x" (fun () ->
        Parseff.one_of_labeled
          [
            ( "a",
              fun () ->
                ignore (Parseff.consume "x");
                Parseff.error `E
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
  Alcotest.(check string) "user error kept" "Error@1 E" (show show_string r)

let test_expect_user_error_through_or () =
  let r =
    Legacy.parse "x" (fun () ->
        Parseff.expect "thing" (fun () ->
            Parseff.or_
              (fun () ->
                ignore (Parseff.consume "x");
                Parseff.error `E
              )
              (fun () ->
                ignore (Parseff.consume "y");
                ""
              )
              ()
        )
    )
  in
  Alcotest.(check string) "user error kept" "Error@1 E" (show show_string r)

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
              d
          | None ->
              -1
        )
        ()
    )
    "Ok ([1;2], 2)"

let test_many_at_least_non_consuming () =
  let r =
    Legacy.parse "abc" (fun () ->
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

let test_take_max_int () =
  let r =
    Legacy.parse "abc" (fun () ->
        ignore (Parseff.any_char ());
        Parseff.take max_int
    )
  in
  Alcotest.(check string)
    "short input" "Error@1 Unexpected_end_of_input" (show show_string r)

let test_take_max_int_streaming () =
  let r =
    Legacy.parse_source (chunked_source "abc") (fun () ->
        ignore (Parseff.any_char ());
        Parseff.take max_int
    )
  in
  match r with
  | Legacy.Error { error = `Unexpected_end_of_input; _ } ->
      ()
  | Legacy.Error { error = `Backtrack_window_exceeded _; _ } ->
      ()
  | _ ->
      Alcotest.fail "expected end of input or window error"

let test_consume_prefix_mismatch () =
  let r = Legacy.parse "x" (fun () -> Parseff.consume "true") in
  Alcotest.(check string)
    "mismatch" {|Error@0 Expected "expected \"true\""|} (show show_string r);
  let r = Legacy.parse "tr" (fun () -> Parseff.consume "true") in
  Alcotest.(check string)
    "proper prefix" "Error@2 Unexpected_end_of_input" (show show_string r)

let test_consume_or_mismatch () =
  let p () =
    Parseff.or_
      (fun () -> Parseff.consume "true")
      (fun () -> Parseff.consume "false")
      ()
  in
  let expected = {|Error@0 Expected "expected \"false\" or \"true\""|} in
  Alcotest.(check string)
    "parse" expected
    (show show_string (Legacy.parse "no" p));
  Alcotest.(check string)
    "parse_source" expected
    (show show_string (Legacy.parse_source (chunked_source "no") p))

let test_consume_streaming_slow_source () =
  let r =
    Legacy.parse_source (chunked_source "true") (fun () ->
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
          test_case "one_of_labeled keeps user error" `Quick
            test_one_of_labeled_user_error;
          test_case "expect keeps user error through or_" `Quick
            test_expect_user_error_through_or;
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
        ]
      );
      ( "take",
        [
          test_case "max_int" `Quick test_take_max_int;
          test_case "max_int streaming" `Quick test_take_max_int_streaming;
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
