let chunked_source ~chunk_size input =
  let pos = ref 0 in
  Parseff.Source.of_function (fun buf off len ->
      let n = min (min chunk_size len) (String.length input - !pos) in
      Bytes.blit_string input !pos buf off n;
      pos := !pos + n;
      n
  )

let kind (e : Parseff.error) =
  match e.kind with
  | Expected _ ->
      "Expected " ^ Parseff.error_message e
  | Failure m ->
      "Failure " ^ m
  | Custom _ ->
      "Custom"
  | Depth_limit_exceeded _ ->
      "Depth_limit_exceeded " ^ Parseff.error_message e
  | Backtrack_window_exceeded _ ->
      "Backtrack_window_exceeded"

let norm = function
  | Ok v ->
      Ok v
  | Error e ->
      Error (e.Parseff.range.start, kind e)

let show_result show_v = function
  | Ok v ->
      "Ok " ^ show_v v
  | Error (pos, k) ->
      Printf.sprintf "Error@%d %s" pos k

let check_all ?(chunk_sizes = [ 1; 2; 3 ]) name show_v input parser expected =
  Alcotest.(check string)
    (name ^ " parse") expected
    (show_result show_v (norm (Parseff.parse input parser)));
  List.iter
    (fun chunk_size ->
      Alcotest.(check string)
        (Printf.sprintf "%s chunk %d" name chunk_size)
        expected
        (show_result show_v
           (norm
              (Parseff.parse_source (chunked_source ~chunk_size input) parser)
           )
        )
    )
    chunk_sizes

let show_code_points l =
  String.concat ","
    (List.map (fun u -> Printf.sprintf "U+%04X" (Uchar.to_int u)) l)

let texts =
  [
    "\xE2\x82\xAC";
    "\xF0\x9F\x98\x80";
    "\xEF\xBF\xBD";
    "\xF4\x8F\xBF\xBF";
    "a\xE2\x82\xACb\xF0\x9F\x98\x80c";
  ]

let expected_code_points = function
  | "\xE2\x82\xAC" ->
      "U+20AC"
  | "\xF0\x9F\x98\x80" ->
      "U+1F600"
  | "\xEF\xBF\xBD" ->
      "U+FFFD"
  | "\xF4\x8F\xBF\xBF" ->
      "U+10FFFF"
  | _ ->
      "U+0061,U+20AC,U+0062,U+1F600,U+0063"

let test_utf8_satisfy_split () =
  List.iter
    (fun text ->
      check_all "satisfy" show_code_points text
        (fun () ->
          Parseff.many
            (fun () -> Parseff.Utf8.satisfy (fun _ -> true) ~label:"any")
            ()
        )
        ("Ok " ^ expected_code_points text)
    )
    texts

let test_utf8_take_while_split () =
  List.iter
    (fun text ->
      check_all "take_while" Fun.id text
        (fun () -> Parseff.Utf8.take_while (fun _ -> true))
        ("Ok " ^ text)
    )
    texts

let test_utf8_take_while_span_split () =
  List.iter
    (fun text ->
      check_all "take_while_span" Fun.id text
        (fun () ->
          Parseff.span_to_string (Parseff.Utf8.take_while_span (fun _ -> true))
        )
        ("Ok " ^ text)
    )
    texts

let test_utf8_skip_while_then_char_split () =
  let cases =
    [
      ("\xE2\x82\xAC", Uchar.of_int 0x20AC, "");
      ("\xF0\x9F\x98\x80", Uchar.of_int 0x1F600, "");
      ("a\xE2\x82\xACb\xF0\x9F\x98\x80", Uchar.of_int 0x1F600, "");
      ("a\xE2\x82\xACb\xF0\x9F\x98\x80c", Uchar.of_int 0x20AC, "c");
    ]
  in
  List.iter
    (fun (text, target, _) ->
      let expected =
        match
          Parseff.parse text (fun () ->
              Parseff.Utf8.skip_while_then_char
                (fun u -> not (Uchar.equal u target))
                target;
              Parseff.position ()
          )
        with
        | Ok p ->
            "Ok " ^ string_of_int p
        | Error _ ->
            Alcotest.fail "memory parse failed"
      in
      check_all "skip_while_then_char" string_of_int text
        (fun () ->
          Parseff.Utf8.skip_while_then_char
            (fun u -> not (Uchar.equal u target))
            target;
          Parseff.position ()
        )
        expected
    )
    cases;
  check_all "skip_while_then_char positions" string_of_int
    "a\xE2\x82\xACb\xF0\x9F\x98\x80"
    (fun () ->
      Parseff.Utf8.skip_while_then_char
        (fun u -> not (Uchar.equal u (Uchar.of_int 0x1F600)))
        (Uchar.of_int 0x1F600);
      Parseff.position ()
    )
    "Ok 9"

let valid_lines =
  String.concat "" (List.init 800 (fun _ -> String.make 50 'x' ^ "\xC3\xA9\n"))

let test_utf8_invalid_after_compaction_skip_while () =
  let text = valid_lines ^ "\xFF" in
  let parser () = Parseff.Utf8.skip_while (fun _ -> true) in
  let expected =
    Printf.sprintf "Error@%d Expected expected valid UTF-8, found '\\xFF'"
      (String.length valid_lines)
  in
  check_all ~chunk_sizes:[ 1; 7; 1000 ] "skip_while"
    (fun () -> "()")
    text parser expected

let test_utf8_invalid_after_compaction_take_while () =
  let text = valid_lines ^ "\xFF" in
  let parser () =
    let n = ref 0 in
    while true do
      ignore (Parseff.Utf8.take_while (fun u -> Uchar.to_int u <> 10));
      ignore (Parseff.char '\n');
      incr n
    done;
    !n
  in
  let expected =
    Printf.sprintf "Error@%d Expected expected valid UTF-8, found '\\xFF'"
      (String.length valid_lines)
  in
  check_all ~chunk_sizes:[ 1; 7; 1000 ] "take_while" string_of_int text parser
    expected

let test_utf8_alphanum_rejects_non_ascii_digit () =
  let arabic_indic_three = "\xD9\xA3" in
  List.iter
    (fun (name, parser) ->
      match Parseff.parse arabic_indic_three parser with
      | Error e when e.range.start = 0 ->
          ()
      | _ ->
          Alcotest.failf "%s accepted U+0663" name
    )
    [ ("alphanum", fun () -> ignore (Parseff.Utf8.alphanum ())) ]

let test_utf8_alphanum_ascii_edges () =
  List.iter
    (fun (c, expected) ->
      let input = String.make 1 c in
      let ok parser =
        match Parseff.parse input (fun () -> ignore (parser ())) with
        | Ok () ->
            true
        | Error _ ->
            false
      in
      Alcotest.(check bool)
        (Printf.sprintf "Utf8.alphanum %C" c)
        expected (ok Parseff.Utf8.alphanum)
    )
    [ ('0', true); ('9', true); ('/', false); (':', false); ('a', true) ]

let test_utf8_is_whitespace () =
  let check cp expected =
    Alcotest.(check bool)
      (Printf.sprintf "U+%04X" cp)
      expected
      (Parseff.Utf8.is_whitespace (Uchar.of_int cp))
  in
  check 0xA0 true;
  check 0x85 true;
  check 0x20 true;
  check 0x3000 true;
  check 0x200B false;
  check 0x61 false

let test_depth_limit () =
  let rec nest n () =
    if n = 0 then
      0
    else
      Parseff.rec_ (fun () -> 1 + nest (n - 1) ())
  in
  let run depth =
    show_result string_of_int
      (norm (Parseff.parse ~max_depth:3 "" (fun () -> nest depth ())))
  in
  Alcotest.(check string) "exactly max_depth" "Ok 3" (run 3);
  Alcotest.(check string)
    "one more" "Error@0 Depth_limit_exceeded maximum nesting depth 3 exceeded"
    (run 4);
  let streaming =
    show_result string_of_int
      (norm
         (Parseff.parse_source ~max_depth:3 (chunked_source ~chunk_size:1 "")
            (fun () -> nest 4 ()
          )
         )
      )
  in
  Alcotest.(check string)
    "streaming" "Error@0 Depth_limit_exceeded maximum nesting depth 3 exceeded"
    streaming

let test_depth_restored_after_exception () =
  let attempt () =
    Parseff.or_
      (fun () ->
        Parseff.rec_ (fun () ->
            ignore (Parseff.consume "x");
            "x"
        )
      )
      (fun () -> "none")
      ()
  in
  match
    Parseff.parse ~max_depth:2 "" (fun () -> List.init 6 (fun _ -> attempt ()))
  with
  | Ok l ->
      Alcotest.(check int) "all attempts ran" 6 (List.length l)
  | Error _ ->
      Alcotest.fail "depth leaked after exception"

let test_count_zero () =
  let calls = ref 0 in
  match
    Parseff.parse "abc" (fun () ->
        Parseff.count 0
          (fun () ->
            incr calls;
            Parseff.any_char ()
          )
          ()
    )
  with
  | Ok l ->
      Alcotest.(check int) "no results" 0 (List.length l);
      Alcotest.(check int) "parser not called" 0 !calls
  | Error _ ->
      Alcotest.fail "count 0 failed"

let accepts parser c =
  match Parseff.parse (String.make 1 c) (fun () -> ignore (parser ())) with
  | Ok () ->
      true
  | Error _ ->
      false

let test_ascii_class_edges () =
  List.iter
    (fun c ->
      Alcotest.(check bool)
        (Printf.sprintf "letter %C" c)
        true (accepts Parseff.letter c)
    )
    [ 'a'; 'z'; 'A'; 'Z' ];
  List.iter
    (fun c ->
      Alcotest.(check bool)
        (Printf.sprintf "not letter %C" c)
        false (accepts Parseff.letter c)
    )
    [ '`'; '{'; '@'; '['; '0' ];
  List.iter
    (fun c ->
      Alcotest.(check bool)
        (Printf.sprintf "alphanum %C" c)
        true
        (accepts Parseff.alphanum c)
    )
    [ '0'; '9'; 'a'; 'Z' ];
  List.iter
    (fun c ->
      Alcotest.(check bool)
        (Printf.sprintf "not alphanum %C" c)
        false
        (accepts Parseff.alphanum c)
    )
    [ '/'; ':'; '_'; ' ' ];
  List.iter
    (fun c ->
      Alcotest.(check bool)
        (Printf.sprintf "whitespace %C" c)
        true (Parseff.is_whitespace c)
    )
    [ ' '; '\t'; '\n'; '\r' ];
  Alcotest.(check bool) "not whitespace" false (Parseff.is_whitespace 'a')

let () =
  let open Alcotest in
  run "Gaps"
    [
      ( "utf8 streaming",
        [
          test_case "satisfy split" `Quick test_utf8_satisfy_split;
          test_case "take_while split" `Quick test_utf8_take_while_split;
          test_case "take_while_span split" `Quick
            test_utf8_take_while_span_split;
          test_case "skip_while_then_char split" `Quick
            test_utf8_skip_while_then_char_split;
          test_case "invalid after compaction skip_while" `Quick
            test_utf8_invalid_after_compaction_skip_while;
          test_case "invalid after compaction take_while" `Quick
            test_utf8_invalid_after_compaction_take_while;
        ]
      );
      ( "utf8 classes",
        [
          test_case "alphanum rejects non-ASCII digit" `Quick
            test_utf8_alphanum_rejects_non_ascii_digit;
          test_case "alphanum ASCII edges" `Quick test_utf8_alphanum_ascii_edges;
          test_case "is_whitespace" `Quick test_utf8_is_whitespace;
        ]
      );
      ( "combinators",
        [
          test_case "depth limit" `Quick test_depth_limit;
          test_case "depth restored" `Quick test_depth_restored_after_exception;
          test_case "count 0" `Quick test_count_zero;
          test_case "ascii class edges" `Quick test_ascii_class_edges;
        ]
      );
    ]
