let show_error (e : Parseff.error) =
  Printf.sprintf "Error@%d %s" e.range.start (Parseff.error_message e)

let chunked_source input =
  let pos = ref 0 in
  Parseff.Source.of_function (fun buf off len ->
      let n = min (min 1 len) (String.length input - !pos) in
      Bytes.blit_string input !pos buf off n;
      pos := !pos + n;
      n
  )

let show_result show_v = function
  | Ok v ->
      "Ok " ^ show_v v
  | Error e ->
      show_error e

let both ?(consume = `Partial) show_v input parser expected =
  let with_pos () =
    let v = parser () in
    (v, Parseff.position ())
  in
  let show_vp (v, p) = Printf.sprintf "(%s, %d)" (show_v v) p in
  Alcotest.(check string)
    "parse" expected
    (show_result show_vp (Parseff.parse ~consume input with_pos));
  Alcotest.(check string)
    "parse_source" expected
    (show_result show_vp
       (Parseff.parse_source ~consume (chunked_source input) with_pos)
    )

let show_unit () = "()"
let show_string s = Printf.sprintf "%S" s
let show_int = string_of_int

let test_keyword_matches () =
  both show_unit "let x" (fun () -> Parseff.keyword "let") "Ok ((), 3)";
  both show_unit "let(" (fun () -> Parseff.keyword "let") "Ok ((), 3)"

let test_keyword_rejects_identifier () =
  Alcotest.(check string)
    "parse" "Error@0 expected \"let\", found 'letter'"
    (show_result show_unit
       (Parseff.parse ~consume:`Partial "letter" (fun () ->
            Parseff.keyword "let"
        )
       )
    );
  both show_unit "let_"
    (fun () -> Parseff.keyword "let")
    "Error@0 expected \"let\", found 'let_'";
  both show_unit "let1"
    (fun () -> Parseff.keyword "let")
    "Error@0 expected \"let\", found 'let1'"

let test_keyword_end_of_input () =
  both ~consume:`All show_unit "let"
    (fun () -> Parseff.keyword "let")
    "Ok ((), 3)"

let test_keyword_reports_at_start () =
  both show_unit "lex"
    (fun () -> Parseff.keyword "let")
    "Error@0 expected \"let\", found 'lex'";
  both show_unit "le"
    (fun () -> Parseff.keyword "let")
    "Error@0 expected \"let\", found 'le'"

let test_keyword_merges_alternatives () =
  both show_unit "inx"
    (Parseff.or_
       (fun () -> Parseff.keyword "let")
       (fun () -> Parseff.keyword "in")
    )
    "Error@0 expected \"in\" or \"let\", found 'inx'"

let identifier () =
  Parseff.take_while ~at_least:1
    (fun c -> c >= 'a' && c <= 'z')
    ~label:"identifier"

let test_keyword_falls_back_to_identifier () =
  let token () =
    Parseff.or_
      (fun () ->
        Parseff.keyword "in";
        "IN"
      )
      identifier ()
  in
  both show_string "int" token "Ok (\"int\", 3)";
  both show_string "in" token "Ok (\"IN\", 2)"

let test_keyword_custom_is_ident () =
  let is_ident c = c = '-' || (c >= 'a' && c <= 'z') in
  both show_unit "if-x"
    (fun () -> Parseff.keyword ~is_ident "if")
    "Error@0 expected \"if\", found 'if'";
  both show_unit "if1" (fun () -> Parseff.keyword ~is_ident "if") "Ok ((), 2)"

let test_keyword_in_many_and_look_ahead () =
  both show_int "ababx"
    (fun () ->
      List.length
        (Parseff.many
           (fun () -> Parseff.keyword ~is_ident:(fun c -> c = 'x') "ab")
           ()
        )
    )
    "Ok (1, 2)";
  both show_unit "do("
    (fun () -> Parseff.look_ahead (fun () -> Parseff.keyword "do"))
    "Ok ((), 0)"

let test_keyword_does_not_allocate () =
  let words_per_parse parser =
    let run () =
      for _ = 1 to 10_000 do
        ignore
          (Sys.opaque_identity (Parseff.parse ~consume:`Partial "let " parser))
      done
    in
    run ();
    let before = Gc.minor_words () in
    run ();
    (Gc.minor_words () -. before) /. 10_000.
  in
  let empty = words_per_parse (fun () -> ()) in
  let with_keyword = words_per_parse (fun () -> Parseff.keyword "let") in
  Alcotest.(check (float 0.)) "minor words" empty with_keyword

let test_keyword_needs_one_more_byte_of_window () =
  let run window parser =
    show_result show_unit
      (Parseff.parse_source ~backtrack_window:window (chunked_source "let")
         parser
      )
  in
  Alcotest.(check string)
    "consume fits" "Ok ()"
    (run 3 (fun () -> ignore (Parseff.consume "let")));
  Alcotest.(check string)
    "keyword exceeds" "Error@0 backtrack window of 3 bytes exceeded"
    (run 3 (fun () -> Parseff.keyword "let"));
  Alcotest.(check string)
    "keyword fits" "Ok ()"
    (run 4 (fun () -> Parseff.keyword "let"))

let test_keyword_empty_raises () =
  List.iter
    (fun input ->
      Alcotest.check_raises input
        (Invalid_argument "Parseff.keyword: empty keyword") (fun () ->
          ignore
            (Parseff.parse ~consume:`Partial input (fun () -> Parseff.keyword "")
            )
      )
    )
    [ "abc"; ""; " " ]

let test_keyword_is_ident_reads_bytes () =
  both show_unit "let\xC3\xA9" (fun () -> Parseff.keyword "let") "Ok ((), 3)"

let test_lexeme_skips_whitespace () =
  both show_string "abc  \n\tx"
    (fun () -> Parseff.lexeme identifier)
    "Ok (\"abc\", 7)";
  both ~consume:`All show_string "abc"
    (fun () -> Parseff.lexeme identifier)
    "Ok (\"abc\", 3)";
  both show_string "1"
    (fun () -> Parseff.lexeme identifier)
    "Error@0 expected identifier, found '1'"

let skip_space_and_comments () =
  Parseff.skip_whitespace ();
  while Parseff.peek_char () = Some '#' do
    Parseff.skip_while (fun c -> c <> '\n');
    Parseff.skip_whitespace ()
  done

let test_lexeme_custom_skip () =
  let token () =
    Parseff.lexeme ~skip:skip_space_and_comments (fun () ->
        Parseff.keyword "let"
    )
  in
  both show_unit "let # note\n  # more\nx" token "Ok ((), 20)"

let () =
  Alcotest.run "Lexeme"
    [
      ( "keyword",
        [
          Alcotest.test_case "matches" `Quick test_keyword_matches;
          Alcotest.test_case "rejects identifier" `Quick
            test_keyword_rejects_identifier;
          Alcotest.test_case "end of input" `Quick test_keyword_end_of_input;
          Alcotest.test_case "reports at start" `Quick
            test_keyword_reports_at_start;
          Alcotest.test_case "merges alternatives" `Quick
            test_keyword_merges_alternatives;
          Alcotest.test_case "falls back to identifier" `Quick
            test_keyword_falls_back_to_identifier;
          Alcotest.test_case "custom is_ident" `Quick
            test_keyword_custom_is_ident;
          Alcotest.test_case "many and look_ahead" `Quick
            test_keyword_in_many_and_look_ahead;
          Alcotest.test_case "no allocation" `Quick
            test_keyword_does_not_allocate;
          Alcotest.test_case "window of len + 1" `Quick
            test_keyword_needs_one_more_byte_of_window;
          Alcotest.test_case "empty raises" `Quick test_keyword_empty_raises;
          Alcotest.test_case "is_ident reads bytes" `Quick
            test_keyword_is_ident_reads_bytes;
        ]
      );
      ( "lexeme",
        [
          Alcotest.test_case "skips whitespace" `Quick
            test_lexeme_skips_whitespace;
          Alcotest.test_case "custom skip" `Quick test_lexeme_custom_skip;
        ]
      );
    ]
