let uchar_testable =
  Alcotest.testable
    (fun fmt u -> Format.fprintf fmt "U+%04X" (Uchar.to_int u))
    Uchar.equal

let parse_with_pos input parser =
  Parseff.parse input (fun () ->
      let value = parser () in
      (value, Parseff.position ())
  )

let test_satisfy_ascii () =
  match
    Parseff.parse "a" (fun () ->
        Parseff.Utf8.satisfy (fun u -> Uchar.to_int u = 0x61) ~label:"'a'"
    )
  with
  | Ok u ->
      Alcotest.(check uchar_testable) "matched 'a'" (Uchar.of_int 0x61) u
  | Error _ ->
      Alcotest.fail "Expected success"

let test_satisfy_multibyte () =
  (* λ = U+03BB, encoded as 0xCE 0xBB (2 bytes) *)
  match
    parse_with_pos "\xCE\xBB" (fun () ->
        Parseff.Utf8.satisfy (fun u -> Uchar.to_int u = 0x03BB) ~label:"lambda"
    )
  with
  | Ok (u, pos) ->
      Alcotest.(check uchar_testable) "matched lambda" (Uchar.of_int 0x03BB) u;
      Alcotest.(check int) "advanced 2 bytes" 2 pos
  | Error _ ->
      Alcotest.fail "Expected success"

let test_satisfy_3byte () =
  (* 中 = U+4E2D, encoded as 0xE4 0xB8 0xAD (3 bytes) *)
  match
    parse_with_pos "\xE4\xB8\xAD" (fun () ->
        Parseff.Utf8.satisfy (fun _ -> true) ~label:"any"
    )
  with
  | Ok (u, pos) ->
      Alcotest.(check uchar_testable) "matched CJK" (Uchar.of_int 0x4E2D) u;
      Alcotest.(check int) "advanced 3 bytes" 3 pos
  | Error _ ->
      Alcotest.fail "Expected success"

let test_satisfy_4byte () =
  (* 😀 = U+1F600, encoded as 0xF0 0x9F 0x98 0x80 (4 bytes) *)
  match
    parse_with_pos "\xF0\x9F\x98\x80" (fun () ->
        Parseff.Utf8.satisfy (fun _ -> true) ~label:"any"
    )
  with
  | Ok (u, pos) ->
      Alcotest.(check uchar_testable) "matched emoji" (Uchar.of_int 0x1F600) u;
      Alcotest.(check int) "advanced 4 bytes" 4 pos
  | Error _ ->
      Alcotest.fail "Expected success"

let test_satisfy_failure () =
  match
    Parseff.parse "a" (fun () ->
        Parseff.Utf8.satisfy (fun u -> Uchar.to_int u > 0x7F) ~label:"non-ASCII"
    )
  with
  | Ok _ ->
      Alcotest.fail "Expected failure"
  | Error { pos; error = `Expected expected } ->
      Alcotest.(check int) "error position" 0 pos;
      Alcotest.(check string) "error message" "expected non-ASCII" expected
  | Error _ ->
      Alcotest.fail "Unexpected error type"

let test_satisfy_eof () =
  match
    Parseff.parse "" (fun () ->
        Parseff.Utf8.satisfy (fun _ -> true) ~label:"any"
    )
  with
  | Ok _ ->
      Alcotest.fail "Expected failure"
  | Error { pos; error = `Unexpected_end_of_input } ->
      Alcotest.(check int) "error position" 0 pos
  | Error _ ->
      Alcotest.fail "Expected `Unexpected_end_of_input"

let test_satisfy_invalid_utf8 () =
  (* 0xFF is never valid in UTF-8 *)
  match
    Parseff.parse "\xFF" (fun () ->
        Parseff.Utf8.satisfy (fun _ -> true) ~label:"any"
    )
  with
  | Ok _ ->
      Alcotest.fail "Expected failure"
  | Error { pos; error = `Expected expected } ->
      Alcotest.(check int) "error position" 0 pos;
      Alcotest.(check string) "error message" "invalid UTF-8" expected
  | Error _ ->
      Alcotest.fail "Unexpected error type"

let test_char_ascii () =
  match
    parse_with_pos "a" (fun () -> Parseff.Utf8.char (Uchar.of_int 0x61))
  with
  | Ok (u, pos) ->
      Alcotest.(check uchar_testable) "matched" (Uchar.of_int 0x61) u;
      Alcotest.(check int) "position" 1 pos
  | Error _ ->
      Alcotest.fail "Expected success"

let test_char_multibyte () =
  (* é = U+00E9 *)
  match
    parse_with_pos "\xC3\xA9" (fun () -> Parseff.Utf8.char (Uchar.of_int 0xE9))
  with
  | Ok (u, pos) ->
      Alcotest.(check uchar_testable) "matched" (Uchar.of_int 0xE9) u;
      Alcotest.(check int) "position" 2 pos
  | Error _ ->
      Alcotest.fail "Expected success"

let test_char_failure () =
  match Parseff.parse "a" (fun () -> Parseff.Utf8.char (Uchar.of_int 0x62)) with
  | Ok _ ->
      Alcotest.fail "Expected failure"
  | Error { pos; error = `Expected _ } ->
      Alcotest.(check int) "error position" 0 pos
  | Error _ ->
      Alcotest.fail "Unexpected error type"

let test_any_char_ascii () =
  match Parseff.parse "x" Parseff.Utf8.any_char with
  | Ok u ->
      Alcotest.(check uchar_testable) "matched" (Uchar.of_int 0x78) u
  | Error _ ->
      Alcotest.fail "Expected success"

let test_any_char_multibyte () =
  (* ñ = U+00F1 *)
  match Parseff.parse "\xC3\xB1" Parseff.Utf8.any_char with
  | Ok u ->
      Alcotest.(check uchar_testable) "matched" (Uchar.of_int 0xF1) u
  | Error _ ->
      Alcotest.fail "Expected success"

let test_any_char_eof () =
  match Parseff.parse "" Parseff.Utf8.any_char with
  | Ok _ ->
      Alcotest.fail "Expected failure"
  | Error { error = `Unexpected_end_of_input; _ } ->
      ()
  | Error _ ->
      Alcotest.fail "Expected `Unexpected_end_of_input"

let test_take_while_ascii () =
  match
    parse_with_pos "abc123" (fun () ->
        Parseff.Utf8.take_while (fun u ->
            let i = Uchar.to_int u in
            (i >= 0x61 && i <= 0x7A) || (i >= 0x41 && i <= 0x5A)
        )
    )
  with
  | Ok (s, pos) ->
      Alcotest.(check string) "matched letters" "abc" s;
      Alcotest.(check int) "position at digits" 3 pos
  | Error _ ->
      Alcotest.fail "Expected success"

let test_take_while_unicode () =
  (* "こんにちは世界" = 7 CJK characters, each 3 bytes = 21 bytes *)
  let input = "こんにちは世界!" in
  match
    parse_with_pos input (fun () ->
        Parseff.Utf8.take_while (fun u -> Uchar.to_int u > 0x7F)
    )
  with
  | Ok (s, pos) ->
      Alcotest.(check string) "matched CJK" "こんにちは世界" s;
      Alcotest.(check int) "position" 21 pos
  | Error _ ->
      Alcotest.fail "Expected success"

let test_take_while_empty () =
  match
    Parseff.parse "123" (fun () ->
        Parseff.Utf8.take_while Uucp.Alpha.is_alphabetic
    )
  with
  | Ok s ->
      Alcotest.(check string) "empty" "" s
  | Error _ ->
      Alcotest.fail "Expected success"

let test_take_while_at_least () =
  match
    Parseff.parse "abc" (fun () ->
        Parseff.Utf8.take_while ~at_least:2 ~label:"letters"
          Uucp.Alpha.is_alphabetic
    )
  with
  | Ok s ->
      Alcotest.(check string) "matched" "abc" s
  | Error _ ->
      Alcotest.fail "Expected success"

let test_take_while_at_least_failure () =
  match
    Parseff.parse "a" (fun () ->
        Parseff.Utf8.take_while ~at_least:2 ~label:"letters"
          Uucp.Alpha.is_alphabetic
    )
  with
  | Ok _ ->
      Alcotest.fail "Expected failure"
  | Error { pos; error = `Expected expected } ->
      (* at_least check fails at the current position *)
      Alcotest.(check int) "error position" 1 pos;
      Alcotest.(check string) "error message" "letters" expected
  | Error _ ->
      Alcotest.fail "Unexpected error type"

let test_take_while_at_least_counts_codepoints () =
  (* "éà" = 2 code points, 4 bytes. at_least:2 should succeed *)
  match
    Parseff.parse "\xC3\xA9\xC3\xA0" (fun () ->
        Parseff.Utf8.take_while ~at_least:2 ~label:"letters"
          Uucp.Alpha.is_alphabetic
    )
  with
  | Ok s ->
      Alcotest.(check string) "matched" "\xC3\xA9\xC3\xA0" s
  | Error _ ->
      Alcotest.fail "Expected success"

let test_take_while_at_least_codepoint_count_fail () =
  (* "é" = 1 code point, 2 bytes. at_least:2 should fail *)
  match
    Parseff.parse "\xC3\xA9" (fun () ->
        Parseff.Utf8.take_while ~at_least:2 ~label:"letters"
          Uucp.Alpha.is_alphabetic
    )
  with
  | Ok _ ->
      Alcotest.fail "Expected failure"
  | Error { error = `Expected expected; _ } ->
      Alcotest.(check string) "error message" "letters" expected
  | Error _ ->
      Alcotest.fail "Unexpected error type"

let test_take_while_invalid_utf8 () =
  (* Valid ASCII then invalid byte *)
  match
    Parseff.parse "a\xFF" (fun () -> Parseff.Utf8.take_while (fun _ -> true))
  with
  | Ok _ ->
      Alcotest.fail "Expected failure"
  | Error { pos; error = `Expected expected } ->
      Alcotest.(check int) "error at invalid byte" 1 pos;
      Alcotest.(check string) "error message" "invalid UTF-8" expected
  | Error _ ->
      Alcotest.fail "Unexpected error type"

let test_skip_while () =
  match
    parse_with_pos "  \t\nhello" (fun () ->
        Parseff.Utf8.skip_while Uucp.White.is_white_space;
        Parseff.consume "hello"
    )
  with
  | Ok (s, pos) ->
      Alcotest.(check string) "matched" "hello" s;
      Alcotest.(check int) "position" 9 pos
  | Error _ ->
      Alcotest.fail "Expected success"

let test_skip_while_unicode_whitespace () =
  (* NO-BREAK SPACE U+00A0 = 0xC2 0xA0 (2 bytes) *)
  match
    parse_with_pos "\xC2\xA0hello" (fun () ->
        Parseff.Utf8.skip_while Uucp.White.is_white_space;
        Parseff.consume "hello"
    )
  with
  | Ok (s, pos) ->
      Alcotest.(check string) "matched" "hello" s;
      Alcotest.(check int) "position" 7 pos
  | Error _ ->
      Alcotest.fail "Expected success"

let test_take_while_span () =
  match
    parse_with_pos "héllo world" (fun () ->
        let sp =
          Parseff.Utf8.take_while_span (fun u ->
              not (Uucp.White.is_white_space u)
          )
        in
        (Parseff.span_to_string sp, sp.off, sp.len)
    )
  with
  | Ok ((s, off, len), pos) ->
      Alcotest.(check string) "span string" "héllo" s;
      Alcotest.(check int) "span offset" 0 off;
      (* "héllo" = 6 bytes: é is 2 bytes + 4 ASCII *)
      Alcotest.(check int) "span length" 6 len;
      Alcotest.(check int) "parser position" 6 pos
  | Error _ ->
      Alcotest.fail "Expected success"

let test_skip_while_then_char () =
  match
    parse_with_pos "  :value" (fun () ->
        Parseff.Utf8.skip_while_then_char Uucp.White.is_white_space
          (Uchar.of_int 0x3A);
        Parseff.consume "value"
    )
  with
  | Ok (s, pos) ->
      Alcotest.(check string) "matched" "value" s;
      Alcotest.(check int) "position" 8 pos
  | Error _ ->
      Alcotest.fail "Expected success"

let test_skip_while_then_char_unicode_term () =
  (* Skip whitespace then match → U+2192 (3 bytes: 0xE2 0x86 0x92) *)
  match
    parse_with_pos "  \xE2\x86\x92ok" (fun () ->
        Parseff.Utf8.skip_while_then_char Uucp.White.is_white_space
          (Uchar.of_int 0x2192);
        Parseff.consume "ok"
    )
  with
  | Ok (s, pos) ->
      Alcotest.(check string) "matched" "ok" s;
      Alcotest.(check int) "position" 7 pos
  | Error _ ->
      Alcotest.fail "Expected success"

let test_skip_while_then_char_failure () =
  match
    Parseff.parse "  x" (fun () ->
        Parseff.Utf8.skip_while_then_char Uucp.White.is_white_space
          (Uchar.of_int 0x3A)
    )
  with
  | Ok _ ->
      Alcotest.fail "Expected failure"
  | Error { pos; error = `Expected _ } ->
      Alcotest.(check int) "error at non-matching char" 2 pos
  | Error _ ->
      Alcotest.fail "Unexpected error type"

let test_skip_while_then_char_eof () =
  match
    Parseff.parse "  " (fun () ->
        Parseff.Utf8.skip_while_then_char Uucp.White.is_white_space
          (Uchar.of_int 0x3A)
    )
  with
  | Ok _ ->
      Alcotest.fail "Expected failure"
  | Error { pos; error = `Unexpected_end_of_input } ->
      Alcotest.(check int) "error at eof" 2 pos
  | Error _ ->
      Alcotest.fail "Expected `Unexpected_end_of_input"

let test_letter_ascii () =
  match Parseff.parse "a" Parseff.Utf8.letter with
  | Ok u ->
      Alcotest.(check uchar_testable) "ASCII letter" (Uchar.of_int 0x61) u
  | Error _ ->
      Alcotest.fail "Expected success"

let test_letter_accented () =
  (* é = U+00E9 *)
  match Parseff.parse "\xC3\xA9" Parseff.Utf8.letter with
  | Ok u ->
      Alcotest.(check uchar_testable) "accented letter" (Uchar.of_int 0xE9) u
  | Error _ ->
      Alcotest.fail "Expected success"

let test_letter_cjk () =
  (* 中 = U+4E2D *)
  match Parseff.parse "\xE4\xB8\xAD" Parseff.Utf8.letter with
  | Ok u ->
      Alcotest.(check uchar_testable) "CJK letter" (Uchar.of_int 0x4E2D) u
  | Error _ ->
      Alcotest.fail "Expected success"

let test_letter_cyrillic () =
  (* д = U+0434 *)
  match Parseff.parse "\xD0\xB4" Parseff.Utf8.letter with
  | Ok u ->
      Alcotest.(check uchar_testable) "Cyrillic letter" (Uchar.of_int 0x434) u
  | Error _ ->
      Alcotest.fail "Expected success"

let test_letter_failure () =
  match Parseff.parse "1" Parseff.Utf8.letter with
  | Ok _ ->
      Alcotest.fail "Expected failure"
  | Error { pos; error = `Expected expected } ->
      Alcotest.(check int) "error position" 0 pos;
      Alcotest.(check string) "error message" "expected letter" expected
  | Error _ ->
      Alcotest.fail "Unexpected error type"

let test_digit_success () =
  match Parseff.parse "7" Parseff.Utf8.digit with
  | Ok n ->
      Alcotest.(check int) "digit value" 7 n
  | Error _ ->
      Alcotest.fail "Expected success"

let test_digit_failure_letter () =
  match Parseff.parse "a" Parseff.Utf8.digit with
  | Ok _ ->
      Alcotest.fail "Expected failure"
  | Error { pos; error = `Expected expected } ->
      Alcotest.(check int) "error position" 0 pos;
      Alcotest.(check string) "error message" "expected digit" expected
  | Error _ ->
      Alcotest.fail "Unexpected error type"

let test_alphanum_letter () =
  match Parseff.parse "\xC3\xA9" Parseff.Utf8.alphanum with
  | Ok u ->
      Alcotest.(check uchar_testable) "accented letter" (Uchar.of_int 0xE9) u
  | Error _ ->
      Alcotest.fail "Expected success"

let test_alphanum_digit () =
  match Parseff.parse "5" Parseff.Utf8.alphanum with
  | Ok u ->
      Alcotest.(check uchar_testable) "digit" (Uchar.of_int 0x35) u
  | Error _ ->
      Alcotest.fail "Expected success"

let test_alphanum_failure () =
  match Parseff.parse "!" Parseff.Utf8.alphanum with
  | Ok _ ->
      Alcotest.fail "Expected failure"
  | Error { pos; error = `Expected expected } ->
      Alcotest.(check int) "error position" 0 pos;
      Alcotest.(check string) "error message" "expected alphanumeric" expected
  | Error _ ->
      Alcotest.fail "Unexpected error type"

let test_whitespace_ascii () =
  match Parseff.parse "  \t\nX" (fun () -> Parseff.Utf8.whitespace ()) with
  | Ok s ->
      Alcotest.(check string) "whitespace" "  \t\n" s
  | Error _ ->
      Alcotest.fail "Expected success"

let test_whitespace_unicode () =
  (* NBSP (U+00A0) + EM SPACE (U+2003) + ASCII space *)
  let input = "\xC2\xA0\xE2\x80\x83 X" in
  match Parseff.parse input (fun () -> Parseff.Utf8.whitespace ()) with
  | Ok s ->
      Alcotest.(check string) "unicode whitespace" "\xC2\xA0\xE2\x80\x83 " s
  | Error _ ->
      Alcotest.fail "Expected success"

let test_whitespace_empty () =
  match Parseff.parse "abc" (fun () -> Parseff.Utf8.whitespace ()) with
  | Ok s ->
      Alcotest.(check string) "no whitespace" "" s
  | Error _ ->
      Alcotest.fail "Expected success"

let test_whitespace_at_least_failure () =
  match
    Parseff.parse "abc" (fun () -> Parseff.Utf8.whitespace ~at_least:1 ())
  with
  | Ok _ ->
      Alcotest.fail "Expected failure"
  | Error { pos; error = `Expected expected } ->
      Alcotest.(check int) "error position" 0 pos;
      Alcotest.(check string) "error message" "whitespace" expected
  | Error _ ->
      Alcotest.fail "Unexpected error type"

let test_skip_whitespace () =
  let input = "\xC2\xA0 hello" in
  match
    parse_with_pos input (fun () ->
        Parseff.Utf8.skip_whitespace ();
        Parseff.consume "hello"
    )
  with
  | Ok (s, pos) ->
      Alcotest.(check string) "matched" "hello" s;
      Alcotest.(check int) "position" 8 pos
  | Error _ ->
      Alcotest.fail "Expected success"

let test_mix_byte_and_utf8 () =
  (* Parse: key=value where key is ASCII, value is Unicode *)
  let input = "name=こんにちは" in
  match
    parse_with_pos input (fun () ->
        let key =
          Parseff.take_while (fun c ->
              (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')
          )
        in
        let _ = Parseff.char '=' in
        let value = Parseff.Utf8.take_while Uucp.Alpha.is_alphabetic in
        (key, value)
    )
  with
  | Ok ((key, value), pos) ->
      Alcotest.(check string) "key" "name" key;
      Alcotest.(check string) "value" "こんにちは" value;
      (* "name" (4) + "=" (1) + 5 * 3 bytes = 20 *)
      Alcotest.(check int) "position" 20 pos
  | Error _ ->
      Alcotest.fail "Expected success"

let test_streaming_satisfy () =
  let chunks = ref [ "\xCE"; "\xBB" ] in
  let source =
    Parseff.Source.of_function (fun buf off len ->
        match !chunks with
        | [] ->
            0
        | chunk :: rest ->
            let n = min len (String.length chunk) in
            Bytes.blit_string chunk 0 buf off n;
            chunks := rest;
            n
    )
  in
  match
    Parseff.parse_source source (fun () ->
        Parseff.Utf8.satisfy (fun _ -> true) ~label:"any"
    )
  with
  | Ok u ->
      Alcotest.(check uchar_testable) "matched lambda" (Uchar.of_int 0x03BB) u
  | Error _ ->
      Alcotest.fail "Expected success"

let test_streaming_take_while () =
  (* Feed "éàü" as individual bytes to test buffer boundary handling *)
  let data = "\xC3\xA9\xC3\xA0\xC3\xBC" in
  let pos_ref = ref 0 in
  let source =
    Parseff.Source.of_function (fun buf off _len ->
        if !pos_ref >= String.length data then
          0
        else begin
          (* One byte at a time *)
            Bytes.set buf off (String.get data !pos_ref);
            incr pos_ref;
            1
        end
    )
  in
  match
    Parseff.parse_source source (fun () ->
        Parseff.Utf8.take_while Uucp.Alpha.is_alphabetic
    )
  with
  | Ok s ->
      Alcotest.(check string) "matched" "\xC3\xA9\xC3\xA0\xC3\xBC" s
  | Error _ ->
      Alcotest.fail "Expected success"

let () =
  let open Alcotest in
  run "Parseff.Utf8"
    [
      ( "satisfy",
        [
          test_case "ASCII" `Quick test_satisfy_ascii;
          test_case "2-byte (lambda)" `Quick test_satisfy_multibyte;
          test_case "3-byte (CJK)" `Quick test_satisfy_3byte;
          test_case "4-byte (emoji)" `Quick test_satisfy_4byte;
          test_case "failure" `Quick test_satisfy_failure;
          test_case "eof" `Quick test_satisfy_eof;
          test_case "invalid UTF-8" `Quick test_satisfy_invalid_utf8;
        ]
      );
      ( "char",
        [
          test_case "ASCII" `Quick test_char_ascii;
          test_case "multibyte" `Quick test_char_multibyte;
          test_case "failure" `Quick test_char_failure;
        ]
      );
      ( "any_char",
        [
          test_case "ASCII" `Quick test_any_char_ascii;
          test_case "multibyte" `Quick test_any_char_multibyte;
          test_case "eof" `Quick test_any_char_eof;
        ]
      );
      ( "take_while",
        [
          test_case "ASCII" `Quick test_take_while_ascii;
          test_case "Unicode" `Quick test_take_while_unicode;
          test_case "empty" `Quick test_take_while_empty;
          test_case "at_least success" `Quick test_take_while_at_least;
          test_case "at_least failure" `Quick test_take_while_at_least_failure;
          test_case "at_least counts codepoints" `Quick
            test_take_while_at_least_counts_codepoints;
          test_case "at_least codepoint count fail" `Quick
            test_take_while_at_least_codepoint_count_fail;
          test_case "invalid UTF-8" `Quick test_take_while_invalid_utf8;
        ]
      );
      ( "skip_while",
        [
          test_case "basic" `Quick test_skip_while;
          test_case "unicode whitespace" `Quick
            test_skip_while_unicode_whitespace;
        ]
      );
      ("take_while_span", [ test_case "basic" `Quick test_take_while_span ]);
      ( "skip_while_then_char",
        [
          test_case "basic" `Quick test_skip_while_then_char;
          test_case "unicode terminator" `Quick
            test_skip_while_then_char_unicode_term;
          test_case "failure" `Quick test_skip_while_then_char_failure;
          test_case "eof" `Quick test_skip_while_then_char_eof;
        ]
      );
      ( "letter",
        [
          test_case "ASCII" `Quick test_letter_ascii;
          test_case "accented" `Quick test_letter_accented;
          test_case "CJK" `Quick test_letter_cjk;
          test_case "Cyrillic" `Quick test_letter_cyrillic;
          test_case "failure" `Quick test_letter_failure;
        ]
      );
      ( "digit",
        [
          test_case "success" `Quick test_digit_success;
          test_case "failure" `Quick test_digit_failure_letter;
        ]
      );
      ( "alphanum",
        [
          test_case "letter" `Quick test_alphanum_letter;
          test_case "digit" `Quick test_alphanum_digit;
          test_case "failure" `Quick test_alphanum_failure;
        ]
      );
      ( "whitespace",
        [
          test_case "ASCII" `Quick test_whitespace_ascii;
          test_case "Unicode" `Quick test_whitespace_unicode;
          test_case "empty" `Quick test_whitespace_empty;
          test_case "at_least failure" `Quick test_whitespace_at_least_failure;
          test_case "skip_whitespace" `Quick test_skip_whitespace;
        ]
      );
      ( "interop",
        [ test_case "mix byte and Utf8" `Quick test_mix_byte_and_utf8 ]
      );
      ( "streaming",
        [
          test_case "satisfy across chunks" `Quick test_streaming_satisfy;
          test_case "take_while byte-at-a-time" `Quick test_streaming_take_while;
        ]
      );
    ]
