let int32_testable =
  Alcotest.testable (fun fmt v -> Format.fprintf fmt "0x%08lX" v) Int32.equal
let int64_testable =
  Alcotest.testable (fun fmt v -> Format.fprintf fmt "0x%016LX" v) Int64.equal

let test_any_uint8 () =
  match
    Parseff.parse "\x00\x7F\xFF" (fun () ->
        let a = Parseff.any_uint8 () in
        let b = Parseff.any_uint8 () in
        let c = Parseff.any_uint8 () in
        (a, b, c)
    )
  with
  | Ok (a, b, c) ->
      Alcotest.(check int) "0x00" 0 a;
      Alcotest.(check int) "0x7F" 127 b;
      Alcotest.(check int) "0xFF" 255 c
  | Error _ ->
      Alcotest.fail "Expected success"

let test_any_int8 () =
  match
    Parseff.parse "\x00\x7F\xFF\x80" (fun () ->
        let a = Parseff.any_int8 () in
        let b = Parseff.any_int8 () in
        let c = Parseff.any_int8 () in
        let d = Parseff.any_int8 () in
        (a, b, c, d)
    )
  with
  | Ok (a, b, c, d) ->
      Alcotest.(check int) "0x00" 0 a;
      Alcotest.(check int) "0x7F" 127 b;
      Alcotest.(check int) "0xFF" (-1) c;
      Alcotest.(check int) "0x80" (-128) d
  | Error _ ->
      Alcotest.fail "Expected success"

let test_uint8_eof () =
  match Parseff.parse "" (fun () -> Parseff.any_uint8 ()) with
  | Ok _ ->
      Alcotest.fail "Expected failure"
  | Error ({ kind = Expected { found = End; _ }; _ } as e) ->
      Alcotest.(check string)
        "expected items" "expected 1 byte, found end of input"
        (Parseff.error_message e);
      ()
  | Error _ ->
      Alcotest.fail "Expected an end of input error"

let test_be_any_int16 () =
  match Parseff.parse "\x01\x02" (fun () -> Parseff.BE.any_int16 ()) with
  | Ok v ->
      Alcotest.(check int) "0x0102" 0x0102 v
  | Error _ ->
      Alcotest.fail "Expected success"

let test_le_any_int16 () =
  match Parseff.parse "\x01\x02" (fun () -> Parseff.LE.any_int16 ()) with
  | Ok v ->
      Alcotest.(check int) "0x0201" 0x0201 v
  | Error _ ->
      Alcotest.fail "Expected success"

let test_be_any_uint16 () =
  match Parseff.parse "\xFF\xFE" (fun () -> Parseff.BE.any_uint16 ()) with
  | Ok v ->
      Alcotest.(check int) "0xFFFE unsigned" 0xFFFE v
  | Error _ ->
      Alcotest.fail "Expected success"

let test_be_any_int16_signed () =
  match Parseff.parse "\xFF\xFE" (fun () -> Parseff.BE.any_int16 ()) with
  | Ok v ->
      Alcotest.(check int) "0xFFFE signed = -2" (-2) v
  | Error _ ->
      Alcotest.fail "Expected success"

let test_int16_eof () =
  match Parseff.parse "\x01" (fun () -> Parseff.BE.any_int16 ()) with
  | Ok _ ->
      Alcotest.fail "Expected failure"
  | Error ({ kind = Expected { found = Short _; _ }; _ } as e) ->
      Alcotest.(check string)
        "expected items" "expected 2 bytes, found only 1 (end of input)"
        (Parseff.error_message e);
      ()
  | Error _ ->
      Alcotest.fail "Expected an end of input error"

let test_be_any_int32 () =
  match
    Parseff.parse "\x01\x02\x03\x04" (fun () -> Parseff.BE.any_int32 ())
  with
  | Ok v ->
      Alcotest.(check int32_testable) "0x01020304" 0x01020304l v
  | Error _ ->
      Alcotest.fail "Expected success"

let test_le_any_int32 () =
  match
    Parseff.parse "\x01\x02\x03\x04" (fun () -> Parseff.LE.any_int32 ())
  with
  | Ok v ->
      Alcotest.(check int32_testable) "0x04030201" 0x04030201l v
  | Error _ ->
      Alcotest.fail "Expected success"

let test_int32_eof () =
  match Parseff.parse "\x01\x02\x03" (fun () -> Parseff.BE.any_int32 ()) with
  | Ok _ ->
      Alcotest.fail "Expected failure"
  | Error ({ kind = Expected { found = Short _; _ }; _ } as e) ->
      Alcotest.(check string)
        "expected items" "expected 4 bytes, found only 3 (end of input)"
        (Parseff.error_message e);
      ()
  | Error _ ->
      Alcotest.fail "Expected an end of input error"

let test_be_any_int64 () =
  match
    Parseff.parse "\x01\x02\x03\x04\x05\x06\x07\x08" (fun () ->
        Parseff.BE.any_int64 ()
    )
  with
  | Ok v ->
      Alcotest.(check int64_testable) "0x0102030405060708" 0x0102030405060708L v
  | Error _ ->
      Alcotest.fail "Expected success"

let test_le_any_int64 () =
  match
    Parseff.parse "\x01\x02\x03\x04\x05\x06\x07\x08" (fun () ->
        Parseff.LE.any_int64 ()
    )
  with
  | Ok v ->
      Alcotest.(check int64_testable) "0x0807060504030201" 0x0807060504030201L v
  | Error _ ->
      Alcotest.fail "Expected success"

let test_int64_eof () =
  match
    Parseff.parse "\x01\x02\x03\x04\x05\x06\x07" (fun () ->
        Parseff.BE.any_int64 ()
    )
  with
  | Ok _ ->
      Alcotest.fail "Expected failure"
  | Error ({ kind = Expected { found = Short _; _ }; _ } as e) ->
      Alcotest.(check string)
        "expected items" "expected 8 bytes, found only 7 (end of input)"
        (Parseff.error_message e);
      ()
  | Error _ ->
      Alcotest.fail "Expected an end of input error"

let test_be_any_float () =
  match
    Parseff.parse "\x3F\x80\x00\x00" (fun () -> Parseff.BE.any_float ())
  with
  | Ok v ->
      Alcotest.(check (float 0.0)) "1.0" 1.0 v
  | Error _ ->
      Alcotest.fail "Expected success"

let test_le_any_float () =
  match
    Parseff.parse "\x00\x00\x80\x3F" (fun () -> Parseff.LE.any_float ())
  with
  | Ok v ->
      Alcotest.(check (float 0.0)) "1.0" 1.0 v
  | Error _ ->
      Alcotest.fail "Expected success"

let test_be_any_double () =
  match
    Parseff.parse "\x3F\xF0\x00\x00\x00\x00\x00\x00" (fun () ->
        Parseff.BE.any_double ()
    )
  with
  | Ok v ->
      Alcotest.(check (float 0.0)) "1.0" 1.0 v
  | Error _ ->
      Alcotest.fail "Expected success"

let test_le_any_double () =
  match
    Parseff.parse "\x00\x00\x00\x00\x00\x00\xF0\x3F" (fun () ->
        Parseff.LE.any_double ()
    )
  with
  | Ok v ->
      Alcotest.(check (float 0.0)) "1.0" 1.0 v
  | Error _ ->
      Alcotest.fail "Expected success"

let test_be_any_float_zero () =
  match
    Parseff.parse "\x00\x00\x00\x00" (fun () -> Parseff.BE.any_float ())
  with
  | Ok v ->
      Alcotest.(check (float 0.0)) "0.0" 0.0 v
  | Error _ ->
      Alcotest.fail "Expected success"

let test_be_any_float_neg () =
  match
    Parseff.parse "\xBF\x80\x00\x00" (fun () -> Parseff.BE.any_float ())
  with
  | Ok v ->
      Alcotest.(check (float 0.0)) "-1.0" (-1.0) v
  | Error _ ->
      Alcotest.fail "Expected success"

let test_be_int16_match () =
  match Parseff.parse "\x01\x02" (fun () -> Parseff.BE.int16 0x0102) with
  | Ok () ->
      ()
  | Error _ ->
      Alcotest.fail "Expected success"

let test_be_int16_mismatch () =
  match Parseff.parse "\x02\x01" (fun () -> Parseff.BE.int16 0x0102) with
  | Ok () ->
      Alcotest.fail "Expected failure"
  | Error ({ kind = Expected { found = Value _; _ }; _ } as e) ->
      Alcotest.(check string)
        "message" "expected int16be 0x0102, found 0x0201"
        (Parseff.error_message e)
  | Error _ ->
      Alcotest.fail "Expected an Expected error"

let test_be_int32_match () =
  match
    Parseff.parse "\x01\x02\x03\x04" (fun () -> Parseff.BE.int32 0x01020304l)
  with
  | Ok () ->
      ()
  | Error _ ->
      Alcotest.fail "Expected success"

let test_be_int64_match () =
  match
    Parseff.parse "\x01\x02\x03\x04\x05\x06\x07\x08" (fun () ->
        Parseff.BE.int64 0x0102030405060708L
    )
  with
  | Ok () ->
      ()
  | Error _ ->
      Alcotest.fail "Expected success"

let test_le_int32_match () =
  match
    Parseff.parse "\x04\x03\x02\x01" (fun () -> Parseff.LE.int32 0x01020304l)
  with
  | Ok () ->
      ()
  | Error _ ->
      Alcotest.fail "Expected success"

let expect_error_at pos msg (result : (_, Parseff.error) Stdlib.result) =
  match result with
  | Error ({ kind = Expected { found = Value _; _ }; _ } as e) ->
      let p = e.range.start in
      Alcotest.(check int) "error position" pos p;
      Alcotest.(check string) "error message" msg (Parseff.error_message e)
  | Ok _ ->
      Alcotest.fail "Expected failure"
  | Error _ ->
      Alcotest.fail "Expected an Expected error"

let expect_ok result =
  match result with Ok () -> () | Error _ -> Alcotest.fail "Expected success"

let test_be_int16_high_bit () =
  expect_ok (Parseff.parse "\xFF\xFE" (fun () -> Parseff.BE.int16 0xFFFE))

let test_be_int16_negative () =
  expect_ok (Parseff.parse "\xFF\xFE" (fun () -> Parseff.BE.int16 (-2)))

let test_le_int16_high_bit () =
  expect_ok (Parseff.parse "\xFE\xFF" (fun () -> Parseff.LE.int16 0xFFFE))

let test_int16_mismatch_message () =
  expect_error_at 0 "expected int16be 0xFFFE, found 0x0102"
    (Parseff.parse "\x01\x02" (fun () -> Parseff.BE.int16 0xFFFE));
  expect_error_at 0 "expected int16be 0xFFFE, found 0xFFFF"
    (Parseff.parse "\xFF\xFF" (fun () -> Parseff.BE.int16 0xFFFE))

let test_int16_out_of_range () =
  List.iter
    (fun v ->
      match Parseff.parse "\x00\x00" (fun () -> Parseff.BE.int16 v) with
      | _ ->
          Alcotest.failf "%d accepted" v
      | exception Invalid_argument _ ->
          ()
    )
    [ -32769; 65536 ]

let test_int16_mismatch_position_after_prefix () =
  expect_error_at 1 "expected int16be 0x0102, found 0x0201"
    (Parseff.parse "\x00\x02\x01" (fun () ->
         ignore (Parseff.any_char ());
         Parseff.BE.int16 0x0102
     )
    )

let test_be_int32_mismatch () =
  expect_error_at 0 "expected int32be 0x01020304, found 0x04030201"
    (Parseff.parse "\x04\x03\x02\x01" (fun () -> Parseff.BE.int32 0x01020304l))

let test_be_int64_mismatch () =
  expect_error_at 0
    "expected int64be 0x0102030405060708, found 0x0807060504030201"
    (Parseff.parse "\x08\x07\x06\x05\x04\x03\x02\x01" (fun () ->
         Parseff.BE.int64 0x0102030405060708L
     )
    )

let test_le_int32_mismatch_position () =
  expect_error_at 1 "expected int32le 0x01020304, found 0x04030201"
    (Parseff.parse "\x00\x01\x02\x03\x04" (fun () ->
         ignore (Parseff.any_char ());
         Parseff.LE.int32 0x01020304l
     )
    )

let test_int64_mismatch_position () =
  expect_error_at 1
    "expected int64be 0x0000000000000001, found 0x0100000000000000"
    (Parseff.parse "\x00\x01\x00\x00\x00\x00\x00\x00\x00" (fun () ->
         ignore (Parseff.any_char ());
         Parseff.BE.int64 1L
     )
    )

let test_le_any_uint16_high_bits () =
  match Parseff.parse "\xFE\xFF" (fun () -> Parseff.LE.any_uint16 ()) with
  | Ok v ->
      Alcotest.(check int) "0xFFFE" 0xFFFE v
  | Error _ ->
      Alcotest.fail "Expected success"

let test_le_any_int16_high_bits () =
  match Parseff.parse "\xFE\xFF" (fun () -> Parseff.LE.any_int16 ()) with
  | Ok v ->
      Alcotest.(check int) "-2" (-2) v
  | Error _ ->
      Alcotest.fail "Expected success"

let test_le_int64_high_bits () =
  match
    Parseff.parse "\xF8\xFF\xFF\xFF\xFF\xFF\xFF\xFF" (fun () ->
        Parseff.LE.any_int64 ()
    )
  with
  | Ok v ->
      Alcotest.(check int64_testable) "-8" (-8L) v
  | Error _ ->
      Alcotest.fail "Expected success"

let test_le_int64_match_high_bits () =
  expect_ok
    (Parseff.parse "\xF8\xFF\xFF\xFF\xFF\xFF\xFF\xFF" (fun () ->
         Parseff.LE.int64 (-8L)
     )
    );
  expect_ok
    (Parseff.parse "\x01\x02\x03\x04\x05\x06\x07\x88" (fun () ->
         Parseff.LE.int64 0x8807060504030201L
     )
    )

let test_take_exact () =
  match
    Parseff.parse ~consume:`Partial "abcdef" (fun () ->
        let s = Parseff.take 4 in
        let pos = Parseff.position () in
        (s, pos)
    )
  with
  | Ok (s, pos) ->
      Alcotest.(check string) "taken" "abcd" s;
      Alcotest.(check int) "position" 4 pos
  | Error _ ->
      Alcotest.fail "Expected success"

let test_take_zero () =
  match
    Parseff.parse ~consume:`Partial "abc" (fun () ->
        let s = Parseff.take 0 in
        let pos = Parseff.position () in
        (s, pos)
    )
  with
  | Ok (s, pos) ->
      Alcotest.(check string) "empty" "" s;
      Alcotest.(check int) "position" 0 pos
  | Error _ ->
      Alcotest.fail "Expected success"

let test_take_eof () =
  match Parseff.parse "abc" (fun () -> Parseff.take 10) with
  | Ok _ ->
      Alcotest.fail "Expected failure"
  | Error ({ kind = Expected { found = Short _; _ }; _ } as e) ->
      Alcotest.(check string)
        "expected items" "expected 10 bytes, found only 3 (end of input)"
        (Parseff.error_message e);
      ()
  | Error _ ->
      Alcotest.fail "Expected an end of input error"

let test_take_negative () =
  match Parseff.parse "abc" (fun () -> Parseff.take (-1)) with
  | Ok _ ->
      Alcotest.fail "Expected an error for negative take"
  | Error e -> (
      Alcotest.(check string)
        "message" "expected non-negative length, found -1"
        (Parseff.error_message e);
      Alcotest.(check int) "offset" 0 e.range.start;
      match e.kind with
      | Parseff.Expected { found = Parseff.Value v; _ } ->
          Alcotest.(check string) "found" "-1" v
      | _ ->
          Alcotest.fail "Expected Expected with found Value"
    )

let test_take_negative_recovers () =
  let p () =
    let v = Parseff.or_ (fun () -> Parseff.take (-1)) (fun () -> "x") in
    let rest = Parseff.take 3 in
    (v, rest)
  in
  match Parseff.parse "abc" p with
  | Ok (v, rest) ->
      Alcotest.(check string) "alternative" "x" v;
      Alcotest.(check string) "input kept" "abc" rest
  | Error e ->
      Alcotest.fail (Parseff.error_message e)

let test_take_negative_optional () =
  let p () = Parseff.optional (fun () -> Parseff.take (-1)) in
  match Parseff.parse "abc" (fun () -> (p (), Parseff.take 3)) with
  | Ok (None, "abc") ->
      ()
  | Ok _ ->
      Alcotest.fail "Expected None and untouched input"
  | Error e ->
      Alcotest.fail (Parseff.error_message e)

let test_take_negative_untrusted_length () =
  let parse_png_chunk () =
    let length = Parseff.BE.any_int32 () in
    Parseff.take (Int32.to_int length)
  in
  match Parseff.parse "\xFF\xFF\xFF\xFF" parse_png_chunk with
  | Ok _ ->
      Alcotest.fail "Expected an error"
  | Error e ->
      Alcotest.(check string)
        "message" "expected non-negative length, found -1"
        (Parseff.error_message e);
      Alcotest.(check int) "offset" 4 e.range.start

let test_tlv_header () =
  let input = "TL\x01\x00\x01\x0A\x00\x03foo" in
  let parser () =
    Parseff.literal "TL";
    let version = Parseff.any_uint8 () in
    let count = Parseff.BE.any_uint16 () in
    let fields =
      Parseff.count count (fun () ->
          let tag = Parseff.any_uint8 () in
          let len = Parseff.BE.any_uint16 () in
          let value = Parseff.take len in
          (tag, value)
      )
    in
    Parseff.end_of_input ();
    (version, fields)
  in
  match Parseff.parse input parser with
  | Ok (version, fields) ->
      Alcotest.(check int) "version" 1 version;
      Alcotest.(check int) "field count" 1 (List.length fields);
      let tag, value = List.hd fields in
      Alcotest.(check int) "tag" 0x0A tag;
      Alcotest.(check string) "value" "foo" value
  | Error _ ->
      Alcotest.fail "Expected success"

let test_backtracking () =
  let input = "\x01\x02\x03\x04" in
  let parser () =
    Parseff.or_
      (fun () ->
        Parseff.BE.int16 0x0201;
        "matched_first"
      )
      (fun () ->
        let v = Parseff.BE.any_uint16 () in
        Printf.sprintf "got_%04X" v
      )
  in
  match Parseff.parse ~consume:`Partial input parser with
  | Ok s ->
      Alcotest.(check string) "backtracked" "got_0102" s
  | Error _ ->
      Alcotest.fail "Expected success"

let byte_at_a_time s =
  let pos = ref 0 in
  Parseff.Source.of_function (fun buf off len ->
      let remaining = String.length s - !pos in
      if remaining <= 0 then
        0
      else begin
        let n = min 1 (min len remaining) in
        Bytes.blit_string s !pos buf off n;
        pos := !pos + n;
        n
      end
  )

let test_streaming_be_int32 () =
  let src = byte_at_a_time "\x01\x02\x03\x04" in
  match Parseff.Source.parse src (fun () -> Parseff.BE.any_int32 ()) with
  | Ok v ->
      Alcotest.(check int32_testable) "0x01020304" 0x01020304l v
  | Error _ ->
      Alcotest.fail "Expected success"

let test_streaming_le_int64 () =
  let src = byte_at_a_time "\x01\x02\x03\x04\x05\x06\x07\x08" in
  match Parseff.Source.parse src (fun () -> Parseff.LE.any_int64 ()) with
  | Ok v ->
      Alcotest.(check int64_testable) "0x0807060504030201" 0x0807060504030201L v
  | Error _ ->
      Alcotest.fail "Expected success"

let test_streaming_take () =
  let src = byte_at_a_time "hello world" in
  match
    Parseff.Source.parse ~consume:`Partial src (fun () -> Parseff.take 5)
  with
  | Ok v ->
      Alcotest.(check string) "taken" "hello" v
  | Error _ ->
      Alcotest.fail "Expected success"

let test_streaming_eof () =
  let src = byte_at_a_time "\x01" in
  match Parseff.Source.parse src (fun () -> Parseff.BE.any_int16 ()) with
  | Ok _ ->
      Alcotest.fail "Expected failure"
  | Error ({ kind = Expected { found = Short _; _ }; _ } as e) ->
      Alcotest.(check string)
        "expected items" "expected 2 bytes, found only 1 (end of input)"
        (Parseff.error_message e);
      ()
  | Error _ ->
      Alcotest.fail "Expected an end of input error"

let test_streaming_composition () =
  let input = "TL\x01\x00\x01\x0A\x00\x03foo" in
  let src = byte_at_a_time input in
  let parser () =
    Parseff.literal "TL";
    let version = Parseff.any_uint8 () in
    let count = Parseff.BE.any_uint16 () in
    let fields =
      Parseff.count count (fun () ->
          let tag = Parseff.any_uint8 () in
          let len = Parseff.BE.any_uint16 () in
          let value = Parseff.take len in
          (tag, value)
      )
    in
    Parseff.end_of_input ();
    (version, fields)
  in
  match Parseff.Source.parse src parser with
  | Ok (version, fields) ->
      Alcotest.(check int) "version" 1 version;
      let tag, value = List.hd fields in
      Alcotest.(check int) "tag" 0x0A tag;
      Alcotest.(check string) "value" "foo" value
  | Error _ ->
      Alcotest.fail "Expected success"

let check_ok testable name expected (result : (_, Parseff.error) Stdlib.result)
    =
  match result with
  | Ok v ->
      Alcotest.check testable name expected v
  | Error e ->
      Alcotest.fail (Parseff.error_message e)

let check_failure ~pos msg (result : (_, Parseff.error) Stdlib.result) =
  match result with
  | Ok _ ->
      Alcotest.fail "Expected failure"
  | Error ({ kind = Expected _; _ } as e) ->
      Alcotest.(check string) "message" msg (Parseff.error_message e);
      Alcotest.(check int) "position" pos e.range.start
  | Error e ->
      Alcotest.failf "Expected an Expected error, got %s"
        (Parseff.error_message e)

let both input parser =
  [
    Parseff.parse input parser;
    Parseff.Source.parse (byte_at_a_time input) parser;
  ]

let test_any_uint32 () =
  List.iter
    (check_ok Alcotest.int "BE.any_uint32" 0xFFFFFFFE)
    (both "\xFF\xFF\xFF\xFE" Parseff.BE.any_uint32);
  List.iter
    (check_ok Alcotest.int "LE.any_uint32" 0xFFFFFFFE)
    (both "\xFE\xFF\xFF\xFF" Parseff.LE.any_uint32);
  List.iter
    (check_ok Alcotest.int "BE.any_uint32 small" 0x01020304)
    (both "\x01\x02\x03\x04" Parseff.BE.any_uint32)

let test_any_uint32_truncated () =
  List.iter
    (check_failure ~pos:0 "expected 4 bytes, found only 3 (end of input)")
    (both "\x01\x02\x03" Parseff.BE.any_uint32);
  List.iter
    (check_failure ~pos:0 "expected 4 bytes, found end of input")
    (both "" Parseff.LE.any_uint32)

let test_take_span_in_parse () =
  let input = "abcdef" in
  match
    Parseff.parse ~consume:`Partial input (fun () ->
        ignore (Parseff.take 1);
        let s = Parseff.Slice.take 3 in
        (s, Parseff.position ())
    )
  with
  | Ok (s, pos) ->
      Alcotest.(check int) "length" 3 (Parseff.Slice.length s);
      Alcotest.(check string) "text" "bcd" (Parseff.Slice.to_string s);
      Alcotest.(check int) "position" 4 pos
  | Error e ->
      Alcotest.fail (Parseff.error_message e)

let test_take_span_streaming () =
  match
    Parseff.Source.parse ~consume:`Partial (byte_at_a_time "abcdef") (fun () ->
        ignore (Parseff.take 1);
        let s = Parseff.Slice.take 3 in
        (s, Parseff.position ())
    )
  with
  | Ok (s, pos) ->
      Alcotest.(check string) "text" "bcd" (Parseff.Slice.to_string s);
      Alcotest.(check int) "length" 3 (Parseff.Slice.length s);
      Alcotest.(check int) "position" 4 pos
  | Error e ->
      Alcotest.fail (Parseff.error_message e)

let test_take_span_zero () =
  List.iter
    (check_ok Alcotest.string "empty" "")
    (both "" (fun () -> Parseff.Slice.to_string (Parseff.Slice.take 0)))

let test_take_span_truncated () =
  List.iter
    (check_failure ~pos:0 "expected 10 bytes, found only 3 (end of input)")
    (both "abc" (fun () -> Parseff.Slice.take 10));
  List.iter
    (check_failure ~pos:0 "expected 2 bytes, found end of input")
    (both "" (fun () -> Parseff.Slice.take 2))

let test_take_span_negative () =
  List.iter
    (check_failure ~pos:0 "expected non-negative length, found -1")
    (both "abc" (fun () -> Parseff.Slice.take (-1)));
  let p () =
    let v =
      Parseff.or_
        (fun () -> Parseff.Slice.to_string (Parseff.Slice.take (-1)))
        (fun () -> "x")
    in
    (v, Parseff.take 3)
  in
  List.iter
    (check_ok Alcotest.(pair string string) "recovers" ("x", "abc"))
    (both "abc" p)

let same_as_take name run n msg =
  let as_take = run (fun () -> Parseff.take n) in
  let as_span =
    run (fun () -> Parseff.Slice.to_string (Parseff.Slice.take n))
  in
  match (as_take, as_span) with
  | Error a, Error b ->
      Alcotest.(check string) (name ^ " take") msg (Parseff.error_message a);
      Alcotest.(check string)
        (name ^ " Slice.take") msg (Parseff.error_message b);
      Alcotest.(check int) (name ^ " position") a.range.start b.range.start
  | _ ->
      Alcotest.failf "%s: expected take and Slice.take to fail" name

let test_take_span_like_take () =
  same_as_take "huge" (Parseff.parse "abc") max_int
    "expected 4611686018427387903 bytes, found only 3 (end of input)";
  same_as_take "huge streaming"
    (fun p -> Parseff.Source.parse (byte_at_a_time "abc") p)
    max_int "expected 4611686018427387903 bytes, found only 3 (end of input)";
  same_as_take "past the window"
    (fun p ->
      Parseff.Source.parse ~backtrack_window:16
        (byte_at_a_time (String.make 64 'a'))
        p
    )
    40 "backtrack window of 16 bytes exceeded"

let varint_after_prefix () =
  ignore (Parseff.any_uint8 ());
  Parseff.any_varint ()

let test_any_varint () =
  List.iter
    (fun (input, expected) ->
      List.iter
        (check_ok Alcotest.(pair int int) (String.escaped input) expected)
        (both input (fun () ->
             let v = Parseff.any_varint () in
             (v, Parseff.position ())
         )
        )
    )
    [
      ("\x00", (0, 1));
      ("\x7F", (127, 1));
      ("\x80\x01", (128, 2));
      ("\xAC\x02", (300, 2));
      ("\x80\x00", (0, 2));
      ("\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\x3F", (max_int, 9));
    ]

let test_any_varint_too_large () =
  List.iter
    (fun (input, msg) ->
      List.iter (check_failure ~pos:1 msg) (both input varint_after_prefix)
    )
    [
      ( "\x00\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\x40",
        "expected varint up to max_int, found '\\xFF'"
      );
      ( "\x00\x80\x80\x80\x80\x80\x80\x80\x80\x80\x00",
        "expected varint up to max_int, found '\\x80'"
      );
    ]

let test_any_varint_never_ends () =
  let input = "\x00" ^ String.make 100 '\x80' in
  List.iter
    (function
      | Ok _ ->
          Alcotest.fail "Expected failure"
      | Error (e : Parseff.error) ->
          Alcotest.(check int) "position" 1 e.range.start
      )
    (both input varint_after_prefix)

let test_any_varint_truncated () =
  List.iter
    (check_failure ~pos:3 "expected 1 byte, found end of input")
    (both "\x00\x80\x80" varint_after_prefix);
  List.iter
    (check_failure ~pos:0 "expected 1 byte, found end of input")
    (both "" Parseff.any_varint)

let test_any_varint_combinators () =
  List.iter
    (check_ok Alcotest.(list int) "many" [ 1; 300; 0 ])
    (both "\x01\xAC\x02\x00" (fun () -> Parseff.many Parseff.any_varint));
  let too_large = "\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\x40" in
  List.iter
    (check_ok Alcotest.int "or_ rewinds" 0xFF)
    (both too_large (fun () ->
         let v = Parseff.or_ Parseff.any_varint Parseff.any_uint8 in
         ignore (Parseff.take 8);
         v
     )
    );
  List.iter
    (check_ok Alcotest.(pair int int) "look_ahead" (300, 0))
    (both "\xAC\x02" (fun () ->
         let v = Parseff.look_ahead Parseff.any_varint in
         let pos = Parseff.position () in
         ignore (Parseff.take 2);
         (v, pos)
     )
    )

let frame () =
  let len = Parseff.BE.any_uint32 () in
  let kind = Parseff.any_uint8 () in
  (kind, Parseff.take (len - 1))

let frames () = Parseff.many frame
let truncated = "\x00\x00\x00\x06\x01hello\x00\x00\x00\x0a\x06cut\xff\x00"

let error_of = function
  | Ok _ ->
      Alcotest.fail "Expected failure"
  | Error e ->
      e

let render ?source e = Parseff.render_error ~name:"f.bin" ?source ~binary:true e

let chunked n input =
  let pos = ref 0 in
  Parseff.Source.of_chunks (fun () ->
      if !pos >= String.length input then
        None
      else begin
        let k = min n (String.length input - !pos) in
        let c = String.sub input !pos k in
        pos := !pos + k;
        Some c
      end
  )

let test_render_both_ways () =
  let e = error_of (Parseff.parse truncated frames) in
  Alcotest.(check string)
    "line and column"
    "f.bin:2:2: [error] expected 9 bytes, found only 5 (end of input)\n\
     1 | \\x00\\x00\\x00\\x06\\x01hello\\x00\\x00\\x00\n\
     2 | \\x06cut\\xFF\\x00\n\
    \  |     ^^^^^^^^^^^"
    (Parseff.render_error ~name:"f.bin" e);
  let bytes =
    "f.bin: byte 15: [error] expected 9 bytes, found only 5 (end of input)\n\
    \ 0 | 00 00 00 06 01 68 65 6c 6c 6f 00 00 00 0a 06 63  .....hello.....c\n\
    \   |                                              ^^                 ^\n\
     16 | 75 74 ff 00                                      ut..\n\
    \   | ^^^^^^^^^^^                                      ^^^^"
  in
  Alcotest.(check string) "binary" bytes (render e);
  Alcotest.(check string)
    "binary with ~source" bytes
    (render ~source:truncated e)

let test_render_streamed () =
  let e = error_of (Parseff.Source.parse (chunked 3 truncated) frames) in
  Alcotest.(check string)
    "the line end the excerpt holds prints as bytes"
    "f.bin: byte 15: [error] expected 9 bytes, found only 5 (end of input)\n\
    \ 0 | 00 00 00 06 01 68 65 6c 6c 6f 00 00 00 0a 06 63  .....hello.....c\n\
    \   |                                              ^^                 ^\n\
     16 | 75 74 ff 00                                      ut..\n\
    \   | ^^^^^^^^^^^                                      ^^^^"
    (render e)

let test_render_streamed_crlf () =
  let input = "ab\r\n\x00\x05xy" in
  let parser () =
    let _ = Parseff.take 4 in
    Parseff.take (Parseff.BE.any_uint16 ())
  in
  let whole = error_of (Parseff.parse input parser) in
  Alcotest.(check string)
    "string input"
    "f.bin: byte 6: [error] expected 5 bytes, found only 2 (end of input)\n\
     0 | 61 62 0d 0a 00 05 78 79                          ab....xy\n\
    \  |                   ^^^^^                                ^^"
    (render whole);
  List.iter
    (fun n ->
      let e = error_of (Parseff.Source.parse (chunked n input) parser) in
      Alcotest.(check string)
        (Printf.sprintf "chunks of %d" n)
        (render whole) (render e);
      Alcotest.(check string)
        (Printf.sprintf "chunks of %d, text" n)
        (Parseff.render_error whole)
        (Parseff.render_error e)
    )
    (* Chunk sizes that keep the line above the error buffered. *)
    [ 3; 8 ]

let test_render_far_into_a_stream () =
  let record i =
    if i = 300 then
      "\xba\xd0\x00"
    else
      "\xca\xfe" ^ String.make 1 (Char.chr (i land 0xff))
  in
  let input = String.concat "" (List.init 301 record) in
  let errors =
    Parseff.Source.parse_each ~backtrack_window:16 (chunked 5 input) (fun () ->
        Parseff.BE.int16 0xcafe;
        Parseff.any_uint8 ()
    )
    |> Seq.filter_map (function Ok _ -> None | Error e -> Some e)
    |> List.of_seq
  in
  let e = match errors with [ e ] -> e | _ -> Alcotest.fail "one error" in
  Alcotest.(check int) "error offset" 900 e.range.start;
  let rows =
    List.filter
      (fun l -> String.length l > 5 && l.[0] <> ' ' && l.[0] <> 'f')
      (String.split_on_char '\n' (render e))
  in
  Alcotest.(check bool) "rows shown" true (rows <> []);
  List.iter
    (fun row ->
      let bar = String.index row '|' in
      let offset = int_of_string (String.trim (String.sub row 0 bar)) in
      for k = 0 to 15 do
        let at = bar + 2 + (3 * k) in
        if at + 1 < String.length row && row.[at] <> ' ' then
          Alcotest.(check string)
            (Printf.sprintf "byte %d" (offset + k))
            (Printf.sprintf "%02x" (Char.code input.[offset + k]))
            (String.sub row at 2)
      done
    )
    rows;
  let first = List.hd rows in
  let bar = String.index first '|' in
  let offset = int_of_string (String.trim (String.sub first 0 bar)) in
  Alcotest.(check string)
    "the error byte is shown" "ba"
    (String.sub first (bar + 2 + (3 * (900 - offset))) 2)

let test_render_nul_and_invalid_utf8 () =
  let input = "\x00\xc3\x28\xff\xfe\x00AB\x00" in
  let e =
    error_of
      (Parseff.parse input (fun () ->
           Parseff.context "magic" (fun () ->
               let _ = Parseff.take 3 in
               Parseff.BE.int16 0x1234
           )
       )
      )
  in
  Alcotest.(check string)
    "excerpt"
    "f.bin: byte 3: [error] expected int16be 0x1234, found 0xFFFE\n\
     0 | 00 c3 28 ff fe 00 41 42 00                       ..(...AB.\n\
    \  |          ^^^^^                                      ^^\n\
    \  * while parsing magic, from byte 0"
    (render e)

let test_render_context_and_notes () =
  let e =
    error_of
      (Parseff.parse "\x01\x02\x03\x04" (fun () ->
           Parseff.context "header" (fun () ->
               let _ = Parseff.any_uint8 () in
               let _ = Parseff.any_uint8 () in
               Parseff.fail ~from:2 ~upto:4
                 ~notes:
                   [
                     { at = Some { start = 0; stop = 2 }; text = "tag" };
                     { at = None; text = "loose" };
                   ]
                 ~help:"check the length" "bad length"
           )
       )
      )
  in
  Alcotest.(check string)
    "contexts, notes, and help"
    "f.bin: byte 2: [error] bad length\n\
     0 | 01 02 03 04                                      ....\n\
    \  |       ^^^^^                                        ^^\n\
    \  | -----                                            -- tag\n\
    \  * while parsing header, from byte 0\n\
    \  * note: loose\n\
    \  * help: check the length"
    (render e)

let test_render_note_rows () =
  let input = String.init 80 (fun i -> Char.chr (0x41 + (i mod 26))) in
  let e =
    error_of
      (Parseff.parse input (fun () ->
           let _ = Parseff.take 70 in
           Parseff.fail ~from:66 ~upto:70
             ~notes:
               [
                 { at = Some { start = 2; stop = 3 }; text = "far" };
                 { at = Some { start = 50; stop = 52 }; text = "near" };
               ]
             "bad"
       )
      )
  in
  Alcotest.(check string)
    "a note row elides the rows between"
    "f.bin: byte 66: [error] bad\n\
    \ 0 | 41 42 43 44 45 46 47 48 49 4a 4b 4c 4d 4e 4f 50  ABCDEFGHIJKLMNOP\n\
    \   |       --                                           - far\n\
    \   | ...\n\
     48 | 57 58 59 5a 41 42 43 44 45 46 47 48 49 4a 4b 4c  WXYZABCDEFGHIJKL\n\
    \   |       -----                                        -- near\n\
     64 | 4d 4e 4f 50 51 52 53 54 55 56 57 58 59 5a 41 42  MNOPQRSTUVWXYZAB\n\
    \   |       ^^^^^^^^^^^                                  ^^^^"
    (render e)

let test_render_long_range () =
  let input = String.make 64 '\x00' in
  let e =
    error_of (Parseff.parse input (fun () -> Parseff.fail ~from:4 ~upto:60 "x"))
  in
  let carets = String.make 35 '^' and ascii = String.make 12 '^' in
  Alcotest.(check string)
    "the first and last rows of the range"
    ("f.bin: byte 4: [error] x\n\
     \ 0 | 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00  ................\n\
     \   |             " ^ carets ^ "      " ^ ascii
   ^ "\n\
     \   | ...\n\
      48 | 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00  ................\n\
     \   | " ^ carets ^ "              " ^ ascii
    )
    (render e)

let test_render_offset_zero () =
  let e = error_of (Parseff.parse "\xde\xad" (fun () -> Parseff.BE.int16 1)) in
  Alcotest.(check string)
    "offset 0"
    "f.bin: byte 0: [error] expected int16be 0x0001, found 0xDEAD\n\
     0 | de ad                                            ..\n\
    \  | ^^^^^                                            ^^"
    (render e)

let test_render_end_of_input () =
  let at_end input =
    render
      (error_of
         (Parseff.parse input (fun () ->
              let _ = Parseff.take (String.length input) in
              Parseff.any_uint8 ()
          )
         )
      )
  in
  Alcotest.(check string)
    "inside a row"
    "f.bin: byte 2: [error] expected 1 byte, found end of input\n\
     0 | 01 02                                            ..\n\
    \  |       ^^                                           ^"
    (at_end "\x01\x02");
  Alcotest.(check string)
    "after a full row: an empty next row holds the caret at slot 0"
    "f.bin: byte 16: [error] expected 1 byte, found end of input\n\
    \ 0 | 00 01 02 03 04 05 06 07 08 09 0a 0b 0c 0d 0e 0f  ................\n\
     16 |\n\
    \   | ^^                                               ^"
    (at_end (String.init 16 Char.chr));
  Alcotest.(check string)
    "after two full rows"
    "f.bin: byte 32: [error] expected 1 byte, found end of input\n\
     16 | 10 11 12 13 14 15 16 17 18 19 1a 1b 1c 1d 1e 1f  ................\n\
     32 |\n\
    \   | ^^                                               ^"
    (at_end (String.init 32 Char.chr));
  Alcotest.(check string)
    "empty input has no bytes to show"
    "f.bin: byte 0: [error] expected 1 byte, found end of input" (at_end "")

let test_render_default_and_no_name () =
  let e = error_of (Parseff.parse "\xde\xad" (fun () -> Parseff.BE.int16 1)) in
  Alcotest.(check string)
    "~binary:false is the default" (Parseff.render_error e)
    (Parseff.render_error ~binary:false e);
  Alcotest.(check string)
    "without ~name"
    "byte 0: [error] expected int16be 0x0001, found 0xDEAD\n\
     0 | de ad                                            ..\n\
    \  | ^^^^^                                            ^^"
    (Parseff.render_error ~binary:true e)

let () =
  Alcotest.run "binary"
    [
      ( "byte reads",
        [
          Alcotest.test_case "any_uint8" `Quick test_any_uint8;
          Alcotest.test_case "any_int8" `Quick test_any_int8;
          Alcotest.test_case "uint8 eof" `Quick test_uint8_eof;
        ]
      );
      ( "16-bit reads",
        [
          Alcotest.test_case "BE.any_int16" `Quick test_be_any_int16;
          Alcotest.test_case "LE.any_int16" `Quick test_le_any_int16;
          Alcotest.test_case "BE.any_uint16" `Quick test_be_any_uint16;
          Alcotest.test_case "BE.any_int16 signed" `Quick
            test_be_any_int16_signed;
          Alcotest.test_case "int16 eof" `Quick test_int16_eof;
        ]
      );
      ( "32-bit reads",
        [
          Alcotest.test_case "BE.any_int32" `Quick test_be_any_int32;
          Alcotest.test_case "LE.any_int32" `Quick test_le_any_int32;
          Alcotest.test_case "int32 eof" `Quick test_int32_eof;
        ]
      );
      ( "unsigned 32-bit reads",
        [
          Alcotest.test_case "any_uint32" `Quick test_any_uint32;
          Alcotest.test_case "any_uint32 truncated" `Quick
            test_any_uint32_truncated;
        ]
      );
      ( "64-bit reads",
        [
          Alcotest.test_case "BE.any_int64" `Quick test_be_any_int64;
          Alcotest.test_case "LE.any_int64" `Quick test_le_any_int64;
          Alcotest.test_case "int64 eof" `Quick test_int64_eof;
        ]
      );
      ( "float/double",
        [
          Alcotest.test_case "BE.any_float 1.0" `Quick test_be_any_float;
          Alcotest.test_case "LE.any_float 1.0" `Quick test_le_any_float;
          Alcotest.test_case "BE.any_double 1.0" `Quick test_be_any_double;
          Alcotest.test_case "LE.any_double 1.0" `Quick test_le_any_double;
          Alcotest.test_case "BE.any_float 0.0" `Quick test_be_any_float_zero;
          Alcotest.test_case "BE.any_float -1.0" `Quick test_be_any_float_neg;
        ]
      );
      ( "exact match",
        [
          Alcotest.test_case "BE.int16 match" `Quick test_be_int16_match;
          Alcotest.test_case "BE.int16 mismatch" `Quick test_be_int16_mismatch;
          Alcotest.test_case "BE.int32 match" `Quick test_be_int32_match;
          Alcotest.test_case "BE.int64 match" `Quick test_be_int64_match;
          Alcotest.test_case "LE.int32 match" `Quick test_le_int32_match;
          Alcotest.test_case "BE.int16 high bit" `Quick test_be_int16_high_bit;
          Alcotest.test_case "BE.int16 negative" `Quick test_be_int16_negative;
          Alcotest.test_case "LE.int16 high bit" `Quick test_le_int16_high_bit;
          Alcotest.test_case "int16 mismatch message" `Quick
            test_int16_mismatch_message;
          Alcotest.test_case "int16 out of range" `Quick test_int16_out_of_range;
          Alcotest.test_case "int16 mismatch position" `Quick
            test_int16_mismatch_position_after_prefix;
          Alcotest.test_case "BE.int32 mismatch" `Quick test_be_int32_mismatch;
          Alcotest.test_case "BE.int64 mismatch" `Quick test_be_int64_mismatch;
          Alcotest.test_case "LE.int32 mismatch position" `Quick
            test_le_int32_mismatch_position;
          Alcotest.test_case "int64 mismatch position" `Quick
            test_int64_mismatch_position;
          Alcotest.test_case "LE.any_uint16 high bits" `Quick
            test_le_any_uint16_high_bits;
          Alcotest.test_case "LE.any_int16 high bits" `Quick
            test_le_any_int16_high_bits;
          Alcotest.test_case "LE.any_int64 high bits" `Quick
            test_le_int64_high_bits;
          Alcotest.test_case "LE.int64 match high bits" `Quick
            test_le_int64_match_high_bits;
        ]
      );
      ( "take",
        [
          Alcotest.test_case "take exact" `Quick test_take_exact;
          Alcotest.test_case "take zero" `Quick test_take_zero;
          Alcotest.test_case "take eof" `Quick test_take_eof;
          Alcotest.test_case "take negative" `Quick test_take_negative;
          Alcotest.test_case "take negative recovers" `Quick
            test_take_negative_recovers;
          Alcotest.test_case "take negative optional" `Quick
            test_take_negative_optional;
          Alcotest.test_case "take negative untrusted length" `Quick
            test_take_negative_untrusted_length;
        ]
      );
      ( "varint",
        [
          Alcotest.test_case "values" `Quick test_any_varint;
          Alcotest.test_case "too large" `Quick test_any_varint_too_large;
          Alcotest.test_case "never ends" `Quick test_any_varint_never_ends;
          Alcotest.test_case "truncated" `Quick test_any_varint_truncated;
          Alcotest.test_case "in combinators" `Quick test_any_varint_combinators;
        ]
      );
      ( "Slice.take",
        [
          Alcotest.test_case "in parse" `Quick test_take_span_in_parse;
          Alcotest.test_case "owned copy in Source.parse" `Quick
            test_take_span_streaming;
          Alcotest.test_case "zero length" `Quick test_take_span_zero;
          Alcotest.test_case "truncated" `Quick test_take_span_truncated;
          Alcotest.test_case "negative" `Quick test_take_span_negative;
          Alcotest.test_case "same as take" `Quick test_take_span_like_take;
        ]
      );
      ("composition", [ Alcotest.test_case "TLV header" `Quick test_tlv_header ]);
      ( "backtracking",
        [ Alcotest.test_case "or_ with binary" `Quick test_backtracking ]
      );
      ( "streaming",
        [
          Alcotest.test_case "BE.any_int32" `Quick test_streaming_be_int32;
          Alcotest.test_case "LE.any_int64" `Quick test_streaming_le_int64;
          Alcotest.test_case "take" `Quick test_streaming_take;
          Alcotest.test_case "eof" `Quick test_streaming_eof;
          Alcotest.test_case "TLV composition" `Quick test_streaming_composition;
        ]
      );
      ( "render_error ~binary:true",
        [
          Alcotest.test_case "both ways" `Quick test_render_both_ways;
          Alcotest.test_case "streamed" `Quick test_render_streamed;
          Alcotest.test_case "streamed CRLF" `Quick test_render_streamed_crlf;
          Alcotest.test_case "far into a stream" `Quick
            test_render_far_into_a_stream;
          Alcotest.test_case "NUL and invalid UTF-8" `Quick
            test_render_nul_and_invalid_utf8;
          Alcotest.test_case "contexts, notes, help" `Quick
            test_render_context_and_notes;
          Alcotest.test_case "note rows" `Quick test_render_note_rows;
          Alcotest.test_case "long range" `Quick test_render_long_range;
          Alcotest.test_case "offset 0" `Quick test_render_offset_zero;
          Alcotest.test_case "end of input" `Quick test_render_end_of_input;
          Alcotest.test_case "default and no name" `Quick
            test_render_default_and_no_name;
        ]
      );
    ]
