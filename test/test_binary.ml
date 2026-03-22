let int32_testable =
  Alcotest.testable (fun fmt v -> Format.fprintf fmt "0x%08lX" v) Int32.equal
let int64_testable =
  Alcotest.testable (fun fmt v -> Format.fprintf fmt "0x%016LX" v) Int64.equal

(* --- byte reads --- *)

let test_be_any_uint8 () =
  match
    Parseff.parse "\x00\x7F\xFF" (fun () ->
        let a = Parseff.BE.any_uint8 () in
        let b = Parseff.BE.any_uint8 () in
        let c = Parseff.BE.any_uint8 () in
        (a, b, c)
    )
  with
  | Ok (a, b, c) ->
      Alcotest.(check int) "0x00" 0 a;
      Alcotest.(check int) "0x7F" 127 b;
      Alcotest.(check int) "0xFF" 255 c
  | Error _ ->
      Alcotest.fail "Expected success"

let test_be_any_int8 () =
  match
    Parseff.parse "\x00\x7F\xFF\x80" (fun () ->
        let a = Parseff.BE.any_int8 () in
        let b = Parseff.BE.any_int8 () in
        let c = Parseff.BE.any_int8 () in
        let d = Parseff.BE.any_int8 () in
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

let test_le_uint8_same_as_be () =
  match Parseff.parse "\xAB" (fun () -> Parseff.LE.any_uint8 ()) with
  | Ok v ->
      Alcotest.(check int) "single byte is endian-agnostic" 0xAB v
  | Error _ ->
      Alcotest.fail "Expected success"

let test_uint8_eof () =
  match Parseff.parse "" (fun () -> Parseff.BE.any_uint8 ()) with
  | Ok _ ->
      Alcotest.fail "Expected failure"
  | Error { error = `Unexpected_end_of_input; _ } ->
      ()
  | Error _ ->
      Alcotest.fail "Expected unexpected end of input"

(* --- 16-bit reads --- *)

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
  | Error { error = `Unexpected_end_of_input; _ } ->
      ()
  | Error _ ->
      Alcotest.fail "Expected unexpected end of input"

(* --- 32-bit reads --- *)

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
  | Error { error = `Unexpected_end_of_input; _ } ->
      ()
  | Error _ ->
      Alcotest.fail "Expected unexpected end of input"

(* --- 64-bit reads --- *)

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
  | Error { error = `Unexpected_end_of_input; _ } ->
      ()
  | Error _ ->
      Alcotest.fail "Expected unexpected end of input"

(* --- float / double --- *)

let test_be_any_float () =
  (* IEEE 754: 1.0f = 0x3F800000 *)
  match
    Parseff.parse "\x3F\x80\x00\x00" (fun () -> Parseff.BE.any_float ())
  with
  | Ok v ->
      Alcotest.(check (float 0.0)) "1.0" 1.0 v
  | Error _ ->
      Alcotest.fail "Expected success"

let test_le_any_float () =
  (* IEEE 754: 1.0f = 0x3F800000, little-endian: 00 00 80 3F *)
  match
    Parseff.parse "\x00\x00\x80\x3F" (fun () -> Parseff.LE.any_float ())
  with
  | Ok v ->
      Alcotest.(check (float 0.0)) "1.0" 1.0 v
  | Error _ ->
      Alcotest.fail "Expected success"

let test_be_any_double () =
  (* IEEE 754: 1.0 = 0x3FF0000000000000 *)
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
  (* IEEE 754: 1.0 = 0x3FF0000000000000, little-endian *)
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
  (* IEEE 754: -1.0f = 0xBF800000 *)
  match
    Parseff.parse "\xBF\x80\x00\x00" (fun () -> Parseff.BE.any_float ())
  with
  | Ok v ->
      Alcotest.(check (float 0.0)) "-1.0" (-1.0) v
  | Error _ ->
      Alcotest.fail "Expected success"

(* --- exact match --- *)

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
  | Error { error = `Expected _; _ } ->
      ()
  | Error _ ->
      Alcotest.fail "Expected Expected error"

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

(* --- take --- *)

let test_take_exact () =
  match
    Parseff.parse "abcdef" (fun () ->
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
    Parseff.parse "abc" (fun () ->
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
  | Error { error = `Unexpected_end_of_input; _ } ->
      ()
  | Error _ ->
      Alcotest.fail "Expected unexpected end of input"

let test_take_negative () =
  match Parseff.take (-1) with
  | exception Invalid_argument _ ->
      ()
  | _ ->
      Alcotest.fail "Expected Invalid_argument"

(* --- composition --- *)

let test_tlv_header () =
  (* TLV: magic "TL", version u8, count u16-be, one field: tag u8, len u16-be, payload *)
  let input = "TL\x01\x00\x01\x0A\x00\x03foo" in
  let parser () =
    let _ = Parseff.consume "TL" in
    let version = Parseff.BE.any_uint8 () in
    let count = Parseff.BE.any_uint16 () in
    let fields =
      Parseff.count count
        (fun () ->
          let tag = Parseff.BE.any_uint8 () in
          let len = Parseff.BE.any_uint16 () in
          let value = Parseff.take len in
          (tag, value)
        )
        ()
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

(* --- backtracking --- *)

let test_backtracking () =
  let input = "\x01\x02\x03\x04" in
  let parser () =
    Parseff.or_
      (fun () ->
        (* Try to match 0xFFFF — will fail *)
        Parseff.BE.int16 0xFFFF;
        "matched_first"
      )
      (fun () ->
        (* Should backtrack, re-read from position 0 *)
        let v = Parseff.BE.any_uint16 () in
        Printf.sprintf "got_%04X" v
      )
      ()
  in
  match Parseff.parse input parser with
  | Ok s ->
      Alcotest.(check string) "backtracked" "got_0102" s
  | Error _ ->
      Alcotest.fail "Expected success"

(* --- streaming --- *)

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
  match Parseff.parse_source src (fun () -> Parseff.BE.any_int32 ()) with
  | Ok v ->
      Alcotest.(check int32_testable) "0x01020304" 0x01020304l v
  | Error _ ->
      Alcotest.fail "Expected success"

let test_streaming_le_int64 () =
  let src = byte_at_a_time "\x01\x02\x03\x04\x05\x06\x07\x08" in
  match Parseff.parse_source src (fun () -> Parseff.LE.any_int64 ()) with
  | Ok v ->
      Alcotest.(check int64_testable) "0x0807060504030201" 0x0807060504030201L v
  | Error _ ->
      Alcotest.fail "Expected success"

let test_streaming_take () =
  let src = byte_at_a_time "hello world" in
  match Parseff.parse_source src (fun () -> Parseff.take 5) with
  | Ok v ->
      Alcotest.(check string) "taken" "hello" v
  | Error _ ->
      Alcotest.fail "Expected success"

let test_streaming_eof () =
  let src = byte_at_a_time "\x01" in
  match Parseff.parse_source src (fun () -> Parseff.BE.any_int16 ()) with
  | Ok _ ->
      Alcotest.fail "Expected failure"
  | Error { error = `Unexpected_end_of_input; _ } ->
      ()
  | Error _ ->
      Alcotest.fail "Expected unexpected end of input"

let test_streaming_composition () =
  let input = "TL\x01\x00\x01\x0A\x00\x03foo" in
  let src = byte_at_a_time input in
  let parser () =
    let _ = Parseff.consume "TL" in
    let version = Parseff.BE.any_uint8 () in
    let count = Parseff.BE.any_uint16 () in
    let fields =
      Parseff.count count
        (fun () ->
          let tag = Parseff.BE.any_uint8 () in
          let len = Parseff.BE.any_uint16 () in
          let value = Parseff.take len in
          (tag, value)
        )
        ()
    in
    Parseff.end_of_input ();
    (version, fields)
  in
  match Parseff.parse_source src parser with
  | Ok (version, fields) ->
      Alcotest.(check int) "version" 1 version;
      let tag, value = List.hd fields in
      Alcotest.(check int) "tag" 0x0A tag;
      Alcotest.(check string) "value" "foo" value
  | Error _ ->
      Alcotest.fail "Expected success"

(* --- runner --- *)

let () =
  Alcotest.run "binary"
    [
      ( "byte reads",
        [
          Alcotest.test_case "BE.any_uint8" `Quick test_be_any_uint8;
          Alcotest.test_case "BE.any_int8" `Quick test_be_any_int8;
          Alcotest.test_case "LE.any_uint8 same as BE" `Quick
            test_le_uint8_same_as_be;
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
        ]
      );
      ( "take",
        [
          Alcotest.test_case "take exact" `Quick test_take_exact;
          Alcotest.test_case "take zero" `Quick test_take_zero;
          Alcotest.test_case "take eof" `Quick test_take_eof;
          Alcotest.test_case "take negative" `Quick test_take_negative;
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
    ]
