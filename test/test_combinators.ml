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

let sized_source size input =
  let pos = ref 0 in
  Parseff.Source.of_function (fun buf off len ->
      let n = min (min size len) (String.length input - !pos) in
      Bytes.blit_string input !pos buf off n;
      pos := !pos + n;
      n
  )

let chunked_source input = sized_source 1 input

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
    "parse_source, chunks of 2" expected
    (show show_string (Parseff.parse_source (sized_source 2 "no") p));
  Alcotest.(check string)
    "parse_source, chunks of 1"
    {|Error@0 Expected "expected \"false\" or \"true\", found 'n'"|}
    (show show_string (Parseff.parse_source (chunked_source "no") p))

let test_consume_each_length_and_offset () =
  for len = 1 to 10 do
    let literal = String.init len (fun i -> Char.chr (Char.code 'a' + i)) in
    let p () = Parseff.consume literal in
    let results input =
      [
        show show_string (Parseff.parse ~consume:`Partial input p);
        show show_string
          (Parseff.parse_source ~consume:`Partial
             (Parseff.Source.of_string input)
             p
          );
        show show_string
          (Parseff.parse_source ~consume:`Partial (sized_source 64 input) p);
      ]
    in
    let one_byte_chunks input =
      show show_string
        (Parseff.parse_source ~consume:`Partial (chunked_source input) p)
    in
    let ok = show show_string (Ok literal) in
    Alcotest.(check (list string))
      (Printf.sprintf "match %S" literal)
      [ ok; ok; ok ]
      (results (literal ^ "!"));
    Alcotest.(check string)
      (Printf.sprintf "match %S, chunks of 1" literal)
      ok
      (one_byte_chunks (literal ^ "!"));
    for k = 0 to len - 1 do
      let input =
        String.mapi
          (fun i c ->
            if i = k then
              'X'
            else
              c
          )
          literal
      in
      let first = Parseff.parse ~consume:`Partial (input ^ "!") p in
      let at = match first with Ok _ -> -1 | Error e -> e.range.start in
      Alcotest.(check int)
        (Printf.sprintf "%S on %S fails at the mismatch" literal input)
        k at;
      let rest = String.sub literal k (len - k) in
      let message found =
        Printf.sprintf {|Error@%d Expected "expected \"%s\", found '%s'"|} k
          rest found
      in
      let expected = message ("X" ^ String.sub literal (k + 1) (len - k - 1)) in
      Alcotest.(check (list string))
        (Printf.sprintf "%S on %S" literal input)
        [ expected; expected; expected ]
        (results (input ^ "!"));
      Alcotest.(check string)
        (Printf.sprintf "%S on %S, chunks of 1" literal input)
        (message "X")
        (one_byte_chunks (input ^ "!"))
    done
  done

exception Would_block

let blocking_source chunks =
  let reads = ref 0 in
  let rest = ref chunks in
  let source =
    Parseff.Source.of_function (fun buf off len ->
        incr reads;
        match !rest with
        | [] ->
            raise Would_block
        | chunk :: tail ->
            let n = String.length chunk in
            if n > len then Alcotest.fail "chunk larger than the read buffer";
            Bytes.blit_string chunk 0 buf off n;
            rest := tail;
            n
    )
  in
  (source, reads)

let check_stream name chunks parser expected expected_reads =
  let source, reads = blocking_source chunks in
  let result =
    match Parseff.parse_source ~consume:`Partial source parser with
    | r ->
        show show_string r
    | exception Would_block ->
        "Would_block"
  in
  Alcotest.(check string) name expected result;
  Alcotest.(check int) (name ^ ": reads") expected_reads !reads

let test_consume_stream_mismatch_reads_no_more () =
  check_stream "HTTP/ on a request line without version" [ "GET / \r\n\r\n" ]
    (fun () ->
      ignore (Parseff.consume "GET / ");
      Parseff.consume "HTTP/"
    )
    {|Error@6 Expected "expected \"HTTP/\", found '\\r'"|} 1;
  check_stream "CRLF on a lone carriage return" [ "\rX" ]
    (fun () -> Parseff.consume "\r\n")
    {|Error@1 Expected "expected \"\\n\", found 'X'"|} 1;
  check_stream "CRLF CRLF on a lone carriage return" [ "\rX" ]
    (fun () -> Parseff.consume "\r\n\r\n")
    {|Error@1 Expected "expected \"\\n\\r\\n\", found 'X'"|} 1

let test_consume_stream_matching_prefix_reads () =
  check_stream "prefix then the rest" [ "HT"; "TP/" ]
    (fun () -> Parseff.consume "HTTP/")
    {|Ok "HTTP/"|} 2;
  check_stream "prefix then end of input" [ "HT"; "" ]
    (fun () -> Parseff.consume "HTTP/")
    {|Error@2 Expected "expected \"TP/\", found end of input"|} 2

let test_consume_stream_or_ () =
  let p () =
    Parseff.or_
      (fun () -> Parseff.consume "ab")
      (fun () -> Parseff.consume "ac")
      ()
  in
  List.iter
    (fun (input, chunks) ->
      let source, _ = blocking_source chunks in
      Alcotest.(check string)
        ("or_ on " ^ input)
        (show show_string (Parseff.parse ~consume:`Partial input p))
        (show show_string (Parseff.parse_source ~consume:`Partial source p))
    )
    [ ("ac", [ "a"; "c" ]); ("ad", [ "a"; "d" ]); ("x", [ "x" ]) ]

let test_consume_streaming_slow_source () =
  let r =
    Parseff.parse_source (chunked_source "true") (fun () ->
        Parseff.consume "true"
    )
  in
  Alcotest.(check string) "slow source" "Ok \"true\"" (show show_string r)

let fold_digit () = Char.code (Parseff.digit ()) - Char.code '0'

let folds =
  [
    ( "fold_left",
      fun element op ->
        Parseff.fold_left element (fun () -> op ()) ~otherwise:(-1) ()
    );
    ( "fold_right",
      fun element op ->
        Parseff.fold_right element (fun () -> op ()) ~otherwise:(-1) ()
    );
  ]

let test_fold_otherwise_first_element () =
  List.iter
    (fun (_, fold) ->
      let plus () =
        ignore (Parseff.char '+');
        ( + )
      in
      both string_of_int "x+1" (fun () -> fold fold_digit plus) "Ok (-1, 0)";
      both string_of_int "" (fun () -> fold fold_digit plus) "Ok (-1, 0)";
      both string_of_int "1+2+x" (fun () -> fold fold_digit plus) "Ok (3, 3)"
    )
    folds

let test_fold_otherwise_commit () =
  List.iter
    (fun (_, fold) ->
      let committed () =
        ignore (Parseff.char '+');
        Parseff.commit ();
        ( + )
      in
      both string_of_int "1+2+x"
        (fun () -> fold fold_digit committed)
        "Error@4 Expected \"expected digit, found 'x'\"";
      let cutting () =
        ignore (Parseff.char '+');
        Parseff.cut ();
        ( + )
      in
      both string_of_int "1+2+x"
        (fun () -> Parseff.context "sum" (fun () -> fold fold_digit cutting))
        "Error@4 Expected \"expected digit, found 'x'\""
    )
    folds

let test_fold_otherwise_releases_input () =
  let input = "1" ^ String.concat "" (List.init 200 (fun _ -> "+1")) in
  List.iter
    (fun (name, fold) ->
      let plus () =
        ignore (Parseff.char '+');
        Parseff.commit ();
        ( + )
      in
      let r =
        Parseff.parse_source ~backtrack_window:16 (chunked_source input)
          (fun () -> fold fold_digit plus
        )
      in
      Alcotest.(check string)
        (name ^ ": a chain longer than the window")
        "Ok 201" (show string_of_int r)
    )
    folds;
  let plus () =
    ignore (Parseff.char '+');
    ( + )
  in
  let r =
    Parseff.parse_source ~backtrack_window:16 (chunked_source input) (fun () ->
        Parseff.fold_left fold_digit plus ~otherwise:(-1) ()
    )
  in
  Alcotest.(check string)
    "fold_left without commit: a chain longer than the window" "Ok 201"
    (show string_of_int r)

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
      ( "fold ~otherwise",
        [
          test_case "first element" `Quick test_fold_otherwise_first_element;
          test_case "failure after commit or cut" `Quick
            test_fold_otherwise_commit;
          test_case "stream window" `Quick test_fold_otherwise_releases_input;
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
          test_case "stream mismatch reads no more" `Quick
            test_consume_stream_mismatch_reads_no_more;
          test_case "stream matching prefix reads" `Quick
            test_consume_stream_matching_prefix_reads;
          test_case "stream or_" `Quick test_consume_stream_or_;
          test_case "each length and offset" `Quick
            test_consume_each_length_and_offset;
          test_case "slow source" `Quick test_consume_streaming_slow_source;
        ]
      );
    ]
