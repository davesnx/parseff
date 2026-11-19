[@@@alert "-unstable"]

type outcome = Value of string | Failed of int * string

let pp_outcome ppf = function
  | Value s ->
      Format.fprintf ppf "Ok %S" s
  | Failed (pos, msg) ->
      Format.fprintf ppf "Error at %d: %s" pos msg

let outcome = Alcotest.testable pp_outcome ( = )

let of_result = function
  | Ok s ->
      Value s
  | Error (e : Parseff.error) ->
      Failed (e.range.start, Parseff.error_message e)

let function_source ~size input =
  let pos = ref 0 in
  Parseff.Source.of_function ?initial_buffer_size:Buf_size_env.buf_size
    (fun buf off len ->
      let n = min (min size len) (String.length input - !pos) in
      Bytes.blit_string input !pos buf off n;
      pos := !pos + n;
      n
  )

let chunks_source ~size input =
  let pos = ref 0 in
  Parseff.Source.of_chunks (fun () ->
      let n = min size (String.length input - !pos) in
      if n = 0 then
        None
      else begin
        let s = String.sub input !pos n in
        pos := !pos + n;
        Some s
      end
  )

let bigstring_source ~size input =
  let buf = Bigarray.Array1.create Bigarray.char Bigarray.c_layout size in
  let pos = ref 0 in
  Parseff.Source.of_bigstring_chunks (fun () ->
      let n = min size (String.length input - !pos) in
      if n = 0 then
        None
      else begin
        for i = 0 to n - 1 do
          buf.{i} <- input.[!pos + i]
        done;
        pos := !pos + n;
        Some (buf, 0, n)
      end
  )

let feed_bytes ?backtrack_window input parser =
  let t =
    Parseff.parse_partial ?backtrack_window (fun () ->
        let v = parser () in
        Parseff.end_of_input ();
        v
    )
  in
  let rec go i =
    if i = String.length input then
      Parseff.finish t
    else
      match Parseff.feed t (String.make 1 input.[i]) with
      | Parseff.Await ->
          go (i + 1)
      | Parseff.Done r ->
          r
  in
  Result.map fst (go 0)

let stream_results ?backtrack_window input parser =
  [
    ( "of_function",
      Parseff.parse_source ?backtrack_window
        (function_source ~size:1 input)
        parser
    );
    ( "of_chunks",
      Parseff.parse_source ?backtrack_window
        (chunks_source ~size:1 input)
        parser
    );
    ( "of_bigstring_chunks",
      Parseff.parse_source ?backtrack_window
        (bigstring_source ~size:1 input)
        parser
    );
    ("feed", feed_bytes ?backtrack_window input parser);
  ]

let check name expected input parser =
  let from_string = of_result (Parseff.parse input parser) in
  Alcotest.check outcome (name ^ " (string)") expected from_string;
  List.iter
    (fun (source, r) ->
      Alcotest.check outcome
        (Printf.sprintf "%s (%s, 1-byte chunks)" name source)
        from_string (of_result r)
    )
    (stream_results input parser)

let comment_take ?at_most ?label () =
  Parseff.literal "/*";
  let body = Parseff.take_until ?at_most ?label "*/" in
  Parseff.literal "*/";
  body

let comment_skip ?at_most ?label () =
  Parseff.literal "/*";
  let start = Parseff.position () in
  Parseff.skip_until ?at_most ?label "*/";
  let stop = Parseff.position () in
  Parseff.literal "*/";
  Printf.sprintf "%d-%d" start stop

let test_block_comment () =
  let cases =
    [
      ("/* a * b */", Value " a * b ", Value "2-9");
      ("/***/", Value "*", Value "2-3");
      ("/* **/", Value " *", Value "2-4");
      ("/**/", Value "", Value "2-2");
      ( "/* a * b",
        Failed (8, "expected \"*/\", found end of input"),
        Failed (8, "expected \"*/\", found end of input")
      );
      ( "/* a *",
        Failed (6, "expected \"*/\", found end of input"),
        Failed (6, "expected \"*/\", found end of input")
      );
      ( "/*",
        Failed (2, "expected \"*/\", found end of input"),
        Failed (2, "expected \"*/\", found end of input")
      );
    ]
  in
  List.iter
    (fun (input, taken, skipped) ->
      check ("take_until " ^ input) taken input comment_take;
      check ("skip_until " ^ input) skipped input comment_skip
    )
    cases

let test_html_comment () =
  let input = "<!-- x -- y --->" in
  check "take_until -->" (Value " x -- y -") input (fun () ->
      Parseff.literal "<!--";
      let body = Parseff.take_until "-->" in
      Parseff.literal "-->";
      body
  );
  check "skip_until -->" (Value "13") input (fun () ->
      Parseff.literal "<!--";
      Parseff.skip_until "-->";
      let at = Parseff.position () in
      Parseff.literal "-->";
      string_of_int at
  );
  check "unterminated -->"
    (Failed (14, "expected \"-->\", found end of input"))
    "<!-- x -- y --"
    (fun () ->
      Parseff.literal "<!--";
      Parseff.skip_until "-->";
      Parseff.literal "-->";
      Parseff.take_while (fun _ -> true)
    )

let test_label () =
  let expected = Failed (8, "expected end of comment, found end of input") in
  check "take_until ~label" expected "/* a * b"
    (comment_take ~label:"end of comment");
  check "skip_until ~label" expected "/* a * b"
    (comment_skip ~label:"end of comment")

let test_terminator_at_start () =
  check "take_until at offset 0" (Value "") "*/" (fun () ->
      let s = Parseff.take_until "*/" in
      Parseff.literal "*/";
      s
  );
  check "skip_until at offset 0" (Value "0") "*/" (fun () ->
      Parseff.skip_until "*/";
      let at = Parseff.position () in
      Parseff.literal "*/";
      string_of_int at
  );
  check "does not consume the terminator" (Value "abc") "abc;" (fun () ->
      let s = Parseff.take_until ";" in
      ignore (Parseff.char ';');
      s
  )

let raises_invalid_arg name f =
  match f () with
  | exception Invalid_argument _ ->
      ()
  | _ ->
      Alcotest.failf "%s: expected Invalid_argument" name

let test_invalid_arguments () =
  raises_invalid_arg "take_until \"\"" (fun () ->
      Parseff.parse "abc" (fun () -> Parseff.take_until "")
  );
  raises_invalid_arg "skip_until \"\"" (fun () ->
      Parseff.parse "abc" (fun () -> Parseff.skip_until "")
  );
  raises_invalid_arg "take_until ~at_most:(-1)" (fun () ->
      Parseff.parse "abc" (fun () -> Parseff.take_until ~at_most:(-1) "c")
  );
  raises_invalid_arg "skip_until ~at_most:(-1)" (fun () ->
      Parseff.parse "abc" (fun () -> Parseff.skip_until ~at_most:(-1) "c")
  )

let check_limit name ~limit ~range input parser =
  let check_one source = function
    | Error
        { Parseff.kind = Parseff.Limit_exceeded { limit = l }; range = r; _ } ->
        Alcotest.(check int) (name ^ " limit " ^ source) limit l;
        Alcotest.(check (pair int int))
          (name ^ " range " ^ source)
          range (r.start, r.stop)
    | Error e ->
        Alcotest.failf "%s (%s): expected Limit_exceeded, got %s" name source
          (Parseff.error_message e)
    | Ok _ ->
        Alcotest.failf "%s (%s): expected Limit_exceeded, got Ok" name source
  in
  check_one "string" (Parseff.parse input parser);
  List.iter (fun (source, r) -> check_one source r) (stream_results input parser)

let test_at_most () =
  check "take_until at the limit" (Value " ab") "/* ab*/"
    (comment_take ~at_most:3);
  check "skip_until at the limit" (Value "2-5") "/* ab*/"
    (comment_skip ~at_most:3);
  check "take_until ~at_most:0" (Value "") "/**/" (comment_take ~at_most:0);
  check_limit "take_until over the limit" ~limit:3 ~range:(2, 6) "/* abc*/"
    (comment_take ~at_most:3);
  check_limit "skip_until over the limit" ~limit:3 ~range:(2, 6) "/* abc*/"
    (comment_skip ~at_most:3);
  check_limit "take_until ~at_most:0" ~limit:0 ~range:(2, 3) "/* */"
    (comment_take ~at_most:0);
  check "unterminated before the limit is decided"
    (Failed (6, "expected \"*/\", found end of input"))
    "/* abc" (comment_take ~at_most:3);
  check_limit "unterminated after the limit is decided" ~limit:3 ~range:(2, 6)
    "/* abcd" (comment_take ~at_most:3)

let test_inside_or () =
  let parser () =
    Parseff.literal "/*";
    Parseff.or_
      (fun () ->
        let s = Parseff.take_until "*/" in
        Parseff.literal "*/";
        s
      )
      (fun () -> "rest:" ^ Parseff.take_while (fun _ -> true))
  in
  check "or_ recovers from take_until" (Value "rest: a * b") "/* a * b" parser;
  check "or_ takes the first branch" (Value " a ") "/* a */" parser;
  let skip_parser () =
    Parseff.or_
      (fun () ->
        Parseff.skip_until "*/";
        Parseff.literal "*/";
        "comment"
      )
      (fun () -> Parseff.take_while (fun _ -> true))
  in
  check "or_ recovers from skip_until" (Value "a * b") "a * b" skip_parser;
  check "skip_until inside or_ succeeds" (Value "comment") "a * b */"
    skip_parser

let long_comment = "/*" ^ String.make 100_000 '*' ^ " x */"

let test_long_comment_window () =
  let skip () =
    Parseff.literal "/*";
    Parseff.skip_until "*/";
    Parseff.literal "*/";
    Parseff.position ()
  in
  Alcotest.(check (result int reject))
    "skip_until (string)"
    (Ok (String.length long_comment))
    (Result.map_error ignore (Parseff.parse long_comment skip));
  List.iter
    (fun (source, r) ->
      match r with
      | Ok n ->
          Alcotest.(check int)
            ("skip_until in a 4096 window, " ^ source)
            (String.length long_comment)
            n
      | Error e ->
          Alcotest.failf "skip_until in a 4096 window, %s: %s" source
            (Parseff.error_message e)
    )
    (stream_results ~backtrack_window:4096 long_comment skip);
  let take () =
    Parseff.literal "/*";
    let s = Parseff.take_until "*/" in
    Parseff.literal "*/";
    String.length s
  in
  Alcotest.(check (result int reject))
    "take_until (string)" (Ok 100_003)
    (Result.map_error ignore (Parseff.parse long_comment take));
  List.iter
    (fun (source, r) ->
      match r with
      | Error { Parseff.kind = Backtrack_window_exceeded { limit; _ }; _ } ->
          Alcotest.(check int) ("take_until window limit, " ^ source) 4096 limit
      | Error e ->
          Alcotest.failf
            "take_until, %s: expected Backtrack_window_exceeded, got %s" source
            (Parseff.error_message e)
      | Ok _ ->
          Alcotest.failf "take_until, %s: expected Backtrack_window_exceeded"
            source
    )
    (stream_results ~backtrack_window:4096 long_comment take)

let () =
  let open Alcotest in
  run "Until"
    [
      ( "take_until and skip_until",
        [
          test_case "block comment" `Quick test_block_comment;
          test_case "html comment" `Quick test_html_comment;
          test_case "label" `Quick test_label;
          test_case "terminator at start" `Quick test_terminator_at_start;
          test_case "invalid arguments" `Quick test_invalid_arguments;
          test_case "at_most" `Quick test_at_most;
          test_case "inside or_" `Quick test_inside_or;
          test_case "long comment in a small window" `Quick
            test_long_comment_window;
        ]
      );
    ]
