let chunked_source ~chunk_size input =
  let pos = ref 0 in
  Parseff.Source.of_function (fun buf off len ->
      let n = min (min chunk_size len) (String.length input - !pos) in
      Bytes.blit_string input !pos buf off n;
      pos := !pos + n;
      n
  )

let show_error (e : Parseff.error) =
  let kind =
    match e.kind with
    | Backtrack_window_exceeded { limit; oldest } ->
        Printf.sprintf "window limit %d oldest %d" limit oldest
    | _ ->
        Parseff.error_message e
  in
  Printf.sprintf "Error %d-%d %s" e.range.start e.range.stop kind

let show show_v = function Ok v -> "Ok " ^ show_v v | Error e -> show_error e

let check_all ?(consume = `All) ?(chunk_sizes = [ 1; 2; 4096 ]) name show_v
    input parser expected =
  Alcotest.(check string)
    (name ^ " parse") expected
    (show show_v (Parseff.parse ~consume input parser));
  List.iter
    (fun chunk_size ->
      Alcotest.(check string)
        (Printf.sprintf "%s chunk %d" name chunk_size)
        expected
        (show show_v
           (Parseff.parse_source ~consume
              (chunked_source ~chunk_size input)
              parser
           )
        )
    )
    chunk_sizes

let show_range (r : Parseff.range) = Printf.sprintf "%d-%d" r.start r.stop

let show_with_range show_v (v, r) =
  Printf.sprintf "%s %s" (show_v v) (show_range r)

let is_digit c = c >= '0' && c <= '9'

let number () =
  int_of_string (Parseff.take_while ~at_least:1 is_digit ~label:"digit")

let test_with_range_of_composed_parser () =
  check_all "with_range"
    (show_with_range string_of_int)
    "x = 42;"
    (fun () ->
      ignore (Parseff.consume "x = ");
      let v = Parseff.with_range number in
      ignore (Parseff.char ';');
      v
    )
    "Ok 42 4-6"

let key_value () =
  ignore
    (Parseff.take_while ~at_least:1 (fun c -> c >= 'a' && c <= 'z') ~label:"key");
  ignore (Parseff.char '=');
  number ()

let test_recognize_returns_consumed_text () =
  check_all "recognize" Fun.id "a=1,bc=23;" ~consume:`Partial
    (fun () ->
      let first = Parseff.recognize key_value in
      ignore (Parseff.char ',');
      let second = Parseff.recognize key_value in
      first ^ "|" ^ second
    )
    "Ok a=1|bc=23"

let test_recognize_span_returns_consumed_text () =
  check_all "recognize_span" Fun.id "a=1,bc=23;" ~consume:`Partial
    (fun () ->
      ignore (Parseff.recognize_span key_value);
      ignore (Parseff.char ',');
      Parseff.span_to_string (Parseff.recognize_span key_value)
    )
    "Ok bc=23"

let test_recognize_span_is_zero_copy_in_parse () =
  let input = "ab=12" in
  match Parseff.parse input (fun () -> Parseff.recognize_span key_value) with
  | Ok s ->
      Alcotest.(check bool) "same buffer" true (s.buf == input);
      Alcotest.(check (pair int int)) "off len" (0, 5) (s.off, s.len)
  | Error e ->
      Alcotest.fail (show_error e)

let test_recognize_longer_than_window_fails () =
  let input = String.make 20 'a' ^ ";" in
  let run parser =
    show Fun.id
      (Parseff.parse_source ~consume:`Partial ~backtrack_window:8
         (chunked_source ~chunk_size:1 input)
         parser
      )
  in
  Alcotest.(check string)
    "unwrapped" "Ok 20"
    (run (fun () ->
         string_of_int
           (List.length (Parseff.many (fun () -> Parseff.char 'a') ()))
     )
    );
  Alcotest.(check string)
    "recognize" "Error 8-8 window limit 8 oldest 0"
    (run (fun () ->
         Parseff.recognize (Parseff.many (fun () -> Parseff.char 'a'))
     )
    )

let test_recognize_tokens_in_small_window () =
  let input =
    String.concat "," (List.init 50 (fun i -> Printf.sprintf "k=%d" i))
  in
  match
    Parseff.parse_source ~backtrack_window:8
      (chunked_source ~chunk_size:1 input)
      (Parseff.sep_by
         (fun () -> Parseff.recognize key_value)
         (fun () -> Parseff.char ',')
      )
  with
  | Ok l ->
      Alcotest.(check int) "count" 50 (List.length l);
      Alcotest.(check string) "last" "k=49" (List.nth l 49)
  | Error e ->
      Alcotest.fail (show_error e)

let ab_committed () =
  ignore (Parseff.char 'a');
  Parseff.commit ();
  ignore (Parseff.char 'b')

let test_commit_inside_recognize () =
  check_all "commit" Fun.id "ac" ~consume:`Partial
    (Parseff.or_ (fun () -> Parseff.recognize ab_committed) (fun () -> "alt"))
    "Error 1-2 expected 'b', found 'c'";
  check_all "no commit" Fun.id "ac" ~consume:`Partial
    (Parseff.or_
       (fun () ->
         Parseff.recognize (fun () ->
             ignore (Parseff.char 'a');
             ignore (Parseff.char 'b')
         )
       )
       (fun () -> "alt")
    )
    "Ok alt"

let statement () =
  Parseff.context "statement" (fun () ->
      Parseff.recognize (fun () ->
          ignore (Parseff.consume "let");
          Parseff.cut ();
          ignore (Parseff.char ';')
      )
  )

let test_cut_inside_recognize () =
  check_all "cut" (String.concat ",") "let;let;letx" ~consume:`Partial
    (Parseff.many statement) "Error 11-12 expected ';', found 'x'";
  check_all "cut reaches or_ outside" Fun.id "ac" ~consume:`Partial
    (fun () ->
      Parseff.context "c" (fun () ->
          Parseff.or_
            (fun () ->
              Parseff.recognize (fun () ->
                  ignore (Parseff.char 'a');
                  Parseff.cut ();
                  ignore (Parseff.char 'b')
              )
            )
            (fun () -> "alt")
            ()
      )
    )
    "Error 1-2 expected 'b', found 'c'"

let alternatives wrap () =
  Parseff.or_
    (fun () -> wrap (fun () -> Parseff.consume "abc"))
    (fun () -> wrap (fun () -> Parseff.consume "abd"))
    ()

let test_failure_is_unchanged () =
  let expected = "Error 2-3 expected \"c\" or \"d\", found 'x'" in
  check_all "unwrapped" Fun.id "abx" (alternatives (fun p -> p ())) expected;
  check_all "recognize" Fun.id "abx" (alternatives Parseff.recognize) expected;
  check_all "with_range" Fun.id "abx"
    (alternatives (fun p -> fst (Parseff.with_range p)))
    expected;
  check_all "recognize_span" Fun.id "abx"
    (alternatives (fun p -> Parseff.span_to_string (Parseff.recognize_span p)))
    expected

let test_inside_look_ahead_and_many () =
  check_all "look_ahead" Fun.id "ab=12;" ~consume:`Partial
    (fun () ->
      let peeked = Parseff.look_ahead (fun () -> Parseff.recognize key_value) in
      let r = Parseff.with_range key_value in
      Printf.sprintf "%s %s" peeked (show_range (snd r))
    )
    "Ok ab=12 0-5";
  check_all "many" (String.concat ",") "a=1;bb=22;c=" ~consume:`Partial
    (Parseff.many (fun () ->
         let s = Parseff.recognize key_value in
         ignore (Parseff.char ';');
         s
     )
    )
    "Ok a=1,bb=22";
  check_all "many with_range" (String.concat ",") "1;22;333" ~consume:`Partial
    (Parseff.sep_by
       (fun () -> show_range (snd (Parseff.with_range number)))
       (fun () -> Parseff.char ';')
    )
    "Ok 0-1,2-4,5-8"

let () =
  let open Alcotest in
  run "Range"
    [
      ( "with_range",
        [
          test_case "composed parser" `Quick test_with_range_of_composed_parser;
        ]
      );
      ( "recognize",
        [
          test_case "consumed text" `Quick test_recognize_returns_consumed_text;
          test_case "span text" `Quick test_recognize_span_returns_consumed_text;
          test_case "span zero-copy in parse" `Quick
            test_recognize_span_is_zero_copy_in_parse;
        ]
      );
      ( "streaming",
        [
          test_case "longer than window" `Quick
            test_recognize_longer_than_window_fails;
          test_case "tokens in small window" `Quick
            test_recognize_tokens_in_small_window;
        ]
      );
      ( "commit and cut",
        [
          test_case "commit inside recognize" `Quick
            test_commit_inside_recognize;
          test_case "cut inside recognize" `Quick test_cut_inside_recognize;
        ]
      );
      ( "composition",
        [
          test_case "failure unchanged" `Quick test_failure_is_unchanged;
          test_case "look_ahead and many" `Quick test_inside_look_ahead_and_many;
        ]
      );
    ]
