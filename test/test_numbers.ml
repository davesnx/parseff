let chunked_source ~chunk_size input =
  let pos = ref 0 in
  Parseff.Source.of_function (fun buf off len ->
      let n = min (min chunk_size len) (String.length input - !pos) in
      Bytes.blit_string input !pos buf off n;
      pos := !pos + n;
      n
  )

let show_result show = function
  | Ok v ->
      "Ok " ^ show v
  | Error (e : Parseff.error) ->
      let kind = match e.kind with Expected _ -> "Expected" | _ -> "Other" in
      Printf.sprintf "Error@%d %s %s" e.range.start kind
        (Parseff.error_message e)

let check ?(chunk_sizes = [ 1; 2; 3 ]) show input parser expected =
  Alcotest.(check string)
    (input ^ " parse") expected
    (show_result show (Parseff.parse input parser));
  List.iter
    (fun chunk_size ->
      Alcotest.(check string)
        (Printf.sprintf "%s chunk %d" input chunk_size)
        expected
        (show_result show
           (Parseff.parse_source (chunked_source ~chunk_size input) parser)
        )
    )
    chunk_sizes

let int_rest () =
  let n = Parseff.int () in
  let rest = Parseff.take_while (fun _ -> true) in
  (n, rest)

let show_int_rest (n, rest) = Printf.sprintf "%d %S" n rest

let test_int_values () =
  check string_of_int "0" Parseff.int "Ok 0";
  check string_of_int "42" Parseff.int "Ok 42";
  check string_of_int "-17" Parseff.int "Ok -17";
  check string_of_int "007" Parseff.int "Ok 7";
  check string_of_int "-0" Parseff.int "Ok 0";
  check show_int_rest "12ab" int_rest {|Ok 12 "ab"|};
  check show_int_rest "3.5" int_rest {|Ok 3 ".5"|};
  check show_int_rest "1_000" int_rest {|Ok 1 "_000"|}

let test_int_failures () =
  check string_of_int "x" Parseff.int
    "Error@0 Expected expected integer, found 'x'";
  check string_of_int "" Parseff.int
    "Error@0 Expected expected integer, found end of input";
  check string_of_int "+1" Parseff.int
    "Error@0 Expected expected integer, found '+'";
  check string_of_int "-x" Parseff.int
    "Error@1 Expected expected digit, found 'x'";
  check string_of_int "-" Parseff.int
    "Error@1 Expected expected digit, found end of input"

let max_text = string_of_int max_int
let min_text = string_of_int min_int

let bump s =
  let b = Bytes.of_string s in
  let last = Bytes.length b - 1 in
  Bytes.set b last (Char.chr (Char.code (Bytes.get b last) + 1));
  Bytes.to_string b

let test_int_range () =
  check string_of_int max_text Parseff.int ("Ok " ^ max_text);
  check string_of_int min_text Parseff.int ("Ok " ^ min_text);
  check string_of_int (bump max_text) Parseff.int
    ("Error@0 Expected expected integer in int range, found '" ^ bump max_text
   ^ "'"
    );
  check string_of_int (bump min_text) Parseff.int
    "Error@0 Expected expected integer in int range, found '-'";
  check string_of_int (max_text ^ "0") Parseff.int
    ("Error@0 Expected expected integer in int range, found '" ^ max_text ^ "0'");
  check ~chunk_sizes:[] string_of_int
    ("1" ^ String.make 40 '0')
    Parseff.int
    "Error@0 Expected expected integer in int range, found \
     '10000000000000000000000000000000'..."

let is_digit c = c >= '0' && c <= '9'

let int_or_text () =
  Parseff.or_
    (fun () -> string_of_int (Parseff.int ()))
    (fun () -> "text " ^ Parseff.take_while ~at_least:1 is_digit)
    ()

let int_or_null () =
  Parseff.or_
    (fun () -> Some (Parseff.int ()))
    (fun () ->
      Parseff.consume "null" |> ignore;
      None
    )
    ()

let show_option = function None -> "None" | Some n -> string_of_int n
let show_list l = String.concat ";" (List.map string_of_int l)

let test_int_composition () =
  check Fun.id (bump max_text) int_or_text ("Ok text " ^ bump max_text);
  check Fun.id "12" int_or_text "Ok 12";
  check show_option "x" int_or_null
    {|Error@0 Expected expected integer or "null", found 'x'|};
  check show_list "1,-2,30"
    (Parseff.sep_by Parseff.int (fun () -> Parseff.char ','))
    "Ok 1;-2;30";
  check show_list "1,2,"
    (Parseff.sep_by Parseff.int (fun () -> Parseff.char ','))
    "Error@4 Expected expected integer, found end of input";
  check
    (fun (a, b) -> Printf.sprintf "%d %d" a b)
    "42"
    (fun () ->
      let a = Parseff.look_ahead Parseff.int in
      (a, Parseff.int ())
    )
    "Ok 42 42"

let hex_rest () =
  let n = Parseff.hex_int () in
  let rest = Parseff.take_while (fun _ -> true) in
  (n, rest)

let test_hex_int () =
  check string_of_int "0" Parseff.hex_int "Ok 0";
  check string_of_int "ff" Parseff.hex_int "Ok 255";
  if Sys.int_size = 63 then
    check string_of_int "DeadBeef" Parseff.hex_int "Ok 3735928559";
  check string_of_int "00A" Parseff.hex_int "Ok 10";
  check string_of_int
    (Printf.sprintf "%X" max_int)
    Parseff.hex_int ("Ok " ^ max_text);
  check show_int_rest "1fg" hex_rest {|Ok 31 "g"|};
  check show_int_rest "0x1f" hex_rest {|Ok 0 "x1f"|};
  check string_of_int "g" Parseff.hex_int
    "Error@0 Expected expected hexadecimal integer, found 'g'";
  check string_of_int "-1" Parseff.hex_int
    "Error@0 Expected expected hexadecimal integer, found '-'";
  check string_of_int "" Parseff.hex_int
    "Error@0 Expected expected hexadecimal integer, found end of input";
  check string_of_int
    (Printf.sprintf "%X" min_int)
    Parseff.hex_int
    (Printf.sprintf
       "Error@0 Expected expected hexadecimal integer in int range, found '%X'"
       min_int
    );
  check string_of_int "00000000000000001" Parseff.hex_int "Ok 1"

let show_float = Printf.sprintf "%.17g"

let float_rest () =
  let x = Parseff.float () in
  let rest = Parseff.take_while (fun _ -> true) in
  (x, rest)

let show_float_rest (x, rest) = Printf.sprintf "%s %S" (show_float x) rest

let test_float_values () =
  check show_float "0" Parseff.float "Ok 0";
  check show_float "42" Parseff.float "Ok 42";
  check show_float "-0" Parseff.float "Ok -0";
  check show_float "3.25" Parseff.float "Ok 3.25";
  check show_float "-0.5" Parseff.float "Ok -0.5";
  check show_float "0.1" Parseff.float "Ok 0.10000000000000001";
  check show_float "1e3" Parseff.float "Ok 1000";
  check show_float "2.5E-3" Parseff.float "Ok 0.0025000000000000001";
  check show_float "1e+2" Parseff.float "Ok 100";
  check show_float "123456.789e-3" Parseff.float "Ok 123.456789";
  check show_float "1e400" Parseff.float "Ok inf";
  check show_float "-1e400" Parseff.float "Ok -inf";
  check show_float "1e-400" Parseff.float "Ok 0";
  check show_float_rest "1.5x" float_rest {|Ok 1.5 "x"|};
  check show_float_rest "1_0" float_rest {|Ok 1 "_0"|};
  check show_float_rest "2,3" float_rest {|Ok 2 ",3"|}

let test_float_failures () =
  check show_float "x" Parseff.float
    "Error@0 Expected expected number, found 'x'";
  check show_float "" Parseff.float
    "Error@0 Expected expected number, found end of input";
  check show_float ".5" Parseff.float
    "Error@0 Expected expected number, found '.'";
  check show_float "+1" Parseff.float
    "Error@0 Expected expected number, found '+'";
  check ~chunk_sizes:[] show_float "inf" Parseff.float
    "Error@0 Expected expected number, found 'inf'";
  check show_float "-" Parseff.float
    "Error@1 Expected expected digit, found end of input";
  check show_float "1." Parseff.float
    "Error@2 Expected expected digit, found end of input";
  check ~chunk_sizes:[] show_float "1.e5" Parseff.float
    "Error@2 Expected expected digit, found 'e5'";
  check show_float "1e" Parseff.float
    "Error@2 Expected expected digit, found end of input";
  check show_float "1e+" Parseff.float
    "Error@3 Expected expected digit, found end of input";
  check show_float "1ex" Parseff.float
    "Error@2 Expected expected digit, found 'x'"

let float_or_null () =
  Parseff.or_
    (fun () -> Some (Parseff.float ()))
    (fun () ->
      Parseff.consume "null" |> ignore;
      None
    )
    ()

let test_float_composition () =
  check
    (function None -> "None" | Some x -> show_float x)
    "x" float_or_null {|Error@0 Expected expected number or "null", found 'x'|};
  check
    (fun l -> String.concat ";" (List.map show_float l))
    "1.5,-2e1,3"
    (Parseff.sep_by Parseff.float (fun () -> Parseff.char ','))
    "Ok 1.5;-20;3"

let test_prefixed_hex () =
  check string_of_int "0xff"
    (fun () ->
      Parseff.consume "0x" |> ignore;
      Parseff.hex_int ()
    )
    "Ok 255";
  check ~chunk_sizes:[]
    (fun (x, y) -> Printf.sprintf "%d %d" x y)
    "99999999999999999999,0"
    (fun () ->
      let x = Parseff.int () in
      let _ = Parseff.char ',' in
      (x, Parseff.int ())
    )
    "Error@0 Expected expected integer in int range, found \
     '99999999999999999999'"

let words_per_call f =
  let iterations = 100_000 in
  for _ = 1 to 1_000 do
    ignore (Sys.opaque_identity (f ()))
  done;
  Gc.full_major ();
  let before = Gc.minor_words () in
  for _ = 1 to iterations do
    ignore (Sys.opaque_identity (f ()))
  done;
  (Gc.minor_words () -. before) /. Float.of_int iterations

let words_in_parse input f =
  match
    Parseff.parse ~consume:`Partial input (fun () ->
        let baseline =
          words_per_call (fun () ->
              Parseff.look_ahead (fun () -> Char.code (Parseff.any_char ()))
          )
        in
        words_per_call (fun () -> Parseff.look_ahead f) -. baseline
    )
  with
  | Ok words ->
      words
  | Error e ->
      Alcotest.fail (Parseff.render_error e)

let test_no_allocation () =
  List.iter
    (fun (name, input, parser) ->
      let words = words_in_parse input parser in
      if words > 0. then
        Alcotest.failf "%s allocated %.3f minor words per call" name words
    )
    [
      ("int", min_text, Parseff.int);
      ("hex_int", Printf.sprintf "%x" max_int, Parseff.hex_int);
    ]

let windowed ~window input parser =
  match
    Parseff.parse_source ~backtrack_window:window
      (chunked_source ~chunk_size:3 input)
      parser
  with
  | Ok v ->
      "Ok " ^ v
  | Error ({ kind = Backtrack_window_exceeded _; _ } as e) ->
      Printf.sprintf "Window@%d" e.range.start
  | Error e ->
      Printf.sprintf "Error@%d %s" e.range.start (Parseff.error_message e)

let spaced p () =
  Parseff.many
    (fun () ->
      let v = p () in
      let _ = Parseff.char ' ' in
      v
    )
    ()

let ints p () = String.concat ";" (List.map string_of_int (spaced p ()))
let floats () = String.concat ";" (List.map show_float (spaced Parseff.float ()))
let digits () = Parseff.take_while is_digit

let after_prefix p () =
  Parseff.consume "ab" |> ignore;
  p ()

let top p show () =
  Parseff.skip_while (fun c -> c >= 'a' && c <= 'h');
  show (p ())

let test_window_values () =
  let cases =
    [
      (2, "1 2 3 ", ints Parseff.int, "Ok 1;2;3");
      (4, "12 -3 456 7 ", ints Parseff.int, "Ok 12;-3;456;7");
      (8, "1234567 -123456 0 ", ints Parseff.int, "Ok 1234567;-123456;0");
      (4, "ff a 1F0 ", ints Parseff.hex_int, "Ok 255;10;496");
      (8, "DeadBee 0 ", ints Parseff.hex_int, "Ok 233495534;0");
      (4, "1.5 -2 3e1 ", floats, "Ok 1.5;-2;30");
      (8, "-1.5e-3 0.25 ", floats, "Ok -0.0015;0.25");
      (2, "abcdef7", top Parseff.int string_of_int, "Ok 7");
      (4, "abcdef-12", top Parseff.int string_of_int, "Ok -12");
      (4, "abcdefFF", top Parseff.hex_int string_of_int, "Ok 255");
      (2, "abcdef5", top Parseff.float show_float, "Ok 5");
      (4, "abcdef1.5", top Parseff.float show_float, "Ok 1.5");
      (8, "abcdefgh-1.25e2", top Parseff.float show_float, "Ok -125");
    ]
  in
  List.iter
    (fun (window, input, parser, expected) ->
      Alcotest.(check string)
        (Printf.sprintf "%S window %d" input window)
        expected
        (windowed ~window input parser)
    )
    cases

let test_window_exceeded () =
  let cases =
    [
      (4, "ab1234567", digits, "Window@2");
      (4, "ab1234567", (fun () -> string_of_int (Parseff.int ())), "Window@2");
      (4, "ab-123456", (fun () -> string_of_int (Parseff.int ())), "Window@2");
      ( 4,
        "abfffffff",
        (fun () -> string_of_int (Parseff.hex_int ())),
        "Window@2"
      );
      (4, "ab1.23456", (fun () -> show_float (Parseff.float ())), "Window@2");
      (8, "ab1e+12345678", (fun () -> show_float (Parseff.float ())), "Window@2");
      (4, "ab123 ", ints Parseff.int, "Ok 123");
      (4, "ab1234 ", ints Parseff.int, "Window@2");
      (8, "ab-123456 ", ints Parseff.int, "Ok -123456");
      (8, "ab-1234567 ", ints Parseff.int, "Window@2");
      (8, "abfffffff ", ints Parseff.hex_int, "Ok 268435455");
      (8, "abffffffff ", ints Parseff.hex_int, "Window@2");
      (8, "ab1.234e5 ", floats, "Ok 123400");
      (8, "ab1.2345e6 ", floats, "Window@2");
    ]
  in
  List.iter
    (fun (window, input, parser, expected) ->
      Alcotest.(check string)
        (Printf.sprintf "%S window %d" input window)
        expected
        (windowed ~window input (after_prefix parser))
    )
    cases

let () =
  Alcotest.run "Numbers"
    [
      ( "int",
        [
          Alcotest.test_case "values" `Quick test_int_values;
          Alcotest.test_case "failures" `Quick test_int_failures;
          Alcotest.test_case "range" `Quick test_int_range;
          Alcotest.test_case "composition" `Quick test_int_composition;
        ]
      );
      ( "hex_int",
        [
          Alcotest.test_case "values" `Quick test_hex_int;
          Alcotest.test_case "prefix and pairs" `Quick test_prefixed_hex;
        ]
      );
      ( "float",
        [
          Alcotest.test_case "values" `Quick test_float_values;
          Alcotest.test_case "failures" `Quick test_float_failures;
          Alcotest.test_case "composition" `Quick test_float_composition;
        ]
      );
      ( "backtrack window",
        [
          Alcotest.test_case "values" `Quick test_window_values;
          Alcotest.test_case "exceeded" `Quick test_window_exceeded;
        ]
      );
      ( "allocation",
        [ Alcotest.test_case "int and hex_int" `Quick test_no_allocation ]
      );
    ]
