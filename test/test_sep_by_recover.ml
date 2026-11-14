open Parseff

type v =
  | Int of int
  | Str of string
  | List of v list
  | Call of string * v list
  | Hole

let rec show = function
  | Int n ->
      string_of_int n
  | Str s ->
      Printf.sprintf "%S" s
  | List xs ->
      "[" ^ String.concat "; " (List.map show xs) ^ "]"
  | Call (f, xs) ->
      f ^ "(" ^ String.concat "; " (List.map show xs) ^ ")"
  | Hole ->
      "_"

let show_result = function
  | Ok v ->
      "Ok " ^ show v
  | Error e ->
      Printf.sprintf "Error at %d: %s" e.range.start (error_message e)

let errors d =
  List.map
    (fun { Diagnostics.value = e; _ } -> (e.range.start, error_message e))
    (Diagnostics.to_list d)

let errs = Alcotest.(list (pair int string))
let is_digit c = c >= '0' && c <= '9'
let is_lower c = c >= 'a' && c <= 'z'
let ws () = skip_whitespace ()

let string_literal () =
  ignore (char '"');
  let s = take_while (fun c -> c <> '"') in
  ignore (char '"');
  s

let pairs = [ ('(', ')'); ('[', ']') ]

let skip_value () =
  skip_nested
    ~atom:(fun () -> ignore (string_literal ()))
    pairs
    (fun c -> c = ',')

let comma () =
  ws ();
  ignore (char ',');
  ws ()

let rec value d () =
  match peek_char () with
  | Some '[' ->
      ignore (char '[');
      List (items d ']')
  | Some '"' ->
      Str (string_literal ())
  | Some c when is_lower c ->
      let name = take_while is_lower in
      ignore (char '(');
      Call (name, items d ')')
  | _ ->
      Int (int_of_string (take_while ~at_least:1 ~label:"integer" is_digit))

and items d close =
  ws ();
  let xs =
    if peek_char () = Some close then
      []
    else
      sep_by_recover d ~sync:skip_value (value d) comma (fun _ -> Hole) ()
  in
  ws ();
  ignore (char close);
  xs

let run input =
  let d = Diagnostics.create () in
  let r = parse ~diagnostics:d input (value d) in
  (show_result r, errors d)

let case input value errors =
  let r, es = run input in
  Alcotest.(check string) (input ^ ": value") value r;
  Alcotest.check errs (input ^ ": errors") errors es

let test_valid () =
  case "[]" "Ok []" [];
  case "[1]" "Ok [1]" [];
  case "f(1, [2, 3], \"a,b\")" "Ok f(1; [2; 3]; \"a,b\")" []

let test_holes () =
  case "[1, ?, 3, @]" "Ok [1; _; 3; _]"
    [ (4, "expected integer, found '?'"); (10, "expected integer, found '@'") ];
  case "[?]" "Ok [_]" [ (1, "expected integer, found '?'") ]

let test_empty_holes () =
  case "[1, , 3]" "Ok [1; _; 3]" [ (4, "expected integer, found ','") ];
  case "[1,]" "Ok [1; _]" [ (3, "expected integer, found ']'") ];
  case "[,]" "Ok [_; _]"
    [ (1, "expected integer, found ','"); (2, "expected integer, found ']'") ]

let test_skips_nested () =
  case "[1, [2, 3] 4, 5]" "Error at 11: expected ',' or ']', found '4'" [];
  case "[1, f(2, 3 4), 5]" "Ok [1; _; 5]"
    [ (11, "expected ')' or ',', found '4'") ]

let test_skip_whole_group () =
  case "[1, f(2, [3, 4]) ?, 5]" "Error at 17: expected ',' or ']', found '?'" [];
  case "[1, ?(2, [3, 4]), 5]" "Ok [1; _; 5]"
    [ (4, "expected integer, found '?'") ]

let test_mixed_kinds () =
  case "g(f(1, ?[2 ), 3)" "Ok g(f(1; _); 3)"
    [ (7, "expected integer, found '?'") ];
  case "[f(1, ?(2 ], 3]" "Error at 11: expected end of input, found ','"
    [ (10, "expected ')' or ',', found ']'") ]

let skip_one_counter () =
  let depth = ref 0 in
  let continue = ref true in
  while !continue do
    match peek_char () with
    | Some ('(' | '[') ->
        incr depth;
        ignore (any_char ())
    | Some (')' | ']') when !depth > 0 ->
        decr depth;
        ignore (any_char ())
    | Some (',' | ')' | ']') when !depth = 0 ->
        continue := false
    | Some _ ->
        ignore (any_char ())
    | None ->
        continue := false
  done

let test_one_counter_differs () =
  let rec value d () =
    match peek_char () with
    | Some c when is_lower c ->
        let name = take_while is_lower in
        ignore (char '(');
        let xs =
          sep_by_recover d ~sync:skip_one_counter (value d) comma
            (fun _ -> Hole)
            ()
        in
        ignore (char ')');
        Call (name, xs)
    | _ ->
        Int (int_of_string (take_while ~at_least:1 ~label:"integer" is_digit))
  in
  let d = Diagnostics.create () in
  let r = parse ~diagnostics:d "g(f(1, ?[2 ), 3)" (value d) in
  Alcotest.(check string)
    "value" "Error at 16: expected ')' or ',', found end of input"
    (show_result r)

let test_atom () =
  case "[1, ?\"a]b,c\"?, 3]" "Ok [1; _; 3]"
    [ (4, "expected integer, found '?'") ];
  case "[1, ?\")(\", 3]" "Ok [1; _; 3]" [ (4, "expected integer, found '?'") ]

let skip_only ?atom input =
  match
    parse ~consume:`Partial input (fun () ->
        skip_nested ?atom
          [ ('(', ')'); ('[', ']'); ('{', '}') ]
          (fun c -> c = ',');
        position ()
    )
  with
  | Ok p ->
      p
  | Error _ ->
      -1

let test_skip_nested () =
  let check name input expected =
    Alcotest.(check int) name expected (skip_only input)
  in
  check "separator" "ab, c" 2;
  check "end of input" "abc" 3;
  check "outer closer" "ab) c" 2;
  check "separator inside brackets" "f(a, b) x, c" 9;
  check "each kind has its own count" "[a ), b" 3;
  check "one counter would stop at 6" "([a) b, c], d" 6;
  check "closer of an outer kind closes the inner ones" "( [ { ) , x" 8;
  check "open bracket to the end" "(a, b" 5;
  check "multi-byte text" "h\xC3\xA9(\xE2\x82\xAC,), x" 9;
  Alcotest.(check int)
    "atom hides brackets and separators" 14
    (skip_only
       ~atom:(fun () -> ignore (string_literal ()))
       "a \"),(\" (\"]\") , b"
    );
  Alcotest.(check int)
    "atom that consumes nothing" 1
    (skip_only ~atom:(fun () -> ()) "a, b")

let test_atom_failure_forgotten () =
  let r =
    parse "ab," (fun () ->
        skip_nested
          ~atom:(fun () -> ignore (consume "abc"))
          []
          (fun c -> c = ',');
        char ';'
    )
  in
  Alcotest.(check string)
    "error" "Error at 2: expected ';', found ','"
    (match r with Ok _ -> "Ok" | Error e -> show_result (Error e))

let test_end_of_input () =
  case "[1, " "Error at 4: expected integer, found end of input" [];
  case "[1, [2" "Error at 6: expected ',' or ']', found end of input" [];
  case "f(1, g(" "Error at 7: expected integer, found end of input" []

let test_streaming () =
  let inputs =
    [
      "[1, ?, 3, @]";
      "[1, , 3]";
      "g(f(1, ?[2 ), 3)";
      "[1, ?\"a]b,c\"?, 3]";
      "[1, [2";
      "[1, ";
      "f(1, [2, 3], \"a,b\")";
    ]
  in
  List.iter
    (fun input ->
      let d = Diagnostics.create () in
      let r =
        parse_source ~diagnostics:d
          (Source.of_seq (Seq.map (String.make 1) (String.to_seq input)))
          (value d)
      in
      let expected, expected_errors = run input in
      Alcotest.(check string) (input ^ ": value") expected (show_result r);
      Alcotest.check errs (input ^ ": errors") expected_errors (errors d)
    )
    inputs

let test_same_as_sep_by () =
  let plain () = sep_by ~at_least:1 (fun () -> any_char ()) comma () in
  let d = Diagnostics.create () in
  let recovering () =
    sep_by_recover d ~sync:skip_value
      (fun () -> any_char ())
      comma
      (fun _ -> '?')
      ()
  in
  List.iter
    (fun input ->
      let a = parse ~consume:`Partial input plain in
      let b = parse ~consume:`Partial ~diagnostics:d input recovering in
      Alcotest.(check bool) (input ^ ": same") true (a = b)
    )
    [ ""; "a"; "a,b"; "a, b ,c"; "a,"; "a,b," ]

let test_wrong_collector () =
  let d = Diagnostics.create () in
  Alcotest.check_raises "raises"
    (Invalid_argument
       "Parseff.sep_by_recover: errors is not the collector given to the runner"
    ) (fun () ->
      ignore
        (parse "1" (fun () ->
             sep_by_recover d ~sync:skip_value (value d) comma
               (fun _ -> Hole)
               ()
         )
        )
  )

let test_rollback () =
  let d = Diagnostics.create () in
  let r =
    parse ~diagnostics:d "[1, ?, 3}"
      (or_ (value d) (fun () ->
           ignore (take_while (fun _ -> true));
           Hole
       )
      )
  in
  Alcotest.(check string) "value" "Ok _" (show_result r);
  Alcotest.check errs "errors of the abandoned branch are removed" [] (errors d)

let note ?from msg =
  match capture (fun () -> (fail ?from msg : unit)) with
  | Error e ->
      e
  | Ok () ->
      Alcotest.fail "expected a failure"

let issue_60_list ?record ?(source = false) input =
  let d = Diagnostics.create () in
  let element () =
    ws ();
    let start = position () in
    ignore (char '"');
    skip_while (fun c -> c <> '"' && c <> '\n');
    ( match peek_char () with
    | Some '"' ->
        ()
    | _ ->
        warn ~pos:start d (note ~from:start "unfinished string")
    );
    ignore (char '"');
    Str ""
  in
  let list () =
    ignore (char '[');
    let xs =
      sep_by_recover
        ?record:(Option.map (fun f -> f d) record)
        d
        ~sync:(fun () -> skip_nested [ ('[', ']') ] (fun c -> c = ','))
        element comma
        (fun _ -> Hole)
        ()
    in
    ws ();
    ignore (char ']');
    List xs
  in
  let r =
    if source then
      parse_source ~diagnostics:d
        (Source.of_seq (Seq.map (String.make 1) (String.to_seq input)))
        list
    else
      parse ~diagnostics:d input list
  in
  (show_result r, errors d)

let keep_inner d e inner =
  List.iter (fun (x : _ Diagnostics.entry) -> warn ~pos:x.pos d x.value) inner;
  if inner = [] then warn ~pos:e.range.start d e

let test_record () =
  let input = "[\"a\", \"bc\n, \"d\"]" in
  let check name expected actual =
    Alcotest.(check (pair string errs)) name expected actual
  in
  check "default records only the failure"
    ("Ok [\"\"; _; \"\"]", [ (9, "expected '\"', found '\\n'") ])
    (issue_60_list input);
  check "the documented record keeps the inner error"
    ("Ok [\"\"; _; \"\"]", [ (6, "unfinished string") ])
    (issue_60_list ~record:keep_inner input);
  check "parse_source, one byte per read"
    ("Ok [\"\"; _; \"\"]", [ (6, "unfinished string") ])
    (issue_60_list ~source:true ~record:keep_inner input);
  check "record replaces the default" ("Ok [\"\"; _; \"\"]", [])
    (issue_60_list ~record:(fun _ _ _ -> ()) input)

let test_record_arguments () =
  let d = Diagnostics.create () in
  let seen = ref [] in
  let element () =
    ignore (char 'a');
    warn d (note "first");
    ignore (char 'b');
    warn ~pos:0 d (note "second");
    ignore (char '!');
    Hole
  in
  let r =
    parse ~diagnostics:d ~consume:`Partial "x,ab?" (fun () ->
        ignore (char 'x');
        ignore (char ',');
        sep_by_recover d
          ~record:(fun e inner ->
            seen :=
              (e.range.start, error_message e)
              :: List.map
                   (fun (x : _ Diagnostics.entry) ->
                     (x.pos, error_message x.value)
                   )
                   inner
          )
          ~sync:(fun () -> skip_while (fun c -> c <> ','))
          element comma
          (fun _ -> Hole)
          ()
    )
  in
  Alcotest.(check string)
    "value" "Ok"
    (match r with Ok _ -> "Ok" | Error _ -> "Error");
  Alcotest.check errs "the failure, then the removed entries in order"
    [ (4, "expected '!', found '?'"); (3, "first"); (0, "second") ]
    !seen;
  Alcotest.check errs "nothing recorded" [] (errors d)

let test_outer_false () =
  let d = Diagnostics.create () in
  let run outer =
    let r =
      parse ~diagnostics:d "[ab, ax]" (fun () ->
          context ~delimited:true "list" (fun () ->
              ignore (char '[');
              let xs =
                sep_by_recover ?outer d ~sync:skip_value
                  (fun () ->
                    context "element" (fun () ->
                        ignore (char 'a');
                        ignore (char 'b');
                        Int 0
                    )
                  )
                  comma
                  (fun _ -> Hole)
                  ()
              in
              ignore (char ']');
              xs
          )
      )
    in
    ignore r;
    List.concat_map
      (fun { Diagnostics.value = (e : error); _ } ->
        List.map (fun (c : context) -> c.name) e.contexts
      )
      (Diagnostics.to_list d)
  in
  Alcotest.(check (list string)) "default" [ "list"; "element" ] (run None);
  Alcotest.(check (list string)) "~outer:false" [ "element" ] (run (Some false))

let separator () =
  skip_whitespace ();
  let _ = char ',' in
  skip_whitespace ();
  ignore (look_ahead (fun () -> satisfy (fun c -> c <> '}') ~label:"field"))

let table errors field () =
  let _ = char '{' in
  skip_whitespace ();
  let fields =
    sep_by_recover errors
      ~sync:(fun () -> skip_nested [ ('{', '}') ] (fun c -> c = ','))
      field separator
      (fun _ -> Hole)
      ()
  in
  skip_whitespace ();
  let _ = optional (fun () -> char ',') () in
  skip_whitespace ();
  let _ = char '}' in
  fields

let run_table input =
  let d = Diagnostics.create () in
  let integer () =
    Int (int_of_string (take_while ~at_least:1 ~label:"integer" is_digit))
  in
  let r = parse ~diagnostics:d input (fun () -> List (table d integer ())) in
  (show_result r, errors d)

let test_trailing_separator () =
  let check input expected =
    Alcotest.(check (pair string errs)) input expected (run_table input)
  in
  check "{1, 2,}" ("Ok [1; 2]", []);
  check "{1, 2, }" ("Ok [1; 2]", []);
  check "{1, 2 , }" ("Ok [1; 2]", []);
  check "{ 1, 2, }" ("Ok [1; 2]", []);
  check "{1, , 3}" ("Ok [1; _; 3]", [ (4, "expected integer, found ','") ])

let test_record_not_called () =
  let d = Diagnostics.create () in
  let calls = ref 0 in
  let run input =
    calls := 0;
    let r =
      parse ~diagnostics:d input (fun () ->
          ignore (char '[');
          let xs =
            sep_by_recover d
              ~record:(fun _ _ -> incr calls)
              ~sync:skip_value
              (fun () ->
                Int
                  (int_of_string
                     (take_while ~at_least:1 ~label:"integer" is_digit)
                  )
              )
              comma
              (fun _ -> Hole)
              ()
          in
          ignore (char ']');
          List xs
      )
    in
    (show_result r, !calls)
  in
  Alcotest.(check (pair string int)) "success" ("Ok [1; 2]", 0) (run "[1, 2]");
  Alcotest.(check (pair string int))
    "failure at the end of the input"
    ("Error at 4: expected integer, found end of input", 0)
    (run "[1, ")

let () =
  let open Alcotest in
  run "Sep_by_recover"
    [
      ( "sep_by_recover",
        [
          test_case "valid input" `Quick test_valid;
          test_case "holes" `Quick test_holes;
          test_case "empty holes" `Quick test_empty_holes;
          test_case "nested lists" `Quick test_skips_nested;
          test_case "skips a whole group" `Quick test_skip_whole_group;
          test_case "brackets of different kinds" `Quick test_mixed_kinds;
          test_case "one counter for all kinds differs" `Quick
            test_one_counter_differs;
          test_case "atom" `Quick test_atom;
          test_case "end of input" `Quick test_end_of_input;
          test_case "streaming" `Quick test_streaming;
          test_case "same as sep_by" `Quick test_same_as_sep_by;
          test_case "wrong collector" `Quick test_wrong_collector;
          test_case "rollback" `Quick test_rollback;
          test_case "record" `Quick test_record;
          test_case "record: arguments" `Quick test_record_arguments;
          test_case "outer:false" `Quick test_outer_false;
          test_case "trailing separator" `Quick test_trailing_separator;
          test_case "record: not called" `Quick test_record_not_called;
        ]
      );
      ( "skip_nested",
        [
          test_case "stop points" `Quick test_skip_nested;
          test_case "atom failure forgotten" `Quick test_atom_failure_forgotten;
        ]
      );
    ]
