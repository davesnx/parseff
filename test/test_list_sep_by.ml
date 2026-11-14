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

let skip_value =
  skip_nested
    ~atom:(fun () -> ignore (string_literal ()))
    pairs
    (fun c -> c = ',')

let comma () =
  ignore (char ',');
  ws ()

let integer () =
  Int (int_of_string (take_while ~at_least:1 ~label:"integer" is_digit))

let rec value d () =
  let v =
    match peek_char () with
    | Some '[' ->
        ignore (char '[');
        ws ();
        List (items d ']')
    | Some '"' ->
        Str (string_literal ())
    | Some c when is_lower c ->
        let name = take_while is_lower in
        ignore (char '(');
        ws ();
        Call (name, items d ')')
    | _ ->
        integer ()
  in
  ws ();
  v

and items d close =
  let xs =
    list ~sep:comma d
      ~close:(fun () -> peek_char () = Some close)
      ~sync:skip_value (value d)
      (fun _ -> Hole)
      ()
  in
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
  case "[1, [2, 3] 4, 5]" "Ok [1; [2; 3]; 4; 5]"
    [ (11, "expected ',', found '4'") ];
  case "[1, f(2, 3 4), 5]" "Ok [1; f(2; 3; 4); 5]"
    [ (11, "expected ',', found '4'") ]

let test_skip_whole_group () =
  case "[1, f(2, [3, 4]) ?, 5]" "Ok [1; f(2; [3; 4]); 5]"
    [ (17, "expected ',', found '?'") ];
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
          list ~sep:comma d
            ~close:(fun () -> peek_char () = Some ')')
            ~sync:skip_one_counter (value d)
            (fun _ -> Hole)
            ()
        in
        ignore (char ')');
        Call (name, xs)
    | _ ->
        integer ()
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

let test_end_of_input () =
  case "[1, " "Error at 4: expected integer, found end of input" [];
  case "[1, [2" "Error at 6: expected ',' or ']', found end of input" [];
  case "f(1, g(" "Error at 7: expected integer or ')', found end of input" []

let test_streaming () =
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
    [
      "[1, ?, 3, @]";
      "[1, , 3]";
      "g(f(1, ?[2 ), 3)";
      "[1, ?\"a]b,c\"?, 3]";
      "[1, [2";
      "[1, ";
      "f(1, [2, 3], \"a,b\")";
    ]

let test_same_as_sep_by () =
  let plain () =
    let xs = sep_by ~at_least:1 (fun () -> any_char ()) comma () in
    ignore (optional comma ());
    xs
  in
  let d = Diagnostics.create () in
  let recovering () =
    list ~sep:comma ~trailing:true d
      ~close:(fun () -> false)
      ~sync:skip_value
      (fun () -> any_char ())
      (fun _ -> '?')
      ()
  in
  let with_position p () =
    let v = p () in
    (v, position ())
  in
  List.iter
    (fun input ->
      let a = parse ~consume:`Partial input (with_position plain) in
      let b =
        parse ~consume:`Partial ~diagnostics:d input (with_position recovering)
      in
      Alcotest.(check bool) (input ^ ": same") true (a = b)
    )
    [ "a"; "a,b"; "a,"; "a,b," ];
  Alcotest.(check bool)
    "an empty list" true
    (parse ~consume:`Partial ~diagnostics:d "" recovering = Ok []);
  Alcotest.(check int) "nothing recorded" 0 (Diagnostics.length d);
  Alcotest.(check bool)
    "a missing separator" true
    (parse ~consume:`Partial ~diagnostics:d "a, b ,c" recovering
    = Ok [ 'a'; 'b'; ' '; 'c' ]
    )

let test_wrong_collector () =
  let d = Diagnostics.create () in
  let calls = ref 0 in
  let list () =
    list ~sep:comma
      ~record:(fun _ _ -> incr calls)
      d
      ~close:(fun () -> false)
      ~sync:skip_value (value d)
      (fun _ -> Hole)
      ()
  in
  let plain () = sep_by ~at_least:1 (value d) comma () in
  List.iter
    (fun input ->
      let expected = show_result (parse input (fun () -> List (plain ()))) in
      Alcotest.(check string)
        (input ^ ": no collector") expected
        (show_result (parse input (fun () -> List (list ()))));
      Alcotest.(check string)
        (input ^ ": other collector")
        expected
        (show_result
           (parse ~diagnostics:(Diagnostics.create ()) input (fun () ->
                List (list ())
            )
           )
        )
    )
    [ "1"; "1, 2"; "1, ?, 3"; "?" ];
  Alcotest.(check int) "record not called" 0 !calls;
  Alcotest.(check int) "nothing recorded" 0 (Diagnostics.length d)

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
    ws ();
    Str ""
  in
  let parser () =
    ignore (char '[');
    let xs =
      list ~sep:comma
        ?record:(Option.map (fun f -> f d) record)
        d
        ~close:(fun () -> peek_char () = Some ']')
        ~sync:(skip_nested [ ('[', ']') ] (fun c -> c = ','))
        element
        (fun _ -> Hole)
        ()
    in
    ignore (char ']');
    List xs
  in
  let r =
    if source then
      parse_source ~diagnostics:d
        (Source.of_seq (Seq.map (String.make 1) (String.to_seq input)))
        parser
    else
      parse ~diagnostics:d input parser
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
        list
          ~sep:(fun () -> ignore (char ','))
          ~record:(fun e inner ->
            seen :=
              (e.range.start, error_message e)
              :: List.map
                   (fun (x : _ Diagnostics.entry) ->
                     (x.pos, error_message x.value)
                   )
                   inner
          )
          d
          ~close:(fun () -> false)
          ~sync:(fun () -> skip_while (fun c -> c <> ','))
          element
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

let test_release () =
  let d = Diagnostics.create () in
  let seen = ref [] in
  let element () =
    ignore (char 'a');
    warn d (note "before");
    commit ();
    ignore (char 'b');
    warn d (note "after");
    ignore (char '!');
    Int 1
  in
  let run ?release ?record () =
    let r =
      parse ~diagnostics:d "ab?,ab!" (fun () ->
          List
            (list ?release ?record
               ~sep:(fun () -> ignore (char ','))
               d
               ~close:(fun () -> false)
               ~sync:(fun () ->
                 seen := position () :: !seen;
                 skip_while (fun c -> c <> ',')
               )
               element
               (fun _ -> Hole)
               ()
            )
      )
    in
    (show_result r, errors d)
  in
  let check name expected actual =
    Alcotest.(check (pair string errs)) name expected actual
  in
  check "default"
    ( "Ok [_; 1]",
      [ (2, "expected '!', found '?'"); (5, "before"); (6, "after") ]
    )
    (run ());
  Alcotest.(check (list int)) "default syncs from the start" [ 0 ] !seen;
  seen := [];
  check "release keeps the entries before the commit"
    ( "Ok [_; 1]",
      [
        (1, "before");
        (2, "expected '!', found '?'");
        (5, "before");
        (6, "after");
      ]
    )
    (run ~release:true ());
  Alcotest.(check (list int)) "release syncs from the commit" [ 1 ] !seen;
  let inner = ref [] in
  ignore
    (run ~release:true
       ~record:(fun _ l ->
         inner := List.map (fun x -> error_message x.Diagnostics.value) l
       )
       ()
    );
  Alcotest.(check (list string))
    "record gets the entries after the commit" [ "after" ] !inner

let test_streaming_release () =
  let input = "[" ^ String.make 40 ' ' ^ "?, 2]" in
  let element () =
    many
      (fun () ->
        ignore (char ' ');
        cut ()
      )
      ()
    |> ignore;
    let v = integer () in
    ws ();
    v
  in
  let run ?release () =
    let d = Diagnostics.create () in
    let r =
      parse_source ~backtrack_window:16 ~diagnostics:d
        (Source.of_seq (Seq.map (String.make 1) (String.to_seq input)))
        (fun () ->
          ignore (char '[');
          let xs =
            list ?release ~sep:comma d
              ~close:(fun () -> peek_char () = Some ']')
              ~sync:skip_value element
              (fun _ -> Hole)
              ()
          in
          ignore (char ']');
          List xs
        )
    in
    match r with
    | Error { kind = Backtrack_window_exceeded _; _ } ->
        ("Backtrack_window_exceeded", [])
    | r ->
        (show_result r, errors d)
  in
  let check name expected actual =
    Alcotest.(check (pair string errs)) name expected actual
  in
  check "default keeps the start of the element"
    ("Backtrack_window_exceeded", [])
    (run ());
  check "release keeps the input after the last cut"
    ("Ok [_; 2]", [ (41, "expected integer or ' ', found '?'") ])
    (run ~release:true ())

let test_failing_hole () =
  let run input hole record =
    let d = Diagnostics.create () in
    show_result
      (parse ~diagnostics:d input (fun () ->
           ignore (char '[');
           let xs =
             list ?record
               ~sep:(fun () -> ignore (char ','))
               d
               ~close:(fun () -> peek_char () = Some ']')
               ~sync:(fun () -> skip_while (fun c -> c <> ',' && c <> ']'))
               (fun () -> Int (Char.code (digit ()) - 48))
               hole ()
           in
           ignore (char ']');
           List xs
       )
      )
  in
  let z _ =
    ignore (char 'z');
    Hole
  in
  let check name expected actual =
    Alcotest.(check string) name expected actual
  in
  check "first element, hole" "Error at 2: expected 'z', found ','"
    (run "[?,2]" z None);
  check "later element, hole" "Error at 4: expected 'z', found ','"
    (run "[1,?,2]" z None);
  check "later element, record" "Error at 3: expected 'z', found '?'"
    (run "[1,?,2]" (fun _ -> Hole) (Some (fun _ _ -> ignore (char 'z'))));
  check "later element, fail in hole" "Error at 4: bad hole"
    (run "[1,?,2]" (fun _ -> fail "bad hole") None)

let test_outer_false () =
  let d = Diagnostics.create () in
  let run outer =
    let r =
      parse ~diagnostics:d "[ab, ax]" (fun () ->
          context ~delimited:true "list" (fun () ->
              ignore (char '[');
              let xs =
                list ?outer ~sep:comma d
                  ~close:(fun () -> peek_char () = Some ']')
                  ~sync:skip_value
                  (fun () ->
                    context "element" (fun () ->
                        ignore (char 'a');
                        ignore (char 'b');
                        Int 0
                    )
                  )
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

let run_table ?trailing input =
  let d = Diagnostics.create () in
  let r =
    parse ~diagnostics:d input (fun () ->
        ignore (char '{');
        ws ();
        let xs =
          list ~sep:comma ?trailing d
            ~close:(fun () -> peek_char () = Some '}')
            ~sync:(skip_nested [ ('{', '}') ] (fun c -> c = ','))
            (fun () ->
              let v = integer () in
              ws ();
              v
            )
            (fun _ -> Hole)
            ()
        in
        ignore (char '}');
        List xs
    )
  in
  (show_result r, errors d)

let test_trailing_separator () =
  let check ?(trailing = true) input expected =
    Alcotest.(check (pair string errs))
      input expected
      (run_table ~trailing input)
  in
  check ~trailing:false "{1, 2,}"
    ("Ok [1; 2; _]", [ (6, "expected integer, found '}'") ]);
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
            list ~sep:comma
              ~record:(fun _ _ -> incr calls)
              d
              ~close:(fun () -> peek_char () = Some ']')
              ~sync:skip_value
              (fun () ->
                let v = integer () in
                ws ();
                v
              )
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
  run "List_sep_by"
    [
      ( "list",
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
          test_case "release" `Quick test_release;
          test_case "failing hole or record" `Quick test_failing_hole;
          test_case "streaming release" `Quick test_streaming_release;
          test_case "rollback" `Quick test_rollback;
          test_case "record" `Quick test_record;
          test_case "record: arguments" `Quick test_record_arguments;
          test_case "outer:false" `Quick test_outer_false;
          test_case "trailing separator" `Quick test_trailing_separator;
          test_case "record: not called" `Quick test_record_not_called;
        ]
      );
    ]
