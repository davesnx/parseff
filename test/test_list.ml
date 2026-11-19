[@@@alert "-unstable"]

open Parseff

type expr = Num of int | Add of expr * expr | Hole_expr

type stmt =
  | Let of string * expr
  | If of expr * stmt list
  | Call of string * expr list
  | Bad

let rec show_expr = function
  | Num n ->
      string_of_int n
  | Add (a, b) ->
      show_expr a ^ "+" ^ show_expr b
  | Hole_expr ->
      "_"

let rec show_stmt = function
  | Let (x, e) ->
      Printf.sprintf "%s=%s" x (show_expr e)
  | If (c, body) ->
      Printf.sprintf "if %s {%s}" (show_expr c) (show_stmts body)
  | Call (f, args) ->
      Printf.sprintf "%s(%s)" f (String.concat "," (List.map show_expr args))
  | Bad ->
      "BAD"

and show_stmts l = String.concat "; " (List.map show_stmt l)

let show_result show = function
  | Ok v ->
      "Ok " ^ show v
  | Error e ->
      Printf.sprintf "Error at %d: %s" e.range.start (error_message e)

let entries d =
  List.map
    (fun { Diagnostics.value = e; _ } -> (e.range.start, Warning.text e))
    (Diagnostics.to_list d)

let errs = Alcotest.(list (pair int string))

let bytes_source input =
  Source.of_seq (Seq.map (String.make 1) (String.to_seq input))

let is_digit c = c >= '0' && c <= '9'
let is_lower c = c >= 'a' && c <= 'z'
let ws () = skip_whitespace ()
let steps = ref 0
let max_steps = ref max_int
let input_length = ref max_int

let tick () =
  incr steps;
  if !steps > !max_steps then Alcotest.fail "the loop did not stop";
  if position () > !input_length then Alcotest.fail "position past the input"

let sym c () =
  ignore (char c);
  ws ()

let kw k () =
  ignore (keyword k);
  ws ()

let at c () = peek_char () = Some c

let ident () =
  let s = take_while ~at_least:1 ~label:"identifier" is_lower in
  ws ();
  s

let number () =
  tick ();
  let n = int_of_string (take_while ~at_least:1 ~label:"integer" is_digit) in
  ws ();
  Num n

let rec expr () =
  let a = number () in
  if at '+' () then begin
    sym '+' ();
    Add (a, expr ())
  end else
    a

let expr_or_hole () =
  recover
    ~sync:(fun () ->
      skip_nested [ ('(', ')'); ('{', '}') ] (fun c -> c = ';' || c = ',')
    )
    (fun () -> require expr (fun _ -> Hole_expr))
    (fun _ -> Hole_expr)

let skip_statement () =
  skip_nested [ ('(', ')'); ('{', '}') ] (fun c -> c = ';');
  ignore (optional (sym ';'))

let skip_argument () =
  skip_nested [ ('(', ')'); ('{', '}') ] (fun c -> c = ',' || c = ';')

let let_stmt () =
  context "statement" (fun () ->
      kw "let" ();
      cut ();
      let name = require ident (fun _ -> "_") in
      require (sym '=') ignore;
      let value = expr_or_hole () in
      require (sym ';') ignore;
      Let (name, value)
  )

let rec if_stmt () =
  context "if" (fun () ->
      kw "if" ();
      cut ();
      let c = expr_or_hole () in
      require (kw "then") ignore;
      If (c, block ())
  )

and call_stmt () =
  context "call" (fun () ->
      let name = ident () in
      sym '(' ();
      cut ();
      let args =
        context ~delimited:true "arguments" (fun () ->
            let args =
              list ~sep:(sym ',') ~close:(at ')') ~sync:skip_argument expr
                (fun _ -> Hole_expr
              )
            in
            require (sym ')') ignore;
            args
        )
      in
      require (sym ';') ignore;
      Call (name, args)
  )

and statement () =
  tick ();
  one_of [ let_stmt; if_stmt; call_stmt ]

and block () =
  context ~delimited:true "block" (fun () ->
      sym '{' ();
      cut ();
      let stmts =
        list ~close:(at '}') ~sync:skip_statement statement (fun _ -> Bad)
      in
      require (sym '}') ignore;
      stmts
  )

let program () =
  ws ();
  list ~close:(fun () -> false) ~sync:skip_statement statement (fun _ -> Bad)

let run input =
  let d = Diagnostics.create () in
  let r = parse ~diagnostics:d input program in
  (show_result show_stmts r, entries d)

let check input expected =
  Alcotest.(check (pair string errs)) input expected (run input)

let test_statements () =
  check "let x = 1; let y = 2;" ("Ok x=1; y=2", []);
  check "let x = 1; let y = ; let z = 3; let = 4;"
    ( "Ok x=1; y=_; z=3; _=4",
      [
        (19, "expected integer, found ';'");
        (36, "expected identifier, found '='");
      ]
    );
  check "let x = 1; @@; let z = 3;"
    ( "Ok x=1; BAD; z=3",
      [ (11, "expected identifier, \"if\", or \"let\", found '@'") ]
    );
  check "" ("Ok ", [])

let test_inserted () =
  check "let x = 1 let y = 2;"
    ("Ok x=1; y=2", [ (10, "expected ';', found 'let'") ]);
  check "if 1 { let x = 1; }"
    ("Ok if 1 {x=1}", [ (5, "expected \"then\", found '{'") ]);
  check "f(1, 2;" ("Ok f(1,2)", [ (6, "expected ')' or ',', found ';'") ])

let test_missing_closer () =
  check "if 1 then { let x = 1;"
    ("Ok if 1 {x=1}", [ (22, "expected '}', found end of input") ]);
  check "f(1, 2"
    ( "Ok f(1,2)",
      [
        (6, "expected ')' or ',', found end of input");
        (6, "expected ';', found end of input");
      ]
    )

let test_nested_closer () =
  check "if 1 then { f(1, } let y = 2;"
    ( "Ok if 1 {f(1,_)}; y=2",
      [
        (17, "expected integer, found '}'");
        (17, "expected ')' or ',', found '}'");
        (17, "expected ';', found '}'");
      ]
    )

let test_expression_hole () =
  check "let x = 1 + * 2; let y = 2;"
    ("Ok x=_; y=2", [ (12, "expected integer, found '*'") ]);
  check "let x = ;" ("Ok x=_", [ (8, "expected integer, found ';'") ])

let test_separated () =
  check "f(1, 2);" ("Ok f(1,2)", []);
  check "f();" ("Ok f()", []);
  check "f(1, 2,);" ("Ok f(1,2,_)", [ (7, "expected integer, found ')'") ]);
  check "f(1, , 3);" ("Ok f(1,_,3)", [ (5, "expected integer, found ','") ]);
  check "f(, 3);" ("Ok f(_,3)", [ (2, "expected integer, found ','") ]);
  check "f(1 2);" ("Ok f(1,2)", [ (4, "expected ',', found '2'") ]);
  check "f(1, ? 2, 3);" ("Ok f(1,_,3)", [ (5, "expected integer, found '?'") ]);
  check "f(1 ?);" ("Ok f(1)", [ (4, "expected ',', found '?'") ]);
  check "f(1 ?, 2);" ("Ok f(1,2)", [ (4, "expected ',', found '?'") ])

let plain_program () =
  ws ();
  many statement

let braces ?trailing () =
  sym '{' ();
  let xs =
    list ~sep:(sym ',') ?trailing ~close:(at '}') ~sync:skip_argument expr
      (fun _ -> Hole_expr
    )
  in
  require (sym '}') ignore;
  xs

let show_exprs l = String.concat "," (List.map show_expr l)

let test_braces () =
  let run ?trailing input =
    let d = Diagnostics.create () in
    let r = parse ~diagnostics:d input (braces ?trailing) in
    (show_result show_exprs r, entries d)
  in
  let check ?trailing input expected =
    Alcotest.(check (pair string errs)) input expected (run ?trailing input)
  in
  check "{1, 2,}" ("Ok 1,2,_", [ (6, "expected integer, found '}'") ]);
  check "{1, 2, }" ("Ok 1,2,_", [ (7, "expected integer, found '}'") ]);
  check "{}" ("Ok ", []);
  check "{,}"
    ( "Ok _,_",
      [ (1, "expected integer, found ','"); (2, "expected integer, found '}'") ]
    );
  check "{1, 2" ("Ok 1,2", [ (5, "expected ',' or '}', found end of input") ]);
  check "{1, "
    ( "Error at 4: expected integer, found end of input",
      [ (2, "expected '}', found ','") ]
    );
  check ~trailing:true "{1, 2,}" ("Ok 1,2", []);
  check ~trailing:true "{1, 2, }" ("Ok 1,2", []);
  check ~trailing:true "{}" ("Ok ", []);
  check ~trailing:true "{,}" ("Ok _", [ (1, "expected integer, found ','") ]);
  check ~trailing:true "{1, "
    ("Ok 1", [ (4, "expected '}', found end of input") ])

let strict_braces () =
  sym '{' ();
  let xs = sep_by expr (sym ',') in
  sym '}' ();
  xs

let loose_braces () =
  sym '{' ();
  let xs = sep_by expr (sym ',') in
  if xs <> [] then ignore (optional (sym ','));
  sym '}' ();
  xs

let test_skip_past_the_error () =
  let d = Diagnostics.create () in
  let element () =
    ignore (char 'a');
    ignore (char 'b');
    ws ();
    "ab"
  in
  let r =
    parse ~diagnostics:d "{ac ab}" (fun () ->
        sym '{' ();
        let l = list ~close:(at '}') ~sync:ignore element (fun _ -> "_") in
        require (sym '}') ignore;
        l
    )
  in
  Alcotest.(check string) "value" "Ok _,_,ab" (show_result (String.concat ",") r);
  Alcotest.check errs "the skip goes past the error, as with recover"
    [ (2, "expected 'b', found 'c'"); (3, "expected 'a', found ' '") ]
    (entries d)

let opt_braces ?trailing () =
  sym '{' ();
  let xs =
    list ~sep:(sym ',') ?trailing ~close:(at '}') ~sync:skip_argument
      (fun () -> optional expr)
      (fun _ -> Some Hole_expr)
  in
  require (sym '}') ignore;
  xs

let opt_strict () =
  sym '{' ();
  let xs = sep_by (fun () -> optional expr) (sym ',') in
  sym '}' ();
  xs

let show_opts l =
  String.concat "," (List.map (function None -> "." | Some e -> show_expr e) l)

let test_never_accepts_invalid () =
  let rs = Random.State.make [| 2026; 11; 15 |] in
  let pieces = [| "{"; "}"; ","; ", "; "1"; "2 "; "+"; " "; "?"; "(" |] in
  let compare_with show listed plain input =
    let d = Diagnostics.create () in
    let r = parse ~diagnostics:d input listed in
    match parse input plain with
    | Ok v ->
        Alcotest.(check string)
          (input ^ ": same value")
          ("Ok " ^ show v)
          (show_result show r);
        Alcotest.check errs (input ^ ": nothing recorded") [] (entries d)
    | Error _ ->
        if Result.is_ok r && Diagnostics.is_empty d then
          Alcotest.failf "%S: rejected by sep_by, accepted by list" input
  in
  for _ = 1 to 5000 do
    let n = 1 + Random.State.int rs 10 in
    let input =
      "{"
      ^ String.concat ""
          (List.init n (fun _ ->
               pieces.(Random.State.int rs (Array.length pieces))
           )
          )
    in
    compare_with show_exprs (braces ?trailing:None) strict_braces input;
    compare_with show_exprs (braces ~trailing:true) loose_braces input;
    compare_with show_opts (opt_braces ?trailing:None) opt_strict input
  done;
  List.iter
    (fun input ->
      let d = Diagnostics.create () in
      let plain = parse input plain_program in
      let r = parse ~diagnostics:d input program in
      match plain with
      | Ok _ ->
          Alcotest.(check string)
            input
            (show_result show_stmts plain)
            (show_result show_stmts r)
      | Error _ ->
          if Result.is_ok r && Diagnostics.is_empty d then
            Alcotest.failf "%S: rejected without recovery, accepted with it"
              input
    )
    [ "f(1,);"; "f(,);"; "f(1 2);"; "let x = ;"; "if 1 then { f(1, 2,); }" ]

let plain_items close () =
  sym
    ( if close = ')' then
        '('
      else
        '{'
    )
    ();
  let xs = sep_by expr (sym ',') in
  if xs <> [] then ignore (optional (sym ','));
  sym close ();
  xs

let list_items close () =
  sym
    ( if close = ')' then
        '('
      else
        '{'
    )
    ();
  let xs =
    list ~sep:(sym ',') ~trailing:true ~close:(at close) ~sync:skip_argument
      expr (fun _ -> Hole_expr
    )
  in
  require (sym close) ignore;
  xs

let valid_inputs =
  [
    "";
    "let x = 1;";
    "let x = 1 + 2; let y = 3;";
    "if 1 then { let x = 1; f(1, 2 + 3); } g();";
    "if 1 then { } f(1);";
    "if 1 then { if 2 then { let x = 1; } }";
  ]

let test_valid_same () =
  List.iter
    (fun input ->
      let d = Diagnostics.create () in
      let expected = show_result show_stmts (parse input plain_program) in
      Alcotest.(check string)
        (input ^ ": many") expected
        (show_result show_stmts (parse ~diagnostics:d input program));
      Alcotest.check errs (input ^ ": nothing recorded") [] (entries d)
    )
    valid_inputs;
  List.iter
    (fun (input, close) ->
      let d = Diagnostics.create () in
      let show =
        show_result (fun l -> String.concat "," (List.map show_expr l))
      in
      Alcotest.(check string)
        (input ^ ": sep_by")
        (show (parse input (plain_items close)))
        (show (parse ~diagnostics:d input (list_items close)));
      Alcotest.check errs (input ^ ": nothing recorded") [] (entries d)
    )
    [
      ("()", ')');
      ("(1)", ')');
      ("( 1 , 2+3 ,4 )", ')');
      ("(1, 2,)", ')');
      ("{1}", '}');
      ("{ 1, 2, }", '}');
    ]

let test_runner_collector () =
  let input = "let x = 1 + * 2; @@; let z = ; let y = 2" in
  let recovered =
    ( "Ok x=_; BAD; z=_; y=2",
      [
        (12, "expected integer, found '*'");
        (17, "expected identifier, \"if\", or \"let\", found '@'");
        (29, "expected integer, found ';'");
        (40, "expected ';', found end of input");
      ]
    )
  in
  let stopped = ("Error at 12: expected integer, found '*'", []) in
  let partial diagnostics =
    let t = Push.parse ?diagnostics program in
    match Push.feed t input with
    | Done r ->
        Result.map fst r
    | Await ->
        Result.map fst (Push.finish t)
  in
  let first_of_each diagnostics =
    match
      Seq.uncons (Source.parse_each ?diagnostics (bytes_source input) program)
    with
    | Some (r, _) ->
        r
    | None ->
        Alcotest.fail "Source.parse_each gave no value"
  in
  let runners =
    [
      ("parse", fun diagnostics -> parse ?diagnostics input program);
      ( "Source.parse",
        fun diagnostics ->
          Source.parse ?diagnostics (bytes_source input) program
      );
      ("Source.parse_each", first_of_each);
      ("Push.parse", partial);
    ]
  in
  List.iter
    (fun (name, run) ->
      let d = Diagnostics.create () in
      let r = run (Some d) in
      Alcotest.(check (pair string errs))
        (name ^ " with ~diagnostics")
        recovered
        (show_result show_stmts r, entries d);
      Alcotest.(check (pair string errs))
        (name ^ " without ~diagnostics")
        stopped
        (show_result show_stmts (run None), [])
    )
    runners

let test_no_collector () =
  List.iter
    (fun (input, expected) ->
      Alcotest.(check string)
        input expected
        (show_result show_stmts (parse input program))
    )
    [
      ("let x = 1; f(1, 2);", "Ok x=1; f(1,2)");
      ("let x = 1; f(1, 2,);", "Error at 18: expected integer, found ')'");
      ("let x = ;", "Error at 8: expected integer, found ';'");
      ("f(1 2);", "Error at 4: expected ')' or ',', found '2'");
      ("f(1, , 3);", "Error at 5: expected integer, found ','");
      ("if 1 { }", "Error at 5: expected \"then\", found '{'");
      ("let x = 1", "Error at 9: expected ';', found end of input");
    ]

let test_require () =
  let run ?(diagnostics = true) input p =
    let d = Diagnostics.create () in
    let r =
      if diagnostics then
        parse ~diagnostics:d input p
      else
        parse input p
    in
    (show_result Fun.id r, entries d)
  in
  let check name expected actual =
    Alcotest.(check (pair string errs)) name expected actual
  in
  let close_paren () =
    sym '(' ();
    let x = ident () in
    require (sym ')') ignore;
    x ^ "!"
  in
  check "present" ("Ok x!", []) (run "(x)" close_paren);
  check "inserted at the end of the input"
    ("Ok x!", [ (2, "expected ')', found end of input") ])
    (run "(x" close_paren);
  check "inserted before other input"
    ( "Error at 3: expected end of input, found ']'",
      [ (3, "expected ')', found ']'") ]
    )
    (run "(x ]" close_paren);
  check "no collector"
    ("Error at 2: expected ')', found end of input", [])
    (run ~diagnostics:false "(x" close_paren);
  let pair () =
    require
      (fun () ->
        sym '(' ();
        sym ')' ();
        "pair"
      )
      (fun _ -> "missing")
  in
  check "a failure after consumed input goes on"
    ("Error at 1: expected ')', found 'x'", [])
    (run "(x" pair);
  check "a failure where it started is inserted"
    ("Ok missing", [ (0, "expected '(', found end of input") ])
    (run "" pair);
  let inner () =
    require
      (fun () ->
        warn
          (Warning.Recorded (Result.get_error (capture (fun () -> fail "inner")))
          );
        ignore (char ';')
      )
      ignore;
    "done"
  in
  check "the diagnostics of the parser are removed"
    ("Ok done", [ (0, "expected ';', found end of input") ])
    (run "" inner)

let test_require_expected_set () =
  let d = Diagnostics.create () in
  let r =
    parse ~diagnostics:d "a" (fun () ->
        ignore (optional (fun () -> char 'b'));
        require (fun () -> ignore (char 'c')) ignore;
        ignore (char 'a')
    )
  in
  Alcotest.(check string)
    "value" "Ok"
    (show_result (fun () -> "") r |> String.trim);
  Alcotest.check errs "the set holds what was tried at the position"
    [ (0, "expected 'b' or 'c', found 'a'") ]
    (entries d)

let test_trace () =
  let events = ref [] in
  let d = Diagnostics.create () in
  let trace = function
    | Captured { start; error } ->
        events := (start, error_message error) :: !events
    | _ ->
        ()
  in
  ignore
    (parse ~trace ~diagnostics:d "f(1 2, , 3" (fun () -> ignore (statement ())));
  Alcotest.check errs "one event for each recorded error" (entries d)
    (List.rev !events)

let test_sync_before_element () =
  let calls = ref 0 in
  let element () =
    incr calls;
    if !calls > 100 then Alcotest.fail "the loop did not stop";
    let v =
      match peek_char () with
      | Some '(' ->
          ignore (char '(');
          let c = letter () in
          ignore (char ')');
          c
      | _ ->
          letter ()
    in
    ws ();
    v
  in
  let d = Diagnostics.create () in
  let r =
    parse ~consume:`Partial ~diagnostics:d "a (b; c" (fun () ->
        list ~sep:(sym ',') ~close:(at ';')
          ~sync:(fun () -> skip_while (fun c -> c <> '(' && c <> ','))
          element
          (fun _ -> '_')
    )
  in
  Alcotest.(check string)
    "value" "Ok a_"
    (show_result (fun l -> String.of_seq (List.to_seq l)) r);
  Alcotest.check errs "errors"
    [ (4, "expected ')', found ';'"); (2, "expected ',', found '('") ]
    (entries d)

type item = Num of int | Sum of int * int | Gap

let show_item = function
  | Num n ->
      string_of_int n
  | Sum (a, b) ->
      Printf.sprintf "(%d+%d)" a b
  | Gap ->
      "_"

let int_item () =
  let n = int_of_string (take_while ~at_least:1 ~label:"integer" is_digit) in
  ws ();
  n

let item () =
  if at '(' () then begin
    sym '(' ();
    let v =
      recover
        ~sync:(fun () -> skip_while (fun c -> c <> ')'))
        (fun () ->
          let a = int_item () in
          sym '+' ();
          Sum (a, int_item ())
        )
        (fun _ -> Gap)
    in
    require (sym ')') ignore;
    v
  end else
    Num (int_item ())

let items () =
  sym '[' ();
  let xs =
    list ~sep:(sym ',') ~close:(at ']')
      ~sync:(fun () -> skip_nested [ ('(', ')'); ('[', ']') ] (fun c -> c = ','))
      item
      (fun _ -> Gap)
  in
  require (sym ']') ignore;
  xs

let run_items input =
  let d = Diagnostics.create () in
  let r = parse ~diagnostics:d input items in
  (show_result (fun l -> String.concat "," (List.map show_item l)) r, entries d)

let test_recover_in_element () =
  let check input expected =
    Alcotest.(check (pair string errs)) input expected (run_items input)
  in
  check "[1, (2 + 3), 4]" ("Ok 1,(2+3),4", []);
  check "[1, (2 + ?), 4]" ("Ok 1,_,4", [ (9, "expected integer, found '?'") ]);
  check "[1 (2 + ?)]"
    ( "Ok 1,_",
      [ (8, "expected integer, found '?'"); (3, "expected ',', found '('") ]
    )

let test_list_in_or_branch () =
  let d = Diagnostics.create () in
  let parser () =
    or_
      (fun () -> String.concat "," (List.map show_item (items ())))
      (fun () ->
        literal "[a]";
        "letter"
      )
  in
  let run input =
    let r = parse ~diagnostics:d input parser in
    (show_result Fun.id r, entries d)
  in
  let check input expected =
    Alcotest.(check (pair string errs)) input expected (run input)
  in
  check "[1, 2]" ("Ok 1,2", []);
  check "[a]" ("Ok letter", [])

exception Loop

let test_element_without_progress () =
  let calls = ref 0 in
  let guard () =
    incr calls;
    if !calls > 1000 then raise Loop
  in
  let run input element =
    calls := 0;
    let d = Diagnostics.create () in
    let r =
      parse ~diagnostics:d input (fun () ->
          sym '{' ();
          let l = list ~close:(at '}') ~sync:ignore element (fun _ -> -2) in
          require (sym '}') ignore;
          l
      )
    in
    ( show_result (fun l -> String.concat "," (List.map string_of_int l)) r,
      entries d
    )
  in
  let only_require () =
    guard ();
    let e =
      require
        (fun () ->
          let v = integer () in
          ws ();
          v
        )
        (fun _ -> -1)
    in
    require (sym ';') ignore;
    e
  in
  let warns () =
    guard ();
    if at '@' () then begin
      warn (Warning.W "empty statement");
      0
    end else begin
      let v = integer () in
      ws ();
      v
    end
  in
  let check name expected actual =
    Alcotest.(check (pair string errs)) name expected actual
  in
  check "require only: { 1; @}"
    ( "Ok 1,-1",
      [ (5, "expected integer, found '@'"); (5, "expected ';', found '@'") ]
    )
    (run "{ 1; @}" only_require);
  check "warn without progress: {1 @2}"
    ("Ok 1,0,2", [ (3, "empty statement") ])
    (run "{1 @2}" warns);
  let rs = Random.State.make [| 2026; 11; 16 |] in
  let pieces = [| "1"; "2 "; ";"; "; "; "@"; " "; "x" |] in
  for _ = 1 to 2000 do
    let n = Random.State.int rs 12 in
    let body =
      String.concat ""
        (List.init n (fun _ ->
             pieces.(Random.State.int rs (Array.length pieces))
         )
        )
    in
    let input = "{" ^ body ^ "}" in
    ( match run input only_require with
    | _ ->
        ()
    | exception Loop ->
        Alcotest.failf "%S: the loop did not stop" input
    );
    if !calls > (2 * String.length input) + 2 then
      Alcotest.failf "%S: %d steps" input !calls
  done

let test_empty_element_is_a_value () =
  List.iter
    (fun input ->
      let d = Diagnostics.create () in
      Alcotest.(check string)
        input
        (show_result show_opts (parse input opt_strict))
        (show_result show_opts (parse ~diagnostics:d input opt_braces));
      Alcotest.check errs (input ^ ": nothing recorded") [] (entries d)
    )
    [ "{1, , 3}"; "{, 1}"; "{1,}"; "{}"; "{,}"; "{ , , }"; "{1}" ]

let test_record_separator () =
  let d = Diagnostics.create () in
  let calls = ref [] in
  let r =
    parse ~diagnostics:d "{1 2, x}" (fun () ->
        sym '{' ();
        let xs =
          list ~sep:(sym ',')
            ~keep:(fun e _ ->
              calls := e.pos :: !calls;
              []
            )
            ~close:(at '}') ~sync:skip_argument expr
            (fun _ -> Hole_expr)
        in
        require (sym '}') ignore;
        xs
    )
  in
  Alcotest.(check string) "value" "Ok 1,2,_" (show_result show_exprs r);
  Alcotest.check errs "keep replaces every recording" [] (entries d);
  Alcotest.(check (list int)) "keep gets both errors" [ 3; 6 ] (List.rev !calls)

let paren_item () =
  if at '(' () then begin
    sym '(' ();
    let v = int_item () in
    sym ')' ();
    v
  end else
    int_item ()

let paren_list () =
  sym '[' ();
  let xs =
    list ~sep:(sym ',') ~close:(at ']')
      ~sync:(fun () -> skip_nested [ ('(', ')'); ('[', ']') ] (fun c -> c = ','))
      paren_item
      (fun _ -> -1)
  in
  require (sym ']') ignore;
  xs

let test_failed_step_leaves_nothing () =
  let events = ref [] in
  let trace = function
    | Captured { start; error } ->
        events := (start, error_message error) :: !events
    | _ ->
        ()
  in
  let d = Diagnostics.create () in
  let r = parse ~trace ~diagnostics:d "[1 (2" paren_list in
  Alcotest.(check string)
    "value" "Error at 3: expected end of input, found '('"
    (show_result (fun _ -> "") r);
  Alcotest.check errs "the list ends before the element, as sep_by does"
    [ (3, "expected ',' or ']', found '('") ]
    (entries d);
  Alcotest.check errs "one event"
    [ (3, "expected ',' or ']', found '('") ]
    !events;
  let d = Diagnostics.create () in
  let r =
    Source.parse ~backtrack_window:3 ~diagnostics:d
      (bytes_source "[1 (2 3) 4]")
      paren_list
  in
  Alcotest.(check string)
    "window" "Backtrack_window_exceeded"
    ( match r with
    | Error { kind = Backtrack_window_exceeded _; _ } ->
        "Backtrack_window_exceeded"
    | r ->
        show_result (fun _ -> "") r
    );
  Alcotest.check errs "no entry after the abort" [] (entries d)

let test_require_commit () =
  let d = Diagnostics.create () in
  let r =
    parse ~diagnostics:d "x" (fun () ->
        require
          (fun () ->
            warn (Warning.W "inner");
            commit ();
            ignore (char ')')
          )
          ignore;
        ignore (char 'x')
    )
  in
  Alcotest.(check string)
    "value" "Ok"
    (show_result (fun () -> "") r |> String.trim);
  Alcotest.check errs "an insertion undoes what the parser did"
    [ (0, "expected ')', found 'x'") ]
    (entries d);
  let branch () =
    or_
      (fun () ->
        require
          (fun () ->
            ignore (char 'a');
            commit ()
          )
          ignore;
        ignore (char 'b');
        "left"
      )
      (fun () ->
        ignore (char 'a');
        ignore (char 'c');
        "right"
      )
  in
  let d = Diagnostics.create () in
  Alcotest.(check string)
    "commit in require: without the collector"
    "Error at 1: expected 'b', found 'c'"
    (show_result Fun.id (parse "ac" branch));
  Alcotest.(check string)
    "commit in require: with the collector, it reaches the or_"
    "Error at 1: expected 'b', found 'c'"
    (show_result Fun.id (parse ~diagnostics:d "ac" branch))

let test_require_undoes_parser () =
  let d = Diagnostics.create () in
  let cut_in_require () =
    or_
      (fun () ->
        require
          (fun () ->
            cut ();
            ignore (char 'x')
          )
          ignore;
        ignore (char 'z');
        "L"
      )
      (fun () ->
        ignore (char 'y');
        "R"
      )
  in
  Alcotest.(check string)
    "a cut in require reaches the or_" "Error at 0: expected 'z', found 'y'"
    (show_result Fun.id (parse ~diagnostics:d "y" cut_in_require));
  let r = Ref.create 0 in
  let d = Diagnostics.create () in
  let value =
    parse ~diagnostics:d "q" (fun () ->
        Ref.set r 0;
        require
          (fun () ->
            Ref.set r 1;
            commit ();
            ignore (char 'x')
          )
          ignore;
        ignore (char 'q');
        Ref.get r
    )
  in
  Alcotest.(check string)
    "a Ref change before a commit is undone" "Ok 0"
    (show_result string_of_int value)

let test_dropped_element_undoes_ref () =
  let run ?sep input element =
    let cell = Ref.create 0 in
    let d = Diagnostics.create () in
    show_result string_of_int
      (parse ~consume:`Partial ~diagnostics:d input (fun () ->
           ignore
             (list ?sep
                ~close:(fun () -> false)
                ~sync:ignore (element cell) ignore
             );
           Ref.get cell
       )
      )
  in
  let plain input parser =
    let cell = Ref.create 0 in
    show_result string_of_int
      (parse ~consume:`Partial input (fun () ->
           ignore (parser cell ());
           Ref.get cell
       )
      )
  in
  let set cell () = Ref.set cell 1 in
  let second cell () =
    if at 'x' () then
      ignore (char 'x')
    else
      Ref.set cell 2
  in
  let sep () = () in
  Alcotest.(check string)
    "many" "Ok 0"
    (plain "x" (fun c -> fun () -> many (set c)));
  Alcotest.(check string) "no progress" "Ok 0" (run "x" set);
  Alcotest.(check string)
    "sep_by" "Ok 0"
    (plain "xy" (fun c -> fun () -> sep_by (second c) sep));
  Alcotest.(check string)
    "no progress after a separator" "Ok 0" (run ~sep "xy" second)

type custom_error += Probe_error

let test_require_fail_rule () =
  let run p =
    let d = Diagnostics.create () in
    let r =
      parse ~diagnostics:d "ac" (fun () ->
          require
            (fun () ->
              or_
                (fun () ->
                  ignore (char 'a');
                  ignore (char 'b')
                )
                p
            )
            ignore;
          ignore (take 2)
      )
    in
    (Result.is_ok r, entries d)
  in
  Alcotest.(check (pair bool errs))
    "fail after a branch that read further: no insertion" (false, [])
    (run (fun () -> fail "x"));
  Alcotest.(check (pair bool errs))
    "error after a branch that read further: no insertion" (false, [])
    (run (fun () -> error Probe_error));
  Alcotest.(check (pair bool errs))
    "a primitive failure: no insertion" (false, [])
    (run (fun () -> ignore (char 'z')))

exception Stop

let test_raising_callbacks () =
  let digit_ws () =
    let c = digit () in
    ws ();
    c
  in
  let sync () = skip_nested [ ('(', ')') ] (fun c -> c = ',' || c = ')') in
  let guarded name input body =
    let d = Diagnostics.create () in
    let r =
      parse ~diagnostics:d input (fun () ->
          let v =
            match
              capture (fun () ->
                  match body () with s -> s | exception Stop -> "stop"
              )
            with
            | Ok s ->
                s
            | Error _ ->
                "error"
          in
          let w = try or_ body (fun () -> "or") with Stop -> "stop" in
          ignore w;
          ignore (take_while (fun _ -> true));
          v
      )
    in
    match r with
    | Ok s ->
        ignore s
    | Error e ->
        Alcotest.failf "%s: %s" name (error_message e)
    | exception e ->
        Alcotest.failf "%s: %s" name (Printexc.to_string e)
  in
  let sep_comma () =
    ignore (char ',');
    ws ()
  in
  let lst ?(keep = fun e _ -> [ e ]) ?(sync = sync) ?(hole = fun _ -> '?')
      ?(close = fun () -> peek_char () = Some ')') ?(sep = sep_comma)
      ?(element = digit_ws) () =
    String.of_seq (List.to_seq (list ~keep ~sep ~close ~sync element hole))
  in
  let stop _ = raise Stop in
  guarded "keep, missing separator" "1 2)" (lst ~keep:(fun _ _ -> raise Stop));
  guarded "keep, element" "1, x)" (lst ~keep:(fun _ _ -> raise Stop));
  guarded "sync" "1, x)" (lst ~sync:(fun () -> raise Stop));
  guarded "sync, missing separator" "1 x)" (lst ~sync:(fun () -> raise Stop));
  guarded "hole" "1, x)" (lst ~hole:stop);
  guarded "close" "1)" (lst ~close:(fun () -> raise Stop));
  guarded "sep" "1, 2)" (lst ~sep:(fun () -> raise Stop));
  guarded "element" "1 2)"
    (lst ~element:(fun () ->
         if peek_char () = Some '2' then
           raise Stop
         else
           digit_ws ()
     )
    );
  guarded "require: parser" "x" (fun () ->
      require (fun () -> raise Stop) ignore;
      "r"
  );
  guarded "require: missing" "x" (fun () ->
      require (fun () -> ignore (char ')')) stop;
      "r"
  )

let test_trace_order () =
  let element () =
    let c =
      recover
        ~sync:(fun () -> skip_while (fun c -> c = 'x'))
        digit
        (fun _ -> '?')
    in
    ignore (char ';');
    ws ();
    c
  in
  let events = ref [] in
  let trace = function
    | Captured { error; _ } ->
        events := error_message error :: !events
    | _ ->
        ()
  in
  let d = Diagnostics.create () in
  let r =
    parse ~trace ~diagnostics:d "1; x)" (fun () ->
        let l =
          list
            ~sep:(fun () ->
              ignore (char ',');
              ws ()
            )
            ~close:(fun () -> peek_char () = Some ')')
            ~sync:(fun () ->
              skip_nested [ ('(', ')') ] (fun c -> c = ',' || c = ')')
            )
            element
            (fun _ -> '!')
        in
        ignore (char ')');
        String.of_seq (List.to_seq l)
    )
  in
  Alcotest.(check string) "value" "Ok 1!" (show_result Fun.id r);
  Alcotest.check errs "entries"
    [ (4, "expected ';', found ')'"); (3, "expected ',', found 'x'") ]
    (entries d);
  Alcotest.(check (list string))
    "the event of the inner recover, then one event for each entry, in order"
    [
      "expected digit, found 'x'";
      "expected ';', found ')'";
      "expected ',', found 'x'";
    ]
    (List.rev !events)

let test_first_element_fail () =
  let element () =
    let c = any_char () in
    if c >= '0' && c <= '9' then
      c
    else
      fail "not a digit"
  in
  let run diagnostics =
    let d = Diagnostics.create () in
    let parser () =
      let l =
        list
          ~sep:(fun () -> ignore (char ','))
          ~close:(fun () -> peek_char () = Some ')')
          ~sync:ignore element
          (fun _ -> '!')
      in
      ignore (char ')');
      String.of_seq (List.to_seq l)
    in
    let r =
      if diagnostics then
        parse ~diagnostics:d ")" parser
      else
        parse ")" parser
    in
    (show_result Fun.id r, entries d)
  in
  Alcotest.(check (pair string errs))
    "with the collector: an empty list and no entry" ("Ok ", []) (run true);
  Alcotest.(check (pair string errs))
    "without the collector: as sep_by" ("Ok ", []) (run false)

let test_fail_at_close_ends_list () =
  let element () =
    let c = any_char () in
    if is_digit c then
      c
    else
      fail "not a digit"
  in
  let parser ?sep ?trailing () =
    let l =
      list ?sep ?trailing
        ~close:(fun () -> peek_char () = Some ')')
        ~sync:ignore element
        (fun _ -> '!')
    in
    ignore (char ')');
    String.of_seq (List.to_seq l)
  in
  let run ?sep ?trailing input =
    let plain = show_result Fun.id (parse input (parser ?sep ?trailing)) in
    let d = Diagnostics.create () in
    let collected =
      show_result Fun.id (parse ~diagnostics:d input (parser ?sep ?trailing))
    in
    (plain, collected, entries d)
  in
  let check name input ?sep ?trailing expected =
    Alcotest.(check (triple string string errs))
      name (expected, expected, []) (run ?sep ?trailing input)
  in
  check "no separator" "12)" "Ok 12";
  check "no separator, empty" ")" "Ok ";
  check "after a trailing separator" "1,2,)"
    ~sep:(fun () -> ignore (char ','))
    ~trailing:true "Ok 12"

let test_no_collector_is_plain () =
  let rs = Random.State.make [| 2026; 11; 17 |] in
  let pieces = [| "{"; "}"; ","; ", "; "1"; "2 "; "+"; " "; "?"; "(" |] in
  let many_braces () =
    sym '{' ();
    let xs = many expr in
    sym '}' ();
    xs
  in
  let list_many () =
    sym '{' ();
    let xs = list ~close:(at '}') ~sync:ignore expr (fun _ -> Hole_expr) in
    require (sym '}') ignore;
    xs
  in
  let same show input listed plain =
    Alcotest.(check string)
      input
      (show_result show (parse input plain))
      (show_result show (parse input listed))
  in
  for _ = 1 to 5000 do
    let n = Random.State.int rs 10 in
    let input =
      "{"
      ^ String.concat ""
          (List.init n (fun _ ->
               pieces.(Random.State.int rs (Array.length pieces))
           )
          )
    in
    same show_exprs input braces strict_braces;
    same show_exprs input (braces ~trailing:true) loose_braces;
    same show_opts input opt_braces opt_strict;
    same show_exprs input list_many many_braces
  done

let test_require_furthest_failure () =
  let run p =
    let d = Diagnostics.create () in
    let r =
      parse ~diagnostics:d "ax" (fun () ->
          require p ignore;
          literal "ax"
      )
    in
    (show_result (fun () -> "") r |> String.trim, entries d)
  in
  let a_then_b () =
    ignore (char 'a');
    ignore (char 'b')
  in
  Alcotest.(check (pair string errs))
    "a branch that read further: no insertion"
    ("Error at 1: expected 'b', found 'x'", [])
    (run (fun () ->
         ignore (optional a_then_b);
         ignore (char ')')
     )
    );
  Alcotest.(check (pair string errs))
    "the same branch in capture: inserted"
    ("Ok", [ (0, "expected ')', found 'ax'") ])
    (run (fun () ->
         ignore (capture a_then_b);
         ignore (char ')')
     )
    )

let alphabet = "let if then x y f 1 2 + = ; , ( ) { } ? @ \n"

let random_input rs =
  let n = Random.State.int rs 40 in
  String.init n (fun _ ->
      if Random.State.int rs 8 = 0 then
        Char.chr (Random.State.int rs 256)
      else
        alphabet.[Random.State.int rs (String.length alphabet)]
  )

let test_progress () =
  let rs = Random.State.make [| 2026; 11; 14 |] in
  for _ = 1 to 5000 do
    let input = random_input rs in
    let len = String.length input in
    steps := 0;
    max_steps := (20 * len) + 20;
    input_length := len;
    let d = Diagnostics.create () in
    ( match parse ~diagnostics:d input program with
    | Ok _ | Error _ ->
        ()
    | exception e ->
        Alcotest.failf "%S: %s" input (Printexc.to_string e)
    );
    List.iter
      (fun { Diagnostics.pos; _ } ->
        if pos < 0 || pos > len then Alcotest.failf "%S: entry at %d" input pos
      )
      (Diagnostics.to_list d)
  done;
  max_steps := max_int;
  input_length := max_int

let split_found m =
  match String.index_opt m ',' with
  | Some _ -> (
      let key = ", found " in
      let n = String.length key in
      let rec find i =
        if i + n > String.length m then
          None
        else if String.sub m i n = key then
          Some i
        else
          find (i + 1)
      in
      match find 0 with
      | Some i ->
          (String.sub m 0 i, String.sub m (i + n) (String.length m - i - n))
      | None ->
          (m, "")
    )
  | None ->
      (m, "")

let same_but_found on_string on_stream =
  let a, fa = split_found on_string in
  let b, fb = split_found on_stream in
  let unquote f =
    if String.length f >= 2 && f.[0] = '\'' then
      String.sub f 1 (String.length f - 2)
    else
      f
  in
  let fa = unquote fa and fb = unquote fb in
  a = b
  && String.length fb <= String.length fa
  && String.sub fa 0 (String.length fb) = fb

let test_streaming () =
  List.iter
    (fun input ->
      let d = Diagnostics.create () in
      let r =
        Source.parse ~backtrack_window:64 ~diagnostics:d (bytes_source input)
          program
      in
      let value, expected = run input in
      Alcotest.(check string) input value (show_result show_stmts r);
      let streamed = entries d in
      Alcotest.(check int)
        (input ^ ": count") (List.length expected) (List.length streamed);
      List.iter2
        (fun (p, m) (p', m') ->
          Alcotest.(check int) (input ^ ": position") p p';
          if not (same_but_found m m') then
            Alcotest.failf "%S: %S on a string, %S on a stream" input m m'
        )
        expected streamed
    )
    (valid_inputs
    @ [
        "let x = 1; let y = ; let z = 3; let = 4;";
        "let x = 1 let y = 2;";
        "if 1 then { let x = 1;";
        "f(1, , 3); f(1 2); f(1, 2";
        "let x = 1 + * 2; @@; let y = 2;";
      ]
    );
  let long =
    String.concat " "
      (List.init 2000 (fun i -> Printf.sprintf "let x = %d;" (i mod 10)))
  in
  let d = Diagnostics.create () in
  match
    Source.parse ~backtrack_window:32 ~diagnostics:d (bytes_source long) program
  with
  | Ok l ->
      Alcotest.(check int) "long input in a small window" 2000 (List.length l);
      Alcotest.check errs "nothing recorded" [] (entries d)
  | Error e ->
      Alcotest.fail (error_message e)

let given = Buffer.create 16

let iter_a () =
  take_while_iter
    (fun c -> c = 'a')
    (fun sp -> Buffer.add_string given (Slice.to_string sp))

let comma_only () = ignore (char ',')
let rest () = take_while (fun _ -> true)

let run_iter ?(window = 4) ~collector ~stream input p =
  Buffer.clear given;
  let d = Diagnostics.create () in
  let r =
    match
      match (stream, collector) with
      | false, true ->
          parse ~diagnostics:d input p
      | false, false ->
          parse input p
      | true, true ->
          Source.parse ~backtrack_window:window ~diagnostics:d
            (bytes_source input) p
      | true, false ->
          Source.parse ~backtrack_window:window (bytes_source input) p
    with
    | Ok s ->
        "Ok " ^ s
    | Error e ->
        Printf.sprintf "Error at %d: %s" e.range.start (error_message e)
    | exception Invalid_argument _ ->
        "Invalid_argument"
  in
  Printf.sprintf "f got %d; %s; entries [%s]" (Buffer.length given) r
    (String.concat "; "
       (List.map
          (fun (x : Diagnostics.entry) ->
            Printf.sprintf "%d:%s" x.pos (Warning.text x.value)
          )
          (Diagnostics.to_list d)
       )
    )

let check_iter name input p ~collector ~plain =
  List.iter
    (fun stream ->
      let where =
        if stream then
          "stream, reads of 1, window 4"
        else
          "string"
      in
      Alcotest.(check string)
        (Printf.sprintf "%s, %s, with the collector" name where)
        collector
        (run_iter ~collector:true ~stream input p);
      Alcotest.(check string)
        (Printf.sprintf "%s, %s, without the collector" name where)
        plain
        (run_iter ~collector:false ~stream input p)
    )
    [ false; true ]

let at_closer failure () =
  let l =
    list ~sep:comma_only
      ~close:(fun () -> peek_char () = Some 'a')
      ~sync:ignore
      (fun () ->
        iter_a ();
        failure ();
        "e"
      )
      (fun _ -> "H")
  in
  String.concat "," l ^ "|" ^ rest ()

let a20b = String.make 20 'a' ^ "b"

let test_iter_first_at_closer () =
  check_iter "first element at the closer, then fail" a20b
    (at_closer (fun () -> fail "u"))
    ~collector:"f got 20; Error at 20: u; entries []"
    ~plain:"f got 20; Error at 20: u; entries []";
  check_iter "first element at the closer, then a primitive failure" a20b
    (at_closer (fun () -> ignore (char 'z')))
    ~collector:"f got 20; Error at 20: expected 'z', found 'b'; entries []"
    ~plain:"f got 20; Error at 20: expected 'z', found 'b'; entries []"

let iter_element () =
  if peek_char () = Some 'a' then begin
    iter_a ();
    ignore (char 'b')
  end;
  "e"

let iter_in_element ~release =
  let name what =
    what
    ^
    if release then
      " with ~release:true"
    else
      ""
  in
  let collector value =
    if release then
      Printf.sprintf "f got 2; Ok %s|; entries []" value
    else
      "f got 0; Invalid_argument; entries []"
  in
  check_iter
    (name "an element without ~sep")
    "baab"
    (fun () ->
      ignore (char 'b');
      let l =
        list ~release
          ~close:(fun () -> false)
          ~sync:ignore iter_element
          (fun _ -> "H")
      in
      String.concat "," l ^ "|" ^ rest ()
    )
    ~collector:(collector "e") ~plain:"f got 2; Ok e|; entries []";
  check_iter
    (name "an element after a separator")
    "b,aab"
    (fun () ->
      ignore (char 'b');
      let l =
        list ~release ~sep:comma_only
          ~close:(fun () -> false)
          ~sync:ignore iter_element
          (fun _ -> "H")
      in
      String.concat "," l ^ "|" ^ rest ()
    )
    ~collector:(collector "e,e") ~plain:"f got 2; Ok e,e|; entries []"

let test_iter_element () = iter_in_element ~release:false
let test_iter_element_release () = iter_in_element ~release:true

let test_iter_missing_separator () =
  check_iter "an element after a missing separator" "b aab"
    (fun () ->
      ignore (char 'b');
      let l =
        list ~sep:comma_only
          ~close:(fun () -> false)
          ~sync:ignore
          (fun () ->
            if peek_char () = Some ' ' then ignore (char ' ');
            iter_element ()
          )
          (fun _ -> "H")
      in
      String.concat "," l ^ "|" ^ rest ()
    )
    ~collector:"f got 0; Invalid_argument; entries []"
    ~plain:"f got 2; Ok e|; entries []"

let test_iter_at_end () =
  check_iter "an element after a separator at the end of the input" "b1,"
    (fun () ->
      ignore (char 'b');
      let l =
        list ~sep:comma_only
          ~close:(fun () -> false)
          ~sync:ignore
          (fun () ->
            if peek_char () = Some '1' then
              ignore (char '1')
            else begin
              iter_a ();
              ignore (char 'z')
            end;
            "e"
          )
          (fun _ -> "H")
      in
      String.concat "," l ^ "|" ^ rest ()
    )
    ~collector:"f got 0; Ok e|,; entries []"
    ~plain:"f got 0; Ok e|,; entries []"

let test_iter_close () =
  check_iter "close" "aab"
    (fun () ->
      let l =
        list ~sep:comma_only
          ~close:(fun () ->
            iter_a ();
            true
          )
          ~sync:ignore
          (fun () -> "e")
          (fun _ -> "H")
      in
      String.concat "," l ^ "|" ^ rest ()
    )
    ~collector:"f got 0; Invalid_argument; entries []"
    ~plain:"f got 0; Ok e|aab; entries []"

let test_iter_require () =
  check_iter "require" "aab"
    (fun () ->
      require
        (fun () ->
          iter_a ();
          (fail "u" : unit)
        )
        ignore;
      rest ()
    )
    ~collector:"f got 0; Invalid_argument; entries []"
    ~plain:"f got 2; Error at 2: u; entries []";
  check_iter "require at the end of the input" ""
    (fun () ->
      require
        (fun () ->
          iter_a ();
          ignore (char ')')
        )
        ignore;
      rest ()
    )
    ~collector:"f got 0; Invalid_argument; entries []"
    ~plain:"f got 0; Error at 0: expected ')', found end of input; entries []"

let commit_at_end ~release tail () =
  let l =
    list ~release ~sep:comma_only
      ~close:(fun () -> peek_char () = Some ')')
      ~sync:ignore
      (fun () ->
        if peek_char () = Some 'n' then begin
          ignore (char 'n');
          "n"
        end else begin
          commit ();
          tail ()
        end
      )
      (fun _ -> "H")
  in
  String.concat "," l

let test_commit_at_end () =
  let tails =
    [
      ("then later a failure", (fun () -> "e"), true, "Ok ALT", "Ok NONE");
      ( "then a primitive failure",
        (fun () ->
          ignore (char 'z');
          "e"
        ),
        false,
        "Ok ALT",
        "Ok NONE"
      );
      ("then fail", (fun () -> fail "u"), false, "Ok ALT", "Ok NONE");
    ]
  in
  List.iter
    (fun release ->
      List.iter
        (fun (name, tail, later, in_or, in_optional) ->
          let left () =
            let s = commit_at_end ~release tail () in
            if later then ignore (char 'q');
            s
          in
          let name =
            name
            ^
            if release then
              ", ~release:true"
            else
              ""
          in
          check_iter
            ("commit at the end of the input, in or_, " ^ name)
            "n,"
            (fun () ->
              or_
                (fun () -> left ())
                (fun () ->
                  ignore (rest ());
                  "ALT"
                )
            )
            ~collector:(Printf.sprintf "f got 0; %s; entries []" in_or)
            ~plain:(Printf.sprintf "f got 0; %s; entries []" in_or);
          check_iter
            ("commit at the end of the input, in optional, " ^ name)
            "n,"
            (fun () ->
              match optional (fun () -> left ()) with
              | Some s ->
                  s
              | None ->
                  ignore (rest ());
                  "NONE"
            )
            ~collector:(Printf.sprintf "f got 0; %s; entries []" in_optional)
            ~plain:(Printf.sprintf "f got 0; %s; entries []" in_optional)
        )
        tails
    )
    [ false; true ]

let committing_braces () =
  sym '{' ();
  let xs =
    list ~sep:(sym ',')
      ~close:(fun () -> peek_char () = Some '}' || peek_char () = Some '^')
      ~sync:skip_argument
      (fun () ->
        match peek_char () with
        | Some '^' ->
            ignore (char '^');
            commit ();
            ignore (char '1');
            ws ();
            "^1"
        | None ->
            commit ();
            ignore (char '1');
            "e"
        | _ ->
            let s =
              take_while ~at_least:1 ~label:"digit" (fun c -> c = '1' || c = '2')
            in
            ws ();
            s
      )
      (fun _ -> "H")
  in
  require (sym '}') ignore;
  end_of_input ();
  String.concat "," xs

let test_commit_never_reaches_the_caller () =
  let rs = Random.State.make [| 2026; 11; 18 |] in
  let pieces = [| "{"; "}"; ","; ", "; "1"; "2 "; "^"; "^1"; " "; "x" |] in
  for _ = 1 to 5000 do
    let n = Random.State.int rs 8 in
    let input =
      String.concat ""
        (List.init n (fun _ ->
             pieces.(Random.State.int rs (Array.length pieces))
         )
        )
    in
    List.iter
      (fun stream ->
        let run collector p = run_iter ~window:64 ~collector ~stream input p in
        let alone collector = run collector committing_braces in
        let in_or collector =
          run collector (fun () ->
              or_
                (fun () -> committing_braces ())
                (fun () ->
                  ignore (rest ());
                  "ALT"
                )
          )
        in
        let ok r = String.length r > 13 && String.sub r 0 12 = "f got 0; Ok " in
        let a = alone false in
        List.iter
          (fun collector ->
            let expected =
              if ok a then
                a
              else
                "f got 0; Ok ALT; entries []"
            in
            Alcotest.(check string)
              (Printf.sprintf "%S, collector %b, stream %b: or_ around list"
                 input collector stream
              )
              expected (in_or collector)
          )
          [ true; false ];
        let plain = alone false in
        if ok plain then
          Alcotest.(check string)
            (Printf.sprintf "%S, stream %b: valid input" input stream)
            plain (alone true)
      )
      [ false; true ]
  done

let test_commit_in_first_at_closer () =
  let first element () =
    let l =
      list ~sep:comma_only
        ~close:(fun () -> peek_char () = Some 'a')
        ~sync:ignore element
        (fun _ -> "H")
    in
    String.concat "," l ^ "|" ^ rest ()
  in
  let pin name element ~collector ~plain =
    check_iter name "ab" (first element) ~collector ~plain
  in
  pin "commit after a, then fail"
    (fun () ->
      ignore (char 'a');
      commit ();
      fail "u"
    )
    ~collector:"f got 0; Error at 1: u; entries []"
    ~plain:"f got 0; Error at 1: u; entries []";
  pin "cut after a, then fail"
    (fun () ->
      ignore (char 'a');
      cut ();
      fail "u"
    )
    ~collector:"f got 0; Error at 1: u; entries []"
    ~plain:"f got 0; Error at 1: u; entries []";
  pin "commit at the start, then fail"
    (fun () ->
      commit ();
      fail "u"
    )
    ~collector:"f got 0; Error at 0: u; entries []"
    ~plain:"f got 0; Error at 0: u; entries []";
  pin "no commit, then fail"
    (fun () ->
      ignore (char 'a');
      fail "u"
    )
    ~collector:"f got 0; Ok |ab; entries []"
    ~plain:"f got 0; Ok |ab; entries []";
  pin "cut inside a context, then fail"
    (fun () ->
      context "c" (fun () ->
          ignore (char 'a');
          cut ();
          fail "u"
      )
    )
    ~collector:"f got 0; Error at 1: u; entries []"
    ~plain:"f got 0; Error at 1: u; entries []"

let () =
  let open Alcotest in
  run "List"
    [
      ( "list",
        [
          test_case "braces and a trailing separator" `Quick test_braces;
          test_case "invalid input is never accepted" `Quick
            test_never_accepts_invalid;
          test_case "valid input: same as many and sep_by" `Quick
            test_valid_same;
          test_case "no collector" `Quick test_no_collector;
          test_case "the collector of the runner" `Quick test_runner_collector;
          test_case "require" `Quick test_require;
          test_case "require: expected set" `Quick test_require_expected_set;
          test_case "trace" `Quick test_trace;
          test_case "element without progress" `Quick
            test_element_without_progress;
          test_case "without sep, skip past the error" `Quick
            test_skip_past_the_error;
          test_case "an empty element is a value" `Quick
            test_empty_element_is_a_value;
          test_case "keep: separator errors" `Quick test_record_separator;
          test_case "a failed step leaves no entry and no event" `Quick
            test_failed_step_leaves_nothing;
          test_case "require: commit in the parser" `Quick test_require_commit;
          test_case "require: undoes the parser" `Quick
            test_require_undoes_parser;
          test_case "a dropped element undoes its Ref changes" `Quick
            test_dropped_element_undoes_ref;
          test_case "require: fail follows the rule" `Quick
            test_require_fail_rule;
          test_case "callbacks that raise" `Quick test_raising_callbacks;
          test_case "trace: events and entries agree" `Quick test_trace_order;
          test_case "first element fails with fail" `Quick
            test_first_element_fail;
          test_case "fail at the closer ends the list" `Quick
            test_fail_at_close_ends_list;
          test_case "without the collector: the plain parser" `Quick
            test_no_collector_is_plain;
          test_case "iter: first element at the closer" `Quick
            test_iter_first_at_closer;
          test_case "iter: an element" `Quick test_iter_element;
          test_case "iter: an element, ~release:true" `Quick
            test_iter_element_release;
          test_case "iter: after a missing separator" `Quick
            test_iter_missing_separator;
          test_case "iter: at the end of the input" `Quick test_iter_at_end;
          test_case "iter: close" `Quick test_iter_close;
          test_case "iter: require" `Quick test_iter_require;
          test_case "commit in an element at the end of the input" `Quick
            test_commit_at_end;
          test_case "a commit in list never reaches the caller" `Quick
            test_commit_never_reaches_the_caller;
          test_case "commit or cut in the first element at the closer" `Quick
            test_commit_in_first_at_closer;
          test_case "require: the furthest failure decides" `Quick
            test_require_furthest_failure;
          test_case "recover inside an element" `Quick test_recover_in_element;
          test_case "rule A: list in a branch of or_" `Quick
            test_list_in_or_branch;
          test_case "forward progress on random input" `Quick test_progress;
          test_case "forward progress: sync stops before an element" `Quick
            test_sync_before_element;
          test_case "streaming" `Quick test_streaming;
          test_case "statement list" `Quick test_statements;
          test_case "inserted tokens" `Quick test_inserted;
          test_case "missing closing token" `Quick test_missing_closer;
          test_case "closing token of an outer construct" `Quick
            test_nested_closer;
          test_case "expression with a hole" `Quick test_expression_hole;
          test_case "separated list" `Quick test_separated;
        ]
      );
    ]
