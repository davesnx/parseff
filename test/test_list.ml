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
    (fun { Diagnostics.value = e; _ } -> (e.range.start, error_message e))
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

let expr_or_hole d () =
  recover d
    ~sync:(skip_nested [ ('(', ')'); ('{', '}') ] (fun c -> c = ';' || c = ','))
    (fun () -> require d expr (fun _ -> Hole_expr))
    (fun _ -> Hole_expr)

let skip_statement () =
  skip_nested [ ('(', ')'); ('{', '}') ] (fun c -> c = ';') ();
  ignore (optional (sym ';') ())

let skip_argument =
  skip_nested [ ('(', ')'); ('{', '}') ] (fun c -> c = ',' || c = ';')

let let_stmt d () =
  context "statement" (fun () ->
      kw "let" ();
      cut ();
      let name = require d ident (fun _ -> "_") in
      require d (sym '=') ignore;
      let value = expr_or_hole d () in
      require d (sym ';') ignore;
      Let (name, value)
  )

let rec if_stmt d () =
  context "if" (fun () ->
      kw "if" ();
      cut ();
      let c = expr_or_hole d () in
      require d (kw "then") ignore;
      If (c, block d ())
  )

and call_stmt d () =
  context "call" (fun () ->
      let name = ident () in
      sym '(' ();
      cut ();
      let args =
        context ~delimited:true "arguments" (fun () ->
            let args =
              list ~sep:(sym ',') d ~close:(at ')') ~sync:skip_argument expr
                (fun _ -> Hole_expr)
                ()
            in
            require d (sym ')') ignore;
            args
        )
      in
      require d (sym ';') ignore;
      Call (name, args)
  )

and statement d () =
  tick ();
  one_of [ let_stmt d; if_stmt d; call_stmt d ] ()

and block d () =
  context ~delimited:true "block" (fun () ->
      sym '{' ();
      cut ();
      let stmts =
        list d ~close:(at '}') ~sync:skip_statement (statement d)
          (fun _ -> Bad)
          ()
      in
      require d (sym '}') ignore;
      stmts
  )

let program d () =
  ws ();
  list d
    ~close:(fun () -> false)
    ~sync:skip_statement (statement d)
    (fun _ -> Bad)
    ()

let run input =
  let d = Diagnostics.create () in
  let r = parse ~diagnostics:d input (program d) in
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
  let no_collector = Diagnostics.create () in
  ws ();
  many (statement no_collector) ()

let braces ?trailing d () =
  sym '{' ();
  let xs =
    list ~sep:(sym ',') ?trailing d ~close:(at '}') ~sync:skip_argument expr
      (fun _ -> Hole_expr)
      ()
  in
  require d (sym '}') ignore;
  xs

let show_exprs l = String.concat "," (List.map show_expr l)

let test_braces () =
  let run ?trailing input =
    let d = Diagnostics.create () in
    let r = parse ~diagnostics:d input (braces ?trailing d) in
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
  check "{1, " ("Error at 4: expected integer, found end of input", []);
  check ~trailing:true "{1, 2,}" ("Ok 1,2", []);
  check ~trailing:true "{1, 2, }" ("Ok 1,2", []);
  check ~trailing:true "{}" ("Ok ", []);
  check ~trailing:true "{,}" ("Ok _", [ (1, "expected integer, found ','") ]);
  check ~trailing:true "{1, "
    ("Ok 1", [ (4, "expected '}', found end of input") ])

let strict_braces () =
  sym '{' ();
  let xs = sep_by expr (sym ',') () in
  sym '}' ();
  xs

let loose_braces () =
  sym '{' ();
  let xs = sep_by expr (sym ',') () in
  if xs <> [] then ignore (optional (sym ',') ());
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
        let l = list d ~close:(at '}') ~sync:ignore element (fun _ -> "_") () in
        require d (sym '}') ignore;
        l
    )
  in
  Alcotest.(check string) "value" "Ok _,_,ab" (show_result (String.concat ",") r);
  Alcotest.check errs "the skip goes past the error, as with recover"
    [ (2, "expected 'b', found 'c'"); (3, "expected 'a', found ' '") ]
    (entries d)

let opt_braces ?trailing d () =
  sym '{' ();
  let xs =
    list ~sep:(sym ',') ?trailing d ~close:(at '}') ~sync:skip_argument
      (fun () -> optional expr ())
      (fun _ -> Some Hole_expr)
      ()
  in
  require d (sym '}') ignore;
  xs

let opt_strict () =
  sym '{' ();
  let xs = sep_by (fun () -> optional expr ()) (sym ',') () in
  sym '}' ();
  xs

let show_opts l =
  String.concat "," (List.map (function None -> "." | Some e -> show_expr e) l)

let test_never_accepts_invalid () =
  let rs = Random.State.make [| 2026; 11; 15 |] in
  let pieces = [| "{"; "}"; ","; ", "; "1"; "2 "; "+"; " "; "?"; "(" |] in
  let compare_with show listed plain input =
    let d = Diagnostics.create () in
    let r = parse ~diagnostics:d input (listed d) in
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
      let r = parse ~diagnostics:d input (program d) in
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
  let xs = sep_by expr (sym ',') () in
  if xs <> [] then ignore (optional (sym ',') ());
  sym close ();
  xs

let list_items d close () =
  sym
    ( if close = ')' then
        '('
      else
        '{'
    )
    ();
  let xs =
    list ~sep:(sym ',') ~trailing:true d ~close:(at close) ~sync:skip_argument
      expr
      (fun _ -> Hole_expr)
      ()
  in
  require d (sym close) ignore;
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
        (show_result show_stmts (parse ~diagnostics:d input (program d)));
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
        (show (parse ~diagnostics:d input (list_items d close)));
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

let test_no_collector () =
  List.iter
    (fun (input, expected) ->
      let d = Diagnostics.create () in
      Alcotest.(check string)
        input expected
        (show_result show_stmts (parse input (program d)));
      Alcotest.(check int)
        (input ^ ": nothing recorded")
        0 (Diagnostics.length d)
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
        parse ~diagnostics:d input (p d)
      else
        parse input (p d)
    in
    (show_result Fun.id r, entries d)
  in
  let check name expected actual =
    Alcotest.(check (pair string errs)) name expected actual
  in
  let close_paren d () =
    sym '(' ();
    let x = ident () in
    require d (sym ')') ignore;
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
  let pair d () =
    require d
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
  let inner d () =
    require d
      (fun () ->
        warn d (Result.get_error (capture (fun () -> fail "inner")));
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
        ignore (optional (fun () -> char 'b') ());
        require d (fun () -> ignore (char 'c')) ignore;
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
    (parse ~trace ~diagnostics:d "f(1 2, , 3" (fun () -> ignore (statement d ()))
    );
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
        list ~sep:(sym ',') d ~close:(at ';')
          ~sync:(fun () -> skip_while (fun c -> c <> '(' && c <> ','))
          element
          (fun _ -> '_')
          ()
    )
  in
  Alcotest.(check string)
    "value" "Ok a_"
    (show_result (fun l -> String.of_seq (List.to_seq l)) r);
  Alcotest.check errs "errors"
    [ (2, "expected ',', found '('"); (4, "expected ')', found ';'") ]
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

let item d () =
  if at '(' () then begin
    sym '(' ();
    let v =
      recover d
        ~sync:(fun () -> skip_while (fun c -> c <> ')'))
        (fun () ->
          let a = int_item () in
          sym '+' ();
          Sum (a, int_item ())
        )
        (fun _ -> Gap)
    in
    require d (sym ')') ignore;
    v
  end else
    Num (int_item ())

let items d () =
  sym '[' ();
  let xs =
    list ~sep:(sym ',') d ~close:(at ']')
      ~sync:(skip_nested [ ('(', ')'); ('[', ']') ] (fun c -> c = ','))
      (item d)
      (fun _ -> Gap)
      ()
  in
  require d (sym ']') ignore;
  xs

let run_items input =
  let d = Diagnostics.create () in
  let r = parse ~diagnostics:d input (items d) in
  (show_result (fun l -> String.concat "," (List.map show_item l)) r, entries d)

let test_recover_in_element () =
  let check input expected =
    Alcotest.(check (pair string errs)) input expected (run_items input)
  in
  check "[1, (2 + 3), 4]" ("Ok 1,(2+3),4", []);
  check "[1, (2 + ?), 4]" ("Ok 1,_,4", [ (9, "expected integer, found '?'") ]);
  check "[1 (2 + ?)]"
    ( "Ok 1,_",
      [ (3, "expected ',', found '('"); (8, "expected integer, found '?'") ]
    )

let test_list_in_or_branch () =
  let d = Diagnostics.create () in
  let parser () =
    or_
      (fun () -> String.concat "," (List.map show_item (items d ())))
      (fun () ->
        ignore (consume "[a]");
        "letter"
      )
      ()
  in
  let run input =
    let r = parse ~diagnostics:d input parser in
    (show_result Fun.id r, entries d)
  in
  let check input expected =
    Alcotest.(check (pair string errs)) input expected (run input)
  in
  check "[1, 2]" ("Ok 1,2", []);
  check "[a]" ("Ok _", [ (1, "expected integer, found 'a'") ])

exception Loop

let note msg = Result.get_error (capture (fun () -> (fail msg : unit)))

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
          let l =
            list d ~close:(at '}') ~sync:ignore (element d) (fun _ -> -2) ()
          in
          require d (sym '}') ignore;
          l
      )
    in
    ( show_result (fun l -> String.concat "," (List.map string_of_int l)) r,
      entries d
    )
  in
  let only_require d () =
    guard ();
    let e =
      require d
        (fun () ->
          let v = integer () in
          ws ();
          v
        )
        (fun _ -> -1)
    in
    require d (sym ';') ignore;
    e
  in
  let warns d () =
    guard ();
    if at '@' () then begin
      warn d (note "empty statement");
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
        (show_result show_opts (parse ~diagnostics:d input (opt_braces d)));
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
            ~record:(fun e _ -> calls := e.range.start :: !calls)
            d ~close:(at '}') ~sync:skip_argument expr
            (fun _ -> Hole_expr)
            ()
        in
        require d (sym '}') ignore;
        xs
    )
  in
  Alcotest.(check string) "value" "Ok 1,2,_" (show_result show_exprs r);
  Alcotest.check errs "record replaces every recording" [] (entries d);
  Alcotest.(check (list int))
    "record gets both errors" [ 3; 6 ] (List.rev !calls)

let paren_item () =
  if at '(' () then begin
    sym '(' ();
    let v = int_item () in
    sym ')' ();
    v
  end else
    int_item ()

let paren_list d () =
  sym '[' ();
  let xs =
    list ~sep:(sym ',') d ~close:(at ']')
      ~sync:(skip_nested [ ('(', ')'); ('[', ']') ] (fun c -> c = ','))
      paren_item
      (fun _ -> -1)
      ()
  in
  require d (sym ']') ignore;
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
  let r = parse ~trace ~diagnostics:d "[1 (2" (paren_list d) in
  Alcotest.(check string)
    "value" "Error at 5: expected ')', found end of input"
    (show_result (fun _ -> "") r);
  Alcotest.check errs "no entry" [] (entries d);
  Alcotest.check errs "no event" [] !events;
  let d = Diagnostics.create () in
  let r =
    parse_source ~backtrack_window:3 ~diagnostics:d
      (bytes_source "[1 (2 3) 4]")
      (paren_list d)
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
        require d
          (fun () ->
            warn d (note "inner");
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
  Alcotest.check errs "an entry before a commit in the parser stays"
    [ (0, "inner"); (0, "expected ')', found 'x'") ]
    (entries d);
  let branch d () =
    or_
      (fun () ->
        require d
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
      ()
  in
  let d = Diagnostics.create () in
  Alcotest.(check string)
    "commit in require: without the collector"
    "Error at 1: expected 'b', found 'c'"
    (show_result Fun.id (parse "ac" (branch d)));
  Alcotest.(check string)
    "commit in require: with the collector, it does not reach the or_"
    "Ok right"
    (show_result Fun.id (parse ~diagnostics:d "ac" (branch d)))

let test_require_furthest_failure () =
  let run p =
    let d = Diagnostics.create () in
    let r =
      parse ~diagnostics:d "ax" (fun () ->
          require d p ignore;
          ignore (consume "ax")
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
         ignore (optional a_then_b ());
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
    ( match parse ~diagnostics:d input (program d) with
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
        parse_source ~backtrack_window:64 ~diagnostics:d (bytes_source input)
          (program d)
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
    parse_source ~backtrack_window:32 ~diagnostics:d (bytes_source long)
      (program d)
  with
  | Ok l ->
      Alcotest.(check int) "long input in a small window" 2000 (List.length l);
      Alcotest.check errs "nothing recorded" [] (entries d)
  | Error e ->
      Alcotest.fail (error_message e)

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
          test_case "require" `Quick test_require;
          test_case "require: expected set" `Quick test_require_expected_set;
          test_case "trace" `Quick test_trace;
          test_case "element without progress" `Quick
            test_element_without_progress;
          test_case "without sep, skip past the error" `Quick
            test_skip_past_the_error;
          test_case "an empty element is a value" `Quick
            test_empty_element_is_a_value;
          test_case "record: separator errors" `Quick test_record_separator;
          test_case "a failed step leaves no entry and no event" `Quick
            test_failed_step_leaves_nothing;
          test_case "require: commit in the parser" `Quick test_require_commit;
          test_case "require: the furthest failure decides" `Quick
            test_require_furthest_failure;
          test_case "recover inside an element" `Quick test_recover_in_element;
          test_case "rule A not built: list in a branch of or_" `Quick
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
