let ws = Parseff.skip_whitespace

let tok s () =
  ws ();
  ignore (Parseff.consume s)

let bin s () =
  tok s ();
  fun a b -> "(" ^ a ^ s ^ b ^ ")"

let pre s () =
  tok s ();
  fun a -> "(" ^ s ^ a ^ ")"

let post s () =
  tok s ();
  fun a -> "(" ^ a ^ s ^ ")"

let rec expr table () = Parseff.expression ~operand:(operand table) table ()

and operand table () =
  ws ();
  Parseff.or_
    (fun () ->
      tok "(" ();
      let e = Parseff.rec_ (expr table) in
      tok ")" ();
      e
    )
    (fun () -> string_of_int (Parseff.digit ()))
    ()

let chunked_source input =
  let pos = ref 0 in
  Parseff.Source.of_function (fun buf off len ->
      let n = min (min 1 len) (String.length input - !pos) in
      Bytes.blit_string input !pos buf off n;
      pos := !pos + n;
      n
  )

let show = function
  | Ok v ->
      "Ok " ^ v
  | Error (e : Parseff.error) ->
      Printf.sprintf "Error@%d %s" e.range.start (Parseff.error_message e)

let check ?(parser = expr) table input expected =
  Alcotest.(check string)
    ("parse " ^ input) expected
    (show (Parseff.parse input (parser table)));
  Alcotest.(check string)
    ("parse_source " ^ input) expected
    (show (Parseff.parse_source (chunked_source input) (parser table)))

let arith =
  Parseff.
    [
      [ infix_left (bin "+"); infix_left (bin "-") ];
      [ infix_left (bin "*"); infix_left (bin "/") ];
    ]

let test_left_assoc () =
  check arith "1+2+3" "Ok ((1+2)+3)";
  check arith "1-2+3-4" "Ok (((1-2)+3)-4)";
  check arith "7" "Ok 7"

let test_levels_lowest_first () =
  check arith "1+2*3" "Ok (1+(2*3))";
  check arith "1*2+3" "Ok ((1*2)+3)";
  check arith "1-2*3/4+5" "Ok ((1-((2*3)/4))+5)";
  check arith "(1+2)*3" "Ok ((1+2)*3)";
  check arith " ( 1 + 2 ) * 3" "Ok ((1+2)*3)"

let with_pow = arith @ [ [ Parseff.infix_right (bin "^") ] ]

let test_right_assoc () =
  check with_pow "2^3^4" "Ok (2^(3^4))";
  check with_pow "1+2^3*4" "Ok (1+((2^3)*4))";
  check with_pow "(2^3)^4" "Ok ((2^3)^4)";
  check [ [ Parseff.infix_right (bin "-") ] ] "1-2-3" "Ok (1-(2-3))"

let with_neg =
  arith @ [ [ Parseff.prefix (pre "-") ]; [ Parseff.infix_right (bin "^") ] ]

let test_prefix () =
  check with_neg "-1" "Ok (-1)";
  check with_neg "- - 1" "Ok (-(-1))";
  check with_neg "-1+2" "Ok ((-1)+2)";
  check with_neg "1--2" "Ok (1-(-2))";
  check with_neg "2*-3" "Ok (2*(-3))";
  check with_neg "-2^2" "Ok (-(2^2))";
  check with_neg "2^-3^4" "Ok (2^(-(3^4)))";
  check with_neg "-(1+2)" "Ok (-(1+2))"

let test_postfix () =
  let high =
    Parseff.
      [
        [ infix_left (bin "+") ];
        [ infix_left (bin "*") ];
        [ prefix (pre "-") ];
        [ postfix (post "!") ];
      ]
  in
  check high "3!" "Ok (3!)";
  check high "3!!" "Ok ((3!)!)";
  check high "1+2!*3" "Ok (1+((2!)*3))";
  check high "-3!" "Ok (-(3!))";
  check high "(1+2)!" "Ok ((1+2)!)";
  let low =
    Parseff.
      [
        [ infix_left (bin "+") ];
        [ postfix (post "!") ];
        [ infix_left (bin "*") ];
        [ prefix (pre "-") ];
      ]
  in
  check low "1*2!" "Ok ((1*2)!)";
  check low "-3!" "Ok ((-3)!)";
  check low "1+2!" "Ok (1+(2!))";
  check low "1+2!+3" "Ok ((1+(2!))+3)"

let rec hand_expr () = Parseff.fold_left hand_term (bin "+") ()

and hand_term () = Parseff.fold_left hand_atom (bin "*") ()

and hand_atom () =
  ws ();
  Parseff.or_
    (fun () ->
      tok "(" ();
      let e = Parseff.rec_ hand_expr in
      tok ")" ();
      e
    )
    (fun () -> string_of_int (Parseff.digit ()))
    ()

let mul_add = Parseff.[ [ infix_left (bin "+") ]; [ infix_left (bin "*") ] ]

let test_errors_match_hand_written () =
  List.iter
    (fun (input, expected) ->
      check ~parser:(fun _ -> hand_expr) [] input expected;
      check mul_add input expected
    )
    [
      ("1 + * 2", "Error@4 expected digit or \"(\", found '*'");
      ("1 +", "Error@3 expected digit or \"(\", found end of input");
      ("(1+2x", "Error@4 expected \")\", \"*\", or \"+\", found 'x'");
      ("1+2 3", "Error@4 expected \"*\" or \"+\", found '3'");
    ]

let test_errors_with_prefix () =
  check with_neg "1 + * 2" "Error@4 expected digit, \"(\", or \"-\", found '*'"

let test_shared_prefix_infix () =
  let cmp a b = Parseff.[ [ infix_left (bin a); infix_left (bin b) ] ] in
  check (cmp "<" "<=") "1<=2" "Ok (1<=2)";
  check (cmp "<=" "<") "1<=2" "Ok (1<=2)";
  check (cmp "<" "<=") "1<2" "Ok (1<2)";
  check
    Parseff.[ [ infix_left (bin "*") ]; [ infix_left (bin "**") ] ]
    "2**3*4" "Ok ((2**3)*4)"

let test_shared_prefix_postfix () =
  let bang_first = Parseff.[ [ postfix (post "!"); infix_left (bin "!=") ] ] in
  check bang_first "1!=2"
    "Error@2 expected \"!\", \"!=\", or end of input, found '='";
  let neq_first = Parseff.[ [ infix_left (bin "!="); postfix (post "!") ] ] in
  check neq_first "1!=2" "Ok (1!=2)";
  check neq_first "1!" "Ok (1!)"

let test_shared_prefix_prefix () =
  check Parseff.[ [ prefix (pre "-"); prefix (pre "--") ] ] "--1" "Ok (-(-1))";
  check Parseff.[ [ prefix (pre "--"); prefix (pre "-") ] ] "--1" "Ok (--1)"

let test_empty_table () =
  check [] "7" "Ok 7";
  check [ []; [] ] "7" "Ok 7";
  check [] "1+2" "Error@1 expected end of input, found '+'"

let test_inside_many () =
  let p () =
    Parseff.many
      (fun () ->
        let e = expr with_neg () in
        tok ";" ();
        e
      )
      ()
    |> String.concat " "
  in
  check ~parser:(fun _ -> p) [] "1+2;-3*4;" "Ok (1+2) ((-3)*4)";
  check
    ~parser:(fun _ -> p)
    [] "1+2;-3*;" "Error@7 expected digit, \"(\", or \"-\", found ';'"

let test_inside_look_ahead () =
  let p () =
    let e = Parseff.look_ahead (expr arith) in
    let rest = Parseff.take_while (fun _ -> true) in
    e ^ " " ^ rest
  in
  check ~parser:(fun _ -> p) [] "1+2*3" "Ok (1+(2*3)) 1+2*3"

let test_commit_in_operator () =
  let committed s () =
    tok s ();
    Parseff.commit ();
    fun a b -> "(" ^ a ^ s ^ b ^ ")"
  in
  let partial table input =
    show
      (Parseff.parse ~consume:`Partial input (fun () ->
           let e = expr table () in
           e ^ "@" ^ string_of_int (Parseff.position ())
       )
      )
  in
  let hand op () = Parseff.fold_left (operand []) op () in
  let partial_hand op input =
    show
      (Parseff.parse ~consume:`Partial input (fun () ->
           let e = hand op () in
           e ^ "@" ^ string_of_int (Parseff.position ())
       )
      )
  in
  Alcotest.(check string)
    "plain stops" "Ok 1@1"
    (partial Parseff.[ [ infix_left (bin "+") ] ] "1+x");
  Alcotest.(check string)
    "plain stops, fold_left" "Ok 1@1"
    (partial_hand (bin "+") "1+x");
  Alcotest.(check string)
    "committed fails" "Error@2 expected digit or \"(\", found 'x'"
    (partial Parseff.[ [ infix_left (committed "+") ] ] "1+x");
  Alcotest.(check string)
    "committed fails, fold_left" "Error@2 expected digit or \"(\", found 'x'"
    (partial_hand (committed "+") "1+x")

let test_depth_limit () =
  let input = String.concat "" (List.init 200 (fun _ -> "-")) ^ "1" in
  match Parseff.parse input (expr with_neg) with
  | Error { kind = Depth_limit_exceeded { limit = 128 }; _ } ->
      ()
  | r ->
      Alcotest.failf "expected depth limit, got %s" (show r)

let test_commit_in_operand () =
  let operand () =
    let d = Parseff.digit () in
    Parseff.commit ();
    string_of_int d
  in
  let run first input =
    show
      (Parseff.parse ~consume:`Partial input (fun () ->
           Parseff.or_
             (fun () ->
               let e = first () in
               tok ";" ();
               e
             )
             (fun () -> "ALT")
             ()
       )
      )
  in
  let table t () = Parseff.expression ~operand t () in
  let plain = Parseff.[ [ infix_left (bin "+") ] ] in
  let prefixed = Parseff.[ [ infix_left (bin "+") ]; [ prefix (pre "-") ] ] in
  List.iter
    (fun (input, expected) ->
      Alcotest.(check string)
        ("fold_left " ^ input) expected
        (run (Parseff.fold_left operand (bin "+")) input);
      Alcotest.(check string)
        ("no prefix " ^ input) expected
        (run (table plain) input);
      Alcotest.(check string)
        ("prefix " ^ input) expected
        (run (table prefixed) input)
    )
    [
      ("1x", "Error@1 expected \"+\" or \";\", found 'x'");
      ("1+2x", "Error@3 expected \"+\" or \";\", found 'x'");
    ]

let test_prefix_no_progress () =
  let empty () = fun a -> "<" ^ a ^ ">" in
  check Parseff.[ [ prefix empty ] ] "1" "Ok 1";
  check Parseff.[ [ prefix empty; prefix (pre "-") ] ] "-1" "Ok (-1)";
  check Parseff.[ [ infix_left (bin "+") ]; [ prefix empty ] ] "1+2" "Ok (1+2)"

let maybe_digit () =
  match Parseff.optional Parseff.digit () with
  | Some d ->
      string_of_int d
  | None ->
      "_"

let maybe_expr table () = Parseff.expression ~operand:maybe_digit table ()

let test_infix_right_no_progress () =
  let empty () = fun a b -> "(" ^ a ^ "." ^ b ^ ")" in
  let right = Parseff.[ [ infix_right empty ] ] in
  check ~parser:maybe_expr right "" "Ok _";
  check ~parser:maybe_expr right "1" "Ok 1";
  check ~parser:maybe_expr right "12" "Ok (1.2)";
  check ~parser:maybe_expr right "123" "Ok (1.(2.3))";
  check ~parser:maybe_expr
    Parseff.[ [ infix_left (bin "+") ]; [ infix_right empty ] ]
    "1+2" "Ok (1+2)";
  check ~parser:maybe_expr
    Parseff.[ [ infix_right (bin "^"); infix_right empty ] ]
    "1^2" "Ok (1^2)"

let test_infix_right_no_progress_undo () =
  let notes = Parseff.Diagnostics.create () in
  let count = Parseff.Ref.create 0 in
  let empty () =
    Parseff.warn notes "empty";
    Parseff.Ref.set count (Parseff.Ref.get count + 1);
    fun a b -> "(" ^ a ^ "." ^ b ^ ")"
  in
  let parser () =
    let e = maybe_expr Parseff.[ [ infix_right empty ] ] () in
    (e, Parseff.Ref.get count)
  in
  match Parseff.parse ~diagnostics:notes "1" parser with
  | Ok (e, n) ->
      Alcotest.(check string) "value" "1" e;
      Alcotest.(check int) "sets undone" 0 n;
      Alcotest.(check int) "warnings undone" 0 (Parseff.Diagnostics.length notes)
  | Error e ->
      Alcotest.fail (Parseff.error_message e)

let test_prefix_no_progress_sealed () =
  let sealed seal () =
    seal ();
    fun a -> "<" ^ a ^ ">"
  in
  List.iter
    (fun seal ->
      check Parseff.[ [ prefix (sealed seal) ] ] "1" "Ok 1";
      check
        Parseff.[ [ prefix (sealed seal); prefix (pre "-") ] ]
        "-1" "Ok (-1)";
      check
        Parseff.[ [ prefix (sealed seal) ] ]
        "x" "Error@0 expected digit or \"(\", found 'x'"
    )
    [ Parseff.commit; Parseff.cut ]

let int_op c f () =
  ignore (Parseff.char c);
  f

let int_table =
  Parseff.
    [
      [ infix_left (int_op '+' ( + )); infix_left (int_op '-' ( - )) ];
      [ infix_left (int_op '*' ( * )) ];
    ]

let int_expr () = Parseff.expression ~operand:Parseff.digit int_table ()

let int_hand () =
  Parseff.fold_left
    (Parseff.fold_left Parseff.digit (int_op '*' ( * )))
    (fun () -> Parseff.or_ (int_op '+' ( + )) (int_op '-' ( - )) ())
    ()

let test_long_flat () =
  let input = String.concat "+" (List.init 100_000 (fun _ -> "1")) in
  match Parseff.parse input int_expr with
  | Ok n ->
      Alcotest.(check int) "sum" 100_000 n
  | Error e ->
      Alcotest.fail (Parseff.error_message e)

let test_streaming_window () =
  let b = Buffer.create 10_000 in
  for i = 0 to 4_999 do
    if i > 0 then
      Buffer.add_char b (match i mod 3 with 0 -> '+' | 1 -> '*' | _ -> '-');
    Buffer.add_char b (Char.chr (Char.code '0' + (i mod 10)))
  done;
  let input = Buffer.contents b in
  let stream p =
    match
      Parseff.parse_source ~backtrack_window:64 (chunked_source input) p
    with
    | Ok n ->
        string_of_int n
    | Error e ->
        Parseff.error_message e
  in
  let reference = stream int_hand in
  Alcotest.(check bool)
    "reference is a number" true
    (int_of_string_opt reference <> None);
  Alcotest.(check string) "table" reference (stream int_expr);
  Alcotest.(check string)
    "parse" reference
    ( match Parseff.parse input int_expr with
    | Ok n ->
        string_of_int n
    | Error e ->
        Parseff.error_message e
    )

module Doc_example = struct
  open Parseff

  type expr = Num of int | Neg of expr | Bin of char * expr * expr

  let token c () =
    skip_whitespace ();
    ignore (char c)

  let bin c () =
    token c ();
    fun a b -> Bin (c, a, b)

  let neg () =
    token '-' ();
    fun a -> Neg a

  let table =
    [
      [ infix_left (bin '+'); infix_left (bin '-') ];
      [ infix_left (bin '*'); infix_left (bin '/') ];
      [ prefix neg ];
      [ infix_right (bin '^') ];
    ]

  let rec expr () = expression ~operand table ()

  and operand () =
    skip_whitespace ();
    or_
      (fun () ->
        token '(' ();
        let e = rec_ expr in
        token ')' ();
        e
      )
      (fun () -> Num (digit ()))
      ()
end

let test_doc_example () =
  let open Doc_example in
  let ok input expected =
    match Parseff.parse input expr with
    | Ok e ->
        Alcotest.(check bool) input true (e = expected)
    | Error e ->
        Alcotest.fail (Parseff.error_message e)
  in
  ok "1+2*3" (Bin ('+', Num 1, Bin ('*', Num 2, Num 3)));
  ok "-2^2" (Neg (Bin ('^', Num 2, Num 2)))

module Guide_example = struct
  type expr = Num of int | Add of expr * expr | Mul of expr * expr

  let add () =
    Parseff.skip_whitespace ();
    let _ = Parseff.expect "a '+' operator" (fun () -> Parseff.char '+') in
    fun a b -> Add (a, b)

  let mul () =
    Parseff.skip_whitespace ();
    let _ = Parseff.expect "a '*' operator" (fun () -> Parseff.char '*') in
    fun a b -> Mul (a, b)

  let table = [ [ Parseff.infix_left add ]; [ Parseff.infix_left mul ] ]

  let rec expr () = Parseff.expression ~operand:factor table ()

  and factor () =
    Parseff.skip_whitespace ();
    Parseff.or_
      (fun () ->
        let _ =
          Parseff.expect "an opening parenthesis" (fun () -> Parseff.char '(')
        in
        Parseff.skip_whitespace ();
        let e = Parseff.rec_ expr in
        Parseff.skip_whitespace ();
        let _ =
          Parseff.expect "a closing parenthesis" (fun () -> Parseff.char ')')
        in
        e
      )
      (fun () ->
        let d = Parseff.expect "a number" Parseff.digit in
        Num d
      )
      ()
end

let test_guide_example () =
  let msg input =
    match Parseff.parse input Guide_example.expr with
    | Ok _ ->
        "ok"
    | Error e ->
        Parseff.error_message e
  in
  Alcotest.(check bool)
    "1+2*3" true
    (Parseff.parse "1+2*3" Guide_example.expr
    = Ok Guide_example.(Add (Num 1, Mul (Num 2, Num 3)))
    );
  Alcotest.(check string)
    "1+x" "expected a number or an opening parenthesis, found 'x'" (msg "1+x");
  Alcotest.(check string)
    "(1+2x"
    "expected a '*' operator, a '+' operator, or a closing parenthesis, found \
     'x'"
    (msg "(1+2x")

let () =
  let open Alcotest in
  run "Expression"
    [
      ( "infix",
        [
          test_case "left associative" `Quick test_left_assoc;
          test_case "levels lowest first" `Quick test_levels_lowest_first;
          test_case "right associative" `Quick test_right_assoc;
        ]
      );
      ( "prefix and postfix",
        [
          test_case "prefix" `Quick test_prefix;
          test_case "postfix" `Quick test_postfix;
        ]
      );
      ( "errors",
        [
          test_case "same as fold_left" `Quick test_errors_match_hand_written;
          test_case "prefix in expected set" `Quick test_errors_with_prefix;
        ]
      );
      ( "shared prefix",
        [
          test_case "infix" `Quick test_shared_prefix_infix;
          test_case "postfix and infix" `Quick test_shared_prefix_postfix;
          test_case "prefix" `Quick test_shared_prefix_prefix;
        ]
      );
      ( "composition",
        [
          test_case "empty table" `Quick test_empty_table;
          test_case "inside many" `Quick test_inside_many;
          test_case "inside look_ahead" `Quick test_inside_look_ahead;
          test_case "commit in operator" `Quick test_commit_in_operator;
          test_case "depth limit" `Quick test_depth_limit;
          test_case "commit in operand" `Quick test_commit_in_operand;
          test_case "prefix without progress" `Quick test_prefix_no_progress;
          test_case "infix_right without progress" `Quick
            test_infix_right_no_progress;
          test_case "infix_right without progress undoes" `Quick
            test_infix_right_no_progress_undo;
          test_case "prefix without progress, committed" `Quick
            test_prefix_no_progress_sealed;
          test_case "long flat chain" `Quick test_long_flat;
          test_case "streaming window" `Quick test_streaming_window;
          test_case "doc example" `Quick test_doc_example;
          test_case "guide example" `Quick test_guide_example;
        ]
      );
    ]
