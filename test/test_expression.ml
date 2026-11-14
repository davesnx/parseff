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

module Precedence = struct
  let lookups = ref 0

  let peeked table () =
    incr lookups;
    ws ();
    match Parseff.peek_char () with
    | Some c ->
        List.assoc_opt c table
    | None ->
        None

  let read s f () =
    ignore (Parseff.consume s);
    f

  let binary s = read s (fun a b -> "(" ^ a ^ s ^ b ^ ")")

  let table =
    Parseff.
      [
        ('+', (1, infix_left (binary "+")));
        ('-', (1, infix_left (binary "-")));
        ('*', (2, infix_left (binary "*")));
        ('/', (2, infix_left (binary "/")));
        ('^', (4, infix_right (binary "^")));
        ('!', (5, postfix (read "!" (fun a -> "(" ^ a ^ "!)"))));
      ]

  let rec expr operator () =
    Parseff.precedence ~operand:(operand operator) ~operator ()

  and operand operator () =
    ws ();
    match Parseff.peek_char () with
    | Some '-' ->
        ignore (Parseff.any_char ());
        let e =
          Parseff.rec_
            (Parseff.precedence ~min:3 ~operand:(operand operator) ~operator)
        in
        "(-" ^ e ^ ")"
    | Some '(' ->
        ignore (Parseff.any_char ());
        let e = Parseff.rec_ (expr operator) in
        tok ")" ();
        e
    | _ ->
        Parseff.expect "expression" (fun () -> string_of_int (Parseff.digit ()))

  let check ?(operator = peeked table) input expected =
    check ~parser:(fun _ -> expr operator) [] input expected

  let consumed () =
    ws ();
    match Parseff.take_while (fun c -> String.contains "<=>" c) with
    | "<" ->
        Some (1, Parseff.infix_left (fun () a b -> "(" ^ a ^ "<" ^ b ^ ")"))
    | "<=" ->
        Some (1, Parseff.infix_left (fun () a b -> "(" ^ a ^ "<=" ^ b ^ ")"))
    | "=>" ->
        Some (0, Parseff.infix_right (fun () a b -> "(" ^ a ^ "=>" ^ b ^ ")"))
    | _ ->
        None

  let partial ?(operator = peeked table) input =
    show
      (Parseff.parse ~consume:`Partial input (fun () ->
           let e = expr operator () in
           e ^ "@" ^ string_of_int (Parseff.position ())
       )
      )

  let test_precedence () =
    check "7" "Ok 7";
    check "1+2+3" "Ok ((1+2)+3)";
    check "1-2*3/4+5" "Ok ((1-((2*3)/4))+5)";
    check " ( 1 + 2 ) * 3" "Ok ((1+2)*3)";
    check "2^3^4" "Ok (2^(3^4))";
    check "1+2^3*4" "Ok (1+((2^3)*4))";
    check "1*2^3^4+5" "Ok ((1*(2^(3^4)))+5)"

  let test_prefix_postfix () =
    check "-2^2" "Ok (-(2^2))";
    check "-2*3" "Ok ((-2)*3)";
    check "2^-3^2" "Ok (2^(-(3^2)))";
    check "- -1+2" "Ok ((-(-1))+2)";
    check "1+2!!*3" "Ok (1+(((2!)!)*3))";
    check "2^3!" "Ok (2^(3!))"

  let test_operator_reads_token () =
    check ~operator:consumed "1<2<=3" "Ok ((1<2)<=3)";
    check ~operator:consumed "1=>2<3=>4" "Ok (1=>((2<3)=>4))";
    Alcotest.(check string)
      "unknown token" "Ok (1<2)@3"
      (partial ~operator:consumed "1<2 == 3")

  let test_one_lookup () =
    lookups := 0;
    Alcotest.(check string)
      "value" "Ok ((1*(2^3))+((4*5)*6))@11" (partial "1*2^3+4*5*6");
    Alcotest.(check int) "one lookup after each operand" 6 !lookups

  let test_position_restored () =
    Alcotest.(check string) "no operator" "Ok (1+2)@3" (partial "1+2  )");
    Alcotest.(check string) "operand fails" "Ok (1+2)@3" (partial "1+2 * )");
    Alcotest.(check string) "below min" "Ok ((-(2^3))+1)@6" (partial "-2^3+1");
    let failing () =
      ws ();
      if Parseff.peek_char () = Some '+' then
        Some (1, Parseff.infix_left (binary "+"))
      else
        Some (1, Parseff.infix_left (read (Parseff.consume "#") (fun a _ -> a)))
    in
    Alcotest.(check string)
      "operator fails" "Ok (1+2)@3"
      (partial ~operator:failing "1+2  )")

  let condition operator () =
    let e = expr operator () in
    tok "then" ();
    e

  let test_expected_set () =
    let msg input = show (Parseff.parse input (condition (peeked table))) in
    Alcotest.(check string)
      "entry 4" "Error@6 expected \"then\", found 'x'" (msg "1 + 2 x");
    Alcotest.(check string)
      "entry 5, after an operator" "Error@4 expected expression, found ')'"
      (msg "1 + )");
    Alcotest.(check string)
      "entry 5, at the start" "Error@0 expected expression, found ')'" (msg ")")

  let test_long_chains () =
    List.iter
      (fun op ->
        let input = String.concat op (List.init 100_000 (fun _ -> "1")) in
        let operator () =
          match Parseff.peek_char () with
          | Some '+' ->
              Some (1, Parseff.infix_left (read "+" ( + )))
          | Some '^' ->
              Some (2, Parseff.infix_right (read "^" ( + )))
          | _ ->
              None
        in
        let p = Parseff.precedence ~operand:Parseff.digit ~operator in
        List.iter
          (fun (name, r) ->
            match r with
            | Ok n ->
                Alcotest.(check int) (name ^ ", chain of " ^ op) 100_000 n
            | Error e ->
                Alcotest.fail (Parseff.error_message e)
          )
          [
            ("parse", Parseff.parse input p);
            ( "parse_source",
              Parseff.parse_source ~backtrack_window:64 (chunked_source input) p
            );
          ]
      )
      [ "+"; "^" ]

  let test_commit_in_operator () =
    let committed () =
      ws ();
      match Parseff.peek_char () with
      | Some '+' ->
          Some
            ( 1,
              Parseff.infix_left (fun () ->
                  ignore (Parseff.any_char ());
                  Parseff.commit ();
                  fun a b -> "(" ^ a ^ "+" ^ b ^ ")"
              )
            )
      | _ ->
          None
    in
    Alcotest.(check string)
      "committed" "Error@4 expected expression, found ')'"
      (partial ~operator:committed "1+2+)");
    Alcotest.(check string) "not committed" "Ok (1+2)@3" (partial "1+2+)")

  let test_no_progress () =
    let empty kind () = Some (1, kind (fun () a b -> "(" ^ a ^ b ^ ")")) in
    let run kind =
      show
        (Parseff.parse ~consume:`Partial "12" (fun () ->
             Parseff.precedence
               ~operand:(fun () ->
                 Parseff.optional Parseff.digit ()
                 |> Option.fold ~none:"_" ~some:string_of_int
               )
               ~operator:(empty kind) ()
         )
        )
    in
    Alcotest.(check string) "left" "Ok (12)" (run Parseff.infix_left);
    Alcotest.(check string) "right" "Ok (12)" (run Parseff.infix_right)

  let test_prefix_rejected () =
    let operator () = Some (1, Parseff.prefix (fun () a -> a)) in
    Alcotest.check_raises "prefix"
      (Invalid_argument
         "Parseff.precedence: operator returned a prefix operator"
      ) (fun () ->
        ignore
          (Parseff.parse "1"
             (Parseff.precedence ~operand:Parseff.digit ~operator)
          )
    )

  let test_prefix_below_min () =
    let operator () = Some (1, Parseff.prefix (fun () a -> a)) in
    Alcotest.check_raises "prefix below min"
      (Invalid_argument
         "Parseff.precedence: operator returned a prefix operator"
      ) (fun () ->
        ignore
          (Parseff.parse ~consume:`Partial "1+"
             (Parseff.precedence ~min:2 ~operand:Parseff.digit ~operator)
          )
    )

  let test_precedence_limits () =
    let run left right input =
      let operator () =
        match Parseff.peek_char () with
        | Some '-' ->
            Some (left, Parseff.infix_left (binary "-"))
        | Some '^' ->
            Some (right, Parseff.infix_right (binary "^"))
        | _ ->
            None
      in
      show
        (Parseff.parse input
           (Parseff.precedence
              ~operand:(fun () -> string_of_int (Parseff.digit ()))
              ~operator
           )
        )
    in
    Alcotest.(check string)
      "infix_left at max_int" "Ok ((1-2)-3)"
      (run max_int min_int "1-2-3");
    Alcotest.(check string)
      "infix_left at max_int over infix_right" "Ok (1^((2-3)-4))"
      (run max_int min_int "1^2-3-4");
    Alcotest.(check string)
      "infix_right at max_int" "Ok (1^(2^3))"
      (run min_int max_int "1^2^3");
    Alcotest.(check string)
      "infix_right at min_int" "Ok (1^(2^3))"
      (run max_int min_int "1^2^3");
    Alcotest.(check string)
      "infix_left at min_int" "Ok ((1-2)-3)"
      (run min_int max_int "1-2-3");
    Alcotest.(check string)
      "both at max_int" "Ok ((1-2)^(3^4))"
      (run max_int max_int "1-2^3^4")

  let test_commit_in_build () =
    let committing s () =
      tok s ();
      fun a b ->
        Parseff.commit ();
        "(" ^ a ^ s ^ b ^ ")"
    in
    let table =
      Parseff.
        [ [ infix_left (committing "+") ]; [ infix_left (committing "*") ] ]
    in
    let operator () =
      ws ();
      match Parseff.peek_char () with
      | Some '+' ->
          Some (1, Parseff.infix_left (committing "+"))
      | Some '*' ->
          Some (2, Parseff.infix_left (committing "*"))
      | _ ->
          None
    in
    let digit () =
      ws ();
      string_of_int (Parseff.digit ())
    in
    let by_table = Parseff.expression ~operand:digit table in
    let by_lookup = Parseff.precedence ~operand:digit ~operator in
    let run p input =
      show
        (Parseff.parse ~consume:`Partial input (fun () ->
             Parseff.or_
               (fun () ->
                 let e = p () in
                 let stop = Parseff.position () in
                 ignore (Parseff.char ';');
                 e ^ "@" ^ string_of_int stop
               )
               (fun () -> "other")
               ()
         )
        )
    in
    let partial p =
      show
        (Parseff.parse ~consume:`Partial "1*2+)" (fun () ->
             let e = p () in
             e ^ "@" ^ string_of_int (Parseff.position ())
         )
        )
    in
    Alcotest.(check string) "expression stops" "Ok (1*2)@3" (partial by_table);
    Alcotest.(check string) "precedence stops" "Ok (1*2)@3" (partial by_lookup);
    List.iter
      (fun (input, expected) ->
        Alcotest.(check string)
          ("expression " ^ input) expected (run by_table input);
        Alcotest.(check string)
          ("precedence " ^ input) expected (run by_lookup input)
      )
      [
        ("1*2+);", "Ok other");
        ("1*2+3;", "Ok ((1*2)+3)@5");
        ("1*2+3", "Ok other");
        ("1+2*3", "Ok other");
        ("1*2", "Ok other");
        ("1+2*)", "Ok other");
      ]

  let test_lookup_again () =
    let run note =
      let count = ref 0 in
      let d = Parseff.Diagnostics.create () in
      let cell = Parseff.Ref.create 0 in
      let operator () =
        incr count;
        match Parseff.peek_char () with
        | Some '+' ->
            note d cell;
            Some (1, Parseff.infix_left (binary "+"))
        | Some '*' ->
            Some (2, Parseff.infix_left (binary "*"))
        | _ ->
            None
      in
      let r =
        Parseff.parse ~diagnostics:d "1*2+3"
          (Parseff.precedence
             ~operand:(fun () -> string_of_int (Parseff.digit ()))
             ~operator
          )
      in
      Printf.sprintf "%s, %d lookups" (show r) !count
    in
    Alcotest.(check string)
      "no note" "Ok ((1*2)+3), 3 lookups"
      (run (fun _ _ -> ()));
    Alcotest.(check string)
      "diagnostic" "Ok ((1*2)+3), 4 lookups"
      (run (fun d _ -> Parseff.warn d "lookup"));
    Alcotest.(check string)
      "Ref.set" "Ok ((1*2)+3), 4 lookups"
      (run (fun _ cell -> Parseff.Ref.set cell 1))

  let test_commit_in_lookup () =
    let run seal mode ~min ~noisy input =
      let d = Parseff.Diagnostics.create () in
      let less () = fun a b -> "(" ^ a ^ "<" ^ b ^ ")" in
      let operator () =
        match Parseff.peek_char () with
        | Some '*' ->
            Some (5, Parseff.infix_left (binary "*"))
        | _ -> (
            ignore (Parseff.char '<');
            seal ();
            for _ = 1 to 7 do
              ignore (Parseff.char '<')
            done;
            if noisy then Parseff.warn d "seen";
            match mode with
            | `Low ->
                Some (1, Parseff.infix_left less)
            | `None ->
                None
            | `Fail ->
                ignore (Parseff.char '#');
                None
          )
      in
      let p () =
        let v =
          Parseff.precedence ~min
            ~operand:(fun () -> String.make 1 (Parseff.any_char ()))
            ~operator ()
        in
        let stop = Parseff.position () in
        let b = Buffer.create 16 in
        while Parseff.peek_char () <> None do
          Buffer.add_char b (Parseff.any_char ())
        done;
        Printf.sprintf "%s@%d %s" v stop (Buffer.contents b)
      in
      ( show (Parseff.parse ~diagnostics:d input p),
        show
          (Parseff.parse_source ~diagnostics:d ~backtrack_window:16
             (chunked_source input) p
          )
      )
    in
    List.iter
      (fun (seal_name, seal) ->
        List.iter
          (fun (name, mode, min, noisy, input, expected) ->
            let from_string, from_source = run seal mode ~min ~noisy input in
            let name = seal_name ^ ", " ^ name in
            Alcotest.(check string) (name ^ ", parse") expected from_string;
            Alcotest.(check string)
              (name ^ ", parse_source") expected from_source
          )
          [
            ("below min", `Low, 2, false, "1<<<<<<<<2", "Ok 1@1 <<<<<<<<2");
            ("no operator", `None, 2, false, "1<<<<<<<<2", "Ok 1@1 <<<<<<<<2");
            ("lookup fails", `Fail, 2, false, "1<<<<<<<<2", "Ok 1@1 <<<<<<<<2");
            ( "no operator after an operator",
              `None,
              0,
              false,
              "1*2<<<<<<<<3",
              "Ok (1*2)@3 <<<<<<<<3"
            );
            ( "lower operator",
              `Low,
              0,
              false,
              "1*2<<<<<<<<3",
              "Ok ((1*2)<3)@12 "
            );
            ( "lower operator, lookup runs again",
              `Low,
              0,
              true,
              "1*2<<<<<<<<3",
              "Ok ((1*2)<3)@12 "
            );
          ]
      )
      [ ("commit", Parseff.commit); ("cut", Parseff.cut) ]

  let test_build_fails () =
    let failing s () =
      tok s ();
      fun a b ->
        ignore (Parseff.char '#');
        "(" ^ a ^ s ^ b ^ ")"
    in
    let failing_post () =
      tok "!" ();
      fun a ->
        ignore (Parseff.char '#');
        "(" ^ a ^ "!)"
    in
    let table =
      Parseff.[ [ infix_left (failing "+") ]; [ postfix failing_post ] ]
    in
    let operator () =
      match Parseff.peek_char () with
      | Some '+' ->
          Some (1, Parseff.infix_left (failing "+"))
      | Some '-' ->
          Some (max_int, Parseff.infix_left (failing "-"))
      | Some '!' ->
          Some (2, Parseff.postfix failing_post)
      | _ ->
          None
    in
    let digit () = string_of_int (Parseff.digit ()) in
    let run p input =
      show
        (Parseff.parse ~consume:`Partial input (fun () ->
             let e = p () in
             e ^ "@" ^ string_of_int (Parseff.position ())
         )
        )
    in
    let by_table = Parseff.expression ~operand:digit table in
    let by_lookup = Parseff.precedence ~operand:digit ~operator in
    Alcotest.(check string)
      "expression drops the infix operator" "Ok 1@1" (run by_table "1+2");
    Alcotest.(check string)
      "expression drops the postfix operator" "Ok 1@1" (run by_table "1!");
    Alcotest.(check string)
      "precedence fails for a deferred build function"
      "Error@3 expected '#', found end of input" (run by_lookup "1+2");
    Alcotest.(check string)
      "precedence drops the postfix operator" "Ok 1@1" (run by_lookup "1!");
    Alcotest.(check string)
      "precedence drops infix_left at max_int" "Ok 1@1" (run by_lookup "1-2")

  let test_build_effects () =
    let run ~noisy input =
      let d = Parseff.Diagnostics.create () in
      let noted s () =
        ignore (Parseff.consume s);
        fun a b ->
          Parseff.warn d (Printf.sprintf "%s%s%s" a s b);
          "(" ^ a ^ s ^ b ^ ")"
      in
      let operator () =
        ws ();
        match Parseff.peek_char () with
        | Some '+' ->
            if noisy then Parseff.warn d "lookup +";
            Some (1, Parseff.infix_left (noted "+"))
        | Some '*' ->
            Some (2, Parseff.infix_left (noted "*"))
        | _ ->
            None
      in
      let r =
        Parseff.parse ~consume:`Partial ~diagnostics:d input (fun () ->
            Parseff.precedence
              ~operand:
                Parseff.(
                  fun () ->
                    ws ();
                    string_of_int (digit ())
                )
              ~operator ()
        )
      in
      show r
      :: List.map
           (fun (e : string Parseff.Diagnostics.entry) ->
             Printf.sprintf "%d:%s" e.pos e.value
           )
           (Parseff.Diagnostics.to_list d)
      |> String.concat " "
    in
    Alcotest.(check string)
      "build runs at the end of its right operand"
      "Ok ((1*2)+3) 3:1*2 10:(1*2)+3"
      (run ~noisy:false "1*2   +  3");
    Alcotest.(check string)
      "build kept when the next operator is dropped" "Ok (1*2) 3:1*2"
      (run ~noisy:false "1*2   +  )");
    Alcotest.(check string)
      "lookup effects dropped with the operator" "Ok (1*2) 3:1*2"
      (run ~noisy:true "1*2   +  )");
    Alcotest.(check string)
      "lookup effects kept with the operator"
      "Ok ((1*2)+3) 3:1*2 6:lookup + 10:(1*2)+3"
      (run ~noisy:true "1*2   +  3")

  let test_same_as_expression () =
    let rs = Random.State.make [| 42 |] in
    for _ = 1 to 500 do
      let levels = 1 + Random.State.int rs 5 in
      let ops =
        List.init levels (fun i ->
            let c = "+*^<&".[i] in
            (c, Random.State.bool rs)
        )
      in
      let table =
        List.map
          (fun (c, right) ->
            let s = String.make 1 c in
            [
              ( if right then
                  Parseff.infix_right
                else
                  Parseff.infix_left
              )
                (bin s);
            ]
          )
          ops
        @ [ [ Parseff.postfix (post "!") ] ]
      in
      let lookup =
        ('!', (levels, Parseff.postfix (read "!" (fun a -> "(" ^ a ^ "!)"))))
        :: List.mapi
             (fun i (c, right) ->
               ( c,
                 ( i,
                   ( if right then
                       Parseff.infix_right
                     else
                       Parseff.infix_left
                   )
                     (binary (String.make 1 c))
                 )
               )
             )
             ops
      in
      let b = Buffer.create 64 in
      let atoms = "123(!+*^<&) " in
      for _ = 0 to Random.State.int rs 30 do
        Buffer.add_char b atoms.[Random.State.int rs (String.length atoms)]
      done;
      let input = Buffer.contents b in
      let run p =
        show
          (Parseff.parse ~consume:`Partial input (fun () ->
               let e = p () in
               e ^ "@" ^ string_of_int (Parseff.position ())
           )
          )
      in
      let rec by_table () =
        Parseff.expression ~operand:(paren by_table) table ()
      and by_lookup () =
        Parseff.precedence ~operand:(paren by_lookup) ~operator:(peeked lookup)
          ()
      and paren e () =
        ws ();
        Parseff.or_
          (fun () ->
            tok "(" ();
            let v = Parseff.rec_ e in
            tok ")" ();
            v
          )
          (fun () -> string_of_int (Parseff.digit ()))
          ()
      in
      let outcome r =
        if String.starts_with ~prefix:"Ok" r then
          r
        else
          List.hd (String.split_on_char ' ' r)
      in
      Alcotest.(check string)
        (Printf.sprintf "%S" input)
        (outcome (run by_table))
        (outcome (run by_lookup))
    done

  module Doc_example = struct
    open Parseff

    let read build () =
      ignore (any_char ());
      build

    let add = infix_left (read ( + ))
    let mul = infix_left (read ( * ))
    let pow = infix_right (read (fun a b -> int_of_float (float a ** float b)))

    let operator () =
      skip_whitespace ();
      match peek_char () with
      | Some '+' ->
          Some (1, add)
      | Some '*' ->
          Some (2, mul)
      | Some '^' ->
          Some (4, pow)
      | _ ->
          None

    let rec expr () = precedence ~operand ~operator ()

    and operand () =
      skip_whitespace ();
      match peek_char () with
      | Some '-' ->
          ignore (any_char ());
          -rec_ (precedence ~min:3 ~operand ~operator)
      | Some '(' ->
          ignore (any_char ());
          let e = rec_ expr in
          skip_whitespace ();
          ignore (char ')');
          e
      | _ ->
          digit ()
  end

  let test_doc_example () =
    let ok input expected =
      match Parseff.parse input Doc_example.expr with
      | Ok v ->
          Alcotest.(check int) input expected v
      | Error e ->
          Alcotest.fail (Parseff.error_message e)
    in
    ok "1 + 2 * 3" 7;
    ok "-2 ^ 2 + 1" (-3);
    ok "2 ^ 3 ^ 2" 512;
    ok "(1 + 2) * -3" (-9)

  let tests =
    let open Alcotest in
    [
      test_case "precedence and associativity" `Quick test_precedence;
      test_case "prefix with min, postfix" `Quick test_prefix_postfix;
      test_case "operator reads the token" `Quick test_operator_reads_token;
      test_case "one lookup after each operand" `Quick test_one_lookup;
      test_case "position restored" `Quick test_position_restored;
      test_case "expected set" `Quick test_expected_set;
      test_case "long chains use no depth" `Quick test_long_chains;
      test_case "commit in operator" `Quick test_commit_in_operator;
      test_case "operator and operand without progress" `Quick test_no_progress;
      test_case "prefix rejected" `Quick test_prefix_rejected;
      test_case "prefix rejected below min" `Quick test_prefix_below_min;
      test_case "precedence at max_int and min_int" `Quick
        test_precedence_limits;
      test_case "commit in build function" `Quick test_commit_in_build;
      test_case "lookup with effects runs again" `Quick test_lookup_again;
      test_case "commit and cut in the lookup" `Quick test_commit_in_lookup;
      test_case "build function fails" `Quick test_build_fails;
      test_case "build effects" `Quick test_build_effects;
      test_case "same as expression" `Quick test_same_as_expression;
      test_case "doc example" `Quick test_doc_example;
    ]
end

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
      ("precedence", Precedence.tests);
    ]
