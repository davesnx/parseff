let digit_val () = Parseff.expect "a digit (0-9)" Parseff.digit

let digits () = Parseff.many ~at_least:1 digit_val ()
let accumulate_digits () =
  List.fold_left (fun acc d -> (acc * 10) + d) 0 (digits ())

let number_0_255_simple () =
  let n = accumulate_digits () in
  if n >= 0 && n <= 255 then
    n
  else
    Parseff.fail (Printf.sprintf "number %d is out of range (must be 0-255)" n)

let ip_address_simple () =
  let a = number_0_255_simple () in
  let _ = Parseff.expect "a dot separator" (fun () -> Parseff.char '.') in
  let b = number_0_255_simple () in
  let _ = Parseff.expect "a dot separator" (fun () -> Parseff.char '.') in
  let c = number_0_255_simple () in
  let _ = Parseff.expect "a dot separator" (fun () -> Parseff.char '.') in
  let d = number_0_255_simple () in
  (a, b, c, d)

type Parseff.custom_error += Out_of_range of int

let number_0_255_with_error () =
  let n = accumulate_digits () in
  if n >= 0 && n <= 255 then
    n
  else
    Parseff.error (Out_of_range n)

let ip_address_with_custom_errors () =
  let a = number_0_255_with_error () in
  let _ = Parseff.expect "a dot separator" (fun () -> Parseff.char '.') in
  let b = number_0_255_with_error () in
  let _ = Parseff.expect "a dot separator" (fun () -> Parseff.char '.') in
  let c = number_0_255_with_error () in
  let _ = Parseff.expect "a dot separator" (fun () -> Parseff.char '.') in
  let d = number_0_255_with_error () in
  (a, b, c, d)

type expr = Num of int | Add of expr * expr | Mul of expr * expr

let rec expr () =
  let left = term () in
  let rest =
    Parseff.many
      (fun () ->
        let _ = Parseff.whitespace () in
        let _ = Parseff.expect "a '+' operator" (fun () -> Parseff.char '+') in
        let _ = Parseff.whitespace () in
        term ()
      )
      ()
  in
  List.fold_left (fun acc t -> Add (acc, t)) left rest

and term () =
  let left = factor () in
  let rest =
    Parseff.many
      (fun () ->
        let _ = Parseff.whitespace () in
        let _ = Parseff.expect "a '*' operator" (fun () -> Parseff.char '*') in
        let _ = Parseff.whitespace () in
        factor ()
      )
      ()
  in
  List.fold_left (fun acc f -> Mul (acc, f)) left rest

and factor () =
  let _ = Parseff.whitespace () in
  Parseff.or_
    (fun () ->
      let _ =
        Parseff.expect "an opening parenthesis" (fun () -> Parseff.char '(')
      in
      let _ = Parseff.whitespace () in
      let e = expr () in
      let _ = Parseff.whitespace () in
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

let rec expr_to_string = function
  | Num n ->
      string_of_int n
  | Add (l, r) ->
      Printf.sprintf "(%s + %s)" (expr_to_string l) (expr_to_string r)
  | Mul (l, r) ->
      Printf.sprintf "(%s * %s)" (expr_to_string l) (expr_to_string r)

let keyword () =
  Parseff.one_of
    [
      (fun () -> Parseff.consume "if");
      (fun () -> Parseff.consume "else");
      (fun () -> Parseff.consume "while");
    ]
    ()

let literal () =
  Parseff.one_of_labeled
    [
      ("number", fun () -> Num (Parseff.digit ()));
      ( "boolean",
        fun () ->
          Parseff.or_
            (fun () ->
              let _ = Parseff.consume "true" in
              Num 1
            )
            (fun () ->
              let _ = Parseff.consume "false" in
              Num 0
            )
            ()
      );
    ]
    ()

let pairs () =
  let items =
    Parseff.sep_by
      (fun () -> Parseff.consume "ab")
      (fun () -> Parseff.char ',')
      ()
  in
  List.length items

let ident () =
  Parseff.take_while ~at_least:1 ~label:"name" (fun c -> c >= 'a' && c <= 'z')

let number () =
  Parseff.take_while ~at_least:1 ~label:"number" (fun c -> c >= '0' && c <= '9')

let value () =
  Parseff.expect "expression" (fun () ->
      Parseff.or_ (fun () -> number ()) (fun () -> ident ()) ()
  )

let let_statement () =
  let _ = Parseff.consume "let" in
  Parseff.cut ();
  Parseff.skip_whitespace ();
  let name = ident () in
  Parseff.skip_whitespace ();
  let _ = Parseff.char '=' in
  Parseff.skip_whitespace ();
  let v = value () in
  let _ = Parseff.char ';' in
  Printf.sprintf "%s = %s" name v

let expression_statement () =
  let v = value () in
  let _ = Parseff.char ';' in
  v

let statement () =
  Parseff.context "statement" (fun () ->
      Parseff.or_ let_statement expression_statement ()
  )

let statements () =
  Parseff.many
    (fun () ->
      Parseff.skip_whitespace ();
      statement ()
    )
    ()

let format_error e =
  Printf.sprintf "Error at pos %d: %s" e.Parseff.range.start
    (Parseff.error_message e)

let ws () = Parseff.skip_whitespace ()

let comma () =
  ws ();
  ignore (Parseff.char ',')

let rec json_value () =
  Parseff.rec_ (fun () ->
      ws ();
      Parseff.expect "value" (fun () ->
          Parseff.one_of
            [
              (fun () -> ignore (Parseff.consume "null"));
              json_array;
              json_object;
              json_string;
              (fun () -> ignore (number ()));
            ]
            ()
      )
  )

and json_array () =
  Parseff.context ~delimited:true "array" (fun () ->
      ignore (Parseff.char '[');
      ws ();
      ignore (Parseff.sep_by json_value comma ());
      ws ();
      ignore (Parseff.char ']')
  )

and json_object () =
  Parseff.context ~delimited:true "object" (fun () ->
      ignore (Parseff.char '{');
      ws ();
      ignore (Parseff.sep_by json_member comma ());
      ws ();
      ignore (Parseff.char '}')
  )

and json_member () =
  json_string ();
  ws ();
  ignore (Parseff.char ':');
  json_value ()

and json_string () =
  ignore (Parseff.char '"');
  ignore (Parseff.take_while (fun c -> c <> '"'));
  ignore (Parseff.char '"')

let run_json input =
  match Parseff.parse input (fun () -> json_value ()) with
  | Ok () ->
      print_endline "valid"
  | Error e ->
      print_endline (Parseff.render_error ~name:"input.json" ~source:input e)

let run_ip input =
  match Parseff.parse input ip_address_simple with
  | Ok (a, b, c, d) ->
      Printf.printf "%-20s -> %d.%d.%d.%d\n" input a b c d
  | Error e ->
      Printf.printf "%-20s -> %s\n" input (format_error e)

let run_ip_custom input =
  match Parseff.parse input ip_address_with_custom_errors with
  | Ok (a, b, c, d) ->
      Printf.printf "%-20s -> %d.%d.%d.%d\n" input a b c d
  | Error { kind = Custom (Out_of_range n); range; _ } ->
      Printf.printf
        "%-20s -> Custom error at pos %d: octet %d out of range (0-255)\n" input
        range.start n
  | Error e ->
      Printf.printf "%-20s -> %s\n" input (format_error e)

let run_expr input =
  match Parseff.parse input expr with
  | Ok result ->
      Printf.printf "%-20s -> %s\n" input (expr_to_string result)
  | Error e ->
      Printf.printf "%-20s -> %s\n" input (format_error e)

let run_keyword input =
  match Parseff.parse input keyword with
  | Ok s ->
      Printf.printf "%-20s -> %s\n" input s
  | Error e ->
      Printf.printf "%-20s -> %s\n" input (format_error e)

let run_literal input =
  match Parseff.parse input literal with
  | Ok e ->
      Printf.printf "%-20s -> %s\n" input (expr_to_string e)
  | Error e ->
      Printf.printf "%-20s -> %s\n" input (format_error e)

let run_pairs input =
  match Parseff.parse input pairs with
  | Ok n ->
      Printf.printf "%-20s -> %d items\n" input n
  | Error e ->
      Printf.printf "%-20s -> %s\n" input (format_error e)

let run_statements input =
  match Parseff.parse input statements with
  | Ok items ->
      Printf.printf "%-20s -> %s\n" input (String.concat " | " items)
  | Error e ->
      Printf.printf "%-20s -> %s\n" input (format_error e)

let () =
  let cmd = Sys.argv.(1) in
  let input = Sys.argv.(2) in
  match cmd with
  | "ip" ->
      run_ip input
  | "ip-custom" ->
      run_ip_custom input
  | "expr" ->
      run_expr input
  | "keyword" ->
      run_keyword input
  | "literal" ->
      run_literal input
  | "pairs" ->
      run_pairs input
  | "json" ->
      run_json input
  | "statements" ->
      run_statements input
  | _ ->
      Printf.eprintf "Unknown command: %s\n" cmd;
      exit 1
