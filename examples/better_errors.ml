let digit_val () = Parseff.expect "a digit (0-9)" Parseff.digit

let number_0_255_simple () =
  let digits = Parseff.many1 digit_val () in
  let n = List.fold_left (fun acc d -> (acc * 10) + d) 0 digits in
  if n >= 0 && n <= 255 then n
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
  Parseff.end_of_input ();
  (a, b, c, d)

let number_0_255_with_error () =
  let digits = Parseff.many1 digit_val () in
  let n = List.fold_left (fun acc d -> (acc * 10) + d) 0 digits in
  if n >= 0 && n <= 255 then n else Parseff.error (`Out_of_range n)

let ip_address_with_custom_errors () =
  let a = number_0_255_with_error () in
  let _ = Parseff.expect "a dot separator" (fun () -> Parseff.char '.') in
  let b = number_0_255_with_error () in
  let _ = Parseff.expect "a dot separator" (fun () -> Parseff.char '.') in
  let c = number_0_255_with_error () in
  let _ = Parseff.expect "a dot separator" (fun () -> Parseff.char '.') in
  let d = number_0_255_with_error () in
  Parseff.end_of_input ();
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
        term ())
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
        factor ())
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
      e)
    (fun () ->
      let d = Parseff.expect "a number" Parseff.digit in
      Num d)
    ()

let rec expr_to_string = function
  | Num n -> string_of_int n
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
              Num 1)
            (fun () ->
              let _ = Parseff.consume "false" in
              Num 0)
            () );
    ]
    ()

let () =
  Printf.printf "Better Error Messages Example\n";
  Printf.printf "==============================\n\n";

  Printf.printf "Example 1: Using 'expect' for clear error messages\n";
  Printf.printf "---------------------------------------------------\n";
  let test_cases_simple =
    [
      ("192.168.1.1", true);
      ("192.168.1.256", false);
      ("192.168.1", false);
      ("192.168.1.", false);
    ]
  in
  List.iter
    (fun (input, should_succeed) ->
      match Parseff.parse input ip_address_simple with
      | Ok ((a, b, c, d)) ->
          Printf.printf "✓ %-20s -> %d.%d.%d.%d\n" input a b c d
      | Error { pos; error = `Expected expected } ->
          Printf.printf "%s %-20s -> Error at pos %d: %s\n"
            (if should_succeed then "✗" else "✓")
            input pos expected
      | Error _ -> Printf.printf "✗ Unknown error\n")
    test_cases_simple;

  Printf.printf "\nExample 2: Custom error types with polymorphic variants\n";
  Printf.printf "--------------------------------------------------------\n";
  let test_cases_custom = [ ("192.168.1.300", false); ("192.168.1.1", true) ] in
  List.iter
    (fun (input, should_succeed) ->
      match Parseff.parse input ip_address_with_custom_errors with
      | Ok ((a, b, c, d)) ->
          Printf.printf "✓ %-20s -> %d.%d.%d.%d\n" input a b c d
      | Error { pos; error = `Expected expected } ->
          Printf.printf "%s %-20s -> Parse error at pos %d: %s\n"
            (if should_succeed then "✗" else "✓")
            input pos expected
      | Error { pos; error = `Out_of_range n } ->
          Printf.printf
            "✓ %-20s -> Custom error at pos %d: octet %d out of range (0-255)\n"
            input pos n
      | Error { pos; error = `Invalid_format msg } ->
          Printf.printf "✓ %-20s -> Custom error at pos %d: %s\n" input pos msg
      | Error _ -> Printf.printf "✗ Unknown error\n")
    test_cases_custom;

  Printf.printf "\nExample 3: Expression parser with precedence\n";
  Printf.printf "--------------------------------------------\n";
  let expr_tests =
    [ ("1+2*3", true); ("(1+2)*3", true); ("1+", false); ("1*)", false) ]
  in
  List.iter
    (fun (input, should_succeed) ->
      match Parseff.parse input expr with
      | Ok (result) ->
          Printf.printf "✓ %-20s -> %s\n" input (expr_to_string result)
      | Error { pos; error = `Expected expected } ->
          Printf.printf "%s %-20s -> Error at pos %d: %s\n"
            (if should_succeed then "✗" else "✓")
            input pos expected
      | Error _ -> Printf.printf "✗ Unknown error\n")
    expr_tests;

  Printf.printf "\nExample 4: Using one_of and one_of_labeled\n";
  Printf.printf "-------------------------------------------\n";
  (match Parseff.parse "if" keyword with
  | Ok (s) -> Printf.printf "✓ Parsed keyword: %s\n" s
  | Error _ -> Printf.printf "✗ Error\n");

  (match Parseff.parse "xyz" keyword with
  | Ok _ -> Printf.printf "✗ Should have failed\n"
  | Error { error = `Expected expected; _ } ->
      Printf.printf "✓ Failed as expected: %s\n" expected
  | Error _ -> Printf.printf "✓ Failed\n");

  (match Parseff.parse "99" literal with
  | Ok (e) -> Printf.printf "✓ Parsed literal: %s\n" (expr_to_string e)
  | Error _ -> Printf.printf "✗ Error\n");

  (match Parseff.parse "xyz" literal with
  | Ok _ -> Printf.printf "✗ Should have failed\n"
  | Error { error = `Expected expected; _ } ->
      Printf.printf "✓ Failed with labels: %s\n" expected
  | Error _ -> Printf.printf "✓ Failed\n");

  Printf.printf "\nConclusion:\n";
  Printf.printf "-----------\n";
  Printf.printf
    "The new API provides:\n\
    \  1. 'expect' combinator for cleaner error messages\n\
    \  2. 'error' for custom polymorphic variant error types\n\
    \  3. 'one_of' and 'one_of_labeled' for cleaner alternations\n\
    \  4. 'or_' as a named alternative to the <|> operator\n"
