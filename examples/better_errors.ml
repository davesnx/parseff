let digit_val () = Parseff.expect "a digit (0-9)" Parseff.digit

let number_0_255_simple () =
  let digits = Parseff.many ~at_least:1 digit_val () in
  let n = List.fold_left (fun acc d -> (acc * 10) + d) 0 digits in
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
  Parseff.end_of_input ();
  (a, b, c, d)

let number_0_255_with_error () =
  let digits = Parseff.many ~at_least:1 digit_val () in
  let n = List.fold_left (fun acc d -> (acc * 10) + d) 0 digits in
  if n >= 0 && n <= 255 then
    n
  else
    Parseff.error (`Out_of_range n)

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

let format_error pos error = Printf.sprintf "Error at pos %d: %s" pos error

let run_ip input =
  match Parseff.parse input ip_address_simple with
  | Ok (a, b, c, d) ->
      Printf.printf "%-20s -> %d.%d.%d.%d\n" input a b c d
  | Error { pos; error = `Expected msg } ->
      Printf.printf "%-20s -> %s\n" input (format_error pos msg)
  | Error { pos; error = `Failure msg } ->
      Printf.printf "%-20s -> %s\n" input (format_error pos msg)
  | Error { pos; error = `Unexpected_end_of_input } ->
      Printf.printf "%-20s -> Unexpected end of input at pos %d\n" input pos
  | Error _ ->
      Printf.printf "%-20s -> Unknown error\n" input

let run_ip_custom input =
  match Parseff.parse input ip_address_with_custom_errors with
  | Ok (a, b, c, d) ->
      Printf.printf "%-20s -> %d.%d.%d.%d\n" input a b c d
  | Error { pos; error = `Out_of_range n } ->
      Printf.printf
        "%-20s -> Custom error at pos %d: octet %d out of range (0-255)\n" input
        pos n
  | Error { pos; error = `Expected msg } ->
      Printf.printf "%-20s -> %s\n" input (format_error pos msg)
  | Error { pos; error = `Unexpected_end_of_input } ->
      Printf.printf "%-20s -> Unexpected end of input at pos %d\n" input pos
  | Error _ ->
      Printf.printf "%-20s -> Unknown error\n" input

let run_expr input =
  match Parseff.parse input expr with
  | Ok result ->
      Printf.printf "%-20s -> %s\n" input (expr_to_string result)
  | Error { pos; error = `Expected msg } ->
      Printf.printf "%-20s -> %s\n" input (format_error pos msg)
  | Error { pos; error = `Unexpected_end_of_input } ->
      Printf.printf "%-20s -> Unexpected end of input at pos %d\n" input pos
  | Error _ ->
      Printf.printf "%-20s -> Unknown error\n" input

let run_keyword input =
  match Parseff.parse input keyword with
  | Ok s ->
      Printf.printf "%-20s -> %s\n" input s
  | Error { pos; error = `Expected msg } ->
      Printf.printf "%-20s -> %s\n" input (format_error pos msg)
  | Error { pos; error = `Unexpected_end_of_input } ->
      Printf.printf "%-20s -> Unexpected end of input at pos %d\n" input pos
  | Error _ ->
      Printf.printf "%-20s -> Unknown error\n" input

let run_literal input =
  match Parseff.parse input literal with
  | Ok e ->
      Printf.printf "%-20s -> %s\n" input (expr_to_string e)
  | Error { pos; error = `Expected msg } ->
      Printf.printf "%-20s -> %s\n" input (format_error pos msg)
  | Error { pos; error = `Unexpected_end_of_input } ->
      Printf.printf "%-20s -> Unexpected end of input at pos %d\n" input pos
  | Error _ ->
      Printf.printf "%-20s -> Unknown error\n" input

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
  | _ ->
      Printf.eprintf "Unknown command: %s\n" cmd;
      exit 1
