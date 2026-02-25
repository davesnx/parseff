(** Example: Adding better error messages in userland *)

open Parseff

(** Custom error type with context (for future extension) *)
type parse_error = {
  pos : int;
  context : string list;
  expected : string;
}
[@@warning "-34-69"]

(** Labeled parser - adds context to errors *)
let label name parser () =
  try parser () with
  | _ ->
      (* In a real implementation, we'd catch Parse_error and add context *)
      fail (Printf.sprintf "in %s: %s" name "parse failed")

(** Expected combinator - improves error messages *)
let ( <?> ) parser msg () =
  try parser () with _ -> fail msg

(** Example: IP address with better errors *)
let digit_with_error () =
  (digit <?> "expected digit (0-9)") ()

let number_0_255 () =
  (label "number (0-255)" (fun () ->
       let digits = many1 digit_with_error () in
       let n = List.fold_left (fun acc d -> (acc * 10) + d) 0 digits in
       if n >= 0 && n <= 255 then n
       else fail (Printf.sprintf "number %d is out of range (must be 0-255)" n)))
    ()

let ip_address_with_errors () =
  let a = number_0_255 () in
  let _ = ((fun () -> consume ".") <?> "expected '.' after first octet") () in
  let b = number_0_255 () in
  let _ = ((fun () -> consume ".") <?> "expected '.' after second octet") () in
  let c = number_0_255 () in
  let _ = ((fun () -> consume ".") <?> "expected '.' after third octet") () in
  let d = number_0_255 () in
  end_of_input ();
  (a, b, c, d)

(** Example: Expression parser with precedence and good errors *)
type expr = Num of int | Add of expr * expr | Mul of expr * expr

let rec expr () =
  (label "expression" (fun () ->
       let left = term () in
       let rest =
         many
           (fun () ->
             let _ = whitespace () in
             let _ = ((fun () -> char '+') <?> "expected '+' operator") () in
             let _ = whitespace () in
             term ())
           ()
       in
       List.fold_left (fun acc t -> Add (acc, t)) left rest))
    ()

and term () =
  (label "term" (fun () ->
       let left = factor () in
       let rest =
         many
           (fun () ->
             let _ = whitespace () in
             let _ = ((fun () -> char '*') <?> "expected '*' operator") () in
             let _ = whitespace () in
             factor ())
           ()
       in
       List.fold_left (fun acc f -> Mul (acc, f)) left rest))
    ()

and factor () =
  (label "factor" (fun () ->
       let _ = whitespace () in
       ((fun () ->
           let _ = ((fun () -> char '(') <?> "expected '('") () in
           let _ = whitespace () in
           let e = expr () in
           let _ = whitespace () in
           let _ = ((fun () -> char ')') <?> "expected ')'") () in
           e)
       <|> fun () ->
           let d = (digit <?> "expected number") () in
           Num d)
         ()))
    ()

let rec expr_to_string = function
  | Num n -> string_of_int n
  | Add (l, r) -> Printf.sprintf "(%s + %s)" (expr_to_string l) (expr_to_string r)
  | Mul (l, r) -> Printf.sprintf "(%s * %s)" (expr_to_string l) (expr_to_string r)

let () =
  Printf.printf "Better Error Messages Example\n";
  Printf.printf "==============================\n\n";

  (* Test cases with errors *)
  let test_cases =
    [
      ("192.168.1.1", true);
      ("192.168.1.256", false);
      ("192.168.1", false);
      ("192.168.1.", false);
    ]
  in

  Printf.printf "IP Address Parsing:\n";
  Printf.printf "-------------------\n";
  List.iter
    (fun (input, should_succeed) ->
      match run input ip_address_with_errors with
      | Ok ((a, b, c, d), _) ->
          Printf.printf "✓ %-20s -> %d.%d.%d.%d\n" input a b c d
      | Error { pos; expected } ->
          Printf.printf "%s %-20s -> Error at pos %d: %s\n"
            (if should_succeed then "✗" else "✓")
            input pos expected)
    test_cases;

  Printf.printf "\nExpression Parsing:\n";
  Printf.printf "-------------------\n";

  let expr_tests = [ ("1+2*3", true); ("(1+2)*3", true); ("1+", false); ("1*)", false) ] in

  List.iter
    (fun (input, should_succeed) ->
      match run input expr with
      | Ok (result, _) ->
          Printf.printf "✓ %-20s -> %s\n" input (expr_to_string result)
      | Error { pos; expected } ->
          Printf.printf "%s %-20s -> Error at pos %d: %s\n"
            (if should_succeed then "✗" else "✓")
            input pos expected)
    expr_tests;

  Printf.printf "\nConclusion:\n";
  Printf.printf "-----------\n";
  Printf.printf
    "Adding better errors in userland is straightforward:\n\
    \  1. Use <?> operator to add custom error messages\n\
    \  2. Use 'label' combinator to add parsing context\n\
    \  3. Custom error types can wrap parse_error with more info\n\
    \  4. Error accumulation can be built on top (track all failures)\n"
