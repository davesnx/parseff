(* sedlex lexes, parseff parses.

   [Lexer.tokenize] runs the sedlex lexer over the whole input first. The
   parser then reads tokens instead of bytes through [Tokens]. Because
   positions are byte offsets in the original input, parseff errors still
   point at the source text. *)

open Parseff

type expr =
  | Num of float
  | Var of string
  | Neg of expr
  | Binop of char * expr * expr
  | Let of string * expr * expr

type custom_error += Unbound of string

let show_error = function
  | Unbound name ->
      Printf.sprintf "unbound variable %s" name
  | _ ->
      "unknown error"

(* Operators are read by [operator], so their token parsers only return the
   build function. *)
let binop c = infix_left (fun () a b -> Binop (c, a, b))
let add = binop '+'
let sub = binop '-'
let mul = binop '*'
let div = binop '/'

(* [optional] keeps the failure, so an error at the next token also says that
   an operator could come there. *)
let operator t () =
  optional (fun () ->
      Tokens.select t "an operator" (function
        | Lexer.Plus ->
            Some (1, add)
        | Minus ->
            Some (1, sub)
        | Star ->
            Some (2, mul)
        | Slash ->
            Some (2, div)
        | _ ->
            None
        )
  )

let rec expr t scope () = precedence ~operator:(operator t) (operand t scope)

and operand t scope () =
  let tok = Tokens.peek t in
  match tok.value with
  | Number n ->
      Tokens.advance t;
      Num n
  | Ident name ->
      if not (List.mem name scope) then
        error ~from:tok.start ~upto:tok.stop (Unbound name);
      Tokens.advance t;
      Var name
  | Minus ->
      Tokens.advance t;
      Neg
        (rec_ (fun () ->
             precedence ~min:3 ~operator:(operator t) (operand t scope)
         )
        )
  | Lparen ->
      context ~delimited:true "parenthesized expression" (fun () ->
          Tokens.advance t;
          let e = rec_ (expr t scope) in
          Tokens.eat t Rparen;
          e
      )
  | Let ->
      let_binding t scope
  | _ ->
      expected_token ~upto:tok.stop "an expression"

and let_binding t scope =
  context "let binding" (fun () ->
      Tokens.advance t;
      let name =
        Tokens.select t "a name" (function Ident name -> Some name | _ -> None)
      in
      Tokens.eat t Equal;
      let value = rec_ (expr t scope) in
      Tokens.eat t In;
      let body = rec_ (expr t (name :: scope)) in
      Let (name, value, body)
  )

let program t () =
  let e = expr t [] () in
  (* [Eof] ends at the end of the input, so this also consumes trailing
     whitespace and comments. *)
  Tokens.eat t Eof;
  e

let rec eval env = function
  | Num n ->
      n
  | Var name ->
      List.assoc name env
  | Neg e ->
      -.eval env e
  | Binop (op, a, b) -> (
      let a = eval env a and b = eval env b in
      match op with
      | '+' ->
          a +. b
      | '-' ->
          a -. b
      | '*' ->
          a *. b
      | _ ->
          a /. b
    )
  | Let (name, value, body) ->
      eval ((name, eval env value) :: env) body

let rec to_string = function
  | Num n ->
      Printf.sprintf "%g" n
  | Var name ->
      name
  | Neg e ->
      Printf.sprintf "(- %s)" (to_string e)
  | Binop (op, a, b) ->
      Printf.sprintf "(%c %s %s)" op (to_string a) (to_string b)
  | Let (name, value, body) ->
      Printf.sprintf "(let %s %s %s)" name (to_string value) (to_string body)

let () =
  match Sys.argv with
  | [| _; input |] -> (
      let t =
        Tokens.create ~show:Lexer.show ~eof:Lexer.Eof (Lexer.tokens input)
      in
      match parse input (program t) with
      | Ok e ->
          Printf.printf "%s = %g\n" (to_string e) (eval [] e)
      | Error e ->
          print_endline (render_error ~custom:show_error e)
    )
  | _ ->
      prerr_endline "usage: main.exe EXPRESSION";
      exit 2
