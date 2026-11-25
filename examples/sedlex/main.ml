(* sedlex lexes, parseff parses.

   [Lexer.tokenize] runs the sedlex lexer over the whole input first. The
   parser then reads tokens instead of bytes: [peek] returns the token at the
   current position, and [advance] moves past it, both with [skip_to].
   Because positions are byte offsets in the original input, parseff errors
   still point at the source text. *)

open Parseff

type expr =
  | Num of float
  | Var of string
  | Neg of expr
  | Binop of char * expr * expr
  | Let of string * expr * expr

type custom_error +=
  | Unexpected of { expected : string; found : string }
  | Unbound of string

let show_error = function
  | Unexpected { expected; found } ->
      Printf.sprintf "expected %s, found %s" expected found
  | Unbound name ->
      Printf.sprintf "unbound variable %s" name
  | _ ->
      "unknown error"

(* [input] is the source text and [tokens] the lexer output, in order. *)
type tokens = { input : string; tokens : Lexer.t array }

(* The first token that starts at or after byte [pos]. A position inside
   whitespace or a comment maps to the token after it. [Eof] starts at the end
   of the input, so the search always finds a token. *)
let token_at t pos =
  let rec search lo hi =
    if lo >= hi then
      lo
    else
      let mid = (lo + hi) / 2 in
      if t.tokens.(mid).start >= pos then
        search lo mid
      else
        search (mid + 1) hi
  in
  t.tokens.(search 0 (Array.length t.tokens - 1))

(* Moves to the start of the next token, past whitespace and comments, so
   errors and contexts start at the token. *)
let peek t =
  let tok = token_at t (position ()) in
  skip_to tok.start;
  tok

let advance (t : Lexer.t) = skip_to t.stop

let unexpected t (tok : Lexer.t) expected =
  error ~from:tok.start ~upto:tok.stop
    (Unexpected { expected; found = Lexer.describe t.input tok })

let eat t expected_token label =
  let current_token = peek t in
  if current_token.token = expected_token then
    advance current_token
  else
    unexpected t current_token label

(* Operators are read by [operator], so their token parsers only return the
   build function. *)
let binop c = infix_left (fun () a b -> Binop (c, a, b))
let add = binop '+'
let sub = binop '-'
let mul = binop '*'
let div = binop '/'

let operator t () =
  let tok = peek t in
  let found prec op =
    advance tok;
    Some (prec, op)
  in
  match tok.token with
  | Plus ->
      found 1 add
  | Minus ->
      found 1 sub
  | Star ->
      found 2 mul
  | Slash ->
      found 2 div
  | _ ->
      None

let rec expr t scope () = precedence ~operator:(operator t) (operand t scope)

and operand t scope () =
  let tok = peek t in
  match tok.token with
  | Number n ->
      advance tok;
      Num n
  | Ident name ->
      if not (List.mem name scope) then
        error ~from:tok.start ~upto:tok.stop (Unbound name);
      advance tok;
      Var name
  | Minus ->
      advance tok;
      Neg
        (rec_ (fun () ->
             precedence ~min:3 ~operator:(operator t) (operand t scope)
         )
        )
  | Lparen ->
      context ~delimited:true "parenthesized expression" (fun () ->
          advance tok;
          let e = rec_ (expr t scope) in
          eat t Rparen "')'";
          e
      )
  | Let ->
      let_binding t scope tok
  | _ ->
      unexpected t tok "an expression"

and let_binding t scope let_token =
  context "let binding" (fun () ->
      advance let_token;
      let name_token = peek t in
      let name =
        match name_token.token with
        | Ident name ->
            advance name_token;
            name
        | _ ->
            unexpected t name_token "a name"
      in
      eat t Equal "'='";
      let value = rec_ (expr t scope) in
      eat t In "'in'";
      let body = rec_ (expr t (name :: scope)) in
      Let (name, value, body)
  )

let program t () =
  let e = expr t [] () in
  (* [Eof] ends at the end of the input, so this also consumes trailing
     whitespace and comments. *)
  eat t Eof "an operator or end of input";
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
      let t = { input; tokens = Lexer.tokenize input } in
      match parse input (program t) with
      | Ok e ->
          Printf.printf "%s = %g\n" (to_string e) (eval [] e)
      | Error e ->
          print_endline (render_error ~custom:show_error e)
    )
  | _ ->
      prerr_endline "usage: main.exe EXPRESSION";
      exit 2
