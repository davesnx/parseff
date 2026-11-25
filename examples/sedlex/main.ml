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
  | Unexpected of { expected : string; found : Lexer.token }
  | Unbound of string

let show_error = function
  | Unexpected { expected; found } ->
      Printf.sprintf "expected %s, found %s" expected (Lexer.describe found)
  | Unbound name ->
      Printf.sprintf "unbound variable %s" name
  | _ ->
      "unknown error"

(* [tokens] holds the lexer output. [index.(pos)] is the first token that
   starts at or after byte [pos], so a position inside whitespace or a comment
   maps to the token after it. *)
type tokens = { tokens : Lexer.t array; index : int array }

let index_tokens input =
  let tokens = Lexer.tokenize input in
  let index = Array.make (String.length input + 1) 0 in
  let next = ref (Array.length tokens - 1) in
  for pos = String.length input downto 0 do
    while !next > 0 && tokens.(!next - 1).start >= pos do
      decr next
    done;
    index.(pos) <- !next
  done;
  { tokens; index }

(* Moves to the start of the next token, past whitespace and comments, so
   errors and contexts start at the token. *)
let peek t =
  let tok = t.tokens.(t.index.(position ())) in
  skip_to tok.start;
  tok

let advance t = skip_to (peek t).stop

let unexpected t expected =
  let tok = peek t in
  error ~from:tok.start ~upto:tok.stop
    (Unexpected { expected; found = tok.token })

let eat t token label =
  if (peek t).token = token then
    advance t
  else
    unexpected t label

(* Operators are read by [operator], so their token parsers only return the
   build function. *)
let binop c = infix_left (fun () a b -> Binop (c, a, b))
let add = binop '+'
let sub = binop '-'
let mul = binop '*'
let div = binop '/'

let operator t () =
  let found prec op =
    advance t;
    Some (prec, op)
  in
  match (peek t).token with
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
      advance t;
      Num n
  | Ident name ->
      if not (List.mem name scope) then
        error ~from:tok.start ~upto:tok.stop (Unbound name);
      advance t;
      Var name
  | Minus ->
      advance t;
      Neg
        (rec_ (fun () ->
             precedence ~min:3 ~operator:(operator t) (operand t scope)
         )
        )
  | Lparen ->
      context ~delimited:true "parenthesized expression" (fun () ->
          advance t;
          let e = rec_ (expr t scope) in
          eat t Rparen "')'";
          e
      )
  | Let ->
      let_binding t scope
  | _ ->
      unexpected t "an expression"

and let_binding t scope =
  context "let binding" (fun () ->
      advance t;
      let name =
        match (peek t).token with
        | Ident name ->
            advance t;
            name
        | _ ->
            unexpected t "a name"
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
  let input = Sys.argv.(1) in
  let t = index_tokens input in
  match parse input (program t) with
  | Ok e ->
      Printf.printf "%s = %g\n" (to_string e) (eval [] e)
  | Error e ->
      print_endline (render_error ~custom:show_error e)
