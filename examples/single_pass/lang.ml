(* The language of examples/sedlex, parsed in a single pass: no token type and
   no token array. Each token reader consumes the token's bytes and then the
   whitespace and comments after them, so between tokens the position is the
   start of the next token. Mismatches fail with [expected_token] over the
   whole token, so the errors read like the sedlex example's.

   One addition shows what a separate lexer cannot do without help: where an
   operand starts, [/] opens a regular expression, and after an operand it is
   division. *)

open Parseff

type expr =
  | Num of float
  | Var of string
  | Regex of string
  | Neg of expr
  | Binop of char * expr * expr
  | Let of string * expr * expr

type custom_error += Unbound of string

let show_error = function
  | Unbound name ->
      Printf.sprintf "unbound variable %s" name
  | _ ->
      "unknown error"

(* Whitespace and [#] comments. It never fails, so it records nothing that
   could show up in a later error. *)
let trivia () =
  skip_whitespace ();
  while peek_char () = Some '#' do
    skip_while (fun c -> c <> '\n');
    skip_whitespace ()
  done

let is_digit c = c >= '0' && c <= '9'

(* [1.5] is one number, but [1.] is the number [1] followed by [.]: the [.]
   belongs to the number only when a digit follows it. *)
let scan_number () =
  skip_while is_digit;
  if peek_char () = Some '.' then
    match peek_char ~ahead:1 () with
    | Some '0' .. '9' ->
        let _ = any_char () in
        skip_while is_digit
    | _ ->
        ()

let is_name_start u =
  Uchar.equal u (Uchar.of_char '_') || Uucp.Id.is_xid_start u

(* Names and keywords end at the same place, so [letπ] is one name. *)
let scan_name () =
  let _ = Utf8.satisfy ~label:"a name" is_name_start in
  Utf8.skip_while Uucp.Id.is_xid_continue

let is_continuation_byte c = Char.code c land 0xc0 = 0x80

(* One token of any kind, without its trivia, to report the whole token in an
   error. Bytes that are not UTF-8 count as one token. *)
let scan_token () =
  match peek_char () with
  | None ->
      ()
  | Some c when is_digit c ->
      scan_number ()
  | Some _ ->
      or_ scan_name (fun () ->
          or_
            (fun () ->
              let _ = Utf8.any_char () in
              ()
            )
            (fun () ->
              let _ = any_char () in
              skip_while is_continuation_byte
            )
      )

(* Fails with [expected <label>], found the token at the position. *)
let unexpected label =
  let (), stop = peek_with scan_token in
  expected_token ~upto:stop label

(* The name or keyword at the position and where it stops, without consuming
   it, or [""]. The caller decides what the word is before reading it, so a
   wrong word fails at its start. *)
let peek_word () =
  peek_with (fun () ->
      match optional (fun () -> recognize scan_name) with
      | Some w ->
          w
      | None ->
          ""
  )

let read_to stop =
  advance_to stop;
  trivia ()

let symbol c label =
  if peek_char () = Some c then begin
    let _ = any_char () in
    trivia ()
  end else
    unexpected label

let keyword kw label =
  match peek_word () with
  | w, stop when w = kw ->
      read_to stop
  | _ ->
      unexpected label

let is_reserved = function "let" | "in" -> true | _ -> false

let name () =
  match peek_word () with
  | w, stop when w <> "" && not (is_reserved w) ->
      read_to stop;
      w
  | _ ->
      unexpected "a name"

let number () =
  let text = recognize scan_number in
  trivia ();
  float_of_string text

let regex () =
  let text =
    recognize (fun () ->
        let _ = char '/' in
        skip_while (fun c -> c <> '/' && c <> '\n');
        let _ = char '/' in
        ()
    )
  in
  trivia ();
  text

let binop c = infix_left (fun () a b -> Binop (c, a, b))
let add = binop '+'
let sub = binop '-'
let mul = binop '*'
let div = binop '/'

(* The next byte decides the operator. Where none comes, [optional] keeps the
   failure, so an error at the next token also says that an operator could
   come there. *)
let operator () =
  let read prec op =
    let _ = any_char () in
    trivia ();
    Some (prec, op)
  in
  match peek_char () with
  | Some '+' ->
      read 1 add
  | Some '-' ->
      read 1 sub
  | Some '*' ->
      read 2 mul
  | Some '/' ->
      read 2 div
  | _ ->
      let _ = optional (fun () -> unexpected "an operator") in
      None

let rec expr scope () = precedence ~operator (operand scope)

and operand scope () =
  match peek_char () with
  | Some c when is_digit c ->
      Num (number ())
  | Some '-' ->
      symbol '-' "'-'";
      Neg (rec_ (fun () -> precedence ~min:3 ~operator (operand scope)))
  | Some '(' ->
      context ~delimited:true "parenthesized expression" (fun () ->
          symbol '(' "'('";
          let e = rec_ (expr scope) in
          symbol ')' "')'";
          e
      )
  | Some '/' ->
      Regex (regex ())
  | _ -> (
      match peek_word () with
      | "let", stop ->
          let_binding scope stop
      | w, stop when w <> "" && not (is_reserved w) ->
          if not (List.mem w scope) then
            error ~from:(position ()) ~upto:stop (Unbound w);
          read_to stop;
          Var w
      | _ ->
          unexpected "an expression"
    )

and let_binding scope stop =
  context "let binding" (fun () ->
      read_to stop;
      let v = name () in
      symbol '=' "'='";
      let value = rec_ (expr scope) in
      keyword "in" "'in'";
      let body = rec_ (expr (v :: scope)) in
      Let (v, value, body)
  )

let program () =
  trivia ();
  let e = expr [] () in
  if peek_char () <> None then unexpected "end of input";
  e

(* A regular expression evaluates to its length, so the language stays
   arithmetic. *)
let rec eval env = function
  | Num n ->
      n
  | Var v ->
      List.assoc v env
  | Regex r ->
      float_of_int (String.length r - 2)
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
  | Let (v, value, body) ->
      eval ((v, eval env value) :: env) body

let rec to_string = function
  | Num n ->
      Printf.sprintf "%g" n
  | Var v ->
      v
  | Regex r ->
      r
  | Neg e ->
      Printf.sprintf "(- %s)" (to_string e)
  | Binop (op, a, b) ->
      Printf.sprintf "(%c %s %s)" op (to_string a) (to_string b)
  | Let (v, value, body) ->
      Printf.sprintf "(let %s %s %s)" v (to_string value) (to_string body)
