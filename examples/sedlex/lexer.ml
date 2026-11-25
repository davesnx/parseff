(* sedlex turns the input into tokens. Each token keeps its byte range in the
   input, so the parser can report errors against the original text. *)

type token =
  | Let
  | In
  | Equal
  | Plus
  | Minus
  | Star
  | Slash
  | Lparen
  | Rparen
  | Number of float
  | Ident of string
  | Invalid of string
  | Eof

type t = { token : token; start : int; stop : int }

let describe = function
  | Let ->
      "'let'"
  | In ->
      "'in'"
  | Equal ->
      "'='"
  | Plus ->
      "'+'"
  | Minus ->
      "'-'"
  | Star ->
      "'*'"
  | Slash ->
      "'/'"
  | Lparen ->
      "'('"
  | Rparen ->
      "')'"
  | Number n ->
      Printf.sprintf "number %g" n
  | Ident name ->
      Printf.sprintf "identifier %s" name
  | Invalid text ->
      Printf.sprintf "invalid input %S" text
  | Eof ->
      "end of input"

let digit = [%sedlex.regexp? '0' .. '9']
let number = [%sedlex.regexp? Plus digit, Opt ('.', Plus digit)]
let ident = [%sedlex.regexp? ('_' | xid_start), Star xid_continue]

let rec next buf =
  match%sedlex buf with
  | Plus white_space ->
      next buf
  | '#', Star (Compl '\n') ->
      next buf
  | "let" ->
      Let
  | "in" ->
      In
  | '=' ->
      Equal
  | '+' ->
      Plus
  | '-' ->
      Minus
  | '*' ->
      Star
  | '/' ->
      Slash
  | '(' ->
      Lparen
  | ')' ->
      Rparen
  | number ->
      Number (float_of_string (Sedlexing.Utf8.lexeme buf))
  | ident ->
      Ident (Sedlexing.Utf8.lexeme buf)
  | eof ->
      Eof
  | any ->
      Invalid (Sedlexing.Utf8.lexeme buf)
  | _ ->
      assert false

(* Lexes all of [input]. The last token is [Eof], with
   [start = stop = String.length input]. Invalid UTF-8 stops the lexer with an
   [Invalid] token from the end of the last token to the end of the input. *)
let tokenize input =
  let len = String.length input in
  let buf = Sedlexing.Utf8.from_string input in
  let rec loop acc last_stop =
    match next buf with
    | token ->
        let start, stop = Sedlexing.bytes_loc buf in
        let tok = { token; start; stop } in
        if token = Eof then
          List.rev (tok :: acc)
        else
          loop (tok :: acc) stop
    | exception Sedlexing.MalFormed ->
        let start = ref last_stop in
        while !start < len && String.contains " \t\r\n" input.[!start] do
          incr start
        done;
        let invalid = String.sub input !start (len - !start) in
        List.rev
          ({ token = Eof; start = len; stop = len }
          :: { token = Invalid invalid; start = !start; stop = len }
          :: acc
          )
  in
  Array.of_list (loop [] 0)
