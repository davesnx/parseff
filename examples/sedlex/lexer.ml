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
  | Regex of string
  | Invalid
  | Eof

type t = token Tokens.token

(* How an error names a token that the parser expected. *)
let show = function
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
  | Number _ ->
      "a number"
  | Ident _ ->
      "a name"
  | Regex _ ->
      "a regular expression"
  | Invalid ->
      "invalid input"
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
      Invalid
  | _ ->
      assert false

(* Where an operand starts, [/] opens a regular expression, as in
   JavaScript. *)
let regex buf =
  match%sedlex buf with
  | '/', Star (Compl ('/' | '\n')), '/' ->
      Regex (Sedlexing.Utf8.lexeme buf)
  | _ ->
      next buf

(* The length of the longest valid UTF-8 prefix of [s]. *)
let valid_utf_8_prefix s =
  let rec loop i =
    if i >= String.length s then
      i
    else
      let d = String.get_utf_8_uchar s i in
      if Uchar.utf_decode_is_valid d then
        loop (i + Uchar.utf_decode_length d)
      else
        i
  in
  loop 0

(* A lexer for [input] from byte [from] that returns one token per call, then
   [Eof] with [start = stop = String.length input] forever. With [~regex], the
   first token can be a regular expression. sedlex raises on invalid UTF-8, so
   it lexes only the valid prefix, and the rest becomes one [Invalid] token. *)
let tokens ?(from = 0) ?regex:(in_regex = false) input =
  let len = String.length input in
  let valid = max from (valid_utf_8_prefix input) in
  let buf = Sedlexing.Utf8.from_string (String.sub input from (valid - from)) in
  let ended = ref false in
  let first = ref in_regex in
  fun () ->
    if !ended then
      { Tokens.value = Eof; start = len; stop = len }
    else
      let lex =
        if !first then
          regex
        else
          next
      in
      first := false;
      match lex buf with
      | Eof when valid < len ->
          ended := true;
          { Tokens.value = Invalid; start = valid; stop = len }
      | Eof ->
          ended := true;
          { Tokens.value = Eof; start = len; stop = len }
      | value ->
          let start, stop = Sedlexing.bytes_loc buf in
          { Tokens.value; start = from + start; stop = from + stop }

(* The same lexer on a stream of bytes. Invalid UTF-8 ends the stream with
   one [Invalid] byte: sedlex raises there, and the rest is not lexed. *)
let stream_tokens gen =
  let buf = Sedlexing.Utf8.from_gen gen in
  let eof = ref None in
  fun () ->
    match !eof with
    | Some tok ->
        tok
    | None -> (
        match next buf with
        | Eof ->
            let start, stop = Sedlexing.bytes_loc buf in
            let tok = { Tokens.value = Eof; start; stop } in
            eof := Some tok;
            tok
        | value ->
            let start, stop = Sedlexing.bytes_loc buf in
            { Tokens.value; start; stop }
        | exception Sedlexing.MalFormed ->
            let _, stop = Sedlexing.bytes_loc buf in
            eof :=
              Some { Tokens.value = Eof; start = stop + 1; stop = stop + 1 };
            { Tokens.value = Invalid; start = stop; stop = stop + 1 }
      )
