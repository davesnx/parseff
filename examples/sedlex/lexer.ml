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
  | Invalid
  | Eof

type t = { token : token; start : int; stop : int }

(* Quotes [text] as it appears in the input. Bytes that are not valid UTF-8
   and control characters print as [\xNN], as in [Parseff.render_error]. *)
let quote text =
  let buf = Buffer.create (String.length text + 2) in
  Buffer.add_char buf '"';
  let rec loop i =
    if i < String.length text then begin
      let d = String.get_utf_8_uchar text i in
      let n = Uchar.utf_decode_length d in
      let c = text.[i] in
      if Uchar.utf_decode_is_valid d && c >= ' ' && c <> '\x7f' then
        Buffer.add_string buf (String.sub text i n)
      else
        for j = i to i + n - 1 do
          Printf.bprintf buf "\\x%02X" (Char.code text.[j])
        done;
      loop (i + n)
    end
  in
  loop 0;
  Buffer.add_char buf '"';
  Buffer.contents buf

(* Describes [t] with its text in [input], so a number reads as written. *)
let describe input t =
  let text () = String.sub input t.start (t.stop - t.start) in
  match t.token with
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
      "number " ^ text ()
  | Ident _ ->
      "identifier " ^ text ()
  | Invalid ->
      "invalid input " ^ quote (text ())
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

(* Lexes all of [input]. The last token is [Eof], with
   [start = stop = String.length input]. sedlex raises on invalid UTF-8, so it
   lexes only the valid prefix, and the rest becomes one [Invalid] token. *)
let tokenize input =
  let len = String.length input in
  let valid = valid_utf_8_prefix input in
  let buf = Sedlexing.Utf8.from_string (String.sub input 0 valid) in
  let rec loop acc =
    let token = next buf in
    let start, stop = Sedlexing.bytes_loc buf in
    if token <> Eof then
      loop ({ token; start; stop } :: acc)
    else if valid < len then
      { token = Invalid; start = valid; stop = len } :: acc
    else
      acc
  in
  let eof = { token = Eof; start = len; stop = len } in
  Array.of_list (List.rev (eof :: loop []))
