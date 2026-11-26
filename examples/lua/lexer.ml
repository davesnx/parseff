type Parseff.custom_error += Lua of { msg : string; eof : bool }

let error ?from ?upto ?notes ?help ?(eof = false) msg : 'a =
  Parseff.error ?from ?upto ?notes ?help (Lua { msg; eof })

let make_error ?from ?upto ?notes ?help ?eof msg =
  match
    Parseff.capture (fun () -> (error ?from ?upto ?notes ?help ?eof msg : unit))
  with
  | Error e ->
      e
  | Ok () ->
      assert false

let is_alpha c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c = '_'
let is_digit c = c >= '0' && c <= '9'
let is_alnum c = is_alpha c || is_digit c

let is_xdigit c = is_digit c || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F')

type kw =
  | And
  | Break
  | Do
  | Else
  | Elseif
  | End
  | False
  | For
  | Function
  | Goto
  | If
  | In
  | Local
  | Nil
  | Not
  | Or
  | Repeat
  | Return
  | Then
  | True
  | Until
  | While

type sym =
  | Plus
  | Minus
  | Star
  | Slash
  | Dslash
  | Percent
  | Caret
  | Hash
  | Amp
  | Tilde
  | Pipe
  | Shl
  | Shr
  | Concat
  | Dots
  | Dot
  | Eq
  | Ne
  | Le
  | Ge
  | Lt
  | Gt
  | Assign
  | Lparen
  | Rparen
  | Lbrace
  | Rbrace
  | Lbracket
  | Rbracket
  | Dcolon
  | Colon
  | Semi
  | Comma

type token =
  | Name of string
  | Kw of kw
  | Sym of sym
  | Number of Ast.number * string
  | String of string
  | Other of string
  | Bad
  | Eof

let keyword = function
  | "and" ->
      Kw And
  | "break" ->
      Kw Break
  | "do" ->
      Kw Do
  | "else" ->
      Kw Else
  | "elseif" ->
      Kw Elseif
  | "end" ->
      Kw End
  | "false" ->
      Kw False
  | "for" ->
      Kw For
  | "function" ->
      Kw Function
  | "goto" ->
      Kw Goto
  | "if" ->
      Kw If
  | "in" ->
      Kw In
  | "local" ->
      Kw Local
  | "nil" ->
      Kw Nil
  | "not" ->
      Kw Not
  | "or" ->
      Kw Or
  | "repeat" ->
      Kw Repeat
  | "return" ->
      Kw Return
  | "then" ->
      Kw Then
  | "true" ->
      Kw True
  | "until" ->
      Kw Until
  | "while" ->
      Kw While
  | w ->
      Name w

let is_reserved w = match keyword w with Kw _ -> true | _ -> false

let kw_text = function
  | And ->
      "and"
  | Break ->
      "break"
  | Do ->
      "do"
  | Else ->
      "else"
  | Elseif ->
      "elseif"
  | End ->
      "end"
  | False ->
      "false"
  | For ->
      "for"
  | Function ->
      "function"
  | Goto ->
      "goto"
  | If ->
      "if"
  | In ->
      "in"
  | Local ->
      "local"
  | Nil ->
      "nil"
  | Not ->
      "not"
  | Or ->
      "or"
  | Repeat ->
      "repeat"
  | Return ->
      "return"
  | Then ->
      "then"
  | True ->
      "true"
  | Until ->
      "until"
  | While ->
      "while"

let sym_text = function
  | Plus ->
      "+"
  | Minus ->
      "-"
  | Star ->
      "*"
  | Slash ->
      "/"
  | Dslash ->
      "//"
  | Percent ->
      "%"
  | Caret ->
      "^"
  | Hash ->
      "#"
  | Amp ->
      "&"
  | Tilde ->
      "~"
  | Pipe ->
      "|"
  | Shl ->
      "<<"
  | Shr ->
      ">>"
  | Concat ->
      ".."
  | Dots ->
      "..."
  | Dot ->
      "."
  | Eq ->
      "=="
  | Ne ->
      "~="
  | Le ->
      "<="
  | Ge ->
      ">="
  | Lt ->
      "<"
  | Gt ->
      ">"
  | Assign ->
      "="
  | Lparen ->
      "("
  | Rparen ->
      ")"
  | Lbrace ->
      "{"
  | Rbrace ->
      "}"
  | Lbracket ->
      "["
  | Rbracket ->
      "]"
  | Dcolon ->
      "::"
  | Colon ->
      ":"
  | Semi ->
      ";"
  | Comma ->
      ","

let quote x = "\"" ^ x ^ "\""
let closing level = "]" ^ String.make level '=' ^ "]"

let escape_found t =
  let b = Buffer.create (String.length t) in
  let valid = String.is_valid_utf_8 t in
  String.iter
    (fun c ->
      match c with
      | c when Char.code c >= 0x80 && not valid ->
          Buffer.add_string b (Printf.sprintf "\\x%02X" (Char.code c))
      | '\n' ->
          Buffer.add_string b "\\n"
      | '\r' ->
          Buffer.add_string b "\\r"
      | '\t' ->
          Buffer.add_string b "\\t"
      | c when Char.code c < 32 || Char.code c = 127 ->
          Buffer.add_string b (Printf.sprintf "\\x%02X" (Char.code c))
      | c ->
          Buffer.add_char b c
    )
    t;
  Buffer.contents b

let contains_word text w =
  let n = String.length text and m = String.length w in
  let rec go k =
    k + m <= n
    && (String.sub text k m = w
        && (k = 0 || not (is_alnum text.[k - 1]))
        && (k + m = n || not (is_alnum text.[k + m]))
       || go (k + 1)
       )
  in
  go 0

let number_help text =
  let lower = String.lowercase_ascii text in
  let hex = String.length lower >= 2 && String.sub lower 0 2 = "0x" in
  let last = lower.[String.length lower - 1] in
  let rec has_dots i =
    i + 1 < String.length text
    && ((text.[i] = '.' && text.[i + 1] = '.') || has_dots (i + 1))
  in
  if has_dots 0 then
    Some "to concatenate numbers, put spaces around '..', as in 1 .. 2"
  else if lower = "0x" then
    Some "a hexadecimal number needs at least one digit after '0x'"
  else if (not hex) && (last = 'e' || last = '+' || last = '-') then
    Some "an exponent needs digits, as in 3e2 or 3e-2"
  else if hex && (last = 'p' || last = '+' || last = '-') then
    Some "a binary exponent needs digits, as in 0x1p4"
  else if is_alpha text.[String.length text - 1] then
    Some "a number cannot be followed directly by a letter or '_'"
  else
    None

let decimal_int text =
  let max_div_10 = Int64.div Int64.max_int 10L in
  let rec go i acc =
    if i = String.length text then
      Some acc
    else
      let d = Int64.of_int (Char.code text.[i] - Char.code '0') in
      if
        Int64.compare acc max_div_10 > 0
        || (Int64.equal acc max_div_10 && Int64.compare d 7L > 0)
      then
        None
      else
        go (i + 1) (Int64.add (Int64.mul acc 10L) d)
  in
  go 0 0L

let hex_int text =
  let v = ref 0L in
  for i = 2 to String.length text - 1 do
    let c = text.[i] in
    let d =
      if is_digit c then
        Char.code c - Char.code '0'
      else
        Char.code (Char.lowercase_ascii c) - Char.code 'a' + 10
    in
    v := Int64.add (Int64.shift_left !v 4) (Int64.of_int d)
  done;
  !v

let number_value text =
  let n = String.length text in
  let hex = n >= 2 && text.[0] = '0' && (text.[1] = 'x' || text.[1] = 'X') in
  let digit =
    if hex then
      is_xdigit
    else
      is_digit
  in
  let i =
    ref
      ( if hex then
          2
        else
          0
      )
  in
  let digits () =
    let start = !i in
    while !i < n && digit text.[!i] do
      incr i
    done;
    !i - start
  in
  let mantissa = digits () in
  let has_dot = !i < n && text.[!i] = '.' in
  let fraction =
    if has_dot then begin
      incr i;
      digits ()
    end else
      0
  in
  let is_expo c =
    if hex then
      c = 'p' || c = 'P'
    else
      c = 'e' || c = 'E'
  in
  let has_expo = !i < n && is_expo text.[!i] in
  let expo_ok =
    (not has_expo)
    || begin
      incr i;
      if !i < n && (text.[!i] = '+' || text.[!i] = '-') then incr i;
      let start = !i in
      while !i < n && is_digit text.[!i] do
        incr i
      done;
      !i > start
    end
  in
  if mantissa + fraction = 0 || (not expo_ok) || !i <> n then
    None
  else if (not has_dot) && not has_expo then
    if hex then
      Some (Ast.Int (hex_int text))
    else
      match decimal_int text with
      | Some v ->
          Some (Ast.Int v)
      | None ->
          Some (Ast.Float (float_of_string text))
  else
    Some (Ast.Float (float_of_string text))

let valid_escapes =
  "valid escapes are \\a \\b \\f \\n \\r \\t \\v \\\\ \\\" \\' \\z \\xXX \\ddd \
   \\u{XXX}, and a backslash before a line break"

let add_utf8 b x =
  let add n = Buffer.add_char b (Char.chr n) in
  if x < 0x80 then
    add x
  else
    let rec bytes x mfb acc =
      if x > mfb then
        bytes (x lsr 6) (mfb lsr 1) ((0x80 lor (x land 0x3F)) :: acc)
      else
        ((lnot mfb lsl 1) land 0xFF lor x) :: acc
    in
    List.iter add (bytes x 0x3f [])

type lexical = {
  err : Parseff.error;
  mutable ends : bool;
  mutable untils : bool;
}

let lexical_errors : (int, lexical) Hashtbl.t = Hashtbl.create 8

let fresh_lexical : Parseff.error list ref = ref []

let take_fresh_lexical () =
  let l = List.rev !fresh_lexical in
  fresh_lexical := [];
  l

let lex_error ~from ?notes ?help msg =
  match Hashtbl.find_opt lexical_errors from with
  | Some l ->
      l
  | None ->
      let l =
        {
          err = make_error ~from ?notes ?help msg;
          ends = false;
          untils = false;
        }
      in
      Hashtbl.replace lexical_errors from l;
      fresh_lexical := l.err :: !fresh_lexical;
      l

let swallowed (l : lexical) text =
  l.ends <- l.ends || contains_word text "end";
  l.untils <- l.untils || contains_word text "until"

let swallowed_all (l : lexical) =
  l.ends <- true;
  l.untils <- true

let is_lexical (e : Parseff.error) =
  match Hashtbl.find_opt lexical_errors e.range.start with
  | Some l ->
      l.err.range = e.range && l.err.kind = e.kind
  | None ->
      false

let lines : int Dynarray.t = Dynarray.create ()
let first_line = ref 1
let scan = ref 0
let breaks = ref 0

(* Start of the line after the last ['\n'] read. A rewind can read a line
   break again, so [add_line] keeps [lines] sorted and without repeats. *)
let last_line_start = ref 0

let add_line p =
  last_line_start := p;
  if p > Dynarray.get_last lines then Dynarray.add_last lines p

let line_index i =
  let rec go lo hi =
    if lo >= hi then
      lo
    else
      let mid = (lo + hi + 1) / 2 in
      if Dynarray.get lines mid <= i then
        go mid hi
      else
        go lo (mid - 1)
  in
  go 0 (Dynarray.length lines - 1)

let loc i =
  if Dynarray.length lines = 0 || Dynarray.get lines 0 > i then
    Printf.sprintf "byte %d" i
  else
    let j = line_index i in
    Printf.sprintf "%d:%d" (!first_line + j) (i - Dynarray.get lines j + 1)

let window () = (Dynarray.get lines 0, !first_line)

let space c =
  match c with
  | '\n' ->
      incr scan;
      incr breaks;
      add_line !scan;
      true
  | '\r' ->
      incr scan;
      incr breaks;
      true
  | ' ' | '\t' | '\011' | '\012' ->
      incr scan;
      true
  | _ ->
      false

let skip_space () =
  scan := Parseff.position ();
  Parseff.skip_while space

let newline () =
  let p = Parseff.position () in
  let c = Parseff.any_char () in
  incr breaks;
  if c = '\n' then add_line (p + 1);
  match Parseff.peek_char () with
  | Some (('\n' | '\r') as d) when d <> c ->
      ignore (Parseff.any_char ());
      if d = '\n' then add_line (p + 2)
  | _ ->
      ()

let level = ref 0

let equal_sign c =
  if c = '=' then begin
    incr level;
    true
  end else
    false

let count_equals () =
  level := 0;
  Parseff.skip_while equal_sign;
  !level

let hex_numeral = ref false
let after_exponent = ref false

let numeral_char c =
  if
    if !hex_numeral then
      c = 'p' || c = 'P'
    else
      c = 'e' || c = 'E'
  then begin
    after_exponent := true;
    true
  end else if (c = '+' || c = '-') && !after_exponent then begin
    after_exponent := false;
    true
  end else begin
    after_exponent := false;
    is_xdigit c || c = '.'
  end

let number ~start ~head =
  hex_numeral := String.length head = 2;
  after_exponent := false;
  let rest = Parseff.take_while numeral_char in
  let extra =
    match Parseff.peek_char () with
    | Some c when is_alpha c ->
        ignore (Parseff.any_char ());
        String.make 1 c
    | _ ->
        ""
  in
  let text =
    if extra = "" then
      head ^ rest
    else
      head ^ rest ^ extra
  in
  match number_value text with
  | Some v ->
      Number (v, text)
  | None ->
      ignore
        (lex_error ~from:start ?help:(number_help text)
           ("malformed number '" ^ text ^ "'")
        );
      Bad

let quote_char = ref '"'
let plain_char c = c <> !quote_char && c <> '\\' && c <> '\n' && c <> '\r'

let rest_of_string l q b =
  let rec go () =
    Buffer.add_string b (Parseff.take_while plain_char);
    match Parseff.peek_char () with
    | Some '\\' -> (
        ignore (Parseff.any_char ());
        match Parseff.peek_char () with
        | Some ('\n' | '\r') ->
            newline ();
            go ()
        | Some _ ->
            ignore (Parseff.any_char ());
            go ()
        | None ->
            ()
      )
    | Some c when c = q ->
        ignore (Parseff.any_char ())
    | Some _ | None ->
        ()
  in
  quote_char := q;
  go ();
  swallowed l (Buffer.contents b);
  Bad

let short_string ~start =
  let q = Parseff.any_char () in
  quote_char := q;
  let first = Parseff.take_while plain_char in
  match Parseff.peek_char () with
  | Some c when c = q ->
      ignore (Parseff.any_char ());
      String first
  | _ -> (
      let b = Buffer.create (String.length first + 16) in
      Buffer.add_string b first;
      let unfinished () =
        let help =
          match Parseff.peek_char () with
          | Some _ ->
              "a short string cannot contain a line break: close it with "
              ^ String.make 1 q
              ^ " on this line, write \\n for a line break, or use a long \
                 string [[...]]"
          | None ->
              "close it with " ^ String.make 1 q
        in
        let l = lex_error ~from:start ~help "unfinished string" in
        swallowed l (Buffer.contents b);
        Bad
      in
      let bad_char () =
        match Parseff.peek_char () with
        | Some ('\n' | '\r') | None ->
            false
        | Some c ->
            ignore (Parseff.any_char ());
            c = q
      in
      let escape_error k ?help ~took_char msg =
        let closed = took_char && bad_char () in
        let l = lex_error ~from:k ?help msg in
        if closed then begin
          swallowed l (Buffer.contents b);
          Bad
        end else
          rest_of_string l q b
      in
      let rec loop () =
        quote_char := q;
        Buffer.add_string b (Parseff.take_while plain_char);
        match Parseff.peek_char () with
        | None | Some ('\n' | '\r') ->
            unfinished ()
        | Some c when c = q ->
            ignore (Parseff.any_char ());
            String (Buffer.contents b)
        | Some _ ->
            escape ()
      and add c =
        ignore (Parseff.any_char ());
        Buffer.add_char b c;
        loop ()
      and escape () =
        let k = Parseff.position () in
        ignore (Parseff.any_char ());
        match Parseff.peek_char () with
        | None ->
            unfinished ()
        | Some 'a' ->
            add '\007'
        | Some 'b' ->
            add '\b'
        | Some 'f' ->
            add '\012'
        | Some 'n' ->
            add '\n'
        | Some 'r' ->
            add '\r'
        | Some 't' ->
            add '\t'
        | Some 'v' ->
            add '\011'
        | Some (('\\' | '"' | '\'') as c) ->
            add c
        | Some ('\n' | '\r') ->
            newline ();
            Buffer.add_char b '\n';
            loop ()
        | Some 'x' ->
            ignore (Parseff.any_char ());
            let digit () =
              match Parseff.peek_char () with
              | Some c when is_xdigit c ->
                  ignore (Parseff.any_char ());
                  Some c
              | _ ->
                  None
            in
            begin match digit () with
            | None ->
                escape_error k ~took_char:true
                  ~help:
                    "write \\x followed by exactly two hexadecimal digits, as \
                     in \\x41"
                  "hexadecimal digit expected in '\\x' escape"
            | Some h1 -> (
                match digit () with
                | None ->
                    escape_error k ~took_char:true
                      ~help:
                        "write \\x followed by exactly two hexadecimal digits, \
                         as in \\x41"
                      "hexadecimal digit expected in '\\x' escape"
                | Some h2 ->
                    Buffer.add_char b
                      (Char.chr
                         (int_of_string
                            ("0x" ^ String.make 1 h1 ^ String.make 1 h2)
                         )
                      );
                    loop ()
              )
            end
        | Some 'z' ->
            ignore (Parseff.any_char ());
            skip_space ();
            loop ()
        | Some 'u' ->
            unicode k
        | Some c when is_digit c ->
            let v = ref 0 and count = ref 0 in
            let continue = ref true in
            while !continue && !count < 3 do
              match Parseff.peek_char () with
              | Some d when is_digit d ->
                  ignore (Parseff.any_char ());
                  v := (!v * 10) + Char.code d - Char.code '0';
                  incr count
              | _ ->
                  continue := false
            done;
            if !v > 255 then
              escape_error k ~took_char:false
                ~help:
                  "a decimal escape is at most \\255; use \\u{...} for other \
                   characters"
                "decimal escape too large"
            else begin
              Buffer.add_char b (Char.chr !v);
              loop ()
            end
        | Some c ->
            ignore (Parseff.any_char ());
            escape_error k ~took_char:false ~help:valid_escapes
              (Printf.sprintf "invalid escape sequence '\\%s'"
                 (escape_found (String.make 1 c))
              )
      and unicode k =
        ignore (Parseff.any_char ());
        match Parseff.peek_char () with
        | Some '{' ->
            ignore (Parseff.any_char ());
            let rec digits v count =
              match Parseff.peek_char () with
              | Some c when is_xdigit c ->
                  ignore (Parseff.any_char ());
                  let v = (v lsl 4) + int_of_string ("0x" ^ String.make 1 c) in
                  if v > 0x7FFFFFFF then
                    escape_error k ~took_char:false
                      ~help:"the largest value of a \\u{XXX} escape is 7FFFFFFF"
                      "UTF-8 value too large"
                  else
                    digits v (count + 1)
              | _ when count = 0 ->
                  escape_error k ~took_char:true
                    "hexadecimal digit expected in '\\u{XXX}' escape"
              | Some '}' ->
                  ignore (Parseff.any_char ());
                  add_utf8 b v;
                  loop ()
              | _ ->
                  escape_error k ~took_char:true
                    "missing '}' in '\\u{XXX}' escape"
            in
            digits 0 0
        | _ ->
            escape_error k ~took_char:true ~help:"write \\u{XXX}, as in \\u{48}"
              "missing '{' in '\\u{XXX}' escape"
      in
      match Parseff.peek_char () with
      | None | Some ('\n' | '\r') ->
          unfinished ()
      | Some _ ->
          escape ()
    )

let long_char c = c <> ']' && c <> '\n' && c <> '\r'

let long_bracket ~start ~level ~keep ~what =
  let b =
    if keep then
      Some (Buffer.create 64)
    else
      None
  in
  let add s = match b with Some b -> Buffer.add_string b s | None -> () in
  (match Parseff.peek_char () with Some ('\n' | '\r') -> newline () | _ -> ());
  let other = ref None in
  let rec loop () =
    ( match b with
    | Some b ->
        Buffer.add_string b (Parseff.take_while long_char)
    | None ->
        Parseff.skip_while long_char
    );
    match Parseff.peek_char () with
    | None ->
        let notes =
          match !other with
          | Some (a, n) ->
              [
                {
                  Parseff.at = Some { start = a; stop = a + n + 2 };
                  text =
                    Printf.sprintf "this '%s' has level %d, the %s needs %s"
                      (closing n) n what (closing level);
                };
              ]
          | None ->
              []
        in
        let help =
          if what = "comment" then
            "close the comment with " ^ closing level
          else
            Printf.sprintf
              "a long string that starts with [%s[ ends at the first %s"
              (String.make level '=') (closing level)
        in
        swallowed_all
          (lex_error ~from:start ~notes ~help ("unfinished long " ^ what));
        None
    | Some ']' -> (
        let a = Parseff.position () in
        ignore (Parseff.any_char ());
        let n = count_equals () in
        match Parseff.peek_char () with
        | Some ']' when n = level ->
            ignore (Parseff.any_char ());
            Some (match b with Some b -> Buffer.contents b | None -> "")
        | Some ']' ->
            (match !other with None -> other := Some (a, n) | Some _ -> ());
            add ("]" ^ String.make n '=');
            loop ()
        | _ ->
            add ("]" ^ String.make n '=');
            loop ()
      )
    | Some _ ->
        newline ();
        add "\n";
        loop ()
  in
  loop ()

let comment ~start =
  match Parseff.peek_char () with
  | Some '[' -> (
      ignore (Parseff.any_char ());
      let n = count_equals () in
      match Parseff.peek_char () with
      | Some '[' ->
          ignore (Parseff.any_char ());
          ignore (long_bracket ~start ~level:n ~keep:false ~what:"comment")
      | _ ->
          Parseff.skip_while (fun c -> c <> '\n' && c <> '\r')
    )
  | _ ->
      Parseff.skip_while (fun c -> c <> '\n' && c <> '\r')

let other_char () =
  let c = Parseff.any_char () in
  if Char.code c >= 0xC0 then
    String.make 1 c ^ Parseff.take_while (fun c -> Char.code c land 0xC0 = 0x80)
  else
    String.make 1 c

let one s =
  ignore (Parseff.any_char ());
  Sym s

let two c s1 s2 =
  ignore (Parseff.any_char ());
  match Parseff.peek_char () with
  | Some d when d = c ->
      ignore (Parseff.any_char ());
      Sym s2
  | _ ->
      Sym s1

(* Reads the token at the position, without the trivia after it. The
   position is a token start, so ['-'] does not start a comment. *)
let lex_token () =
  let p = Parseff.position () in
  match Parseff.peek_char () with
  | None ->
      Eof
  | Some c -> (
      match c with
      | 'a' .. 'z' | 'A' .. 'Z' | '_' ->
          keyword (Parseff.take_while is_alnum)
      | '0' .. '9' ->
          let d = Parseff.any_char () in
          let head =
            match (d, Parseff.peek_char ()) with
            | '0', Some (('x' | 'X') as x) ->
                ignore (Parseff.any_char ());
                String.init 2 (fun i ->
                    if i = 0 then
                      '0'
                    else
                      x
                )
            | _ ->
                String.make 1 d
          in
          number ~start:p ~head
      | '"' | '\'' ->
          short_string ~start:p
      | '-' ->
          one Minus
      | '[' -> (
          ignore (Parseff.any_char ());
          let n = count_equals () in
          match Parseff.peek_char () with
          | Some '[' -> (
              ignore (Parseff.any_char ());
              match
                long_bracket ~start:p ~level:n ~keep:true ~what:"string"
              with
              | Some s ->
                  String s
              | None ->
                  Bad
            )
          | _ when n = 0 ->
              Sym Lbracket
          | _ ->
              ignore
                (lex_error ~from:p
                   ~help:
                     "a long string starts with '[', any number of '=' and \
                      another '[', as in [==["
                   "invalid long string delimiter"
                );
              Bad
        )
      | '.' -> (
          ignore (Parseff.any_char ());
          match Parseff.peek_char () with
          | Some '.' -> (
              ignore (Parseff.any_char ());
              match Parseff.peek_char () with
              | Some '.' ->
                  ignore (Parseff.any_char ());
                  Sym Dots
              | _ ->
                  Sym Concat
            )
          | Some d when is_digit d ->
              number ~start:p ~head:"."
          | _ ->
              Sym Dot
        )
      | '=' ->
          two '=' Assign Eq
      | '/' ->
          two '/' Slash Dslash
      | ':' ->
          two ':' Colon Dcolon
      | '~' ->
          two '=' Tilde Ne
      | '<' -> (
          ignore (Parseff.any_char ());
          match Parseff.peek_char () with
          | Some '=' ->
              one Le
          | Some '<' ->
              one Shl
          | _ ->
              Sym Lt
        )
      | '>' -> (
          ignore (Parseff.any_char ());
          match Parseff.peek_char () with
          | Some '=' ->
              one Ge
          | Some '>' ->
              one Shr
          | _ ->
              Sym Gt
        )
      | '+' ->
          one Plus
      | '*' ->
          one Star
      | '%' ->
          one Percent
      | '^' ->
          one Caret
      | '#' ->
          one Hash
      | '&' ->
          one Amp
      | '|' ->
          one Pipe
      | '(' ->
          one Lparen
      | ')' ->
          one Rparen
      | '{' ->
          one Lbrace
      | '}' ->
          one Rbrace
      | ']' ->
          one Rbracket
      | ';' ->
          one Semi
      | ',' ->
          one Comma
      | '!' -> (
          ignore (Parseff.any_char ());
          match Parseff.peek_char () with
          | Some '=' ->
              ignore (Parseff.any_char ());
              Other "!="
          | _ ->
              Other "!"
        )
      | _ ->
          Other (other_char ())
    )

(* Single pass: each token parser reads its bytes, then [trivia] reads the
   whitespace and comments after them, so the position between tokens is the
   start of the next token. *)

(* The trivia before the token at [at]: where the token before it stopped,
   whether it has a line break, the indent of the line of the token at [at],
   and the start of a comment on the line of the token before ([-1] for none).
   [trivia] writes every field after each token. A rewind does not restore
   it, so every rewind past a [trivia] must: the parser rewinds through
   [attempt] and [look_ahead] below. Its other rewinds, an operator lookup
   and a peek, read no trivia. The fields are mutable so that a token does
   not allocate. *)
type gap = {
  mutable at : int;
  mutable stop : int;
  mutable nl : bool;
  mutable indent : int;
  mutable comment : int;
}

let gap = { at = -1; stop = 0; nl = false; indent = 0; comment = -1 }

let copy_gap ~src ~dst =
  dst.at <- src.at;
  dst.stop <- src.stop;
  dst.nl <- src.nl;
  dst.indent <- src.indent;
  dst.comment <- src.comment

(* The gap at the last [cut]. *)
let cut_gap = { gap with at = -1 }

(* [Parseff.cut], keeping the gap at the cut for [attempt]. *)
let cut () =
  Parseff.cut ();
  assert (gap.at = Parseff.position ());
  copy_gap ~src:gap ~dst:cut_gap

(* [Parseff.capture ~release:true]. A failure rewinds to the start or to the
   last [cut] inside, and the gap there is put back. The gap at the start is
   saved in locals: [attempt] is on the hot path and must not allocate. *)
let attempt f =
  let start = Parseff.position () in
  let { at; stop; nl; indent; comment } = gap in
  match Parseff.capture ~release:true f with
  | Ok _ as ok ->
      ok
  | Error _ as err ->
      let p = Parseff.position () in
      if p = start then
        copy_gap ~src:{ at; stop; nl; indent; comment } ~dst:gap
      else if p = cut_gap.at then
        copy_gap ~src:cut_gap ~dst:gap
      else
        assert false;
      err

(* [Parseff.look_ahead], putting back the gap and the last cut. *)
let look_ahead f =
  let g = { gap with at = gap.at } and c = { cut_gap with at = cut_gap.at } in
  let v = Parseff.look_ahead f in
  copy_gap ~src:g ~dst:gap;
  copy_gap ~src:c ~dst:cut_gap;
  v

(* Where the token before the position stopped, and whether a line break
   comes before it. *)
let prev_stop () =
  assert (gap.at = Parseff.position ());
  gap.stop

let nl () =
  assert (gap.at = Parseff.position ());
  gap.nl

(* The indent of the line of the token at [s], the position. Taking [s]
   keeps the check cheap where the caller has it. *)
let[@inline] indent_at s =
  assert (gap.at = s);
  gap.indent

(* Whether the gap that ends at [at] has a comment on the line of the token
   before it, starting at [from] or later. *)
let comment_in_gap ~at ~from = gap.at = at && gap.comment >= from

(* Where a ['&'] or ['|'] operator stopped, for the hints on [&&] and [||]. *)
let amp_end = ref (-1)
let pipe_end = ref (-1)

let line_start_at p =
  if !last_line_start <= p then
    !last_line_start
  else
    Dynarray.get lines (line_index p)

let second () =
  let _ = Parseff.any_char () in
  Parseff.peek_char ()

(* The byte after the next one. *)
let peek_second () = Parseff.look_ahead second

let is_blank c = c = ' ' || c = '\t' || c = '\011' || c = '\012'

let[@inline] close_gap at stop nl comment =
  gap.at <- at;
  gap.stop <- stop;
  gap.nl <- nl;
  if nl then gap.indent <- at - line_start_at at;
  gap.comment <- comment

let rec skip_gap stop nl cmt =
  match Parseff.peek_char () with
  | Some (' ' | '\t' | '\011' | '\012') ->
      Parseff.skip_while is_blank;
      skip_gap stop nl cmt
  | Some '\n' ->
      let _ = Parseff.any_char () in
      incr breaks;
      add_line (Parseff.position ());
      skip_gap stop true cmt
  | Some '\r' ->
      let _ = Parseff.any_char () in
      incr breaks;
      skip_gap stop true cmt
  | Some '-' -> (
      match peek_second () with
      | Some '-' ->
          let p = Parseff.position () in
          let _ = Parseff.any_char () in
          let _ = Parseff.any_char () in
          let b = !breaks in
          comment ~start:p;
          let cmt =
            if cmt < 0 && not nl then
              p
            else
              cmt
          in
          skip_gap stop (nl || !breaks > b) cmt
      | _ ->
          close_gap (Parseff.position ()) stop nl cmt
    )
  | _ ->
      close_gap (Parseff.position ()) stop nl cmt

(* [p] is where the token before stopped. *)
let trivia_from p =
  match Parseff.peek_char () with
  | Some (' ' | '\t' | '\n' | '\r' | '\011' | '\012' | '-') ->
      skip_gap p false (-1)
  | _ ->
      close_gap p p false (-1)

let trivia () = trivia_from (Parseff.position ())

(* [trivia], returning where the token before stopped. *)
let finish () =
  let p = Parseff.position () in
  trivia_from p;
  p

let word_text () = Parseff.take_while is_alnum

let word () =
  let w = Parseff.take_while is_alnum in
  trivia ();
  w

let sym1 () =
  let _ = Parseff.any_char () in
  trivia ()

(* A token read on a rare or error path: its kind, its range, and whether a
   line break comes before it or inside it. *)
type view = { tok : token; start : int; stop : int; nl : bool; inside : bool }

let lex_counting () =
  let b = !breaks in
  let t = lex_token () in
  (t, !breaks > b)

(* The gap counts only if it is for [p]: a token read right after bytes read
   without their trivia, as in [Parser.not_a_statement], has none. *)
let view () =
  let p = Parseff.position () in
  let (tok, inside), stop = Parseff.peek_with lex_counting in
  { tok; start = p; stop; nl = inside || (gap.at = p && gap.nl); inside }

(* A token with a line break inside, such as a long string, gives the tokens
   after it on its last line the indent [s - l], where [l] is the start of that
   line. [l] comes after the token start [s], so the indent is negative. *)
let after_token s inside =
  if inside then gap.indent <- s - line_start_at (Parseff.position ());
  finish ()

let skip_view v =
  Parseff.skip_to v.stop;
  let _ = after_token v.start v.inside in
  ()

let word_view w ~start ~stop =
  { tok = keyword w; start; stop; nl = false; inside = false }

let quoted t =
  let t' =
    if String.length t > 32 then
      String.sub t 0 32
    else
      t
  in
  "'" ^ escape_found t' ^ "'"
  ^
  if String.length t > 32 then
    "..."
  else
    ""

let found_tok = function
  | Eof ->
      "end of input"
  | Name w ->
      quoted w
  | Kw k ->
      quoted (kw_text k)
  | Sym s ->
      quoted (sym_text s)
  | Number (_, t) ->
      quoted t
  | String v ->
      quoted ("\"" ^ v ^ "\"")
  | Other t ->
      quoted t
  | Bad ->
      "an invalid token"

let begin_chunk () =
  Hashtbl.reset lexical_errors;
  fresh_lexical := [];
  Dynarray.clear lines;
  Dynarray.add_last lines 0;
  first_line := 1;
  last_line_start := 0;
  amp_end := -1;
  pipe_end := -1;
  ignore (Parseff.optional (fun () -> Parseff.literal "\xEF\xBB\xBF"));
  ( match Parseff.peek_char () with
  | Some '#' ->
      Parseff.skip_while (fun c -> c <> '\n')
  | _ ->
      ()
  );
  trivia ();
  (* The first token starts a line and has no token before it. *)
  gap.stop <- 0;
  gap.nl <- true;
  gap.indent <- gap.at - line_start_at gap.at

let trim_lines_at p =
  let j = max 0 (line_index p - 1) in
  if j > 0 then begin
    let kept =
      Dynarray.init
        (Dynarray.length lines - j)
        (fun i -> Dynarray.get lines (i + j))
    in
    Dynarray.clear lines;
    Dynarray.append lines kept;
    first_line := !first_line + j
  end
