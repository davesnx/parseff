module P = Parseff

type P.custom_error += Lua of { msg : string; help : string option; eof : bool }

let error ?from ?notes ?help ?(eof = false) msg : 'a =
  P.error ?from ?notes (Lua { msg; help; eof })

let make_error ?from ?notes ?help ?eof msg =
  match P.capture (fun () -> (error ?from ?notes ?help ?eof msg : unit)) with
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

type lexical = { err : P.error; mutable ends : bool; mutable untils : bool }

let lexical_errors : (int, lexical) Hashtbl.t = Hashtbl.create 8

let fresh_lexical : P.error list ref = ref []

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

let is_lexical (e : P.error) =
  match Hashtbl.find_opt lexical_errors e.range.start with
  | Some l ->
      l.err.range = e.range && l.err.kind = e.kind
  | None ->
      false

let lines : int Dynarray.t = Dynarray.create ()
let first_line = ref 1
let scan = ref 0

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

let line_start () = Dynarray.get_last lines
let window () = (Dynarray.get lines 0, !first_line)

let space c =
  match c with
  | '\n' ->
      incr scan;
      Dynarray.add_last lines !scan;
      true
  | ' ' | '\t' | '\r' | '\011' | '\012' ->
      incr scan;
      true
  | _ ->
      false

let skip_space () =
  scan := P.position ();
  P.skip_while space

let newline () =
  let p = P.position () in
  let c = P.any_char () in
  if c = '\n' then Dynarray.add_last lines (p + 1);
  match P.peek_char () with
  | Some (('\n' | '\r') as d) when d <> c ->
      ignore (P.any_char ());
      if d = '\n' then Dynarray.add_last lines (p + 2)
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
  P.skip_while equal_sign;
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
  let rest = P.take_while numeral_char in
  let extra =
    match P.peek_char () with
    | Some c when is_alpha c ->
        ignore (P.any_char ());
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
    Buffer.add_string b (P.take_while plain_char);
    match P.peek_char () with
    | Some '\\' -> (
        ignore (P.any_char ());
        match P.peek_char () with
        | Some ('\n' | '\r') ->
            newline ();
            go ()
        | Some _ ->
            ignore (P.any_char ());
            go ()
        | None ->
            ()
      )
    | Some c when c = q ->
        ignore (P.any_char ())
    | Some _ | None ->
        ()
  in
  quote_char := q;
  go ();
  swallowed l (Buffer.contents b);
  Bad

let short_string ~start =
  let q = P.any_char () in
  quote_char := q;
  let first = P.take_while plain_char in
  match P.peek_char () with
  | Some c when c = q ->
      ignore (P.any_char ());
      String first
  | _ -> (
      let b = Buffer.create (String.length first + 16) in
      Buffer.add_string b first;
      let unfinished () =
        let help =
          match P.peek_char () with
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
        match P.peek_char () with
        | Some ('\n' | '\r') | None ->
            false
        | Some c ->
            ignore (P.any_char ());
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
        Buffer.add_string b (P.take_while plain_char);
        match P.peek_char () with
        | None | Some ('\n' | '\r') ->
            unfinished ()
        | Some c when c = q ->
            ignore (P.any_char ());
            String (Buffer.contents b)
        | Some _ ->
            escape ()
      and add c =
        ignore (P.any_char ());
        Buffer.add_char b c;
        loop ()
      and escape () =
        let k = P.position () in
        ignore (P.any_char ());
        match P.peek_char () with
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
            ignore (P.any_char ());
            let digit () =
              match P.peek_char () with
              | Some c when is_xdigit c ->
                  ignore (P.any_char ());
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
            ignore (P.any_char ());
            skip_space ();
            loop ()
        | Some 'u' ->
            unicode k
        | Some c when is_digit c ->
            let v = ref 0 and count = ref 0 in
            let continue = ref true in
            while !continue && !count < 3 do
              match P.peek_char () with
              | Some d when is_digit d ->
                  ignore (P.any_char ());
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
            ignore (P.any_char ());
            escape_error k ~took_char:false ~help:valid_escapes
              (Printf.sprintf "invalid escape sequence '\\%s'"
                 (escape_found (String.make 1 c))
              )
      and unicode k =
        ignore (P.any_char ());
        match P.peek_char () with
        | Some '{' ->
            ignore (P.any_char ());
            let rec digits v count =
              match P.peek_char () with
              | Some c when is_xdigit c ->
                  ignore (P.any_char ());
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
                  ignore (P.any_char ());
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
      match P.peek_char () with
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
  (match P.peek_char () with Some ('\n' | '\r') -> newline () | _ -> ());
  let other = ref None in
  let rec loop () =
    ( match b with
    | Some b ->
        Buffer.add_string b (P.take_while long_char)
    | None ->
        P.skip_while long_char
    );
    match P.peek_char () with
    | None ->
        let notes =
          match !other with
          | Some (a, n) ->
              [
                {
                  P.at = Some { start = a; stop = a + n + 2 };
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
        let a = P.position () in
        ignore (P.any_char ());
        let n = count_equals () in
        match P.peek_char () with
        | Some ']' when n = level ->
            ignore (P.any_char ());
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
  match P.peek_char () with
  | Some '[' -> (
      ignore (P.any_char ());
      let n = count_equals () in
      match P.peek_char () with
      | Some '[' ->
          ignore (P.any_char ());
          ignore (long_bracket ~start ~level:n ~keep:false ~what:"comment")
      | _ ->
          P.skip_while (fun c -> c <> '\n' && c <> '\r')
    )
  | _ ->
      P.skip_while (fun c -> c <> '\n' && c <> '\r')

let lx_tok = ref Eof
let lx_start = ref 0
let lx_stop = ref 0
let lx_comment = ref (-1)
let lx_lines_before = ref 0

let other_char () =
  let c = P.any_char () in
  if Char.code c >= 0xC0 then
    String.make 1 c ^ P.take_while (fun c -> Char.code c land 0xC0 = 0x80)
  else
    String.make 1 c

let one s =
  ignore (P.any_char ());
  Sym s

let two c s1 s2 =
  ignore (P.any_char ());
  match P.peek_char () with
  | Some d when d = c ->
      ignore (P.any_char ());
      Sym s2
  | _ ->
      Sym s1

let rec lex () =
  skip_space ();
  let p = P.position () in
  lx_start := p;
  let tok =
    match P.peek_char () with
    | None ->
        Eof
    | Some c -> (
        match c with
        | 'a' .. 'z' | 'A' .. 'Z' | '_' ->
            keyword (P.take_while is_alnum)
        | '0' .. '9' ->
            let d = P.any_char () in
            let head =
              match (d, P.peek_char ()) with
              | '0', Some (('x' | 'X') as x) ->
                  ignore (P.any_char ());
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
        | '-' -> (
            ignore (P.any_char ());
            match P.peek_char () with
            | Some '-' ->
                ignore (P.any_char ());
                if !lx_comment < 0 && Dynarray.length lines = !lx_lines_before
                then
                  lx_comment := p;
                comment ~start:p;
                Other ""
            | _ ->
                Sym Minus
          )
        | '[' -> (
            ignore (P.any_char ());
            let n = count_equals () in
            match P.peek_char () with
            | Some '[' -> (
                ignore (P.any_char ());
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
            ignore (P.any_char ());
            match P.peek_char () with
            | Some '.' -> (
                ignore (P.any_char ());
                match P.peek_char () with
                | Some '.' ->
                    ignore (P.any_char ());
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
            ignore (P.any_char ());
            match P.peek_char () with
            | Some '=' ->
                one Le
            | Some '<' ->
                one Shl
            | _ ->
                Sym Lt
          )
        | '>' -> (
            ignore (P.any_char ());
            match P.peek_char () with
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
            ignore (P.any_char ());
            match P.peek_char () with
            | Some '=' ->
                ignore (P.any_char ());
                Other "!="
            | _ ->
                Other "!"
          )
        | _ ->
            Other (other_char ())
      )
  in
  match tok with
  | Other "" ->
      lex ()
  | tok ->
      lx_tok := tok;
      lx_stop := P.position ()

type state = {
  mutable tok : token;
  mutable start : int;
  mutable stop : int;
  mutable nl : bool;
  mutable comment : int;
  mutable indent : int;
  mutable prev : token;
  mutable prev_stop : int;
  mutable ahead : state option;
}

let st =
  {
    tok = Eof;
    start = 0;
    stop = 0;
    nl = true;
    comment = -1;
    indent = 0;
    prev = Eof;
    prev_stop = 0;
    ahead = None;
  }

let lex_into (s : state) ~indent =
  let lines_before = Dynarray.length lines in
  lx_lines_before := lines_before;
  lx_comment := -1;
  lex ();
  s.tok <- !lx_tok;
  s.start <- !lx_start;
  s.stop <- !lx_stop;
  s.nl <- Dynarray.length lines > lines_before;
  s.comment <- !lx_comment;
  s.indent <-
    ( if s.nl then
        s.start - line_start ()
      else
        indent
    )

let advance () =
  st.prev <- st.tok;
  st.prev_stop <- st.stop;
  match st.ahead with
  | Some a ->
      st.tok <- a.tok;
      st.start <- a.start;
      st.stop <- a.stop;
      st.nl <- a.nl;
      st.comment <- a.comment;
      st.indent <- a.indent;
      st.ahead <- None
  | None ->
      lex_into st ~indent:st.indent

let peek2 () =
  match st.ahead with
  | Some a ->
      a.tok
  | None ->
      let a = { st with ahead = None } in
      lex_into a ~indent:st.indent;
      st.ahead <- Some a;
      a.tok

type snapshot = { saved : state; saved_lines : int; saved_first : int }

let save () =
  {
    saved =
      { st with ahead = Option.map (fun a -> { a with tok = a.tok }) st.ahead };
    saved_lines = Dynarray.length lines;
    saved_first = !first_line;
  }

let restore s =
  let v = s.saved in
  st.tok <- v.tok;
  st.start <- v.start;
  st.stop <- v.stop;
  st.nl <- v.nl;
  st.comment <- v.comment;
  st.indent <- v.indent;
  st.prev <- v.prev;
  st.prev_stop <- v.prev_stop;
  st.ahead <- v.ahead;
  Dynarray.truncate lines s.saved_lines;
  first_line := s.saved_first

let start () =
  Hashtbl.reset lexical_errors;
  fresh_lexical := [];
  Dynarray.clear lines;
  Dynarray.add_last lines 0;
  first_line := 1;
  ignore (P.optional (fun () -> P.consume "\xEF\xBB\xBF") ());
  ( match P.peek_char () with
  | Some '#' ->
      P.skip_while (fun c -> c <> '\n')
  | _ ->
      ()
  );
  st.prev <- Eof;
  st.prev_stop <- 0;
  st.ahead <- None;
  lex_into st ~indent:0;
  st.nl <- true;
  st.indent <- st.start - line_start ()

let trim_lines () =
  let j = max 0 (line_index st.start - 1) in
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

let found () =
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
  in
  match st.tok with
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
