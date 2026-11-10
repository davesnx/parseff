module P = Parseff

type P.custom_error += Lua of { msg : string; help : string option }

let src = ref ""

let error ?from ?notes ?help msg : 'a = P.error ?from ?notes (Lua { msg; help })

let is_space = function
  | ' ' | '\t' | '\n' | '\r' | '\011' | '\012' ->
      true
  | _ ->
      false

let is_alpha c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c = '_'
let is_digit c = c >= '0' && c <= '9'
let is_alnum c = is_alpha c || is_digit c

let is_xdigit c = is_digit c || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F')

let is_reserved = function
  | "and"
  | "break"
  | "do"
  | "else"
  | "elseif"
  | "end"
  | "false"
  | "for"
  | "function"
  | "goto"
  | "if"
  | "in"
  | "local"
  | "nil"
  | "not"
  | "or"
  | "repeat"
  | "return"
  | "then"
  | "true"
  | "until"
  | "while" ->
      true
  | _ ->
      false

let ident_end s i =
  let n = String.length s in
  let j = ref i in
  while !j < n && is_alnum s.[!j] do
    incr j
  done;
  !j

type bracket = Open of int | Bad | Plain

let long_bracket s i =
  let n = String.length s in
  let j = ref (i + 1) in
  while !j < n && s.[!j] = '=' do
    incr j
  done;
  if !j < n && s.[!j] = '[' then
    Open (!j - i - 1)
  else if !j = i + 1 then
    Plain
  else
    Bad

let closes s j level =
  let n = String.length s in
  let k = j + 1 + level in
  k < n
  && s.[k] = ']'
  &&
  let ok = ref true in
  for m = j + 1 to k - 1 do
    if s.[m] <> '=' then ok := false
  done;
  !ok

let rec find_close s k level =
  match String.index_from_opt s k ']' with
  | None ->
      None
  | Some j ->
      if closes s j level then
        Some j
      else
        find_close s (j + 1) level

let line_end s i =
  let n = String.length s in
  let j = ref i in
  while !j < n && s.[!j] <> '\n' && s.[!j] <> '\r' do
    incr j
  done;
  !j

exception Unfinished_comment of int * int

let rec trivia_end s i =
  let n = String.length s in
  if i >= n then
    i
  else
    match s.[i] with
    | ' ' | '\t' | '\n' | '\r' | '\011' | '\012' ->
        trivia_end s (i + 1)
    | '-' when i + 1 < n && s.[i + 1] = '-' -> (
        let j = i + 2 in
        match
          if j < n && s.[j] = '[' then
            long_bracket s j
          else
            Plain
        with
        | Open level -> (
            match find_close s (j + level + 2) level with
            | Some c ->
                trivia_end s (c + level + 2)
            | None ->
                raise (Unfinished_comment (i, level))
          )
        | Plain | Bad ->
            trivia_end s (line_end s j)
      )
    | _ ->
        i

let skip_raw s i =
  try trivia_end s i with Unfinished_comment _ -> String.length s

let numeral_end s i =
  let n = String.length s in
  let k = ref (i + 1) in
  let hex = s.[i] = '0' && !k < n && (s.[!k] = 'x' || s.[!k] = 'X') in
  if hex then incr k;
  let is_expo c =
    if hex then
      c = 'p' || c = 'P'
    else
      c = 'e' || c = 'E'
  in
  let continue = ref true in
  while !continue do
    if !k < n && is_expo s.[!k] then begin
      incr k;
      if !k < n && (s.[!k] = '+' || s.[!k] = '-') then incr k
    end else if !k < n && (is_xdigit s.[!k] || s.[!k] = '.') then
      incr k
    else
      continue := false
  done;
  if !k < n && is_alpha s.[!k] then incr k;
  !k

let short_string_end s i =
  let n = String.length s in
  let q = s.[i] in
  let rec go k =
    if k >= n then
      n
    else
      match s.[k] with
      | '\n' | '\r' ->
          k
      | '\\' when k + 1 < n ->
          go (k + 2)
      | c when c = q ->
          k + 1
      | _ ->
          go (k + 1)
  in
  go (i + 1)

let utf8_len c =
  let c = Char.code c in
  if c < 0xC0 then
    1
  else if c < 0xE0 then
    2
  else if c < 0xF0 then
    3
  else
    4

let symbol_len s i =
  let n = String.length s in
  let c = s.[i] in
  let next =
    if i + 1 < n then
      s.[i + 1]
    else
      '\000'
  in
  match (c, next) with
  | '.', '.' ->
      if i + 2 < n && s.[i + 2] = '.' then
        3
      else
        2
  | ('=' | '~' | '<' | '>'), '=' | '/', '/' | ':', ':' | '<', '<' | '>', '>' ->
      2
  | c, _ when Char.code c >= 0x80 ->
      min (utf8_len c) (n - i)
  | _ ->
      1

let token_end s i =
  let n = String.length s in
  if i >= n then
    i
  else
    let c = s.[i] in
    if is_alpha c then
      ident_end s i
    else if is_digit c || (c = '.' && i + 1 < n && is_digit s.[i + 1]) then
      numeral_end s i
    else if c = '"' || c = '\'' then
      short_string_end s i
    else if c = '[' then (
      match long_bracket s i with
      | Open level -> (
          match find_close s (i + level + 2) level with
          | Some j ->
              j + level + 2
          | None ->
              n
        )
      | Plain ->
          i + 1
      | Bad ->
          let j = ref (i + 1) in
          while !j < n && s.[!j] = '=' do
            incr j
          done;
          !j
    ) else
      i + symbol_len s i

type tok = Eof | Word of string | Sym of string | Number | String

let tok_at s i =
  let n = String.length s in
  if i >= n then
    Eof
  else
    let c = s.[i] in
    if is_alpha c then
      Word (String.sub s i (ident_end s i - i))
    else if is_digit c || (c = '.' && i + 1 < n && is_digit s.[i + 1]) then
      Number
    else if c = '"' || c = '\'' then
      String
    else if c = '[' && long_bracket s i <> Plain then
      String
    else
      Sym (String.sub s i (symbol_len s i))

let sym_at s i =
  match tok_at s i with Sym x -> x | Eof | Word _ | Number | String -> ""

let escape_found t =
  let b = Buffer.create (String.length t) in
  String.iter
    (fun c ->
      match c with
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

let found_at s i =
  let n = String.length s in
  if i >= n then
    "end of input"
  else
    let stop = min (token_end s i) (line_end s i) in
    let stop = max stop (i + 1) in
    let t = String.sub s i (min (stop - i) 32) in
    let more =
      if stop - i > 32 then
        "..."
      else
        ""
    in
    "'" ^ escape_found t ^ "'" ^ more

let line_start s i =
  let rec back k =
    if k < 0 then
      true
    else
      match s.[k] with
      | ' ' | '\t' ->
          back (k - 1)
      | '\n' | '\r' ->
          true
      | _ ->
          false
  in
  back (i - 1)

let newline_between s a b =
  let rec go k = k < b && (s.[k] = '\n' || s.[k] = '\r' || go (k + 1)) in
  go a

let position = P.position

let advance_to q =
  let p = position () in
  if q > p then ignore (P.take_span (q - p))

let quote x = "\"" ^ x ^ "\""

let closing level = "]" ^ String.make level '=' ^ "]"

let lexical_errors : (int, P.error * int) Hashtbl.t = Hashtbl.create 8

let lexical ~from ~swallow ?notes ?help msg : 'a =
  if not (Hashtbl.mem lexical_errors from) then
    begin match P.capture (fun () -> (error ~from ?notes ?help msg : unit)) with
    | Error e ->
        Hashtbl.replace lexical_errors from (e, swallow)
    | Ok () ->
        assert false
  end;
  error ~from ?notes ?help msg

let is_lexical (e : P.error) =
  match Hashtbl.find_opt lexical_errors e.range.start with
  | Some (l, _) ->
      l.range = e.range && l.kind = e.kind
  | None ->
      false

let skip () =
  let s = !src in
  let p = position () in
  match trivia_end s p with
  | q ->
      if q > p then ignore (P.take_span (q - p))
  | exception Unfinished_comment (start, level) ->
      advance_to (String.length s);
      lexical ~from:start ~swallow:(String.length s)
        ~help:("close the comment with " ^ closing level)
        "unfinished long comment"

let next_token () =
  let s = !src in
  match trivia_end s (position ()) with
  | i ->
      i
  | exception Unfinished_comment _ ->
      skip ();
      assert false

let peek () =
  let i = next_token () in
  (tok_at !src i, i)

let never label =
  ignore (P.satisfy (fun _ -> false) ~label);
  assert false

let token_range f =
  skip ();
  snd (P.with_range f)

let name_token () =
  skip ();
  let s = !src in
  let p = position () in
  if p < String.length s && is_alpha s.[p] then
    let q = ident_end s p in
    let w = String.sub s p (q - p) in
    if is_reserved w then
      never "name"
    else begin
      advance_to q;
      (w, { Ast.start = p; stop = q })
    end
  else
    never "name"

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

let number () =
  skip ();
  let s = !src in
  let p = position () in
  let q = numeral_end s p in
  let text = String.sub s p (q - p) in
  advance_to q;
  match number_value text with
  | Some v ->
      (v, text, { Ast.start = p; stop = q })
  | None ->
      lexical ~from:p ~swallow:q ?help:(number_help text)
        ("malformed number '" ^ text ^ "'")

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

let short_string () =
  skip ();
  let s = !src in
  let n = String.length s in
  let p = position () in
  let q = s.[p] in
  let b = Buffer.create 16 in
  let fail_at ~from ~upto ?help msg =
    advance_to upto;
    lexical ~from ~swallow:(short_string_end s p) ?help msg
  in
  let unfinished k =
    let help =
      if k < n then
        "a short string cannot contain a line break: close it with "
        ^ String.make 1 q
        ^ " on this line, write \\n for a line break, or use a long string \
           [[...]]"
      else
        "close it with " ^ String.make 1 q
    in
    fail_at ~from:p ~upto:k ~help "unfinished string"
  in
  let bad_escape k upto msg ?help () =
    let upto =
      if upto < n && s.[upto] <> '\n' && s.[upto] <> '\r' then
        upto + 1
      else
        upto
    in
    fail_at ~from:k ~upto ?help msg
  in
  let rec loop k =
    if k >= n then
      unfinished k
    else
      match s.[k] with
      | '\n' | '\r' ->
          unfinished k
      | '\\' ->
          escape k
      | c when c = q ->
          k + 1
      | c ->
          Buffer.add_char b c;
          loop (k + 1)
  and add c k =
    Buffer.add_char b c;
    loop k
  and escape k =
    if k + 1 >= n then
      unfinished (k + 1)
    else
      match s.[k + 1] with
      | 'a' ->
          add '\007' (k + 2)
      | 'b' ->
          add '\b' (k + 2)
      | 'f' ->
          add '\012' (k + 2)
      | 'n' ->
          add '\n' (k + 2)
      | 'r' ->
          add '\r' (k + 2)
      | 't' ->
          add '\t' (k + 2)
      | 'v' ->
          add '\011' (k + 2)
      | ('\\' | '"' | '\'') as c ->
          add c (k + 2)
      | ('\n' | '\r') as c ->
          let j = k + 2 in
          let j =
            if j < n && (s.[j] = '\n' || s.[j] = '\r') && s.[j] <> c then
              j + 1
            else
              j
          in
          add '\n' j
      | 'x' ->
          let h1 = k + 2 and h2 = k + 3 in
          if h1 < n && is_xdigit s.[h1] && h2 < n && is_xdigit s.[h2] then
            add (Char.chr (int_of_string ("0x" ^ String.sub s h1 2))) (k + 4)
          else
            let bad =
              if h1 < n && is_xdigit s.[h1] then
                h2
              else
                h1
            in
            bad_escape k bad "hexadecimal digit expected in '\\x' escape"
              ~help:
                "write \\x followed by exactly two hexadecimal digits, as in \
                 \\x41"
              ()
      | 'z' ->
          let j = ref (k + 2) in
          while !j < n && is_space s.[!j] do
            incr j
          done;
          loop !j
      | 'u' ->
          unicode k
      | c when is_digit c ->
          let j = ref (k + 1) and v = ref 0 in
          while !j < n && !j < k + 4 && is_digit s.[!j] do
            v := (!v * 10) + Char.code s.[!j] - Char.code '0';
            incr j
          done;
          if !v > 255 then
            fail_at ~from:k ~upto:!j
              ~help:
                "a decimal escape is at most \\255; use \\u{...} for other \
                 characters"
              "decimal escape too large"
          else
            add (Char.chr !v) !j
      | _ ->
          bad_escape k (k + 1)
            (Printf.sprintf "invalid escape sequence '\\%s'"
               (escape_found (String.make 1 s.[k + 1]))
            )
            ~help:valid_escapes ()
  and unicode k =
    let j = k + 2 in
    if j >= n || s.[j] <> '{' then
      bad_escape k j "missing '{' in '\\u{XXX}' escape"
        ~help:"write \\u{XXX}, as in \\u{48}" ()
    else
      let rec digits m v count =
        if m < n && is_xdigit s.[m] then
          let d = int_of_string ("0x" ^ String.make 1 s.[m]) in
          let v = (v lsl 4) + d in
          if v > 0x7FFFFFFF then
            fail_at ~from:k ~upto:(m + 1) "UTF-8 value too large"
              ~help:"the largest value of a \\u{XXX} escape is 7FFFFFFF"
          else
            digits (m + 1) v (count + 1)
        else if count = 0 then
          bad_escape k m "hexadecimal digit expected in '\\u{XXX}' escape" ()
        else if m < n && s.[m] = '}' then begin
          add_utf8 b v;
          loop (m + 1)
        end else
          bad_escape k m "missing '}' in '\\u{XXX}' escape" ()
      in
      digits (j + 1) 0 0
  in
  let stop = loop (p + 1) in
  advance_to stop;
  (Buffer.contents b, { Ast.start = p; stop })

let other_close s from upto =
  let rec go k =
    match String.index_from_opt s k ']' with
    | Some j when j < upto -> (
        let m = ref (j + 1) in
        while !m < upto && s.[!m] = '=' do
          incr m
        done;
        match !m < String.length s && s.[!m] = ']' with
        | true ->
            Some (j, !m + 1)
        | false ->
            go (j + 1)
      )
    | _ ->
        None
  in
  if from >= String.length s then
    None
  else
    go from

let long_string () =
  skip ();
  let s = !src in
  let n = String.length s in
  let p = position () in
  match long_bracket s p with
  | Plain ->
      never "string"
  | Bad ->
      let j = ref (p + 1) in
      while !j < n && s.[!j] = '=' do
        incr j
      done;
      advance_to !j;
      lexical ~from:p ~swallow:!j
        ~help:
          "a long string starts with '[', any number of '=' and another '[', \
           as in [==["
        "invalid long string delimiter"
  | Open level -> (
      let body = p + level + 2 in
      match find_close s body level with
      | None ->
          let notes =
            match other_close s body n with
            | Some (a, z) ->
                [
                  {
                    P.at = Some { start = a; stop = z };
                    text =
                      Printf.sprintf
                        "this '%s' has level %d, the string needs %s"
                        (String.sub s a (z - a))
                        (z - a - 2)
                        (closing level);
                  };
                ]
            | None ->
                []
          in
          advance_to n;
          lexical ~from:p ~swallow:n ~notes
            ~help:
              (Printf.sprintf
                 "a long string that starts with %s ends at the first %s"
                 (String.sub s p (level + 2))
                 (closing level)
              )
            "unfinished long string"
      | Some j ->
          let b = Buffer.create (j - body) in
          let k = ref body in
          let newline () =
            let c = s.[!k] in
            incr k;
            if !k < j && (s.[!k] = '\n' || s.[!k] = '\r') && s.[!k] <> c then
              incr k
          in
          if !k < j && (s.[!k] = '\n' || s.[!k] = '\r') then newline ();
          while !k < j do
            match s.[!k] with
            | '\n' | '\r' ->
                Buffer.add_char b '\n';
                newline ()
            | c ->
                Buffer.add_char b c;
                incr k
          done;
          let stop = j + level + 2 in
          advance_to stop;
          (Buffer.contents b, { Ast.start = p; stop })
    )

let string_token () =
  skip ();
  let s = !src in
  let p = position () in
  if p < String.length s && (s.[p] = '"' || s.[p] = '\'') then
    short_string ()
  else
    long_string ()

let lex_token () =
  skip ();
  let s = !src in
  let p = position () in
  match tok_at s p with
  | Number ->
      ignore (P.look_ahead (fun () -> ignore (number ())))
  | String ->
      ignore (P.look_ahead (fun () -> ignore (string_token ())))
  | Eof | Word _ | Sym _ ->
      ()

let kw w =
  token_range (fun () ->
      if tok_at !src (position ()) <> Word w then lex_token ();
      P.keyword w
  )

let sym x =
  token_range (fun () ->
      let p = position () in
      if sym_at !src p = x then
        advance_to (p + String.length x)
      else begin
        lex_token ();
        never (quote x)
      end
  )
