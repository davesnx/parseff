open Ast
module L = Lexer

let errors : Parseff.Diagnostics.t = Parseff.Diagnostics.create ()

type Parseff.custom_error += Reported of Parseff.error

type opener = { kind : string; at : range; indent : int }
type closer = Chunk | End_only | If_branch | Until

(* The word that ended a block, already read, or the end of the input. *)
type term = T_eof | T_end | T_else | T_elseif | T_until
type ending = { term : term; term_at : range; term_indent : int }

let unclosed_hints : (string * range * range) list Parseff.Ref.t =
  Parseff.Ref.create []
let else_ifs : (range * range) list Parseff.Ref.t = Parseff.Ref.create []
let loc = L.loc
let pos = Parseff.position
let peek = Parseff.peek_char
let is_eof () = match peek () with None -> true | Some _ -> false
let next_is c = match peek () with Some d -> d = c | None -> false
let is_kw k = function L.Kw k' -> k' == k | _ -> false

let caused_by_lexical ?word ~region (e : Parseff.error) =
  (not (L.is_lexical e))
  && Hashtbl.fold
       (fun start (l : L.lexical) found ->
         found
         || start >= region && start <= e.range.stop
            &&
            match word with
            | Some `End ->
                l.ends
            | Some `Until ->
                l.untils
            | None ->
                true
       )
       L.lexical_errors false

let report ?word ?region (e : Parseff.error) =
  let consequence =
    match region with
    | Some region ->
        caused_by_lexical ?word ~region e
    | None ->
        false
  in
  if not consequence then
    Parseff.warn ~from:e.range.start ~upto:e.range.stop (Reported e)

let found (v : L.view) = L.found_tok v.tok

let fail_at ?notes ?help (v : L.view) msg : 'a =
  match v.tok with
  | L.Eof ->
      L.error ~from:(L.prev_stop ()) ?notes ?help ~eof:true msg
  | _ ->
      L.error ~from:v.start ~upto:v.stop ?notes ?help msg

let error_at_view ?notes ?help (v : L.view) msg =
  match v.tok with
  | L.Eof ->
      L.make_error ~from:(L.prev_stop ()) ?notes ?help ~eof:true msg
  | _ ->
      L.make_error ~from:v.start ~upto:v.stop ?notes ?help msg

let soft_at ?word ?region ?notes ?help v msg =
  report ?word ?region (error_at_view ?notes ?help v msg)

let is_eof_error (e : Parseff.error) =
  match e.kind with Custom (L.Lua { eof = true; _ }) -> true | _ -> false

let span a b = { start = a.start; stop = b.stop }
let mk_expr edesc erange = { edesc; erange }
let bad_expr r = mk_expr Bad_expr r

let token_hint (v : L.view) =
  match v.tok with
  | L.Other "!=" ->
      Some "Lua writes \"not equal\" as '~='"
  | L.Other "!" ->
      Some "Lua writes logical negation as 'not'"
  | L.Sym L.Amp when !L.amp_end = v.start ->
      Some "Lua writes logical \"and\" as 'and'"
  | L.Sym L.Pipe when !L.pipe_end = v.start ->
      Some "Lua writes logical \"or\" as 'or'"
  | L.Sym L.Assign ->
      Some "use '==' to compare values; '=' only assigns"
  | L.Sym L.Lbrace ->
      Some "Lua blocks do not use braces"
  | _ ->
      None

let expression_hint (v : L.view) =
  match v.tok with L.Sym (L.Assign | L.Lbrace) -> None | _ -> token_hint v

let reserved_help w =
  Printf.sprintf "'%s' is a reserved word in Lua and cannot be used as a name" w

let is_terminator = function
  | L.Kw (L.End | L.Else | L.Elseif | L.Until) ->
      true
  | _ ->
      false

let statement_only = function
  | L.Kw
      (L.Local | L.If | L.While | L.For | L.Repeat | L.Return | L.Break | L.Goto)
    ->
      true
  | _ ->
      false

let starts_statement = function
  | L.Name _
  | L.Kw
      ( L.If
      | L.While
      | L.Do
      | L.For
      | L.Repeat
      | L.Function
      | L.Local
      | L.Return
      | L.Break
      | L.Goto )
  | L.Sym (L.Lparen | L.Dcolon) ->
      true
  | _ ->
      false

(* Recovery skips one token at a time, with the depth of blocks and
   brackets. [cur] is the token at the position, read on the side. *)
type scan = {
  mutable depth : int;
  mutable bal : int;
  mutable last : int;
  mutable cur : L.view;
}

let count sc (tok : L.token) stop =
  ( match tok with
  | L.Kw (L.Function | L.If | L.Do | L.Repeat) ->
      sc.depth <- sc.depth + 1
  | L.Kw (L.End | L.Until) ->
      sc.depth <- max 0 (sc.depth - 1)
  | L.Sym (L.Lparen | L.Lbracket | L.Lbrace) ->
      sc.bal <- sc.bal + 1
  | L.Sym (L.Rparen | L.Rbracket | L.Rbrace) ->
      sc.bal <- max 0 (sc.bal - 1)
  | _ ->
      ()
  );
  sc.last <- stop

let account sc =
  count sc sc.cur.tok sc.cur.stop;
  L.skip_view sc.cur;
  sc.cur <- L.view ()

let at_eof sc = match sc.cur.tok with L.Eof -> true | _ -> false

(* [first] is a word that the parser already read: it counts as the first
   token. *)
let scan ?sc ?first ~skip_first stop =
  let sc =
    match sc with
    | Some sc ->
        sc
    | None ->
        let cur = L.view () in
        { depth = 0; bal = 0; last = cur.start; cur }
  in
  (match first with Some (f : L.view) -> count sc f.tok f.stop | None -> ());
  if skip_first && not (at_eof sc) then account sc;
  while (not (at_eof sc)) && not (stop sc) do
    account sc
  done;
  sc

let hard_stop sc =
  sc.depth = 0
  && (is_terminator sc.cur.tok || (statement_only sc.cur.tok && sc.cur.nl))

let sync_statement ?first () =
  (scan ?first ~skip_first:(Option.is_none first) (fun sc ->
       let t = sc.cur.tok in
       (sc.depth = 0 && (is_terminator t || statement_only t))
       || (sc.depth = 0 && sc.bal = 0 && sc.cur.nl && starts_statement t)
   )
  )
    .last

let closing_bracket = function
  | L.Sym (L.Rparen | L.Rbracket | L.Rbrace) ->
      true
  | _ ->
      false

let sync_elem ?sc ~stops ~line_stop ~err () =
  scan ?sc ~skip_first:false (fun sc ->
      hard_stop sc
      || sc.depth = 0 && sc.bal = 0
         && (stops sc.cur.tok || closing_bracket sc.cur.tok)
      || line_stop && sc.depth = 0 && sc.bal = 0 && sc.cur.start > err
         && sc.cur.nl
  )

let sync_kw ?first ~kw ~err () =
  scan ?first ~skip_first:false (fun sc ->
      (sc.bal = 0 && is_kw kw sc.cur.tok)
      || hard_stop sc
      || (sc.depth = 0 && sc.bal = 0 && sc.cur.start > err && sc.cur.nl)
  )

let comma = function L.Sym L.Comma -> true | _ -> false

let list_elem ?(line_stop = false) ?(skip_word = false) ?region
    ?(at_end = `Fail) ~stops parse bad =
  let fail_at_end = match at_end with `Fail -> true | `Report -> false in
  if fail_at_end && is_eof () then
    parse ()
  else
    let start = pos () in
    match L.attempt parse with
    | Ok v ->
        v
    | Error e when fail_at_end && is_eof_error e ->
        parse ()
    | Error e ->
        report ~region:(Option.value region ~default:start) e;
        let err = e.range.start in
        let sc = sync_elem ~stops ~line_stop ~err () in
        let sc =
          match sc.cur.tok with
          | L.Kw _ when skip_word && sc.cur.start = start ->
              account sc;
              sync_elem ~sc ~stops ~line_stop ~err ()
          | _ ->
              sc
        in
        bad { start; stop = max start sc.last }

exception Truncated

let header ~region ~kw parse =
  match L.attempt parse with
  | Ok v ->
      Some v
  | Error e ->
      report ~region e;
      let sc = sync_kw ~kw ~err:e.range.start () in
      if at_eof sc then raise Truncated;
      if is_kw kw sc.cur.tok then L.skip_view sc.cur;
      None

let what_header = function
  | "for" ->
      "the \"for\" header"
  | k ->
      "the \"" ^ k ^ "\" condition"

(* Reads a name; a reserved word or another token fails. The name stops at
   [L.prev_stop ()]. *)
let need_name ~what () =
  match peek () with
  | Some c when L.is_alpha c ->
      let s = pos () in
      let w = L.word () in
      if L.is_reserved w then
        L.error ~from:s ~upto:(L.prev_stop ()) ~help:(reserved_help w)
          (Printf.sprintf "expected %s, found reserved word '%s'" what w)
      else
        w
  | _ ->
      let v = L.view () in
      fail_at v (Printf.sprintf "expected %s, found %s" what (found v))

let field_name ~what () =
  match peek () with
  | Some c when L.is_alpha c ->
      let s = pos () in
      let w = L.word () in
      let r = { start = s; stop = s + String.length w } in
      if L.is_reserved w then
        report
          (L.make_error ~from:s ~upto:r.stop
             ~help:
               (Printf.sprintf
                  "'%s' is a reserved word in Lua; write [\"%s\"] to use it as \
                   a key"
                  w w
               )
             (Printf.sprintf "expected %s, found reserved word '%s'" what w)
          );
      (w, r)
  | _ ->
      let v = L.view () in
      fail_at v (Printf.sprintf "expected %s, found %s" what (found v))

let expect_view s =
  let v = L.view () in
  fail_at v
    (Printf.sprintf "expected %s, found %s" (L.quote (L.sym_text s)) (found v))

(* ['='], not ['==']. *)
let expect_assign () =
  match peek () with
  | Some '=' -> (
      let s = pos () in
      let _ = Parseff.any_char () in
      match peek () with
      | Some '=' ->
          L.error ~from:s ~upto:(s + 2) "expected \"=\", found '=='"
      | _ ->
          L.trivia ()
    )
  | _ ->
      expect_view L.Assign

let expect_comma () =
  match peek () with Some ',' -> L.sym1 () | _ -> expect_view L.Comma

let note_close opener (t : ending) =
  if t.term_indent < opener.indent then
    Parseff.Ref.set unclosed_hints
      ((opener.kind, opener.at, t.term_at) :: Parseff.Ref.get unclosed_hints)

let missing_end opener =
  let inside (r : range) = r.start > opener.at.start in
  let else_if =
    List.find_opt (fun (e, _) -> inside e) (List.rev (Parseff.Ref.get else_ifs))
  in
  let dedented =
    List.find_opt
      (fun (_, r, _) -> inside r)
      (List.rev (Parseff.Ref.get unclosed_hints))
  in
  let base =
    {
      Parseff.at = Some opener.at;
      text = Printf.sprintf "this \"%s\" is not closed" opener.kind;
    }
  in
  let notes, help =
    match (else_if, dedented) with
    | Some (r_else, r_if), _ ->
        ( [
            base;
            {
              Parseff.at = Some (span r_else r_if);
              text = "this \"else if\" opens a new \"if\"";
            };
          ],
          Some
            "\"else if\" starts a nested \"if\" that needs its own \"end\"; \
             write \"elseif\" to continue the same \"if\""
        )
    | None, Some (kind, r_open, r_end) ->
        ( [
            base;
            {
              Parseff.at = Some r_open;
              text =
                Printf.sprintf "this \"%s\" may be missing its \"end\"" kind;
            };
          ],
          Some
            (Printf.sprintf
               "the \"end\" at %s closes the \"%s\" at %s, but it is indented \
                less than that \"%s\""
               (loc r_end.start) kind (loc r_open.start) kind
            )
        )
    | None, None ->
        ([ base ], None)
  in
  let v = L.view () in
  soft_at ~word:`End ~region:opener.at.start ~notes ?help v
    (Printf.sprintf "expected \"end\" to close the \"%s\" at %s, found %s"
       opener.kind (loc opener.at.start) (found v)
    )

let close_end opener (t : ending) =
  match t.term with
  | T_end ->
      note_close opener t;
      t.term_at.stop
  | _ ->
      missing_end opener;
      L.prev_stop ()

let one_of_text = function
  | [] ->
      ""
  | [ x ] ->
      x
  | xs ->
      let rev = List.rev xs in
      String.concat ", " (List.rev (List.tl rev)) ^ " or " ^ List.hd rev

let line_of i = L.line_index i

let unclosed ~(opener : range) ~what expected =
  let v = L.view () in
  let caret = match v.tok with L.Eof -> L.prev_stop () | _ -> v.start in
  let notes =
    if line_of caret > line_of opener.start then
      [
        {
          Parseff.at = Some opener;
          text = Printf.sprintf "the %s starts here" what;
        };
      ]
    else
      []
  in
  fail_at ~notes v
    (Printf.sprintf "expected %s in the %s that starts at %s, found %s" expected
       what (loc opener.start) (found v)
    )

let quote_char c = L.quote (String.make 1 c)

let close_sym ~opener ~what c =
  match peek () with
  | Some d when d = c ->
      let p = pos () in
      L.sym1 ();
      { start = p; stop = p + 1 }
  | _ ->
      unclosed ~opener ~what (quote_char c)

(* A separator ([sep1] or [sep2]) gives [-1], the closer gives its stop. *)
let sep_or_close ~opener ~what sep1 sep2 closer =
  match peek () with
  | Some c when c = sep1 || c = sep2 ->
      L.sym1 ();
      -1
  | Some c when c = closer ->
      let p = pos () in
      L.sym1 ();
      p + 1
  | _ ->
      let seps =
        if sep1 = sep2 then
          [ sep1 ]
        else
          [ sep1; sep2 ]
      in
      unclosed ~opener ~what
        (one_of_text (List.map quote_char (seps @ [ closer ])))

let unary_priority = 12

let binop op =
  let combine a b = mk_expr (Binop (op, a, b)) (span a.erange b.erange) in
  let token () =
    L.trivia ();
    Parseff.commit ();
    combine
  in
  let prio = Printer.prio_left op in
  Some
    ( prio,
      if Printer.prio_right op < prio then
        Parseff.infix_right token
      else
        Parseff.infix_left token
    )

let op_or = binop Or
let op_and = binop And
let op_lt = binop Lt
let op_gt = binop Gt
let op_le = binop Le
let op_ge = binop Ge
let op_ne = binop Ne
let op_eq = binop Eq
let op_bxor = binop Bxor
let op_shl = binop Shl
let op_shr = binop Shr
let op_concat = binop Concat
let op_add = binop Add
let op_sub = binop Sub
let op_mul = binop Mul
let op_div = binop Div
let op_idiv = binop Idiv
let op_mod = binop Mod
let op_pow = binop Pow

let marked mark op =
  let combine a b = mk_expr (Binop (op, a, b)) (span a.erange b.erange) in
  let token () =
    mark := pos ();
    L.trivia ();
    Parseff.commit ();
    combine
  in
  Some (Printer.prio_left op, Parseff.infix_left token)

let op_band = marked L.amp_end Band
let op_bor = marked L.pipe_end Bor

let skip1 () =
  let _ = Parseff.any_char () in
  ()

(* Reads [c] if it comes next. *)
let follows c =
  match peek () with
  | Some d when d = c ->
      skip1 ();
      true
  | _ ->
      false

let not_ident () =
  match peek () with Some c -> not (L.is_alnum c) | None -> true

(* [precedence] rewinds a lookup that returns [None], so the lookup reads the
   whole operator, and the operator's token parser reads the trivia. *)
let operator () =
  match peek () with
  | Some '+' ->
      skip1 ();
      op_add
  | Some '-' ->
      skip1 ();
      op_sub
  | Some '*' ->
      skip1 ();
      op_mul
  | Some '/' ->
      skip1 ();
      if follows '/' then
        op_idiv
      else
        op_div
  | Some '%' ->
      skip1 ();
      op_mod
  | Some '^' ->
      skip1 ();
      op_pow
  | Some '&' ->
      skip1 ();
      op_band
  | Some '|' ->
      skip1 ();
      op_bor
  | Some '.' ->
      skip1 ();
      if follows '.' && not (next_is '.') then
        op_concat
      else
        None
  | Some '=' ->
      skip1 ();
      if follows '=' then
        op_eq
      else
        None
  | Some '~' ->
      skip1 ();
      if follows '=' then
        op_ne
      else
        op_bxor
  | Some '<' ->
      skip1 ();
      if follows '=' then
        op_le
      else if follows '<' then
        op_shl
      else
        op_lt
  | Some '>' ->
      skip1 ();
      if follows '=' then
        op_ge
      else if follows '>' then
        op_shr
      else
        op_gt
  | Some 'a' ->
      skip1 ();
      if follows 'n' && follows 'd' && not_ident () then
        op_and
      else
        None
  | Some 'o' ->
      skip1 ();
      if follows 'r' && not_ident () then
        op_or
      else
        None
  | _ ->
      None

let expected_expression_view v =
  fail_at ?help:(expression_hint v) v
    (Printf.sprintf "expected expression, found %s" (found v))

let expected_expression_word w s e =
  L.error ~from:s ~upto:e
    (Printf.sprintf "expected expression, found %s" (L.quoted w))

let rec expr () = Parseff.rec_ (fun () -> Parseff.precedence ~operator operand)

and unary_operand () =
  Parseff.rec_ (fun () ->
      Parseff.precedence ~min:(unary_priority + 1) ~operator operand
  )

(* An expression whose first word is already read. *)
and expr_from_word w s e ind =
  let first = ref true in
  Parseff.rec_ (fun () ->
      Parseff.precedence ~operator (fun () ->
          if !first then begin
            first := false;
            operand_word w s e ind
          end else
            operand ()
      )
  )

and operand () =
  match peek () with
  | Some c when L.is_alpha c ->
      let s = pos () in
      let ind = L.indent_at s in
      let w = L.word () in
      operand_word w s (s + String.length w) ind
  | _ ->
      operand_token ()

and unop op start =
  let e = unary_operand () in
  mk_expr (Unop (op, e)) { start; stop = e.erange.stop }

and operand_word w s e ind =
  match w with
  | "not" ->
      unop Not s
  | "nil" ->
      mk_expr Nil { start = s; stop = e }
  | "true" ->
      mk_expr True { start = s; stop = e }
  | "false" ->
      mk_expr False { start = s; stop = e }
  | "function" ->
      let op =
        { kind = "function"; at = { start = s; stop = e }; indent = ind }
      in
      let f, stop = funcbody op in
      mk_expr (Function f) { start = s; stop }
  | _ when L.is_reserved w ->
      expected_expression_word w s e
  | _ ->
      suffixes (mk_expr (Name w) { start = s; stop = e })

(* A token that is not a word: the scanner reads it, then its trivia when it
   can start an operand. *)
and operand_token () =
  let s = pos () in
  let b = !L.breaks in
  match L.lex_token () with
  | L.Sym L.Minus ->
      L.trivia ();
      unop Neg s
  | L.Sym L.Hash ->
      L.trivia ();
      unop Len s
  | L.Sym L.Tilde ->
      L.trivia ();
      unop Bnot s
  | L.Number (v, text) ->
      let stop = L.finish () in
      mk_expr (Number (v, text)) { start = s; stop }
  | L.String str ->
      let stop = L.after_token s (!L.breaks > b) in
      mk_expr (String str) { start = s; stop }
  | L.Sym L.Dots ->
      let stop = L.finish () in
      mk_expr Vararg { start = s; stop }
  | L.Sym L.Lbrace ->
      L.trivia ();
      table_after_brace s
  | L.Sym L.Lparen ->
      L.trivia ();
      suffixes (paren s)
  | L.Eof ->
      expected_expression_view (L.view ())
  | tok ->
      let v = { L.tok; start = s; stop = pos (); nl = false; inside = false } in
      expected_expression_view v

and paren s =
  let r1 = { start = s; stop = s + 1 } in
  let e = expr () in
  let r2 = close_sym ~opener:r1 ~what:"parenthesized expression" ')' in
  mk_expr (Paren e) (span r1 r2)

and primary () =
  match peek () with
  | Some c when L.is_alpha c ->
      let s = pos () in
      let w = L.word () in
      let e = s + String.length w in
      if L.is_reserved w then
        expected_expression_word w s e
      else
        mk_expr (Name w) { start = s; stop = e }
  | Some '(' ->
      let s = pos () in
      L.sym1 ();
      paren s
  | _ ->
      expected_expression_view (L.view ())

and suffixed () = suffixes (primary ())

and suffixes e =
  match peek () with
  | Some '.' -> (
      match L.peek_second () with
      | Some ('.' | '0' .. '9') ->
          e
      | _ ->
          L.sym1 ();
          let n, r = field_name ~what:"a field name after '.'" () in
          suffixes (mk_expr (Index (e, mk_expr (String n) r)) (span e.erange r))
    )
  | Some '[' -> (
      match L.peek_second () with
      | Some ('[' | '=') ->
          string_suffix e
      | _ ->
          let s = pos () in
          L.sym1 ();
          let k = expr () in
          let r =
            close_sym ~opener:{ start = s; stop = s + 1 } ~what:"index" ']'
          in
          suffixes (mk_expr (Index (e, k)) (span e.erange r))
    )
  | Some ':' -> (
      match L.peek_second () with
      | Some ':' ->
          e
      | _ ->
          L.sym1 ();
          let m = need_name ~what:"a method name after ':'" () in
          let args, stop = method_args () in
          suffixes
            (mk_expr (Method_call (e, m, args)) { start = e.erange.start; stop })
    )
  | Some ('"' | '\'') ->
      string_suffix e
  | Some '(' ->
      let args, stop = paren_args () in
      suffixes (mk_expr (Call (e, args)) { start = e.erange.start; stop })
  | Some '{' ->
      let t = table () in
      suffixes
        (mk_expr
           (Call (e, [ t ]))
           { start = e.erange.start; stop = t.erange.stop }
        )
  | _ ->
      e

(* A string argument is read on the side: an invalid one is not an argument,
   and the call ends before it. *)
and string_suffix e =
  let v = L.view () in
  match v.tok with
  | L.String s ->
      L.skip_view v;
      let arg = mk_expr (String s) { start = v.start; stop = v.stop } in
      suffixes
        (mk_expr (Call (e, [ arg ])) { start = e.erange.start; stop = v.stop })
  | _ ->
      e

and method_args () =
  let fail v =
    fail_at ~help:"a method call needs arguments, as in obj:name()" v
      (Printf.sprintf "expected arguments after the method name, found %s"
         (found v)
      )
  in
  match peek () with
  | Some '(' ->
      paren_args ()
  | Some '{' ->
      let t = table () in
      ([ t ], t.erange.stop)
  | Some ('"' | '\'' | '[') -> (
      let v = L.view () in
      match v.tok with
      | L.String s ->
          L.skip_view v;
          ([ mk_expr (String s) { start = v.start; stop = v.stop } ], v.stop)
      | _ ->
          fail v
    )
  | _ ->
      fail (L.view ())

and paren_args () =
  let s = pos () in
  L.sym1 ();
  let opener = { start = s; stop = s + 1 } in
  match peek () with
  | Some ')' ->
      let p = pos () in
      L.sym1 ();
      ([], p + 1)
  | _ ->
      let rec loop acc =
        let e = list_elem ~stops:comma expr bad_expr in
        let c = sep_or_close ~opener ~what:"argument list" ',' ',' ')' in
        if c < 0 then
          loop (e :: acc)
        else
          (List.rev (e :: acc), c)
      in
      loop []

and table () =
  let s = pos () in
  L.sym1 ();
  table_after_brace s

and table_after_brace s =
  let r1 = { start = s; stop = s + 1 } in
  let stops = function L.Sym (L.Comma | L.Semi) -> true | _ -> false in
  let rec loop acc =
    match peek () with
    | Some '}' ->
        let p = pos () in
        L.sym1 ();
        (List.rev acc, p + 1)
    | _ ->
        let f = list_elem ~stops field (fun _ -> Bad_field) in
        let c = sep_or_close ~opener:r1 ~what:"table" ',' ';' '}' in
        if c < 0 then
          loop (f :: acc)
        else
          (List.rev (f :: acc), c)
  in
  let fields, stop = loop [] in
  mk_expr (Table fields) { start = s; stop }

and field () =
  match peek () with
  | Some '[' -> (
      match L.peek_second () with
      | Some ('[' | '=') ->
          Positional (expr ())
      | _ ->
          let s = pos () in
          L.sym1 ();
          let k = expr () in
          let _ =
            close_sym ~opener:{ start = s; stop = s + 1 } ~what:"key" ']'
          in
          expect_assign ();
          Keyed (k, expr ())
    )
  | Some c when L.is_alpha c ->
      let s = pos () in
      let ind = L.indent_at s in
      let w = L.word () in
      let e = s + String.length w in
      if
        (not (L.is_reserved w))
        && next_is '='
        && match L.peek_second () with Some '=' -> false | _ -> true
      then begin
        L.sym1 ();
        Named (w, expr ())
      end else
        Positional (expr_from_word w s e ind)
  | _ ->
      Positional (expr ())

and funcbody opener =
  let params, vararg = param_list () in
  let body, t = block End_only (Some opener) in
  let stop = close_end opener t in
  ({ params; vararg; body }, stop)

and param_list () =
  if not (next_is '(') then begin
    let v = L.view () in
    fail_at v (Printf.sprintf "expected \"(\", found %s" (found v))
  end;
  let s = pos () in
  L.sym1 ();
  let opener = { start = s; stop = s + 1 } in
  match peek () with
  | Some ')' ->
      L.sym1 ();
      ([], false)
  | _ ->
      let param acc loop =
        let p =
          list_elem ~skip_word:true ~stops:comma
            (fun () -> Param (need_name ~what:"a parameter name" ()))
            (fun _ -> Bad_param)
        in
        let c = sep_or_close ~opener ~what:"parameter list" ',' ',' ')' in
        if c < 0 then
          loop (p :: acc)
        else
          (List.rev (p :: acc), false)
      in
      let rec loop acc =
        match peek () with
        | Some '.' -> (
            let v = L.view () in
            match v.tok with
            | L.Sym L.Dots ->
                L.skip_view v;
                ignore (close_sym ~opener ~what:"parameter list" ')');
                (List.rev acc, true)
            | _ ->
                param acc loop
          )
        | _ ->
            param acc loop
      in
      loop []

and explist () =
  let rec loop acc =
    let e = list_elem ~line_stop:true ~stops:comma expr bad_expr in
    if next_is ',' then begin
      L.sym1 ();
      loop (e :: acc)
    end else
      List.rev (e :: acc)
  in
  loop []

and ending_of closer w =
  match (closer, w) with
  | Chunk, _ ->
      None
  | (End_only | If_branch | Until), "end" ->
      Some T_end
  | If_branch, "else" ->
      Some T_else
  | If_branch, "elseif" ->
      Some T_elseif
  | Until, "until" ->
      Some T_until
  | _ ->
      None

(* [lead] is the keyword that must start the block, as "then" after an "if"
   condition. *)
and block ?lead closer opener : block * ending =
  Parseff.rec_ (fun () ->
      let first =
        match lead with Some (kw, op) -> expect_lead kw op | None -> None
      in
      block_loop closer opener [] None first
  )

and block_loop closer opener acc returned first =
  match first with
  | Some (w, s, e, ind) ->
      block_word closer opener acc returned w s e ind
  | None -> (
      match peek () with
      | None ->
          let p = pos () in
          ( List.rev acc,
            {
              term = T_eof;
              term_at = { start = p; stop = p };
              term_indent = L.indent_at p;
            }
          )
      | Some ';' ->
          L.sym1 ();
          block_loop closer opener acc returned None
      | Some c when L.is_alpha c ->
          let s = pos () in
          let ind = L.indent_at s in
          let w = L.word () in
          block_word closer opener acc returned w s (s + String.length w) ind
      | Some _ ->
          after_return_view returned;
          let st = statement_sym () in
          next_statement closer opener acc st
    )

and block_word closer opener acc returned w s e ind =
  match ending_of closer w with
  | Some term ->
      ( List.rev acc,
        { term; term_at = { start = s; stop = e }; term_indent = ind }
      )
  | None ->
      after_return_word returned w s e;
      let st = statement_word opener w s e ind in
      next_statement closer opener acc st

and next_statement closer opener acc st =
  L.cut ();
  let returned =
    match st.sdesc with
    | Return _ ->
        Some { start = st.srange.start; stop = st.srange.start + 6 }
    | _ ->
        None
  in
  block_loop closer opener (st :: acc) returned None

and after_return_word returned w s e =
  match returned with
  | Some r when starts_statement (L.keyword w) ->
      after_return r (L.word_view w ~start:s ~stop:e)
  | _ ->
      ()

and after_return_view returned =
  match returned with
  | Some r ->
      let v = L.view () in
      if starts_statement v.tok then after_return r v
  | None ->
      ()

and after_return r v =
  soft_at ~region:r.start
    ~notes:[ { Parseff.at = Some r; text = "this \"return\" ends the block" } ]
    ~help:
      "remove the code after \"return\", or write \"do return end\" to return \
       early"
    v
    (Printf.sprintf
       "\"return\" must be the last statement of a block, found %s after it"
       (found v)
    )

(* Before the block's first statement, [kw] must come; a missing one is
   reported, and the word read in its place starts the block. *)
and expect_lead kw opener =
  let name = L.kw_text kw in
  let missing ~from ~upto =
    report ~region:opener.at.start
      (L.make_error ~from ~upto
         ~help:
           (Printf.sprintf "add \"%s\" at the end of %s" name
              (what_header opener.kind)
           )
         (Printf.sprintf "missing \"%s\" after %s" name (what_header opener.kind)
         )
      )
  in
  let unexpected v =
    soft_at ~region:opener.at.start ?help:(token_hint v) v
      (Printf.sprintf "expected \"%s\" after %s, found %s" name
         (what_header opener.kind) (found v)
      )
  in
  match peek () with
  | Some c when L.is_alpha c ->
      let s = pos () in
      let ind = L.indent_at s in
      let nl = L.nl () and prev = L.prev_stop () in
      let w = L.word () in
      let e = s + String.length w in
      if String.equal w name then
        None
      else if nl then begin
        missing ~from:prev ~upto:e;
        Some (w, s, e, ind)
      end else begin
        let v = L.word_view w ~start:s ~stop:e in
        unexpected v;
        if is_terminator v.tok then
          Some (w, s, e, ind)
        else begin
          let sc = sync_kw ~first:v ~kw ~err:s () in
          if is_kw kw sc.cur.tok then L.skip_view sc.cur;
          None
        end
      end
  | None ->
      missing ~from:(L.prev_stop ()) ~upto:(pos ());
      None
  | Some _ ->
      let v = L.view () in
      if v.nl then
        missing ~from:(L.prev_stop ()) ~upto:v.stop
      else begin
        unexpected v;
        let sc = sync_kw ~kw ~err:v.start () in
        if is_kw kw sc.cur.tok then L.skip_view sc.cur
      end;
      None

and bad_stat start =
  { sdesc = Bad_stat; srange = { start; stop = L.prev_stop () } }

and recovered ?first start e =
  report ~region:start e;
  let last = sync_statement ?first () in
  { sdesc = Bad_stat; srange = { start; stop = last } }

(* A statement whose first word is already read. *)
and statement_word opener w s e ind =
  match w with
  | "if" -> (
      try if_stat s e ind with Truncated -> bad_stat s
    )
  | "while" -> (
      try
        let op =
          { kind = "while"; at = { start = s; stop = e }; indent = ind }
        in
        let c, lead = condition ~kw:L.Do op in
        let body, t = block ?lead End_only (Some op) in
        {
          sdesc = While (c, body);
          srange = { start = s; stop = close_end op t };
        }
      with Truncated -> bad_stat s
    )
  | "do" -> (
      try
        let op = { kind = "do"; at = { start = s; stop = e }; indent = ind } in
        let body, t = block End_only (Some op) in
        { sdesc = Do body; srange = { start = s; stop = close_end op t } }
      with Truncated -> bad_stat s
    )
  | "for" -> (
      try for_stat s e ind with Truncated -> bad_stat s
    )
  | "repeat" -> (
      try repeat_stat s e ind with Truncated -> bad_stat s
    )
  | "function" -> (
      try function_stat s e ind with Truncated -> bad_stat s
    )
  | "local" when local_function_next () -> (
      try local_function s with Truncated -> bad_stat s
    )
  | _ -> (
      let a = pos () in
      match L.attempt (fun () -> simple_word opener w s e) with
      | Ok st ->
          st
      | Error err ->
          let first =
            if pos () = a then
              Some (L.word_view w ~start:s ~stop:e)
            else
              None
          in
          recovered ?first s err
    )

and local_function_next () =
  match peek () with
  | Some 'f' ->
      String.equal (fst (Parseff.peek_with L.word_text)) "function"
  | _ ->
      false

and simple_word opener w s e =
  let mk sdesc stop = { sdesc; srange = { start = s; stop } } in
  match w with
  | "return" ->
      return_stat s e
  | "break" ->
      mk Break e
  | "goto" ->
      let n = need_name ~what:"a label name after \"goto\"" () in
      mk (Goto n) (L.prev_stop ())
  | "local" ->
      local_names s
  | "end" | "else" | "elseif" | "until" ->
      stray w s e opener
  | _ when L.is_reserved w ->
      let help =
        match w with
        | "nil" | "true" | "false" | "not" ->
            Some
              "an expression alone is not a statement; only a call or an \
               assignment is"
        | "then" ->
            Some "\"then\" must follow the condition of an \"if\" or \"elseif\""
        | _ ->
            None
      in
      L.error ~from:s ~upto:e ?help
        (Printf.sprintf "expected a statement, found %s" (L.quoted w))
  | _ ->
      expr_stat s (suffixes (mk_expr (Name w) { start = s; stop = e }))

(* A statement that starts with a token other than a word. *)
and statement_sym () =
  let a = pos () in
  match L.attempt simple_sym with Ok st -> st | Error err -> recovered a err

and simple_sym () =
  let start = pos () in
  match peek () with
  | Some '(' ->
      expr_stat start (suffixed ())
  | _ -> (
      let v = L.view () in
      match v.tok with
      | L.Sym L.Dcolon ->
          L.skip_view v;
          let n = need_name ~what:"a label name after '::'" () in
          let c = L.view () in
          ( match c.tok with
          | L.Sym L.Dcolon ->
              L.skip_view c
          | _ ->
              fail_at c
                (Printf.sprintf "expected %s, found %s" (L.quote "::") (found c))
          );
          { sdesc = Label n; srange = { start; stop = c.stop } }
      | t ->
          let help =
            match t with
            | L.Sym (L.Rbrace | L.Rparen | L.Rbracket) ->
                Some "this bracket has no matching opening bracket"
            | L.Number _
            | L.String _
            | L.Sym L.Lbrace
            | L.Kw (L.Nil | L.True | L.False | L.Not) ->
                Some
                  "an expression alone is not a statement; only a call or an \
                   assignment is"
            | L.Kw L.Then ->
                Some
                  "\"then\" must follow the condition of an \"if\" or \
                   \"elseif\""
            | _ ->
                token_hint v
          in
          fail_at ?help v
            (Printf.sprintf "expected a statement, found %s" (found v))
    )

and stray w s e opener =
  let inside =
    match opener with
    | Some o ->
        Printf.sprintf " in the body of the \"%s\" at %s" o.kind (loc o.at.start)
    | None ->
        ""
  in
  let help =
    match (opener, w) with
    | Some o, ("else" | "elseif") when o.kind = "if" ->
        Some
          "an \"if\" has at most one \"else\", and every \"elseif\" comes \
           before it"
    | Some o, _ ->
        Some
          (Printf.sprintf "the \"%s\" at %s may be missing its \"end\"" o.kind
             (loc o.at.start)
          )
    | None, "end" ->
        Some
          "there is no open block here; remove this \"end\" or check the \
           blocks above"
    | None, _ ->
        None
  in
  L.error ~from:s ~upto:e ?help (Printf.sprintf "unexpected \"%s\"%s" w inside)

and condition ~kw opener =
  match header ~region:opener.at.start ~kw expr with
  | Some c ->
      (c, Some (kw, opener))
  | None ->
      let p = L.prev_stop () in
      (bad_expr { start = p; stop = p }, None)

and if_stat s e ind =
  let op = { kind = "if"; at = { start = s; stop = e }; indent = ind } in
  let c, lead = condition ~kw:L.Then op in
  let b, t = block ?lead If_branch (Some op) in
  let rec branches acc (t : ending) =
    match t.term with
    | T_elseif ->
        let eop = { kind = "elseif"; at = t.term_at; indent = t.term_indent } in
        let c, lead = condition ~kw:L.Then eop in
        let b, t = block ?lead If_branch (Some op) in
        branches ((c, b) :: acc) t
    | T_else ->
        ( match peek () with
        | Some 'i'
          when (not (L.nl ()))
               && String.equal (fst (Parseff.peek_with L.word_text)) "if" ->
            let p = pos () in
            Parseff.Ref.set else_ifs
              ((t.term_at, { start = p; stop = p + 2 })
              :: Parseff.Ref.get else_ifs
              )
        | _ ->
            ()
        );
        let b, t = block End_only (Some op) in
        (List.rev acc, Some b, t)
    | _ ->
        (List.rev acc, None, t)
  in
  let brs, els, t = branches [ (c, b) ] t in
  let stop = close_end op t in
  { sdesc = If (brs, els); srange = { start = s; stop } }

and for_stat s e ind =
  let op = { kind = "for"; at = { start = s; stop = e }; indent = ind } in
  let r = op.at in
  let h = header ~region:s ~kw:L.Do for_header in
  let lead = match h with Some _ -> Some (L.Do, op) | None -> None in
  let body, t = block ?lead End_only (Some op) in
  let stop = close_end op t in
  let sdesc =
    match h with
    | Some (`Num (v, a, b, c)) ->
        Numeric_for (v, a, b, c, body)
    | Some (`Gen (vs, es)) ->
        Generic_for (vs, es, body)
    | None ->
        Generic_for ([], [ bad_expr r ], body)
  in
  { sdesc; srange = { start = s; stop } }

and for_header () =
  let v = need_name ~what:"a loop variable after \"for\"" () in
  let bad_after found ~from ~upto =
    L.error ~from ~upto
      (Printf.sprintf "expected '=' or \"in\" after the loop variable, found %s"
         found
      )
  in
  let generic () =
    let rec names acc =
      if next_is ',' then begin
        L.sym1 ();
        names (need_name ~what:"a loop variable after ','" () :: acc)
      end else
        List.rev acc
    in
    let vs = names [ v ] in
    ( match peek () with
    | Some c when L.is_alpha c ->
        let s = pos () in
        let w = L.word () in
        if not (String.equal w "in") then
          L.error ~from:s ~upto:(L.prev_stop ())
            (Printf.sprintf "expected \"in\", found %s" (L.quoted w))
    | _ ->
        let c = L.view () in
        fail_at c (Printf.sprintf "expected \"in\", found %s" (found c))
    );
    let rec exprs acc =
      let e = expr () in
      if next_is ',' then begin
        L.sym1 ();
        exprs (e :: acc)
      end else
        List.rev (e :: acc)
    in
    `Gen (vs, exprs [])
  in
  match peek () with
  | Some '=' ->
      let s = pos () in
      skip1 ();
      if next_is '=' then bad_after "'=='" ~from:s ~upto:(s + 2);
      L.trivia ();
      let a = expr () in
      expect_comma ();
      let b = expr () in
      let c =
        if next_is ',' then begin
          L.sym1 ();
          Some (expr ())
        end else
          None
      in
      `Num (v, a, b, c)
  | Some ',' ->
      generic ()
  | Some c when L.is_alpha c ->
      let s = pos () in
      let w = L.word () in
      if String.equal w "in" then
        let rec exprs acc =
          let e = expr () in
          if next_is ',' then begin
            L.sym1 ();
            exprs (e :: acc)
          end else
            List.rev (e :: acc)
        in
        `Gen ([ v ], exprs [])
      else
        bad_after (L.quoted w) ~from:s ~upto:(L.prev_stop ())
  | _ ->
      let c = L.view () in
      bad_after (found c) ~from:c.start ~upto:c.stop

and repeat_stat s e ind =
  let op = { kind = "repeat"; at = { start = s; stop = e }; indent = ind } in
  let r = op.at in
  let body, t = block Until (Some op) in
  let note = { Parseff.at = Some r; text = "this \"repeat\" is not closed" } in
  match t.term with
  | T_until ->
      let c =
        list_elem ~line_stop:true ~region:r.start ~at_end:`Report
          ~stops:(fun _ -> false)
          expr bad_expr
      in
      { sdesc = Repeat (body, c); srange = { start = s; stop = c.erange.stop } }
  | T_end ->
      report
        (L.make_error ~from:t.term_at.start ~upto:t.term_at.stop ~notes:[ note ]
           ~help:"a \"repeat\" loop ends with \"until\" and a condition"
           (Printf.sprintf
              "expected \"until\" to close the \"repeat\" at %s, found 'end'"
              (loc r.start)
           )
        );
      {
        sdesc = Repeat (body, bad_expr t.term_at);
        srange = { start = s; stop = t.term_at.stop };
      }
  | _ ->
      let v = L.view () in
      soft_at ~word:`Until ~region:r.start ~notes:[ note ] v
        (Printf.sprintf
           "expected \"until\" to close the \"repeat\" at %s, found %s"
           (loc r.start) (found v)
        );
      let p = L.prev_stop () in
      {
        sdesc = Repeat (body, bad_expr { start = p; stop = p });
        srange = { start = s; stop = p };
      }

and dot_next () =
  next_is '.'
  && match L.peek_second () with Some ('.' | '0' .. '9') -> false | _ -> true

and colon_next () =
  next_is ':' && match L.peek_second () with Some ':' -> false | _ -> true

and function_stat s e ind =
  let op = { kind = "function"; at = { start = s; stop = e }; indent = ind } in
  let a = pos () in
  match
    L.attempt (fun () ->
        if next_is '(' then
          fail_at
            ~help:
              "an anonymous function is an expression: assign it, as in local \
               f = function(...) ... end"
            (L.view ()) "expected a function name after \"function\", found '('";
        let n = need_name ~what:"a function name after \"function\"" () in
        let rec path acc =
          if dot_next () then begin
            L.sym1 ();
            path (need_name ~what:"a name after '.'" () :: acc)
          end else
            List.rev acc
        in
        let names = path [ n ] in
        let meth =
          if colon_next () then begin
            L.sym1 ();
            Some (need_name ~what:"a method name after ':'" ())
          end else
            None
        in
        let params = param_list () in
        (names, meth, params)
    )
  with
  | Error err ->
      let first =
        if pos () = a then
          Some (L.word_view "function" ~start:s ~stop:e)
        else
          None
      in
      recovered ?first s err
  | Ok (names, meth, (params, vararg)) ->
      let body, t = block End_only (Some op) in
      let stop = close_end op t in
      {
        sdesc = Function_stat (names, meth, { params; vararg; body });
        srange = { start = s; stop };
      }

and local_function start =
  let fs = pos () in
  let op =
    {
      kind = "function";
      at = { start = fs; stop = fs + 8 };
      indent = L.indent_at fs;
    }
  in
  match
    L.attempt (fun () ->
        let _ = L.word () in
        let name_start = pos () in
        let n = need_name ~what:"a function name after \"local function\"" () in
        let qualified =
          match peek () with
          | Some ('.' | ':') -> (
              let v = L.view () in
              match v.tok with
              | L.Sym (L.Dot | L.Colon) ->
                  let text = Buffer.create 32 in
                  Buffer.add_string text n;
                  let err = ref None in
                  let rec parts (v : L.view) =
                    match v.tok with
                    | L.Sym ((L.Dot | L.Colon) as sy) -> (
                        Buffer.add_string text (L.sym_text sy);
                        L.skip_view v;
                        let part = need_name ~what:"a name" () in
                        Buffer.add_string text part;
                        err :=
                          Some
                            (L.make_error ~from:name_start ~upto:(L.prev_stop ())
                               ~help:
                                 "write \"function a.b()\" without \"local\" \
                                  to set a field, or \"local function b()\" \
                                  for a local"
                               (Printf.sprintf
                                  "a local function cannot have a qualified \
                                   name '%s'"
                                  (Buffer.contents text)
                               )
                            );
                        match peek () with
                        | Some ('.' | ':') ->
                            parts (L.view ())
                        | _ ->
                            ()
                      )
                    | _ ->
                        ()
                  in
                  parts v;
                  Option.iter report !err;
                  true
              | _ ->
                  false
            )
          | _ ->
              false
        in
        let params = param_list () in
        (n, qualified, params)
    )
  with
  | Error e ->
      recovered start e
  | Ok (n, qualified, (params, vararg)) ->
      let body, t = block End_only (Some op) in
      let stop = close_end op t in
      {
        sdesc =
          ( if qualified then
              Bad_stat
            else
              Local_function (n, { params; vararg; body })
          );
        srange = { start; stop };
      }

and local_names start =
  let closes = ref 0 in
  let stops = function L.Sym (L.Comma | L.Assign) -> true | _ -> false in
  let rec names acc =
    let v =
      list_elem ~line_stop:true ~skip_word:true ~stops (local_name closes)
        (fun _ -> Bad_var
      )
    in
    if next_is ',' then begin
      L.sym1 ();
      names (v :: acc)
    end else
      List.rev (v :: acc)
  in
  let vars = names [] in
  let stop = L.prev_stop () in
  match (peek (), vars) with
  | Some '(', [ Var (n, None) ]
    when (not (L.nl ())) && not (parses_as_statement ()) ->
      soft_at
        ~help:
          (Printf.sprintf
             "to declare a local function, write \"local function %s(...)\"" n
          )
        (L.view ())
        (Printf.sprintf "unexpected '(' after \"local %s\"" n);
      let _, stop =
        funcbody
          {
            kind = "function";
            at = { start; stop };
            indent = L.indent_at (pos ());
          }
      in
      { sdesc = Bad_stat; srange = { start; stop } }
  | Some '=', _ when match L.peek_second () with Some '=' -> false | _ -> true
    ->
      L.sym1 ();
      let es = explist () in
      let stop =
        match List.rev es with e :: _ -> e.erange.stop | [] -> stop
      in
      { sdesc = Local (vars, es); srange = { start; stop } }
  | _ ->
      { sdesc = Local (vars, []); srange = { start; stop } }

and parses_as_statement () =
  let before = Parseff.Diagnostics.length errors in
  L.look_ahead (fun () ->
      match
        Parseff.capture (fun () ->
            match expr_stat (pos ()) (suffixed ()) with
            | _ ->
                true
            | exception Truncated ->
                false
        )
      with
      | Ok parsed ->
          parsed && Parseff.Diagnostics.length errors = before
      | Error _ ->
          false
  )

and local_name closes () =
  let n = need_name ~what:"a variable name after \"local\"" () in
  match peek () with
  | Some '<'
    when match L.peek_second () with Some ('=' | '<') -> false | _ -> true ->
      L.sym1 ();
      let attrib =
        match peek () with
        | Some c when L.is_alpha c -> (
            let s = pos () in
            let a = L.word () in
            let e = s + String.length a in
            match a with
            | "const" ->
                Const
            | "close" ->
                incr closes;
                if !closes > 1 then
                  report
                    (L.make_error ~from:s ~upto:e
                       ~help:
                         "declare each <close> variable in its own \"local\" \
                          statement"
                       "multiple to-be-closed variables in one local \
                        declaration"
                    );
                Close
            | _ when L.is_reserved a ->
                L.error ~from:s ~upto:e ~help:(reserved_help a)
                  (Printf.sprintf
                     "expected an attribute name, found reserved word '%s'" a
                  )
            | _ ->
                L.error ~from:s ~upto:e
                  ~help:"the attributes are <const> and <close>"
                  (Printf.sprintf "unknown attribute '%s'" a)
          )
        | _ ->
            ignore (need_name ~what:"an attribute name" ());
            Const
      in
      ( match peek () with
      | Some '>' -> (
          let s = pos () in
          skip1 ();
          match peek () with
          | Some (('=' | '>') as c) ->
              L.error ~from:s ~upto:(s + 2)
                (Printf.sprintf "expected \"%s\", found '>%c'" ">" c)
          | _ ->
              L.trivia ()
        )
      | _ ->
          expect_view L.Gt
      );
      Var (n, Some attrib)
  | _ ->
      Var (n, None)

and return_stat s e =
  let r = { start = s; stop = e } in
  let next_word () = L.keyword (fst (Parseff.peek_with L.word_text)) in
  let es =
    match peek () with
    | None | Some ';' ->
        []
    | Some c when L.is_alpha c && is_terminator (next_word ()) ->
        []
    | _ ->
        explist ()
  in
  let stop = match List.rev es with e :: _ -> e.erange.stop | [] -> r.stop in
  let stop =
    if next_is ';' then begin
      let p = pos () in
      L.sym1 ();
      p + 1
    end else
      stop
  in
  let fail v =
    fail_at ~help:"the values of a \"return\" are separated by ','" v
      (Printf.sprintf "unexpected %s after the return statement" (found v))
  in
  ( match peek () with
  | None ->
      ()
  | Some c when L.is_alpha c ->
      let t = next_word () in
      if not (is_terminator t || starts_statement t) then fail (L.view ())
  | Some _ ->
      let v = L.view () in
      if not (starts_statement v.tok) then fail v
  );
  { sdesc = Return es; srange = { start = s; stop } }

and expr_stat start e =
  let assign ts =
    let es = explist () in
    let stop =
      match List.rev es with x :: _ -> x.erange.stop | [] -> e.erange.stop
    in
    { sdesc = Assign (ts, es); srange = { start; stop } }
  in
  let rec targets acc =
    if next_is ',' then begin
      L.sym1 ();
      let t = suffixed () in
      check_target t;
      targets (t :: acc)
    end else
      List.rev acc
  in
  match peek () with
  | Some ',' ->
      check_target e;
      let ts = targets [ e ] in
      expect_assign ();
      assign ts
  | Some '=' ->
      let call_eq =
        match e.edesc with
        | Call _ | Method_call _ -> (
            match L.peek_second () with Some '=' -> true | _ -> false
          )
        | _ ->
            false
      in
      if call_eq then
        { sdesc = Call_stat e; srange = e.erange }
      else
        let s = pos () in
        skip1 ();
        if next_is '=' then not_a_statement ~at:s e;
        check_target e;
        L.trivia ();
        assign [ e ]
  | _ -> (
      match e.edesc with
      | Call _ | Method_call _ ->
          { sdesc = Call_stat e; srange = e.erange }
      | _ ->
          not_a_statement ~at:(pos ()) e
    )

and check_target t =
  match t.edesc with
  | Name _ | Index _ ->
      ()
  | Call _ | Method_call _ ->
      L.error ~from:t.erange.start ~upto:t.erange.stop
        "cannot assign to a function call"
  | Paren _ ->
      L.error ~from:t.erange.start ~upto:t.erange.stop
        ~help:"remove the parentheses around the target"
        "cannot assign to a parenthesized expression"
  | _ ->
      L.error ~from:t.erange.start ~upto:t.erange.stop
        "cannot assign to this expression"

(* [at] is the start of the token after [e]. *)
and not_a_statement : 'a. at:int -> expr -> 'a =
 fun ~at e ->
  let target = match e.edesc with Name n -> n | _ -> "x" in
  let generic ?help msg =
    L.error ~from:e.erange.start ~upto:e.erange.stop ?help msg
  in
  let compound =
    if pos () = at then
      let v = L.view () in
      match v.tok with
      | L.Sym
          ( ( L.Plus
            | L.Minus
            | L.Star
            | L.Slash
            | L.Dslash
            | L.Percent
            | L.Caret
            | L.Concat
            | L.Pipe
            | L.Amp
            | L.Shl
            | L.Shr ) as s
          ) ->
          Some (s, v)
      | _ ->
          None
    else
      None
  in
  match compound with
  | Some (s, v) -> (
      let op_start = v.start in
      Parseff.skip_to v.stop;
      let adjacent () =
        let n = L.view () in
        (n.tok, n.stop)
      in
      match peek () with
      | Some '=' ->
          let _, stop = adjacent () in
          L.error ~from:op_start ~upto:stop
            ~help:
              (Printf.sprintf "write %s = %s %s ..." target target (L.sym_text s)
              )
            (Printf.sprintf "Lua has no '%s=' operator" (L.sym_text s))
      | Some '+' when s == L.Plus ->
          let _, stop = adjacent () in
          L.error ~from:op_start ~upto:stop
            ~help:(Printf.sprintf "write %s = %s + 1" target target)
            "Lua has no '++' operator"
      | _ ->
          generic
            ~help:
              "only a function call or an assignment can stand alone as a \
               statement"
            "this expression is not a statement"
    )
  | None ->
      if L.comment_in_gap ~at ~from:e.erange.stop then
        generic
          ~help:
            (Printf.sprintf
               "'--' starts a comment in Lua; to decrement, write %s = %s - 1"
               target target
            )
          "this expression is not a statement"
      else if match e.edesc with Name "continue" -> true | _ -> false then
        generic
          ~help:
            "Lua has no \"continue\"; write \"goto continue\" and put the \
             label \"::continue::\" at the end of the loop body"
          "\"continue\" is not a Lua statement"
      else
        generic
          ~help:
            "only a function call or an assignment can stand alone as a \
             statement"
          "this expression is not a statement"

let returned = ref None
let started = ref false

let reset () =
  started := false;
  returned := None;
  Parseff.Diagnostics.clear errors

let one () =
  Parseff.Ref.set unclosed_hints [];
  Parseff.Ref.set else_ifs [];
  let set_returned s =
    returned :=
      match s.sdesc with
      | Return _ ->
          Some { start = s.srange.start; stop = s.srange.start + 6 }
      | _ ->
          None
  in
  match peek () with
  | None ->
      None
  | Some ';' ->
      L.sym1 ();
      None
  | Some c when L.is_alpha c ->
      let s = pos () in
      let ind = L.indent_at s in
      let w = L.word () in
      let e = s + String.length w in
      after_return_word !returned w s e;
      let st = statement_word None w s e ind in
      set_returned st;
      Some st
  | Some _ ->
      after_return_view !returned;
      let st = statement_sym () in
      set_returned st;
      Some st

let top () =
  if not !started then begin
    L.begin_chunk ();
    started := true
  end;
  L.trim_lines_at (pos ());
  one ()

let error_at source (r : range) ?notes ?help msg =
  match
    Parseff.parse ~consume:`Partial source (fun () ->
        if r.start > 0 then ignore (Parseff.Slice.take r.start);
        (L.error ~from:r.start ~upto:r.stop ?notes ?help msg : unit)
    )
  with
  | Error e ->
      e
  | Ok () ->
      assert false
