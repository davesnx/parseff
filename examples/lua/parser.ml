open Ast

let errors : Parseff.error Parseff.Diagnostics.t = Parseff.Diagnostics.create ()

type opener = { kind : string; at : range; indent : int }
type closer = Chunk | End_only | If_branch | Until

let unclosed_hints : (string * range * range) list Parseff.Ref.t =
  Parseff.Ref.create []
let else_ifs : (range * range) list Parseff.Ref.t = Parseff.Ref.create []
let lexer = Lexer.lexer
let tok () = Parseff.Lexer.peek lexer
let tok_start () = Parseff.Lexer.start lexer
let tok_stop () = Parseff.Lexer.stop lexer
let advance () = Parseff.Lexer.advance lexer
let peek2 () =
  Parseff.look_ahead (fun () ->
      advance ();
      tok ()
  )
let cur () = Parseff.Lexer.current lexer
let range_of (t : _ Parseff.Lexer.token) = { start = t.start; stop = t.stop }
let here () = Parseff.Lexer.range lexer
let opener kind = { kind; at = here (); indent = Lexer.indent () }
let loc = Lexer.loc
let is_eof () = match tok () with Lexer.Eof -> true | _ -> false
let is_kw k = match tok () with Lexer.Kw k' -> k' == k | _ -> false
let is_sym s = match tok () with Lexer.Sym s' -> s' == s | _ -> false

let attempt f = Parseff.capture ~release:true f

let caused_by_lexical ?word ~region (e : Parseff.error) =
  (not (Lexer.is_lexical e))
  && Hashtbl.fold
       (fun start (l : Lexer.lexical) found ->
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
       Lexer.lexical_errors false

let report ?word ?region (e : Parseff.error) =
  let consequence =
    match region with
    | Some region ->
        caused_by_lexical ?word ~region e
    | None ->
        false
  in
  if not consequence then Parseff.warn ~pos:e.range.start errors e

let fail_here ?notes ?help msg : 'a =
  match tok () with
  | Lexer.Eof ->
      Lexer.error ~upto:(tok_stop ()) ~from:(Lexer.prev_stop ()) ?notes ?help
        ~eof:true msg
  | _ ->
      Lexer.error ~upto:(tok_stop ()) ~from:(tok_start ()) ?notes ?help msg

let error_here ?notes ?help msg =
  match tok () with
  | Lexer.Eof ->
      Lexer.make_error ~upto:(tok_stop ()) ~from:(Lexer.prev_stop ()) ?notes
        ?help ~eof:true msg
  | _ ->
      Lexer.make_error ~upto:(tok_stop ()) ~from:(tok_start ()) ?notes ?help msg

let soft_here ?word ?region ?notes ?help msg =
  report ?word ?region (error_here ?notes ?help msg)

let is_eof_error (e : Parseff.error) =
  match e.kind with Custom (Lexer.Lua { eof = true; _ }) -> true | _ -> false

let span a b = { start = a.start; stop = b.stop }
let mk_expr edesc erange = { edesc; erange }
let bad_expr r = mk_expr Bad_expr r

let adjacent_after s =
  match Parseff.Lexer.last lexer with
  | Some { value = Lexer.Sym s'; _ } ->
      s' == s && Lexer.prev_stop () = tok_start ()
  | _ ->
      false

let token_hint () =
  match tok () with
  | Lexer.Other "!=" ->
      Some "Lua writes \"not equal\" as '~='"
  | Lexer.Other "!" ->
      Some "Lua writes logical negation as 'not'"
  | Lexer.Sym Lexer.Amp when adjacent_after Lexer.Amp ->
      Some "Lua writes logical \"and\" as 'and'"
  | Lexer.Sym Lexer.Pipe when adjacent_after Lexer.Pipe ->
      Some "Lua writes logical \"or\" as 'or'"
  | Lexer.Sym Lexer.Assign ->
      Some "use '==' to compare values; '=' only assigns"
  | Lexer.Sym Lexer.Lbrace ->
      Some "Lua blocks do not use braces"
  | _ ->
      None

let expression_hint () =
  match token_hint () with
  | Some _ as h -> (
      match tok () with
      | Lexer.Sym (Lexer.Assign | Lexer.Lbrace) ->
          None
      | _ ->
          h
    )
  | None ->
      None

let reserved_help w =
  Printf.sprintf "'%s' is a reserved word in Lua and cannot be used as a name" w

let is_terminator = function
  | Lexer.Kw (Lexer.End | Lexer.Else | Lexer.Elseif | Lexer.Until) ->
      true
  | _ ->
      false

let statement_only = function
  | Lexer.Kw
      ( Lexer.Local
      | Lexer.If
      | Lexer.While
      | Lexer.For
      | Lexer.Repeat
      | Lexer.Return
      | Lexer.Break
      | Lexer.Goto ) ->
      true
  | _ ->
      false

let starts_statement = function
  | Lexer.Name _
  | Lexer.Kw
      ( Lexer.If
      | Lexer.While
      | Lexer.Do
      | Lexer.For
      | Lexer.Repeat
      | Lexer.Function
      | Lexer.Local
      | Lexer.Return
      | Lexer.Break
      | Lexer.Goto )
  | Lexer.Sym (Lexer.Lparen | Lexer.Dcolon) ->
      true
  | _ ->
      false

type scan = { mutable depth : int; mutable bal : int; mutable last : int }

let account sc =
  ( match tok () with
  | Lexer.Kw (Lexer.Function | Lexer.If | Lexer.Do | Lexer.Repeat) ->
      sc.depth <- sc.depth + 1
  | Lexer.Kw (Lexer.End | Lexer.Until) ->
      sc.depth <- max 0 (sc.depth - 1)
  | Lexer.Sym (Lexer.Lparen | Lexer.Lbracket | Lexer.Lbrace) ->
      sc.bal <- sc.bal + 1
  | Lexer.Sym (Lexer.Rparen | Lexer.Rbracket | Lexer.Rbrace) ->
      sc.bal <- max 0 (sc.bal - 1)
  | _ ->
      ()
  );
  sc.last <- tok_stop ();
  advance ()

let scan ?(sc = { depth = 0; bal = 0; last = tok_start () }) ~skip_first stop =
  if skip_first && not (is_eof ()) then account sc;
  while (not (is_eof ())) && not (stop sc) do
    account sc
  done;
  sc

let hard_stop sc =
  sc.depth = 0
  && (is_terminator (tok ()) || (statement_only (tok ()) && Lexer.nl ()))

let sync_statement () =
  (scan ~skip_first:true (fun sc ->
       (sc.depth = 0 && (is_terminator (tok ()) || statement_only (tok ())))
       || sc.depth = 0 && sc.bal = 0 && Lexer.nl ()
          && starts_statement (tok ())
   )
  )
    .last

let closing_bracket = function
  | Lexer.Sym (Lexer.Rparen | Lexer.Rbracket | Lexer.Rbrace) ->
      true
  | _ ->
      false

let sync_elem ?sc ~stops ~line_stop ~err () =
  scan ?sc ~skip_first:false (fun sc ->
      hard_stop sc
      || sc.depth = 0 && sc.bal = 0
         && (stops (tok ()) || closing_bracket (tok ()))
      || line_stop && sc.depth = 0 && sc.bal = 0
         && tok_start () > err
         && Lexer.nl ()
  )

let sync_kw ~kw ~err =
  ignore
    (scan ~skip_first:false (fun sc ->
         (sc.bal = 0 && is_kw kw)
         || hard_stop sc
         || (sc.depth = 0 && sc.bal = 0 && tok_start () > err && Lexer.nl ())
     )
    )

let comma = function Lexer.Sym Lexer.Comma -> true | _ -> false

let list_elem ?(line_stop = false) ?(skip_word = false) ?region
    ?(at_end = `Fail) ~stops parse bad =
  let fail_at_end = match at_end with `Fail -> true | `Report -> false in
  let t = cur () in
  if fail_at_end && match t.value with Lexer.Eof -> true | _ -> false then
    parse ()
  else
    let start = t.start in
    match attempt parse with
    | Ok v ->
        v
    | Error e when fail_at_end && is_eof_error e ->
        parse ()
    | Error e ->
        report ~region:(Option.value region ~default:start) e;
        let err = e.range.start in
        let sc = sync_elem ~stops ~line_stop ~err () in
        let sc =
          match tok () with
          | Lexer.Kw _ when skip_word && tok_start () = start ->
              account sc;
              sync_elem ~sc ~stops ~line_stop ~err ()
          | _ ->
              sc
        in
        bad { start; stop = max start sc.last }

exception Truncated

let header ~region ~kw parse =
  match attempt parse with
  | Ok v ->
      Some v
  | Error e ->
      report ~region e;
      sync_kw ~kw ~err:e.range.start;
      if is_eof () then raise Truncated;
      if is_kw kw then advance ();
      None

let what_header = function
  | "for" ->
      "the \"for\" header"
  | k ->
      "the \"" ^ k ^ "\" condition"

let expect_kw ~kw opener =
  let name = Lexer.kw_text kw in
  if is_kw kw then
    advance ()
  else if is_eof () || Lexer.nl () then
    report ~region:opener.at.start
      (Lexer.make_error ~upto:(tok_stop ()) ~from:(Lexer.prev_stop ())
         ~help:
           (Printf.sprintf "add \"%s\" at the end of %s" name
              (what_header opener.kind)
           )
         (Printf.sprintf "missing \"%s\" after %s" name (what_header opener.kind)
         )
      )
  else begin
    soft_here ~region:opener.at.start ?help:(token_hint ())
      (Printf.sprintf "expected \"%s\" after %s, found %s" name
         (what_header opener.kind) (Lexer.found ())
      );
    sync_kw ~kw ~err:(tok_start ());
    if is_kw kw then advance ()
  end

let need_name ~what () =
  let t = cur () in
  match t.value with
  | Lexer.Name w ->
      let r = range_of t in
      advance ();
      (w, r)
  | Lexer.Kw k ->
      let w = Lexer.kw_text k in
      fail_here ~help:(reserved_help w)
        (Printf.sprintf "expected %s, found reserved word '%s'" what w)
  | _ ->
      fail_here (Printf.sprintf "expected %s, found %s" what (Lexer.found ()))

let field_name ~what () =
  match tok () with
  | Lexer.Kw k ->
      let w = Lexer.kw_text k in
      soft_here
        ~help:
          (Printf.sprintf
             "'%s' is a reserved word in Lua; write [\"%s\"] to use it as a key"
             w w
          )
        (Printf.sprintf "expected %s, found reserved word '%s'" what w);
      let r = here () in
      advance ();
      (w, r)
  | _ ->
      need_name ~what ()

let expect_sym s =
  if is_sym s then
    advance ()
  else
    fail_here
      (Printf.sprintf "expected %s, found %s"
         (Lexer.quote (Lexer.sym_text s))
         (Lexer.found ())
      )

let note_close opener (r_end : range) =
  if Lexer.indent () < opener.indent then
    Parseff.Ref.set unclosed_hints
      ((opener.kind, opener.at, r_end) :: Parseff.Ref.get unclosed_hints)

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
  soft_here ~word:`End ~region:opener.at.start ~notes ?help
    (Printf.sprintf "expected \"end\" to close the \"%s\" at %s, found %s"
       opener.kind (loc opener.at.start) (Lexer.found ())
    )

let close_end opener =
  if is_kw Lexer.End then begin
    let r = here () in
    note_close opener r;
    advance ();
    r.stop
  end else begin
    missing_end opener;
    Lexer.prev_stop ()
  end

let one_of_text = function
  | [] ->
      ""
  | [ x ] ->
      x
  | xs ->
      let rev = List.rev xs in
      String.concat ", " (List.rev (List.tl rev)) ^ " or " ^ List.hd rev

let line_of i = Lexer.line_index i

let unclosed ~(opener : range) ~what expected =
  let caret =
    if is_eof () then
      Lexer.prev_stop ()
    else
      tok_start ()
  in
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
  fail_here ~notes
    (Printf.sprintf "expected %s in the %s that starts at %s, found %s" expected
       what (loc opener.start) (Lexer.found ())
    )

let close_sym ~opener ~what s =
  let t = cur () in
  if match t.value with Lexer.Sym s' -> s' == s | _ -> false then begin
    advance ();
    range_of t
  end else
    unclosed ~opener ~what (Lexer.quote (Lexer.sym_text s))

let sep_or_close ~opener ~what seps closer =
  let t = cur () in
  match t.value with
  | Lexer.Sym s when List.exists (fun x -> x == s) seps ->
      advance ();
      `Sep
  | Lexer.Sym s when s == closer ->
      advance ();
      `Close (range_of t)
  | _ ->
      unclosed ~opener ~what
        (one_of_text
           (List.map
              (fun s -> Lexer.quote (Lexer.sym_text s))
              (seps @ [ closer ])
           )
        )

let mk_bin op a b = mk_expr (Binop (op, a, b)) (span a.erange b.erange)

let binop_of = function
  | Lexer.Kw Lexer.Or ->
      Some Or
  | Lexer.Kw Lexer.And ->
      Some And
  | Lexer.Sym Lexer.Lt ->
      Some Lt
  | Lexer.Sym Lexer.Gt ->
      Some Gt
  | Lexer.Sym Lexer.Le ->
      Some Le
  | Lexer.Sym Lexer.Ge ->
      Some Ge
  | Lexer.Sym Lexer.Ne ->
      Some Ne
  | Lexer.Sym Lexer.Eq ->
      Some Eq
  | Lexer.Sym Lexer.Pipe ->
      Some Bor
  | Lexer.Sym Lexer.Tilde ->
      Some Bxor
  | Lexer.Sym Lexer.Amp ->
      Some Band
  | Lexer.Sym Lexer.Shl ->
      Some Shl
  | Lexer.Sym Lexer.Shr ->
      Some Shr
  | Lexer.Sym Lexer.Concat ->
      Some Concat
  | Lexer.Sym Lexer.Plus ->
      Some Add
  | Lexer.Sym Lexer.Minus ->
      Some Sub
  | Lexer.Sym Lexer.Star ->
      Some Mul
  | Lexer.Sym Lexer.Slash ->
      Some Div
  | Lexer.Sym Lexer.Dslash ->
      Some Idiv
  | Lexer.Sym Lexer.Percent ->
      Some Mod
  | Lexer.Sym Lexer.Caret ->
      Some Pow
  | _ ->
      None

let unop_of = function
  | Lexer.Kw Lexer.Not ->
      Some Not
  | Lexer.Sym Lexer.Minus ->
      Some Neg
  | Lexer.Sym Lexer.Hash ->
      Some Len
  | Lexer.Sym Lexer.Tilde ->
      Some Bnot
  | _ ->
      None

let unary_priority = 12

let expected_expression () =
  fail_here ?help:(expression_hint ())
    (Printf.sprintf "expected expression, found %s" (Lexer.found ()))

let operator () =
  match binop_of (tok ()) with
  | Some op ->
      let build () =
        advance ();
        Parseff.commit ();
        mk_bin op
      in
      Some
        ( Printer.prio_left op,
          if Printer.prio_right op < Printer.prio_left op then
            Parseff.infix_right build
          else
            Parseff.infix_left build
        )
  | None ->
      None

let rec expr () =
  Parseff.rec_ (fun () -> Parseff.precedence ~operand ~operator ())

and operand () =
  let c = cur () in
  match unop_of c.value with
  | Some op ->
      let start = c.start in
      advance ();
      let e =
        Parseff.rec_ (fun () ->
            Parseff.precedence ~min:(unary_priority + 1) ~operand ~operator ()
        )
      in
      mk_expr (Unop (op, e)) { start; stop = e.erange.stop }
  | None ->
      simpleexp c

and simpleexp c =
  let atom e =
    advance ();
    mk_expr e (range_of c)
  in
  match c.value with
  | Lexer.Number (v, text) ->
      atom (Number (v, text))
  | Lexer.String s ->
      atom (String s)
  | Lexer.Kw Lexer.Nil ->
      atom Nil
  | Lexer.Kw Lexer.True ->
      atom True
  | Lexer.Kw Lexer.False ->
      atom False
  | Lexer.Sym Lexer.Dots ->
      atom Vararg
  | Lexer.Sym Lexer.Lbrace ->
      table_cons ()
  | Lexer.Kw Lexer.Function ->
      let op = opener "function" in
      advance ();
      let f, stop = funcbody op in
      mk_expr (Function f) { start = op.at.start; stop }
  | _ ->
      suffixes (primary c)

and primary c =
  match c.value with
  | Lexer.Name w ->
      advance ();
      mk_expr (Name w) (range_of c)
  | Lexer.Sym Lexer.Lparen ->
      let r1 = range_of c in
      advance ();
      let e = expr () in
      let r2 =
        close_sym ~opener:r1 ~what:"parenthesized expression" Lexer.Rparen
      in
      mk_expr (Paren e) (span r1 r2)
  | _ ->
      expected_expression ()

and suffixed () = suffixes (primary (cur ()))

and suffixes e =
  let c = cur () in
  match c.value with
  | Lexer.Sym Lexer.Dot ->
      advance ();
      let n, r = field_name ~what:"a field name after '.'" () in
      suffixes (mk_expr (Index (e, mk_expr (String n) r)) (span e.erange r))
  | Lexer.Sym Lexer.Lbracket ->
      let r1 = range_of c in
      advance ();
      let k = expr () in
      let r = close_sym ~opener:r1 ~what:"index" Lexer.Rbracket in
      suffixes (mk_expr (Index (e, k)) (span e.erange r))
  | Lexer.Sym Lexer.Colon ->
      advance ();
      let m, _ = need_name ~what:"a method name after ':'" () in
      let args, stop = call_args ~after_method:true () in
      suffixes
        (mk_expr (Method_call (e, m, args)) { start = e.erange.start; stop })
  | Lexer.Sym Lexer.Lparen | Lexer.String _ | Lexer.Sym Lexer.Lbrace ->
      let args, stop = call_args () in
      suffixes (mk_expr (Call (e, args)) { start = e.erange.start; stop })
  | _ ->
      e

and call_args ?(after_method = false) () =
  let c = cur () in
  match c.value with
  | Lexer.String s ->
      let r = range_of c in
      advance ();
      ([ mk_expr (String s) r ], r.stop)
  | Lexer.Sym Lexer.Lbrace ->
      let t = table_cons () in
      ([ t ], t.erange.stop)
  | Lexer.Sym Lexer.Lparen -> (
      let opener = range_of c in
      advance ();
      let c = cur () in
      match c.value with
      | Lexer.Sym Lexer.Rparen ->
          advance ();
          ([], c.stop)
      | _ ->
          let rec loop acc =
            let e = list_elem ~stops:comma expr bad_expr in
            match
              sep_or_close ~opener ~what:"argument list" [ Lexer.Comma ]
                Lexer.Rparen
            with
            | `Sep ->
                loop (e :: acc)
            | `Close r ->
                (List.rev (e :: acc), r.stop)
          in
          loop []
    )
  | _ ->
      if after_method then
        fail_here ~help:"a method call needs arguments, as in obj:name()"
          (Printf.sprintf "expected arguments after the method name, found %s"
             (Lexer.found ())
          )
      else
        expected_expression ()

and table_cons () =
  let r1 = here () in
  advance ();
  let stops = function
    | Lexer.Sym (Lexer.Comma | Lexer.Semi) ->
        true
    | _ ->
        false
  in
  let rec loop acc =
    match tok () with
    | Lexer.Sym Lexer.Rbrace ->
        let r = here () in
        advance ();
        (List.rev acc, r)
    | _ -> (
        let f = list_elem ~stops field (fun _ -> Bad_field) in
        match
          sep_or_close ~opener:r1 ~what:"table"
            [ Lexer.Comma; Lexer.Semi ]
            Lexer.Rbrace
        with
        | `Sep ->
            loop (f :: acc)
        | `Close r ->
            (List.rev (f :: acc), r)
      )
  in
  let fields, r2 = loop [] in
  mk_expr (Table fields) (span r1 r2)

and field () =
  match tok () with
  | Lexer.Sym Lexer.Lbracket ->
      let r1 = here () in
      advance ();
      let k = expr () in
      ignore (close_sym ~opener:r1 ~what:"key" Lexer.Rbracket);
      expect_sym Lexer.Assign;
      Keyed (k, expr ())
  | Lexer.Name n
    when match peek2 () with Lexer.Sym Lexer.Assign -> true | _ -> false ->
      advance ();
      advance ();
      Named (n, expr ())
  | _ ->
      Positional (expr ())

and funcbody opener =
  let params, vararg = param_list () in
  let body = block End_only (Some opener) in
  let stop = close_end opener in
  ({ params; vararg; body }, stop)

and param_list () =
  if not (is_sym Lexer.Lparen) then
    fail_here (Printf.sprintf "expected \"(\", found %s" (Lexer.found ()));
  let opener = here () in
  advance ();
  match tok () with
  | Lexer.Sym Lexer.Rparen ->
      advance ();
      ([], false)
  | _ ->
      let stops = function Lexer.Sym Lexer.Comma -> true | _ -> false in
      let rec loop acc =
        match tok () with
        | Lexer.Sym Lexer.Dots ->
            advance ();
            ignore (close_sym ~opener ~what:"parameter list" Lexer.Rparen);
            (List.rev acc, true)
        | _ -> (
            let p =
              list_elem ~skip_word:true ~stops
                (fun () ->
                  let n, _ = need_name ~what:"a parameter name" () in
                  Param n
                )
                (fun _ -> Bad_param)
            in
            match
              sep_or_close ~opener ~what:"parameter list" [ Lexer.Comma ]
                Lexer.Rparen
            with
            | `Sep ->
                loop (p :: acc)
            | `Close _ ->
                (List.rev (p :: acc), false)
          )
      in
      loop []

and explist () =
  let rec loop acc =
    let e = list_elem ~line_stop:true ~stops:comma expr bad_expr in
    if is_sym Lexer.Comma then begin
      advance ();
      loop (e :: acc)
    end else
      List.rev (e :: acc)
  in
  loop []

and block closer opener : block =
  Parseff.rec_ (fun () ->
      let stops = function
        | Lexer.Kw w -> (
            match (closer, w) with
            | Chunk, _ ->
                false
            | End_only, Lexer.End ->
                true
            | If_branch, (Lexer.End | Lexer.Else | Lexer.Elseif) ->
                true
            | Until, (Lexer.Until | Lexer.End) ->
                true
            | _ ->
                false
          )
        | _ ->
            false
      in
      let rec loop acc returned =
        match tok () with
        | Lexer.Eof ->
            List.rev acc
        | t when stops t ->
            List.rev acc
        | Lexer.Sym Lexer.Semi ->
            advance ();
            loop acc returned
        | t ->
            after_return returned t;
            let s = statement_rec opener in
            Parseff.cut ();
            let returned =
              match s.sdesc with
              | Return _ ->
                  Some { start = s.srange.start; stop = s.srange.start + 6 }
              | _ ->
                  None
            in
            loop (s :: acc) returned
      in
      loop [] None
  )

and after_return returned t =
  match returned with
  | Some r when starts_statement t ->
      soft_here ~region:r.start
        ~notes:
          [ { Parseff.at = Some r; text = "this \"return\" ends the block" } ]
        ~help:
          "remove the code after \"return\", or write \"do return end\" to \
           return early"
        (Printf.sprintf
           "\"return\" must be the last statement of a block, found %s after it"
           (Lexer.found ())
        )
  | _ ->
      ()

and statement_rec opener =
  let c = cur () in
  let start = c.start in
  let compound =
    match c.value with
    | Lexer.Kw
        ( Lexer.If
        | Lexer.While
        | Lexer.Do
        | Lexer.For
        | Lexer.Repeat
        | Lexer.Function ) ->
        true
    | Lexer.Kw Lexer.Local -> (
        match peek2 () with Lexer.Kw Lexer.Function -> true | _ -> false
      )
    | _ ->
        false
  in
  if compound then
    try statement opener
    with Truncated ->
      { sdesc = Bad_stat; srange = { start; stop = Lexer.prev_stop () } }
  else
    match attempt (fun () -> statement opener) with
    | Ok s ->
        s
    | Error e ->
        recovered start e

and recovered start e =
  report ~region:start e;
  let last = sync_statement () in
  { sdesc = Bad_stat; srange = { start; stop = last } }

and statement opener =
  let c = cur () in
  let start = c.start in
  let mk sdesc stop = { sdesc; srange = { start; stop } } in
  match c.value with
  | Lexer.Kw Lexer.If ->
      if_stat ()
  | Lexer.Kw Lexer.While ->
      let op = opener_of "while" in
      advance ();
      let c = condition ~kw:Lexer.Do op in
      let body = block End_only (Some op) in
      mk (While (c, body)) (close_end op)
  | Lexer.Kw Lexer.Do ->
      let op = opener_of "do" in
      advance ();
      let body = block End_only (Some op) in
      mk (Do body) (close_end op)
  | Lexer.Kw Lexer.For ->
      for_stat ()
  | Lexer.Kw Lexer.Repeat ->
      repeat_stat ()
  | Lexer.Kw Lexer.Function ->
      function_stat ()
  | Lexer.Kw Lexer.Local ->
      local_stat ()
  | Lexer.Kw Lexer.Return ->
      return_stat ()
  | Lexer.Kw Lexer.Break ->
      let r = here () in
      advance ();
      mk Break r.stop
  | Lexer.Kw Lexer.Goto ->
      advance ();
      let n, r = need_name ~what:"a label name after \"goto\"" () in
      mk (Goto n) r.stop
  | Lexer.Sym Lexer.Dcolon ->
      advance ();
      let n, _ = need_name ~what:"a label name after '::'" () in
      let stop = tok_stop () in
      expect_sym Lexer.Dcolon;
      mk (Label n) stop
  | Lexer.Kw ((Lexer.End | Lexer.Else | Lexer.Elseif | Lexer.Until) as w) ->
      stray (Lexer.kw_text w) opener
  | Lexer.Name _ | Lexer.Sym Lexer.Lparen ->
      expr_stat ()
  | t ->
      let help =
        match t with
        | Lexer.Sym (Lexer.Rbrace | Lexer.Rparen | Lexer.Rbracket) ->
            Some "this bracket has no matching opening bracket"
        | Lexer.Number _
        | Lexer.String _
        | Lexer.Sym Lexer.Lbrace
        | Lexer.Kw (Lexer.Nil | Lexer.True | Lexer.False | Lexer.Not) ->
            Some
              "an expression alone is not a statement; only a call or an \
               assignment is"
        | Lexer.Kw Lexer.Then ->
            Some "\"then\" must follow the condition of an \"if\" or \"elseif\""
        | _ ->
            token_hint ()
      in
      fail_here ?help
        (Printf.sprintf "expected a statement, found %s" (Lexer.found ()))

and opener_of kind = opener kind

and stray w opener =
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
  fail_here ?help (Printf.sprintf "unexpected \"%s\"%s" w inside)

and condition ~kw opener =
  match header ~region:opener.at.start ~kw expr with
  | Some c ->
      expect_kw ~kw opener;
      c
  | None ->
      bad_expr { start = Lexer.prev_stop (); stop = Lexer.prev_stop () }

and if_stat () =
  let start = tok_start () in
  let op = opener "if" in
  advance ();
  let c = condition ~kw:Lexer.Then op in
  let b = block If_branch (Some op) in
  let rec branches acc =
    match tok () with
    | Lexer.Kw Lexer.Elseif ->
        let eop = opener "elseif" in
        advance ();
        let c = condition ~kw:Lexer.Then eop in
        let b = block If_branch (Some op) in
        branches ((c, b) :: acc)
    | Lexer.Kw Lexer.Else ->
        let r_else = here () in
        advance ();
        ( match tok () with
        | Lexer.Kw Lexer.If when not (Lexer.nl ()) ->
            Parseff.Ref.set else_ifs
              ((r_else, here ()) :: Parseff.Ref.get else_ifs)
        | _ ->
            ()
        );
        let b = block End_only (Some op) in
        (List.rev acc, Some b)
    | _ ->
        (List.rev acc, None)
  in
  let brs, els = branches [ (c, b) ] in
  let stop = close_end op in
  { sdesc = If (brs, els); srange = { start; stop } }

and for_stat () =
  let start = tok_start () in
  let op = opener "for" in
  let r = op.at in
  advance ();
  let h = header ~region:op.at.start ~kw:Lexer.Do for_header in
  (match h with Some _ -> expect_kw ~kw:Lexer.Do op | None -> ());
  let body = block End_only (Some op) in
  let stop = close_end op in
  let sdesc =
    match h with
    | Some (`Num (v, a, b, c)) ->
        Numeric_for (v, a, b, c, body)
    | Some (`Gen (vs, es)) ->
        Generic_for (vs, es, body)
    | None ->
        Generic_for ([], [ bad_expr r ], body)
  in
  { sdesc; srange = { start; stop } }

and for_header () =
  let v, _ = need_name ~what:"a loop variable after \"for\"" () in
  match tok () with
  | Lexer.Sym Lexer.Assign ->
      advance ();
      let a = expr () in
      expect_sym Lexer.Comma;
      let b = expr () in
      let c =
        if is_sym Lexer.Comma then begin
          advance ();
          Some (expr ())
        end else
          None
      in
      `Num (v, a, b, c)
  | Lexer.Sym Lexer.Comma | Lexer.Kw Lexer.In ->
      let rec names acc =
        if is_sym Lexer.Comma then begin
          advance ();
          names (fst (need_name ~what:"a loop variable after ','" ()) :: acc)
        end else
          List.rev acc
      in
      let vs = names [ v ] in
      if not (is_kw Lexer.In) then
        fail_here (Printf.sprintf "expected \"in\", found %s" (Lexer.found ()));
      advance ();
      let rec exprs acc =
        let e = expr () in
        if is_sym Lexer.Comma then begin
          advance ();
          exprs (e :: acc)
        end else
          List.rev (e :: acc)
      in
      `Gen (vs, exprs [])
  | _ ->
      fail_here
        (Printf.sprintf
           "expected '=' or \"in\" after the loop variable, found %s"
           (Lexer.found ())
        )

and repeat_stat () =
  let start = tok_start () in
  let op = opener "repeat" in
  let r = op.at in
  advance ();
  let body = block Until (Some op) in
  let note = { Parseff.at = Some r; text = "this \"repeat\" is not closed" } in
  match tok () with
  | Lexer.Kw Lexer.Until ->
      advance ();
      let c =
        list_elem ~line_stop:true ~region:r.start ~at_end:`Report
          ~stops:(fun _ -> false)
          expr bad_expr
      in
      { sdesc = Repeat (body, c); srange = { start; stop = c.erange.stop } }
  | Lexer.Kw Lexer.End ->
      soft_here ~notes:[ note ]
        ~help:"a \"repeat\" loop ends with \"until\" and a condition"
        (Printf.sprintf
           "expected \"until\" to close the \"repeat\" at %s, found 'end'"
           (loc r.start)
        );
      let e = here () in
      advance ();
      { sdesc = Repeat (body, bad_expr e); srange = { start; stop = e.stop } }
  | _ ->
      soft_here ~word:`Until ~region:r.start ~notes:[ note ]
        (Printf.sprintf
           "expected \"until\" to close the \"repeat\" at %s, found %s"
           (loc r.start) (Lexer.found ())
        );
      let p = Lexer.prev_stop () in
      {
        sdesc = Repeat (body, bad_expr { start = p; stop = p });
        srange = { start; stop = p };
      }

and function_stat () =
  let start = tok_start () in
  let op = opener "function" in
  match
    attempt (fun () ->
        advance ();
        ( match tok () with
        | Lexer.Sym Lexer.Lparen ->
            fail_here
              ~help:
                "an anonymous function is an expression: assign it, as in \
                 local f = function(...) ... end"
              "expected a function name after \"function\", found '('"
        | _ ->
            ()
        );
        let n, _ = need_name ~what:"a function name after \"function\"" () in
        let rec path acc =
          if is_sym Lexer.Dot then begin
            advance ();
            path (fst (need_name ~what:"a name after '.'" ()) :: acc)
          end else
            List.rev acc
        in
        let names = path [ n ] in
        let meth =
          if is_sym Lexer.Colon then begin
            advance ();
            Some (fst (need_name ~what:"a method name after ':'" ()))
          end else
            None
        in
        let params = param_list () in
        (names, meth, params)
    )
  with
  | Error e ->
      recovered start e
  | Ok (names, meth, (params, vararg)) ->
      let body = block End_only (Some op) in
      let stop = close_end op in
      {
        sdesc = Function_stat (names, meth, { params; vararg; body });
        srange = { start; stop };
      }

and local_function start =
  let op = opener "function" in
  match
    attempt (fun () ->
        advance ();
        let n, r_name =
          need_name ~what:"a function name after \"local function\"" ()
        in
        let qualified =
          match tok () with
          | Lexer.Sym (Lexer.Dot | Lexer.Colon) ->
              let text = Buffer.create 32 in
              Buffer.add_string text n;
              let err = ref None in
              while is_sym Lexer.Dot || is_sym Lexer.Colon do
                Buffer.add_string text
                  (match tok () with Lexer.Sym s -> Lexer.sym_text s | _ -> "");
                advance ();
                ( match tok () with
                | Lexer.Name part ->
                    Buffer.add_string text part;
                    err :=
                      Some
                        (Lexer.make_error ~upto:(tok_stop ()) ~from:r_name.start
                           ~help:
                             "write \"function a.b()\" without \"local\" to \
                              set a field, or \"local function b()\" for a \
                              local"
                           (Printf.sprintf
                              "a local function cannot have a qualified name \
                               '%s'"
                              (Buffer.contents text)
                           )
                        )
                | _ ->
                    ()
                );
                ignore (need_name ~what:"a name" ())
              done;
              Option.iter report !err;
              true
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
      let body = block End_only (Some op) in
      let stop = close_end op in
      {
        sdesc =
          ( if qualified then
              Bad_stat
            else
              Local_function (n, { params; vararg; body })
          );
        srange = { start; stop };
      }

and local_stat () =
  let start = tok_start () in
  advance ();
  match tok () with
  | Lexer.Kw Lexer.Function ->
      local_function start
  | _ -> (
      let closes = ref 0 in
      let stops = function
        | Lexer.Sym (Lexer.Comma | Lexer.Assign) ->
            true
        | _ ->
            false
      in
      let rec names acc =
        let v =
          list_elem ~line_stop:true ~skip_word:true ~stops (local_name closes)
            (fun _ -> Bad_var
          )
        in
        if is_sym Lexer.Comma then begin
          advance ();
          names (v :: acc)
        end else
          List.rev (v :: acc)
      in
      let vars = names [] in
      let stop = Lexer.prev_stop () in
      match (tok (), vars) with
      | Lexer.Sym Lexer.Lparen, [ Var (n, None) ]
        when (not (Lexer.nl ())) && not (parses_as_statement ()) ->
          soft_here
            ~help:
              (Printf.sprintf
                 "to declare a local function, write \"local function %s(...)\""
                 n
              )
            (Printf.sprintf "unexpected '(' after \"local %s\"" n);
          let _, stop =
            funcbody
              {
                kind = "function";
                at = { start; stop };
                indent = Lexer.indent ();
              }
          in
          { sdesc = Bad_stat; srange = { start; stop } }
      | Lexer.Sym Lexer.Assign, _ ->
          advance ();
          let es = explist () in
          let stop =
            match List.rev es with e :: _ -> e.erange.stop | [] -> stop
          in
          { sdesc = Local (vars, es); srange = { start; stop } }
      | _ ->
          { sdesc = Local (vars, []); srange = { start; stop } }
    )

and parses_as_statement () =
  let before = Parseff.Diagnostics.length errors in
  let valid =
    Parseff.look_ahead (fun () ->
        match
          Parseff.capture (fun () ->
              match expr_stat () with _ -> true | exception Truncated -> false
          )
        with
        | Ok parsed ->
            parsed && Parseff.Diagnostics.length errors = before
        | Error _ ->
            false
    )
  in
  valid

and local_name closes () =
  let n, _ = need_name ~what:"a variable name after \"local\"" () in
  if is_sym Lexer.Lt then begin
    advance ();
    let attrib =
      match tok () with
      | Lexer.Name "const" ->
          Const
      | Lexer.Name "close" ->
          incr closes;
          if !closes > 1 then
            soft_here
              ~help:
                "declare each <close> variable in its own \"local\" statement"
              "multiple to-be-closed variables in one local declaration";
          Close
      | Lexer.Name a ->
          fail_here ~help:"the attributes are <const> and <close>"
            (Printf.sprintf "unknown attribute '%s'" a)
      | _ ->
          ignore (need_name ~what:"an attribute name" ());
          Const
    in
    advance ();
    expect_sym Lexer.Gt;
    Var (n, Some attrib)
  end else
    Var (n, None)

and return_stat () =
  let start = tok_start () in
  let r = here () in
  advance ();
  let es =
    match tok () with
    | Lexer.Eof | Lexer.Sym Lexer.Semi ->
        []
    | t when is_terminator t ->
        []
    | _ ->
        explist ()
  in
  let stop = match List.rev es with e :: _ -> e.erange.stop | [] -> r.stop in
  let stop =
    if is_sym Lexer.Semi then begin
      let s = tok_stop () in
      advance ();
      s
    end else
      stop
  in
  ( match tok () with
  | Lexer.Eof ->
      ()
  | t when is_terminator t || starts_statement t ->
      ()
  | _ ->
      fail_here ~help:"the values of a \"return\" are separated by ','"
        (Printf.sprintf "unexpected %s after the return statement"
           (Lexer.found ())
        )
  );
  { sdesc = Return es; srange = { start; stop } }

and expr_stat () =
  let start = tok_start () in
  let e = suffixed () in
  match tok () with
  | Lexer.Sym (Lexer.Assign | Lexer.Comma) ->
      check_target e;
      let rec targets acc =
        if is_sym Lexer.Comma then begin
          advance ();
          let t = suffixed () in
          check_target t;
          targets (t :: acc)
        end else
          List.rev acc
      in
      let ts = targets [ e ] in
      expect_sym Lexer.Assign;
      let es = explist () in
      let stop =
        match List.rev es with x :: _ -> x.erange.stop | [] -> e.erange.stop
      in
      { sdesc = Assign (ts, es); srange = { start; stop } }
  | _ -> (
      match e.edesc with
      | Call _ | Method_call _ ->
          { sdesc = Call_stat e; srange = e.erange }
      | _ ->
          not_a_statement e
    )

and check_target t =
  match t.edesc with
  | Name _ | Index _ ->
      ()
  | Call _ | Method_call _ ->
      Lexer.error ~from:t.erange.start ~upto:t.erange.stop
        "cannot assign to a function call"
  | Paren _ ->
      Lexer.error ~from:t.erange.start ~upto:t.erange.stop
        ~help:"remove the parentheses around the target"
        "cannot assign to a parenthesized expression"
  | _ ->
      Lexer.error ~from:t.erange.start ~upto:t.erange.stop
        "cannot assign to this expression"

and not_a_statement e =
  let target = match e.edesc with Name n -> n | _ -> "x" in
  let compound =
    match tok () with
    | Lexer.Sym
        ( ( Lexer.Plus
          | Lexer.Minus
          | Lexer.Star
          | Lexer.Slash
          | Lexer.Dslash
          | Lexer.Percent
          | Lexer.Caret
          | Lexer.Concat
          | Lexer.Pipe
          | Lexer.Amp
          | Lexer.Shl
          | Lexer.Shr ) as s
        ) ->
        Some s
    | _ ->
        None
  in
  let generic ?help msg =
    Lexer.error ~from:e.erange.start ~upto:e.erange.stop ?help msg
  in
  match compound with
  | Some s -> (
      let op_start = tok_start () in
      advance ();
      match tok () with
      | Lexer.Sym (Lexer.Assign | Lexer.Eq)
        when tok_start () = Lexer.prev_stop () ->
          Lexer.error ~upto:(tok_stop ()) ~from:op_start
            ~help:
              (Printf.sprintf "write %s = %s %s ..." target target
                 (Lexer.sym_text s)
              )
            (Printf.sprintf "Lua has no '%s=' operator" (Lexer.sym_text s))
      | Lexer.Sym Lexer.Plus
        when s == Lexer.Plus && tok_start () = Lexer.prev_stop () ->
          Lexer.error ~upto:(tok_stop ()) ~from:op_start
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
      if Lexer.comment_follows () then
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
  match tok () with
  | Lexer.Eof ->
      advance ();
      None
  | Lexer.Sym Lexer.Semi ->
      advance ();
      None
  | t ->
      after_return !returned t;
      let s = statement_rec None in
      (returned :=
         match s.sdesc with
         | Return _ ->
             Some { start = s.srange.start; stop = s.srange.start + 6 }
         | _ ->
             None
      );
      Some s

let top () =
  if not !started then begin
    Lexer.begin_input ();
    started := true
  end;
  Lexer.trim_lines ();
  one ()

let error_at source (r : range) ?notes ?help msg =
  match
    Parseff.parse ~consume:`Partial source (fun () ->
        if r.start > 0 then ignore (Parseff.take_span r.start);
        (Lexer.error ~from:r.start ~upto:r.stop ?notes ?help msg : unit)
    )
  with
  | Error e ->
      e
  | Ok () ->
      assert false
