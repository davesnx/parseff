module P = Parseff
module L = Lexer
open Ast

let errors : P.error P.Diagnostics.t = P.Diagnostics.create ()

type opener = { kind : string; at : range; indent : int }
type closer = Chunk | End_only | If_branch | Until

type P.custom_error +=
  | Deferred of {
      from : int;
      upto : int;
      msg : string;
      notes : P.note list;
      help : string option;
    }

let unclosed_hints : (string * range * range) list P.Ref.t = P.Ref.create []
let else_ifs : (range * range) list P.Ref.t = P.Ref.create []
let st = L.st
let here () = { start = st.start; stop = st.stop }
let opener kind = { kind; at = here (); indent = st.indent }
let advance = L.advance
let loc = L.loc
let is_eof () = match st.tok with L.Eof -> true | _ -> false
let is_kw k = match st.tok with L.Kw k' -> k' == k | _ -> false
let is_sym s = match st.tok with L.Sym s' -> s' == s | _ -> false

let advance_to q =
  let p = P.position () in
  if q > p then ignore (P.take_span (q - p))

let defer ~from ~upto ?(notes = []) ?help msg : 'a =
  P.error (Deferred { from; upto; msg; notes; help })

let materialize (e : P.error) =
  match e.kind with
  | Custom (Deferred d) -> (
      match
        P.capture (fun () ->
            advance_to d.upto;
            (L.error ~from:d.from ~notes:d.notes ?help:d.help d.msg : unit)
        )
      with
      | Error e ->
          e
      | Ok () ->
          assert false
    )
  | _ ->
      e

let attempt f =
  let snap = L.save () in
  match P.capture f with
  | Ok v ->
      Ok v
  | Error e ->
      L.restore snap;
      Error (materialize e)

let caused_by_lexical ?word ~region (e : P.error) =
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

let report ?word ?region (e : P.error) =
  let consequence =
    match region with
    | Some region ->
        caused_by_lexical ?word ~region e
    | None ->
        false
  in
  if not consequence then P.warn ~pos:e.range.start errors e

let fail_here ?notes ?help msg : 'a =
  match st.tok with
  | L.Eof ->
      L.error ~from:st.prev_stop ?notes ?help ~eof:true msg
  | _ ->
      L.error ~from:st.start ?notes ?help msg

let error_here ?notes ?help msg =
  match st.tok with
  | L.Eof ->
      L.make_error ~from:st.prev_stop ?notes ?help ~eof:true msg
  | _ ->
      L.make_error ~from:st.start ?notes ?help msg

let soft_here ?word ?region ?notes ?help msg =
  report ?word ?region (error_here ?notes ?help msg)

let is_eof_error (e : P.error) =
  match e.kind with Custom (L.Lua { eof = true; _ }) -> true | _ -> false

let span a b = { start = a.start; stop = b.stop }
let mk_expr edesc erange = { edesc; erange }
let bad_expr r = mk_expr Bad_expr r

let adjacent_after s =
  match st.prev with
  | L.Sym s' ->
      s' == s && st.prev_stop = st.start
  | _ ->
      false

let token_hint () =
  match st.tok with
  | L.Other "!=" ->
      Some "Lua writes \"not equal\" as '~='"
  | L.Other "!" ->
      Some "Lua writes logical negation as 'not'"
  | L.Sym L.Amp when adjacent_after L.Amp ->
      Some "Lua writes logical \"and\" as 'and'"
  | L.Sym L.Pipe when adjacent_after L.Pipe ->
      Some "Lua writes logical \"or\" as 'or'"
  | L.Sym L.Assign ->
      Some "use '==' to compare values; '=' only assigns"
  | L.Sym L.Lbrace ->
      Some "Lua blocks do not use braces"
  | _ ->
      None

let expression_hint () =
  match token_hint () with
  | Some _ as h -> (
      match st.tok with L.Sym (L.Assign | L.Lbrace) -> None | _ -> h
    )
  | None ->
      None

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

type scan = { mutable depth : int; mutable bal : int; mutable last : int }

let account sc =
  ( match st.tok with
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
  sc.last <- st.stop;
  advance ()

let scan ?(sc = { depth = 0; bal = 0; last = st.start }) ~skip_first stop =
  if skip_first && not (is_eof ()) then account sc;
  while (not (is_eof ())) && not (stop sc) do
    account sc
  done;
  sc

let hard_stop sc =
  sc.depth = 0 && (is_terminator st.tok || (statement_only st.tok && st.nl))

let sync_statement () =
  (scan ~skip_first:true (fun sc ->
       (sc.depth = 0 && (is_terminator st.tok || statement_only st.tok))
       || (sc.depth = 0 && sc.bal = 0 && st.nl && starts_statement st.tok)
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
      || (sc.depth = 0 && sc.bal = 0 && (stops st.tok || closing_bracket st.tok))
      || (line_stop && sc.depth = 0 && sc.bal = 0 && st.start > err && st.nl)
  )

let sync_kw ~kw ~err =
  ignore
    (scan ~skip_first:false (fun sc ->
         (sc.bal = 0 && is_kw kw)
         || hard_stop sc
         || (sc.depth = 0 && sc.bal = 0 && st.start > err && st.nl)
     )
    )

let comma = function L.Sym L.Comma -> true | _ -> false

let list_elem ?(line_stop = false) ?(skip_word = false) ?region
    ?(at_end = `Fail) ~stops parse bad =
  let fail_at_end = match at_end with `Fail -> true | `Report -> false in
  if fail_at_end && is_eof () then
    parse ()
  else
    let start = st.start in
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
          match st.tok with
          | L.Kw _ when skip_word && st.start = start ->
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
  let name = L.kw_text kw in
  if is_kw kw then
    advance ()
  else if is_eof () || st.nl then
    report ~region:opener.at.start
      (L.make_error ~from:st.prev_stop
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
         (what_header opener.kind) (L.found ())
      );
    sync_kw ~kw ~err:st.start;
    if is_kw kw then advance ()
  end

let need_name ~what () =
  match st.tok with
  | L.Name w ->
      let r = here () in
      advance ();
      (w, r)
  | L.Kw k ->
      let w = L.kw_text k in
      fail_here ~help:(reserved_help w)
        (Printf.sprintf "expected %s, found reserved word '%s'" what w)
  | _ ->
      fail_here (Printf.sprintf "expected %s, found %s" what (L.found ()))

let field_name ~what () =
  match st.tok with
  | L.Kw k ->
      let w = L.kw_text k in
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
         (L.quote (L.sym_text s))
         (L.found ())
      )

let note_close opener (r_end : range) =
  if st.indent < opener.indent then
    P.Ref.set unclosed_hints
      ((opener.kind, opener.at, r_end) :: P.Ref.get unclosed_hints)

let missing_end opener =
  let inside (r : range) = r.start > opener.at.start in
  let else_if =
    List.find_opt (fun (e, _) -> inside e) (List.rev (P.Ref.get else_ifs))
  in
  let dedented =
    List.find_opt
      (fun (_, r, _) -> inside r)
      (List.rev (P.Ref.get unclosed_hints))
  in
  let base =
    {
      P.at = Some opener.at;
      text = Printf.sprintf "this \"%s\" is not closed" opener.kind;
    }
  in
  let notes, help =
    match (else_if, dedented) with
    | Some (r_else, r_if), _ ->
        ( [
            base;
            {
              P.at = Some (span r_else r_if);
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
              P.at = Some r_open;
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
       opener.kind (loc opener.at.start) (L.found ())
    )

let close_end opener =
  if is_kw L.End then begin
    let r = here () in
    note_close opener r;
    advance ();
    r.stop
  end else begin
    missing_end opener;
    st.prev_stop
  end

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
  let caret =
    if is_eof () then
      st.prev_stop
    else
      st.start
  in
  let notes =
    if line_of caret > line_of opener.start then
      [
        { P.at = Some opener; text = Printf.sprintf "the %s starts here" what };
      ]
    else
      []
  in
  fail_here ~notes
    (Printf.sprintf "expected %s in the %s that starts at %s, found %s" expected
       what (loc opener.start) (L.found ())
    )

let close_sym ~opener ~what s =
  if is_sym s then begin
    let r = here () in
    advance ();
    r
  end else
    unclosed ~opener ~what (L.quote (L.sym_text s))

let sep_or_close ~opener ~what seps closer =
  match st.tok with
  | L.Sym s when List.exists (fun x -> x == s) seps ->
      advance ();
      `Sep
  | L.Sym s when s == closer ->
      let r = here () in
      advance ();
      `Close r
  | _ ->
      unclosed ~opener ~what
        (one_of_text
           (List.map (fun s -> L.quote (L.sym_text s)) (seps @ [ closer ]))
        )

let mk_bin op a b = mk_expr (Binop (op, a, b)) (span a.erange b.erange)

let binop_of = function
  | L.Kw L.Or ->
      Some Or
  | L.Kw L.And ->
      Some And
  | L.Sym L.Lt ->
      Some Lt
  | L.Sym L.Gt ->
      Some Gt
  | L.Sym L.Le ->
      Some Le
  | L.Sym L.Ge ->
      Some Ge
  | L.Sym L.Ne ->
      Some Ne
  | L.Sym L.Eq ->
      Some Eq
  | L.Sym L.Pipe ->
      Some Bor
  | L.Sym L.Tilde ->
      Some Bxor
  | L.Sym L.Amp ->
      Some Band
  | L.Sym L.Shl ->
      Some Shl
  | L.Sym L.Shr ->
      Some Shr
  | L.Sym L.Concat ->
      Some Concat
  | L.Sym L.Plus ->
      Some Add
  | L.Sym L.Minus ->
      Some Sub
  | L.Sym L.Star ->
      Some Mul
  | L.Sym L.Slash ->
      Some Div
  | L.Sym L.Dslash ->
      Some Idiv
  | L.Sym L.Percent ->
      Some Mod
  | L.Sym L.Caret ->
      Some Pow
  | _ ->
      None

let unop_of = function
  | L.Kw L.Not ->
      Some Not
  | L.Sym L.Minus ->
      Some Neg
  | L.Sym L.Hash ->
      Some Len
  | L.Sym L.Tilde ->
      Some Bnot
  | _ ->
      None

let unary_priority = 12

let expected_expression () =
  fail_here ?help:(expression_hint ())
    (Printf.sprintf "expected expression, found %s" (L.found ()))

let rec expr () = P.rec_ (fun () -> subexpr 0)

and subexpr limit =
  let left =
    match unop_of st.tok with
    | Some op ->
        let start = st.start in
        advance ();
        let e = P.rec_ (fun () -> subexpr unary_priority) in
        mk_expr (Unop (op, e)) { start; stop = e.erange.stop }
    | None ->
        simpleexp ()
  in
  climb limit left

and climb limit left =
  match binop_of st.tok with
  | Some op when Printer.prio_left op > limit ->
      advance ();
      let right = P.rec_ (fun () -> subexpr (Printer.prio_right op)) in
      climb limit (mk_bin op left right)
  | _ ->
      left

and simpleexp () =
  let r = here () in
  match st.tok with
  | L.Number (v, text) ->
      advance ();
      mk_expr (Number (v, text)) r
  | L.String s ->
      advance ();
      mk_expr (String s) r
  | L.Kw L.Nil ->
      advance ();
      mk_expr Nil r
  | L.Kw L.True ->
      advance ();
      mk_expr True r
  | L.Kw L.False ->
      advance ();
      mk_expr False r
  | L.Sym L.Dots ->
      advance ();
      mk_expr Vararg r
  | L.Sym L.Lbrace ->
      table_cons ()
  | L.Kw L.Function ->
      let op = opener "function" in
      advance ();
      let f, stop = funcbody op in
      mk_expr (Function f) { start = r.start; stop }
  | _ ->
      suffixed ()

and primary () =
  match st.tok with
  | L.Name w ->
      let r = here () in
      advance ();
      mk_expr (Name w) r
  | L.Sym L.Lparen ->
      let r1 = here () in
      advance ();
      let e = expr () in
      let r2 = close_sym ~opener:r1 ~what:"parenthesized expression" L.Rparen in
      mk_expr (Paren e) (span r1 r2)
  | _ ->
      expected_expression ()

and suffixed () = suffixes (primary ())

and suffixes e =
  match st.tok with
  | L.Sym L.Dot ->
      advance ();
      let n, r = field_name ~what:"a field name after '.'" () in
      suffixes (mk_expr (Index (e, mk_expr (String n) r)) (span e.erange r))
  | L.Sym L.Lbracket ->
      let r1 = here () in
      advance ();
      let k = expr () in
      let r = close_sym ~opener:r1 ~what:"index" L.Rbracket in
      suffixes (mk_expr (Index (e, k)) (span e.erange r))
  | L.Sym L.Colon ->
      advance ();
      let m, _ = need_name ~what:"a method name after ':'" () in
      let args, stop = call_args ~after_method:true () in
      suffixes
        (mk_expr (Method_call (e, m, args)) { start = e.erange.start; stop })
  | L.Sym L.Lparen | L.String _ | L.Sym L.Lbrace ->
      let args, stop = call_args () in
      suffixes (mk_expr (Call (e, args)) { start = e.erange.start; stop })
  | _ ->
      e

and call_args ?(after_method = false) () =
  match st.tok with
  | L.String s ->
      let r = here () in
      advance ();
      ([ mk_expr (String s) r ], r.stop)
  | L.Sym L.Lbrace ->
      let t = table_cons () in
      ([ t ], t.erange.stop)
  | L.Sym L.Lparen -> (
      let opener = here () in
      advance ();
      match st.tok with
      | L.Sym L.Rparen ->
          let r = here () in
          advance ();
          ([], r.stop)
      | _ ->
          let rec loop acc =
            let e = list_elem ~stops:comma expr bad_expr in
            match
              sep_or_close ~opener ~what:"argument list" [ L.Comma ] L.Rparen
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
             (L.found ())
          )
      else
        expected_expression ()

and table_cons () =
  let r1 = here () in
  advance ();
  let stops = function L.Sym (L.Comma | L.Semi) -> true | _ -> false in
  let rec loop acc =
    match st.tok with
    | L.Sym L.Rbrace ->
        let r = here () in
        advance ();
        (List.rev acc, r)
    | _ -> (
        let f = list_elem ~stops field (fun _ -> Bad_field) in
        match
          sep_or_close ~opener:r1 ~what:"table" [ L.Comma; L.Semi ] L.Rbrace
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
  match st.tok with
  | L.Sym L.Lbracket ->
      let r1 = here () in
      advance ();
      let k = expr () in
      ignore (close_sym ~opener:r1 ~what:"key" L.Rbracket);
      expect_sym L.Assign;
      Keyed (k, expr ())
  | L.Name n when match L.peek2 () with L.Sym L.Assign -> true | _ -> false ->
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
  if not (is_sym L.Lparen) then
    fail_here (Printf.sprintf "expected \"(\", found %s" (L.found ()));
  let opener = here () in
  advance ();
  match st.tok with
  | L.Sym L.Rparen ->
      advance ();
      ([], false)
  | _ ->
      let stops = function L.Sym L.Comma -> true | _ -> false in
      let rec loop acc =
        match st.tok with
        | L.Sym L.Dots ->
            advance ();
            ignore (close_sym ~opener ~what:"parameter list" L.Rparen);
            (List.rev acc, true)
        | _ -> (
            let p =
              list_elem ~skip_word:true ~stops
                (fun () ->
                  let n, r = need_name ~what:"a parameter name" () in
                  Param (n, r)
                )
                (fun _ -> Bad_param)
            in
            match
              sep_or_close ~opener ~what:"parameter list" [ L.Comma ] L.Rparen
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
    if is_sym L.Comma then begin
      advance ();
      loop (e :: acc)
    end else
      List.rev (e :: acc)
  in
  loop []

and block closer opener : block =
  P.rec_ (fun () ->
      let stops = function
        | L.Kw w -> (
            match (closer, w) with
            | Chunk, _ ->
                false
            | End_only, L.End ->
                true
            | If_branch, (L.End | L.Else | L.Elseif) ->
                true
            | Until, (L.Until | L.End) ->
                true
            | _ ->
                false
          )
        | _ ->
            false
      in
      let rec loop acc returned =
        match st.tok with
        | L.Eof ->
            List.rev acc
        | t when stops t ->
            List.rev acc
        | L.Sym L.Semi ->
            advance ();
            loop acc returned
        | t ->
            after_return returned t;
            let s = statement_rec opener in
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
        ~notes:[ { P.at = Some r; text = "this \"return\" ends the block" } ]
        ~help:
          "remove the code after \"return\", or write \"do return end\" to \
           return early"
        (Printf.sprintf
           "\"return\" must be the last statement of a block, found %s after it"
           (L.found ())
        )
  | _ ->
      ()

and statement_rec opener =
  let start = st.start in
  let compound =
    match st.tok with
    | L.Kw (L.If | L.While | L.Do | L.For | L.Repeat | L.Function) ->
        true
    | L.Kw L.Local -> (
        match L.peek2 () with L.Kw L.Function -> true | _ -> false
      )
    | _ ->
        false
  in
  if compound then
    try statement opener
    with Truncated ->
      { sdesc = Bad_stat; srange = { start; stop = st.prev_stop } }
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
  let start = st.start in
  let mk sdesc stop = { sdesc; srange = { start; stop } } in
  match st.tok with
  | L.Kw L.If ->
      if_stat ()
  | L.Kw L.While ->
      let op = opener_of "while" in
      advance ();
      let c = condition ~kw:L.Do op in
      let body = block End_only (Some op) in
      mk (While (c, body)) (close_end op)
  | L.Kw L.Do ->
      let op = opener_of "do" in
      advance ();
      let body = block End_only (Some op) in
      mk (Do body) (close_end op)
  | L.Kw L.For ->
      for_stat ()
  | L.Kw L.Repeat ->
      repeat_stat ()
  | L.Kw L.Function ->
      function_stat ()
  | L.Kw L.Local ->
      local_stat ()
  | L.Kw L.Return ->
      return_stat ()
  | L.Kw L.Break ->
      let r = here () in
      advance ();
      mk Break r.stop
  | L.Kw L.Goto ->
      advance ();
      let n, r = need_name ~what:"a label name after \"goto\"" () in
      mk (Goto n) r.stop
  | L.Sym L.Dcolon ->
      advance ();
      let n, _ = need_name ~what:"a label name after '::'" () in
      let stop = st.stop in
      expect_sym L.Dcolon;
      mk (Label n) stop
  | L.Kw ((L.End | L.Else | L.Elseif | L.Until) as w) ->
      stray (L.kw_text w) opener
  | L.Name _ | L.Sym L.Lparen ->
      expr_stat ()
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
            Some "\"then\" must follow the condition of an \"if\" or \"elseif\""
        | _ ->
            token_hint ()
      in
      fail_here ?help
        (Printf.sprintf "expected a statement, found %s" (L.found ()))

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
      bad_expr { start = st.prev_stop; stop = st.prev_stop }

and if_stat () =
  let start = st.start in
  let op = opener "if" in
  advance ();
  let c = condition ~kw:L.Then op in
  let b = block If_branch (Some op) in
  let rec branches acc =
    match st.tok with
    | L.Kw L.Elseif ->
        let eop = opener "elseif" in
        advance ();
        let c = condition ~kw:L.Then eop in
        let b = block If_branch (Some op) in
        branches ((c, b) :: acc)
    | L.Kw L.Else ->
        let r_else = here () in
        advance ();
        ( match st.tok with
        | L.Kw L.If when not st.nl ->
            P.Ref.set else_ifs ((r_else, here ()) :: P.Ref.get else_ifs)
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
  let start = st.start in
  let op = opener "for" in
  let r = op.at in
  advance ();
  let h = header ~region:op.at.start ~kw:L.Do for_header in
  (match h with Some _ -> expect_kw ~kw:L.Do op | None -> ());
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
  match st.tok with
  | L.Sym L.Assign ->
      advance ();
      let a = expr () in
      expect_sym L.Comma;
      let b = expr () in
      let c =
        if is_sym L.Comma then begin
          advance ();
          Some (expr ())
        end else
          None
      in
      `Num (v, a, b, c)
  | L.Sym L.Comma | L.Kw L.In ->
      let rec names acc =
        if is_sym L.Comma then begin
          advance ();
          names (fst (need_name ~what:"a loop variable after ','" ()) :: acc)
        end else
          List.rev acc
      in
      let vs = names [ v ] in
      if not (is_kw L.In) then
        fail_here (Printf.sprintf "expected \"in\", found %s" (L.found ()));
      advance ();
      let rec exprs acc =
        let e = expr () in
        if is_sym L.Comma then begin
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
           (L.found ())
        )

and repeat_stat () =
  let start = st.start in
  let op = opener "repeat" in
  let r = op.at in
  advance ();
  let body = block Until (Some op) in
  let note = { P.at = Some r; text = "this \"repeat\" is not closed" } in
  match st.tok with
  | L.Kw L.Until ->
      advance ();
      let c =
        list_elem ~line_stop:true ~region:r.start ~at_end:`Report
          ~stops:(fun _ -> false)
          expr bad_expr
      in
      { sdesc = Repeat (body, c); srange = { start; stop = c.erange.stop } }
  | L.Kw L.End ->
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
           (loc r.start) (L.found ())
        );
      let p = st.prev_stop in
      {
        sdesc = Repeat (body, bad_expr { start = p; stop = p });
        srange = { start; stop = p };
      }

and function_stat () =
  let start = st.start in
  let op = opener "function" in
  match
    attempt (fun () ->
        advance ();
        ( match st.tok with
        | L.Sym L.Lparen ->
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
          if is_sym L.Dot then begin
            advance ();
            path (fst (need_name ~what:"a name after '.'" ()) :: acc)
          end else
            List.rev acc
        in
        let names = path [ n ] in
        let meth =
          if is_sym L.Colon then begin
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
          match st.tok with
          | L.Sym (L.Dot | L.Colon) ->
              let text = Buffer.create 32 in
              Buffer.add_string text n;
              let err = ref None in
              while is_sym L.Dot || is_sym L.Colon do
                Buffer.add_string text
                  (match st.tok with L.Sym s -> L.sym_text s | _ -> "");
                advance ();
                ( match st.tok with
                | L.Name part ->
                    Buffer.add_string text part;
                    err :=
                      Some
                        (L.make_error ~from:r_name.start
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
  let start = st.start in
  advance ();
  match st.tok with
  | L.Kw L.Function ->
      local_function start
  | _ -> (
      let closes = ref 0 in
      let stops = function L.Sym (L.Comma | L.Assign) -> true | _ -> false in
      let rec names acc =
        let v =
          list_elem ~line_stop:true ~skip_word:true ~stops (local_name closes)
            (fun _ -> Bad_var
          )
        in
        if is_sym L.Comma then begin
          advance ();
          names (v :: acc)
        end else
          List.rev (v :: acc)
      in
      let vars = names [] in
      let stop = st.prev_stop in
      match (st.tok, vars) with
      | L.Sym L.Lparen, [ Var (n, None, _) ] ->
          soft_here
            ~help:
              (Printf.sprintf
                 "to declare a local function, write \"local function %s(...)\""
                 n
              )
            (Printf.sprintf "unexpected '(' after \"local %s\"" n);
          let _, stop =
            funcbody
              { kind = "function"; at = { start; stop }; indent = st.indent }
          in
          { sdesc = Bad_stat; srange = { start; stop } }
      | L.Sym L.Assign, _ ->
          advance ();
          let es = explist () in
          let stop =
            match List.rev es with e :: _ -> e.erange.stop | [] -> stop
          in
          { sdesc = Local (vars, es); srange = { start; stop } }
      | _ ->
          { sdesc = Local (vars, []); srange = { start; stop } }
    )

and local_name closes () =
  let n, r = need_name ~what:"a variable name after \"local\"" () in
  if is_sym L.Lt then begin
    advance ();
    let attrib =
      match st.tok with
      | L.Name "const" ->
          Const
      | L.Name "close" ->
          incr closes;
          if !closes > 1 then
            soft_here
              ~help:
                "declare each <close> variable in its own \"local\" statement"
              "multiple to-be-closed variables in one local declaration";
          Close
      | L.Name a ->
          fail_here ~help:"the attributes are <const> and <close>"
            (Printf.sprintf "unknown attribute '%s'" a)
      | _ ->
          ignore (need_name ~what:"an attribute name" ());
          Const
    in
    advance ();
    expect_sym L.Gt;
    Var (n, Some attrib, r)
  end else
    Var (n, None, r)

and return_stat () =
  let start = st.start in
  let r = here () in
  advance ();
  let es =
    match st.tok with
    | L.Eof | L.Sym L.Semi ->
        []
    | t when is_terminator t ->
        []
    | _ ->
        explist ()
  in
  let stop = match List.rev es with e :: _ -> e.erange.stop | [] -> r.stop in
  let stop =
    if is_sym L.Semi then begin
      let s = st.stop in
      advance ();
      s
    end else
      stop
  in
  ( match st.tok with
  | L.Eof ->
      ()
  | t when is_terminator t || starts_statement t ->
      ()
  | _ ->
      fail_here ~help:"the values of a \"return\" are separated by ','"
        (Printf.sprintf "unexpected %s after the return statement" (L.found ()))
  );
  { sdesc = Return es; srange = { start; stop } }

and expr_stat () =
  let start = st.start in
  let e = suffixed () in
  match st.tok with
  | L.Sym (L.Assign | L.Comma) ->
      check_target e;
      let rec targets acc =
        if is_sym L.Comma then begin
          advance ();
          let t = suffixed () in
          check_target t;
          targets (t :: acc)
        end else
          List.rev acc
      in
      let ts = targets [ e ] in
      expect_sym L.Assign;
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
      defer ~from:t.erange.start ~upto:t.erange.stop
        "cannot assign to a function call"
  | Paren _ ->
      defer ~from:t.erange.start ~upto:t.erange.stop
        ~help:"remove the parentheses around the target"
        "cannot assign to a parenthesized expression"
  | _ ->
      defer ~from:t.erange.start ~upto:t.erange.stop
        "cannot assign to this expression"

and not_a_statement e =
  let target = match e.edesc with Name n -> n | _ -> "x" in
  let compound =
    match st.tok with
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
        Some s
    | _ ->
        None
  in
  let generic ?help msg =
    defer ~from:e.erange.start ~upto:e.erange.stop ?help msg
  in
  match compound with
  | Some s -> (
      let op_start = st.start in
      advance ();
      match st.tok with
      | L.Sym (L.Assign | L.Eq) when st.start = st.prev_stop ->
          L.error ~from:op_start
            ~help:
              (Printf.sprintf "write %s = %s %s ..." target target (L.sym_text s)
              )
            (Printf.sprintf "Lua has no '%s=' operator" (L.sym_text s))
      | L.Sym L.Plus when s == L.Plus && st.start = st.prev_stop ->
          L.error ~from:op_start
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
      if st.comment >= e.erange.stop then
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
  P.Diagnostics.clear errors

let one () =
  P.Ref.set unclosed_hints [];
  P.Ref.set else_ifs [];
  match st.tok with
  | L.Eof ->
      None
  | L.Sym L.Semi ->
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
    L.start ();
    started := true
  end;
  L.trim_lines ();
  let rec go acc =
    let acc = match one () with Some s -> s :: acc | None -> acc in
    let exhausted =
      match P.peek_char () with None -> true | Some _ -> false
    in
    if exhausted && not (is_eof ()) then
      go acc
    else
      List.rev acc
  in
  go []

let error_at source (r : range) ?notes ?help msg =
  match
    P.parse ~consume:`Partial source (fun () ->
        if r.start > 0 then ignore (P.take_span r.start);
        if r.stop > r.start then ignore (P.take_span (r.stop - r.start));
        (L.error ~from:r.start ?notes ?help msg : unit)
    )
  with
  | Error e ->
      e
  | Ok () ->
      assert false
