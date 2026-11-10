module P = Parseff
open Ast
open Lexer

let errors : P.error P.Diagnostics.t = P.Diagnostics.create ()

type opener = { kind : string; at : range }
type closer = Chunk | End_only | If_branch | Until

let unclosed_hints : (string * range * range) list P.Ref.t = P.Ref.create []
let else_ifs : (range * range) list P.Ref.t = P.Ref.create []

let loc i =
  let l = P.location_of_position !src i in
  Printf.sprintf "%d:%d" l.line l.col

let make_error ?from ?upto ?notes ?help msg =
  match
    P.capture (fun () ->
        Option.iter advance_to upto;
        (error ?from ?notes ?help msg : unit)
    )
  with
  | Error e ->
      e
  | Ok () ->
      assert false

let contains_word s a b w =
  let rec go k =
    k < b
    && (is_alpha s.[k]
        && (k = 0 || not (is_alnum s.[k - 1]))
        && String.sub s k (ident_end s k - k) = w
       || go (k + 1)
       )
  in
  go a

let caused_by_lexical ?word ~region (e : P.error) =
  (not (is_lexical e))
  && Hashtbl.fold
       (fun start (_, swallow) found ->
         found
         || start >= region && start <= e.range.start
            &&
            match word with
            | Some w ->
                contains_word !src start swallow w
            | None ->
                true
       )
       lexical_errors false

let report ?word ?region (e : P.error) =
  let consequence =
    match region with
    | Some region ->
        caused_by_lexical ?word ~region e
    | None ->
        false
  in
  if not consequence then P.warn ~pos:e.range.start errors e

let token_stop s i =
  if i >= String.length s then
    i
  else if s.[i] = '!' && i + 1 < String.length s && s.[i + 1] = '=' then
    i + 2
  else
    max (min (token_end s i) (line_end s i)) (i + 1)

let error_at_token ?notes ?help msg : 'a =
  let s = !src in
  if next_token () >= String.length s then
    error ?notes ?help msg
  else begin
    lex_token ();
    let p = position () in
    advance_to (token_stop s p);
    error ~from:p ?notes ?help msg
  end

let soft_at_token ?region ?notes ?help msg =
  report ?region
    ( match P.capture (fun () -> (error_at_token ?notes ?help msg : unit)) with
    | Error e ->
        e
    | Ok () ->
        assert false
    )

let span a b = { start = a.start; stop = b.stop }
let mk_expr edesc erange = { edesc; erange }
let bad_expr r = mk_expr Bad_expr r

let token_hint s i =
  let n = String.length s in
  let at k =
    if k < n then
      s.[k]
    else
      '\000'
  in
  match tok_at s i with
  | Sym "!" when at (i + 1) = '=' ->
      Some "Lua writes \"not equal\" as '~='"
  | Sym "!" ->
      Some "Lua writes logical negation as 'not'"
  | Sym "&" when at (i + 1) = '&' ->
      Some "Lua writes logical \"and\" as 'and'"
  | Sym "|" when at (i + 1) = '|' ->
      Some "Lua writes logical \"or\" as 'or'"
  | Sym "=" ->
      Some "use '==' to compare values; '=' only assigns"
  | Sym "{" ->
      Some "Lua blocks do not use braces"
  | _ ->
      None

let block_delta = function
  | "function" | "if" | "do" | "repeat" ->
      1
  | "end" | "until" ->
      -1
  | _ ->
      0

let bracket_delta = function
  | "(" | "[" | "{" ->
      1
  | ")" | "]" | "}" ->
      -1
  | _ ->
      0

let is_terminator = function
  | "end" | "else" | "elseif" | "until" ->
      true
  | _ ->
      false

let statement_only = function
  | "local" | "if" | "while" | "for" | "repeat" | "return" | "break" | "goto" ->
      true
  | _ ->
      false

let starts_statement = function
  | Word w ->
      (not (is_reserved w)) || statement_only w || w = "function" || w = "do"
  | Sym ("(" | "::") ->
      true
  | Eof | Sym _ | Number | String ->
      false

type scan = { mutable depth : int; mutable bal : int; mutable last : int }

let account sc s k =
  let e = token_end s k in
  ( match tok_at s k with
  | Word w ->
      sc.depth <- max 0 (sc.depth + block_delta w)
  | Sym x ->
      sc.bal <- max 0 (sc.bal + bracket_delta x)
  | Eof | Number | String ->
      ()
  );
  sc.last <- e;
  e

let scan s i ~skip_first ~stop =
  let sc = { depth = 0; bal = 0; last = i } in
  let n = String.length s in
  let rec go j =
    let k = skip_raw s j in
    if k >= n then
      k
    else if stop sc k (tok_at s k) then
      k
    else
      go (account sc s k)
  in
  let k = skip_raw s i in
  let j =
    if skip_first && k < n then
      account sc s k
    else
      k
  in
  let target = go j in
  (target, sc.last)

let hard_stop s sc k = function
  | Word w ->
      sc.depth = 0 && (is_terminator w || (statement_only w && line_start s k))
  | Eof | Sym _ | Number | String ->
      false

let sync_statement s i =
  scan s i ~skip_first:true ~stop:(fun sc k t ->
      (sc.depth = 0
      &&
      match t with
      | Word w ->
          is_terminator w || statement_only w
      | Eof | Sym _ | Number | String ->
          false
      )
      || (sc.depth = 0 && sc.bal = 0 && line_start s k && starts_statement t)
  )

let sync_elem s i ~stops ~line_stop ~err =
  scan s i ~skip_first:false ~stop:(fun sc k t ->
      hard_stop s sc k t
      || (sc.depth = 0 && sc.bal = 0
         &&
         match t with
         | Sym x ->
             List.mem x stops || bracket_delta x < 0
         | Eof | Word _ | Number | String ->
             false
         )
      || (line_stop && sc.depth = 0 && sc.bal = 0 && k > err && line_start s k)
  )

let sync_kw s i ~kw ~err =
  scan s i ~skip_first:false ~stop:(fun sc k t ->
      (sc.bal = 0 && t = Word kw)
      || hard_stop s sc k t
      || (sc.depth = 0 && sc.bal = 0 && k > err && line_start s k)
  )

let at_end () = next_token () >= String.length !src

let list_elem ?(line_stop = false) ?(skip_word = false) ~stops parse bad =
  skip ();
  let s = !src in
  let n = String.length s in
  let start = position () in
  if start >= n then
    parse ()
  else
    match P.capture parse with
    | Ok v ->
        v
    | Error e when e.range.start >= n ->
        parse ()
    | Error e ->
        report ~region:start e;
        let err = e.range.start in
        let target, last = sync_elem s start ~stops ~line_stop ~err in
        let target, last =
          match tok_at s start with
          | Word w when skip_word && target = start ->
              sync_elem s (start + String.length w) ~stops ~line_stop ~err
          | _ ->
              (target, last)
        in
        advance_to target;
        bad { start; stop = max start last }

let header ~kw parse =
  skip ();
  let s = !src in
  let n = String.length s in
  let start = position () in
  if start >= n then
    Some (parse ())
  else
    match P.capture parse with
    | Ok v ->
        Some v
    | Error e when e.range.start >= n ->
        Some (parse ())
    | Error e ->
        report ~region:start e;
        let target, _ = sync_kw s start ~kw ~err:e.range.start in
        advance_to target;
        if tok_at s target = Word kw then ignore (Lexer.kw kw);
        None

let what_header = function
  | "for" ->
      "the \"for\" header"
  | k ->
      "the \"" ^ k ^ "\" condition"

let expect_kw ~kw opener =
  let s = !src in
  let here = position () in
  let t, i = peek () in
  if t = Word kw then
    ignore (Lexer.kw kw)
  else if t = Eof || newline_between s here i then
    report
      (make_error
         ~help:
           (Printf.sprintf "add \"%s\" at the end of %s" kw
              (what_header opener.kind)
           )
         (Printf.sprintf "missing \"%s\" after %s" kw (what_header opener.kind))
      )
  else begin
    soft_at_token ?help:(token_hint s i)
      (Printf.sprintf "expected \"%s\" after %s, found %s" kw
         (what_header opener.kind) (found_at s i)
      );
    let target, _ = sync_kw s i ~kw ~err:i in
    advance_to target;
    if tok_at s target = Word kw then ignore (Lexer.kw kw)
  end

let need_name ~what () =
  skip ();
  let s = !src in
  let p = position () in
  match tok_at s p with
  | Word w when is_reserved w ->
      advance_to (p + String.length w);
      error ~from:p
        ~help:
          (Printf.sprintf
             "'%s' is a reserved word in Lua and cannot be used as a name" w
          )
        (Printf.sprintf "expected %s, found reserved word '%s'" what w)
  | Word _ ->
      name_token ()
  | Eof | Sym _ | Number | String ->
      error_at_token (Printf.sprintf "expected %s, found %s" what (found_at s p))

let field_name ~what () =
  skip ();
  let s = !src in
  let p = position () in
  match tok_at s p with
  | Word w when is_reserved w ->
      report
        (make_error ~from:p
           ~upto:(p + String.length w)
           ~help:
             (Printf.sprintf
                "'%s' is a reserved word in Lua; write [\"%s\"] to use it as a \
                 key"
                w w
             )
           (Printf.sprintf "expected %s, found reserved word '%s'" what w)
        );
      advance_to (p + String.length w);
      (w, { start = p; stop = p + String.length w })
  | _ ->
      need_name ~what ()

let indent_at s i =
  let rec start k =
    if k > 0 && s.[k - 1] <> '\n' && s.[k - 1] <> '\r' then
      start (k - 1)
    else
      k
  in
  let b = start i in
  let rec count k =
    if k < String.length s && (s.[k] = ' ' || s.[k] = '\t') then
      count (k + 1)
    else
      k
  in
  count b - b

let note_close opener (r_end : range) =
  let s = !src in
  if indent_at s r_end.start < indent_at s opener.at.start then
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
  if not (at_end ()) then skip ();
  report ~word:"end" ~region:opener.at.start
    (make_error ~notes ?help
       (Printf.sprintf "expected \"end\" to close the \"%s\" at %s, found %s"
          opener.kind (loc opener.at.start)
          (found_at !src (skip_raw !src (position ())))
       )
    )

let close_end opener =
  match peek () with
  | Word "end", _ ->
      let r = kw "end" in
      note_close opener r;
      r.stop
  | _ ->
      missing_end opener;
      position ()

let one_of_text = function
  | [] ->
      ""
  | [ x ] ->
      x
  | xs ->
      let rev = List.rev xs in
      String.concat ", " (List.rev (List.tl rev)) ^ " or " ^ List.hd rev

let line_of i = (P.location_of_position !src i).line

let unclosed ~(opener : range) ~what expected =
  let s = !src in
  let i = next_token () in
  let caret =
    if i >= String.length s then
      position ()
    else
      i
  in
  let notes =
    if line_of caret > line_of opener.start then
      [
        { P.at = Some opener; text = Printf.sprintf "the %s starts here" what };
      ]
    else
      []
  in
  error_at_token ~notes
    (Printf.sprintf "expected %s in the %s that starts at %s, found %s" expected
       what (loc opener.start) (found_at s i)
    )

let close_sym ~opener ~what x =
  match peek () with
  | Sym y, _ when y = x ->
      sym x
  | _ ->
      unclosed ~opener ~what (quote x)

let sep_or_close ~opener ~what seps closer =
  match peek () with
  | Sym x, _ when List.mem x seps ->
      ignore (sym x);
      `Sep
  | Sym x, _ when x = closer ->
      `Close (sym x)
  | _ ->
      unclosed ~opener ~what (one_of_text (List.map quote (seps @ [ closer ])))

let mk_bin op a b = mk_expr (Binop (op, a, b)) (span a.erange b.erange)

let op_level ops () =
  let s = !src in
  let i = skip_raw s (position ()) in
  let t =
    match tok_at s i with
    | Sym x ->
        x
    | Word w ->
        w
    | Eof | Number | String ->
        ""
  in
  match List.assoc_opt t ops with
  | Some op ->
      advance_to (i + String.length t);
      P.commit ();
      mk_bin op
  | None ->
      P.one_of [] ()

let table =
  [
    [ P.infix_left (op_level [ ("or", Or) ]) ];
    [ P.infix_left (op_level [ ("and", And) ]) ];
    [
      P.infix_left
        (op_level
           [
             ("<", Lt); (">", Gt); ("<=", Le); (">=", Ge); ("~=", Ne); ("==", Eq);
           ]
        );
    ];
    [ P.infix_left (op_level [ ("|", Bor) ]) ];
    [ P.infix_left (op_level [ ("~", Bxor) ]) ];
    [ P.infix_left (op_level [ ("&", Band) ]) ];
    [ P.infix_left (op_level [ ("<<", Shl); (">>", Shr) ]) ];
    [ P.infix_right (op_level [ ("..", Concat) ]) ];
    [ P.infix_left (op_level [ ("+", Add); ("-", Sub) ]) ];
    [
      P.infix_left
        (op_level [ ("*", Mul); ("/", Div); ("//", Idiv); ("%", Mod) ]);
    ];
    [ P.infix_right (op_level [ ("^", Pow) ]) ];
  ]

let pow_table = [ [ P.infix_right (op_level [ ("^", Pow) ]) ] ]

let rec expr () = P.rec_ (fun () -> P.expression ~atom table ())

and atom () =
  skip ();
  let s = !src in
  let p = position () in
  match tok_at s p with
  | Number ->
      let v, text, r = number () in
      mk_expr (Number (v, text)) r
  | String ->
      let v, r = string_token () in
      mk_expr (String v) r
  | Sym "{" ->
      table_cons ()
  | Sym "..." ->
      mk_expr Vararg (sym "...")
  | Sym "(" ->
      suffixed ()
  | Sym "-" ->
      unary Neg (sym "-")
  | Sym "#" ->
      unary Len (sym "#")
  | Sym "~" ->
      unary Bnot (sym "~")
  | Word "nil" ->
      mk_expr Nil (kw "nil")
  | Word "true" ->
      mk_expr True (kw "true")
  | Word "false" ->
      mk_expr False (kw "false")
  | Word "not" ->
      unary Not (kw "not")
  | Word "function" ->
      let r = kw "function" in
      let f, stop = funcbody { kind = "function"; at = r } in
      mk_expr (Function f) { start = r.start; stop }
  | Word w when is_reserved w ->
      never "expression"
  | Word _ ->
      suffixed ()
  | Eof | Sym _ ->
      never "expression"

and unary op r =
  let e = P.rec_ (fun () -> P.expression ~atom pow_table ()) in
  mk_expr (Unop (op, e)) (span r e.erange)

and primary () =
  match peek () with
  | Sym "(", _ ->
      let r1 = sym "(" in
      let e = expr () in
      let r2 = close_sym ~opener:r1 ~what:"parenthesized expression" ")" in
      mk_expr (Paren e) (span r1 r2)
  | _ ->
      let n, r = name_token () in
      mk_expr (Name n) r

and suffixed () = suffixes (primary ())

and suffixes e =
  match peek () with
  | Sym ".", _ ->
      ignore (sym ".");
      let n, r = field_name ~what:"a field name after '.'" () in
      suffixes (mk_expr (Index (e, mk_expr (String n) r)) (span e.erange r))
  | Sym "[", _ ->
      let r1 = sym "[" in
      let k = expr () in
      let r = close_sym ~opener:r1 ~what:"index" "]" in
      suffixes (mk_expr (Index (e, k)) (span e.erange r))
  | Sym ":", _ ->
      ignore (sym ":");
      let m, _ = need_name ~what:"a method name after ':'" () in
      let args, stop = call_args ~after_method:true () in
      suffixes
        (mk_expr (Method_call (e, m, args)) { start = e.erange.start; stop })
  | (Sym "(" | String | Sym "{"), _ ->
      let args, stop = call_args () in
      suffixes (mk_expr (Call (e, args)) { start = e.erange.start; stop })
  | _ ->
      e

and call_args ?(after_method = false) () =
  match peek () with
  | String, _ ->
      let v, r = string_token () in
      ([ mk_expr (String v) r ], r.stop)
  | Sym "{", _ ->
      let t = table_cons () in
      ([ t ], t.erange.stop)
  | Sym "(", _ -> (
      let opener = sym "(" in
      match peek () with
      | Sym ")", _ ->
          ([], (sym ")").stop)
      | _ ->
          let rec loop acc =
            let e = list_elem ~stops:[ ","; ")" ] expr bad_expr in
            match sep_or_close ~opener ~what:"argument list" [ "," ] ")" with
            | `Sep ->
                loop (e :: acc)
            | `Close r ->
                (List.rev (e :: acc), r.stop)
          in
          loop []
    )
  | _ ->
      if after_method then
        error_at_token ~help:"a method call needs arguments, as in obj:name()"
          (Printf.sprintf "expected arguments after the method name, found %s"
             (found_at !src (skip_raw !src (position ())))
          )
      else
        never "arguments"

and table_cons () =
  let r1 = sym "{" in
  let rec loop acc =
    match peek () with
    | Sym "}", _ ->
        (List.rev acc, sym "}")
    | _ -> (
        let f = list_elem ~stops:[ ","; ";"; "}" ] field (fun _ -> Bad_field) in
        match sep_or_close ~opener:r1 ~what:"table" [ ","; ";" ] "}" with
        | `Sep ->
            loop (f :: acc)
        | `Close r ->
            (List.rev (f :: acc), r)
      )
  in
  let fields, r2 = loop [] in
  mk_expr (Table fields) (span r1 r2)

and field () =
  let s = !src in
  match peek () with
  | Sym "[", _ ->
      let r1 = sym "[" in
      let k = expr () in
      ignore (close_sym ~opener:r1 ~what:"key" "]");
      ignore (sym "=");
      Keyed (k, expr ())
  | Word w, i
    when (not (is_reserved w)) && sym_at s (skip_raw s (ident_end s i)) = "=" ->
      let n, _ = name_token () in
      ignore (sym "=");
      Named (n, expr ())
  | _ ->
      Positional (expr ())

and funcbody opener =
  let params, vararg = param_list () in
  let body = block End_only (Some opener) in
  let stop = close_end opener in
  ({ params; vararg; body }, stop)

and param_list () =
  P.context ~delimited:true "parameter list" (fun () ->
      let opener = sym "(" in
      match peek () with
      | Sym ")", _ ->
          ignore (sym ")");
          ([], false)
      | _ ->
          let rec loop acc =
            match peek () with
            | Sym "...", _ ->
                ignore (sym "...");
                ignore (close_sym ~opener ~what:"parameter list" ")");
                (List.rev acc, true)
            | _ -> (
                let p =
                  list_elem ~skip_word:true ~stops:[ ","; ")" ]
                    (fun () ->
                      let n, r = need_name ~what:"a parameter name" () in
                      Param (n, r)
                    )
                    (fun _ -> Bad_param)
                in
                match
                  sep_or_close ~opener ~what:"parameter list" [ "," ] ")"
                with
                | `Sep ->
                    loop (p :: acc)
                | `Close _ ->
                    (List.rev (p :: acc), false)
              )
          in
          loop []
  )

and explist () =
  let rec loop acc =
    let e = list_elem ~line_stop:true ~stops:[ "," ] expr bad_expr in
    match peek () with
    | Sym ",", _ ->
        ignore (sym ",");
        loop (e :: acc)
    | _ ->
        List.rev (e :: acc)
  in
  loop []

and block closer opener : block =
  P.rec_ (fun () ->
      let stops w =
        match (closer, w) with
        | Chunk, _ ->
            false
        | End_only, "end" ->
            true
        | If_branch, ("end" | "else" | "elseif") ->
            true
        | Until, ("until" | "end") ->
            true
        | _ ->
            false
      in
      let rec loop acc returned =
        match peek () with
        | Eof, _ ->
            List.rev acc
        | Word w, _ when stops w ->
            List.rev acc
        | Sym ";", _ ->
            ignore (sym ";");
            loop acc returned
        | t, _ ->
            ( match returned with
            | Some r when starts_statement t ->
                soft_at_token ~region:r.start
                  ~notes:
                    [
                      { P.at = Some r; text = "this \"return\" ends the block" };
                    ]
                  ~help:
                    "remove the code after \"return\", or write \"do return \
                     end\" to return early"
                  (Printf.sprintf
                     "\"return\" must be the last statement of a block, found \
                      %s after it"
                     (found_at !src (skip_raw !src (position ())))
                  )
            | _ ->
                ()
            );
            let st = statement_rec opener in
            let returned =
              match st.sdesc with
              | Return _ ->
                  Some (kw_range_of st)
              | _ ->
                  None
            in
            loop (st :: acc) returned
      in
      loop [] None
  )

and kw_range_of st = { start = st.srange.start; stop = st.srange.start + 6 }

and statement_rec opener =
  skip ();
  let start = position () in
  match P.capture (fun () -> statement opener) with
  | Ok st ->
      st
  | Error e ->
      report ~region:start e;
      let target, last = sync_statement !src start in
      advance_to target;
      { sdesc = Bad_stat; srange = { start; stop = last } }

and statement opener =
  let s = !src in
  let start = position () in
  let mk sdesc stop = { sdesc; srange = { start; stop } } in
  match peek () with
  | Word "if", _ ->
      if_stat ()
  | Word "while", _ ->
      let r = kw "while" in
      let opener = { kind = "while"; at = r } in
      let c = condition ~kw:"do" opener in
      let body = block End_only (Some opener) in
      mk (While (c, body)) (close_end opener)
  | Word "do", _ ->
      let r = kw "do" in
      let opener = { kind = "do"; at = r } in
      let body = block End_only (Some opener) in
      mk (Do body) (close_end opener)
  | Word "for", _ ->
      for_stat ()
  | Word "repeat", _ ->
      repeat_stat ()
  | Word "function", _ ->
      function_stat ()
  | Word "local", _ ->
      local_stat ()
  | Word "return", _ ->
      return_stat ()
  | Word "break", _ ->
      mk Break (kw "break").stop
  | Word "goto", _ ->
      P.context "goto statement" (fun () ->
          ignore (kw "goto");
          let n, r = need_name ~what:"a label name after \"goto\"" () in
          mk (Goto n) r.stop
      )
  | Sym "::", _ ->
      P.context "label" (fun () ->
          ignore (sym "::");
          let n, _ = need_name ~what:"a label name after '::'" () in
          mk (Label n) (sym "::").stop
      )
  | Word (("end" | "else" | "elseif" | "until") as w), _ ->
      stray w opener
  | Word w, _ when not (is_reserved w) ->
      expr_stat ()
  | Sym "(", _ ->
      expr_stat ()
  | t, i ->
      let help =
        match t with
        | Sym ("}" | ")" | "]") ->
            Some "this bracket has no matching opening bracket"
        | Number | String | Sym "{" | Word ("nil" | "true" | "false" | "not") ->
            Some
              "an expression alone is not a statement; only a call or an \
               assignment is"
        | Word "then" ->
            Some "\"then\" must follow the condition of an \"if\" or \"elseif\""
        | _ ->
            token_hint s i
      in
      error_at_token ?help
        (Printf.sprintf "expected a statement, found %s" (found_at s i))

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
  error_at_token ?help (Printf.sprintf "unexpected \"%s\"%s" w inside)

and condition ~kw opener =
  match header ~kw expr with
  | Some c ->
      expect_kw ~kw opener;
      c
  | None ->
      bad_expr { start = position (); stop = position () }

and if_stat () =
  let start = position () in
  let r_if = kw "if" in
  let opener = { kind = "if"; at = r_if } in
  let c = condition ~kw:"then" opener in
  let b = block If_branch (Some opener) in
  let rec branches acc =
    match peek () with
    | Word "elseif", _ ->
        let r = kw "elseif" in
        let c = condition ~kw:"then" { kind = "elseif"; at = r } in
        let b = block If_branch (Some opener) in
        branches ((c, b) :: acc)
    | Word "else", _ ->
        let r_else = kw "else" in
        ( match peek () with
        | Word "if", i when not (newline_between !src r_else.stop i) ->
            P.Ref.set else_ifs
              ((r_else, { start = i; stop = i + 2 }) :: P.Ref.get else_ifs)
        | _ ->
            ()
        );
        let b = block End_only (Some opener) in
        (List.rev acc, Some b)
    | _ ->
        (List.rev acc, None)
  in
  let brs, els = branches [ (c, b) ] in
  let stop = close_end opener in
  { sdesc = If (brs, els); srange = { start; stop } }

and for_stat () =
  let start = position () in
  let r = kw "for" in
  let opener = { kind = "for"; at = r } in
  let h = P.context "for header" (fun () -> header ~kw:"do" for_header) in
  (match h with Some _ -> expect_kw ~kw:"do" opener | None -> ());
  let body = block End_only (Some opener) in
  let stop = close_end opener in
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
  match peek () with
  | Sym "=", _ ->
      ignore (sym "=");
      let a = expr () in
      ignore (sym ",");
      let b = expr () in
      let c =
        match peek () with
        | Sym ",", _ ->
            ignore (sym ",");
            Some (expr ())
        | _ ->
            None
      in
      `Num (v, a, b, c)
  | (Sym "," | Word "in"), _ ->
      let rec names acc =
        match peek () with
        | Sym ",", _ ->
            ignore (sym ",");
            names (fst (need_name ~what:"a loop variable after ','" ()) :: acc)
        | _ ->
            List.rev acc
      in
      let vs = names [ v ] in
      ignore (kw "in");
      let rec exprs acc =
        let e = expr () in
        match peek () with
        | Sym ",", _ ->
            ignore (sym ",");
            exprs (e :: acc)
        | _ ->
            List.rev (e :: acc)
      in
      `Gen (vs, exprs [])
  | _, i ->
      error_at_token
        (Printf.sprintf
           "expected '=' or \"in\" after the loop variable, found %s"
           (found_at !src i)
        )

and repeat_stat () =
  let start = position () in
  let r = kw "repeat" in
  let opener = { kind = "repeat"; at = r } in
  let body = block Until (Some opener) in
  let note = { P.at = Some r; text = "this \"repeat\" is not closed" } in
  match peek () with
  | Word "until", _ ->
      ignore (kw "until");
      let c = list_elem ~line_stop:true ~stops:[] expr bad_expr in
      { sdesc = Repeat (body, c); srange = { start; stop = c.erange.stop } }
  | Word "end", _ ->
      soft_at_token ~notes:[ note ]
        ~help:"a \"repeat\" loop ends with \"until\" and a condition"
        (Printf.sprintf
           "expected \"until\" to close the \"repeat\" at %s, found 'end'"
           (loc r.start)
        );
      let e = kw "end" in
      { sdesc = Repeat (body, bad_expr e); srange = { start; stop = e.stop } }
  | _ ->
      skip ();
      report ~word:"until" ~region:r.start
        (make_error ~notes:[ note ]
           (Printf.sprintf
              "expected \"until\" to close the \"repeat\" at %s, found %s"
              (loc r.start)
              (found_at !src (position ()))
           )
        );
      {
        sdesc =
          Repeat (body, bad_expr { start = position (); stop = position () });
        srange = { start; stop = position () };
      }

and function_stat () =
  let start = position () in
  let r = kw "function" in
  ( match peek () with
  | Sym "(", _ ->
      error_at_token
        ~help:
          "an anonymous function is an expression: assign it, as in local f = \
           function(...) ... end"
        "expected a function name after \"function\", found '('"
  | _ ->
      ()
  );
  let n, _ = need_name ~what:"a function name after \"function\"" () in
  let rec path acc =
    match peek () with
    | Sym ".", _ ->
        ignore (sym ".");
        path (fst (need_name ~what:"a name after '.'" ()) :: acc)
    | _ ->
        List.rev acc
  in
  let names = path [ n ] in
  let meth =
    match peek () with
    | Sym ":", _ ->
        ignore (sym ":");
        Some (fst (need_name ~what:"a method name after ':'" ()))
    | _ ->
        None
  in
  let f, stop = funcbody { kind = "function"; at = r } in
  { sdesc = Function_stat (names, meth, f); srange = { start; stop } }

and local_stat () =
  let s = !src in
  let start = position () in
  ignore (kw "local");
  match peek () with
  | Word "function", _ ->
      let rf = kw "function" in
      let n, r_name =
        need_name ~what:"a function name after \"local function\"" ()
      in
      let qualified =
        match peek () with
        | Sym ("." | ":"), _ ->
            let rec rest last =
              match peek () with
              | Sym (("." | ":") as x), _ ->
                  ignore (sym x);
                  rest (snd (need_name ~what:"a name" ()))
              | _ ->
                  last
            in
            let last = rest r_name in
            report
              (make_error ~from:r_name.start
                 ~help:
                   "write \"function a.b()\" without \"local\" to set a field, \
                    or \"local function b()\" for a local"
                 (Printf.sprintf
                    "a local function cannot have a qualified name '%s'"
                    (String.sub s r_name.start (last.stop - r_name.start))
                 )
              );
            true
        | _ ->
            false
      in
      let f, stop = funcbody { kind = "function"; at = rf } in
      {
        sdesc =
          ( if qualified then
              Bad_stat
            else
              Local_function (n, f)
          );
        srange = { start; stop };
      }
  | _ -> (
      let rec names acc =
        let v =
          list_elem ~line_stop:true ~skip_word:true ~stops:[ ","; "=" ]
            local_name (fun _ -> Bad_var
          )
        in
        match peek () with
        | Sym ",", _ ->
            ignore (sym ",");
            names (v :: acc)
        | _ ->
            List.rev (v :: acc)
      in
      let vars = names [] in
      let closes =
        List.filter
          (function Var (_, Some Close, _) -> true | _ -> false)
          vars
      in
      if List.length closes > 1 then
        report
          (make_error
             ~help:
               "declare each <close> variable in its own \"local\" statement"
             "multiple to-be-closed variables in one local declaration"
          );
      let stop = position () in
      match (peek (), vars) with
      | (Sym "(", _), [ Var (n, None, _) ] ->
          soft_at_token
            ~help:
              (Printf.sprintf
                 "to declare a local function, write \"local function %s(...)\""
                 n
              )
            (Printf.sprintf "unexpected '(' after \"local %s\"" n);
          let _, stop = funcbody { kind = "function"; at = { start; stop } } in
          { sdesc = Bad_stat; srange = { start; stop } }
      | (Sym "=", _), _ ->
          ignore (sym "=");
          let es = explist () in
          let stop =
            match List.rev es with e :: _ -> e.erange.stop | [] -> stop
          in
          { sdesc = Local (vars, es); srange = { start; stop } }
      | _ ->
          { sdesc = Local (vars, []); srange = { start; stop } }
    )

and local_name () =
  let n, r = need_name ~what:"a variable name after \"local\"" () in
  match peek () with
  | Sym "<", _ ->
      ignore (sym "<");
      let a, ra = need_name ~what:"an attribute name" () in
      let attrib =
        match a with
        | "const" ->
            Const
        | "close" ->
            Close
        | _ ->
            error ~from:ra.start ~help:"the attributes are <const> and <close>"
              (Printf.sprintf "unknown attribute '%s'" a)
      in
      ignore (sym ">");
      Var (n, Some attrib, r)
  | _ ->
      Var (n, None, r)

and return_stat () =
  let start = position () in
  let r = kw "return" in
  let es =
    match peek () with
    | (Eof | Sym ";"), _ ->
        []
    | Word w, _ when is_terminator w ->
        []
    | _ ->
        explist ()
  in
  let stop = match List.rev es with e :: _ -> e.erange.stop | [] -> r.stop in
  let stop = match peek () with Sym ";", _ -> (sym ";").stop | _ -> stop in
  ( match peek () with
  | Eof, _ ->
      ()
  | Word w, _ when is_terminator w ->
      ()
  | t, _ when starts_statement t ->
      ()
  | _, i ->
      error_at_token ~help:"the values of a \"return\" are separated by ','"
        (Printf.sprintf "unexpected %s after the return statement"
           (found_at !src i)
        )
  );
  { sdesc = Return es; srange = { start; stop } }

and expr_stat () =
  let s = !src in
  let start = position () in
  let e = suffixed () in
  match peek () with
  | (Sym "=" | Sym ","), _ -> begin
      check_target e;
      let rec targets acc =
        match peek () with
        | Sym ",", _ ->
            ignore (sym ",");
            let t = suffixed () in
            check_target t;
            targets (t :: acc)
        | _ ->
            List.rev acc
      in
      let ts = targets [ e ] in
      ignore (sym "=");
      let es = explist () in
      let stop =
        match List.rev es with x :: _ -> x.erange.stop | [] -> e.erange.stop
      in
      { sdesc = Assign (ts, es); srange = { start; stop } }
    end
  | t, i -> (
      match e.edesc with
      | Call _ | Method_call _ ->
          { sdesc = Call_stat e; srange = e.erange }
      | _ ->
          not_a_statement s e t i
    )

and check_target t =
  match t.edesc with
  | Name _ | Index _ ->
      ()
  | Call _ | Method_call _ ->
      error ~from:t.erange.start "cannot assign to a function call"
  | Paren _ ->
      error ~from:t.erange.start
        ~help:"remove the parentheses around the target"
        "cannot assign to a parenthesized expression"
  | _ ->
      error ~from:t.erange.start "cannot assign to this expression"

and not_a_statement s e t i =
  let n = String.length s in
  let at k =
    if k < n then
      s.[k]
    else
      '\000'
  in
  let target = String.sub s e.erange.start (e.erange.stop - e.erange.start) in
  let target =
    if String.length target > 20 || String.contains target '\n' then
      "x"
    else
      target
  in
  let after =
    e.erange.stop
    +
    let k = ref e.erange.stop in
    while !k < n && (s.[!k] = ' ' || s.[!k] = '\t') do
      incr k
    done;
    !k - e.erange.stop
  in
  match t with
  | Sym
      ( ( "+"
        | "-"
        | "*"
        | "/"
        | "//"
        | "%"
        | "^"
        | ".."
        | "|"
        | "&"
        | "<<"
        | ">>" ) as op
      )
    when at (i + String.length op) = '=' ->
      advance_to (i + String.length op + 1);
      error ~from:i
        ~help:(Printf.sprintf "write %s = %s %s ..." target target op)
        (Printf.sprintf "Lua has no '%s=' operator" op)
  | Sym "+" when at (i + 1) = '+' ->
      advance_to (i + 2);
      error ~from:i
        ~help:(Printf.sprintf "write %s = %s + 1" target target)
        "Lua has no '++' operator"
  | _
    when at after = '-'
         && at (after + 1) = '-'
         && not (newline_between s e.erange.stop after) ->
      error ~from:e.erange.start
        ~help:
          (Printf.sprintf
             "'--' starts a comment in Lua; to decrement, write %s = %s - 1"
             target target
          )
        "this expression is not a statement"
  | _ when e.edesc = Name "continue" ->
      error ~from:e.erange.start
        ~help:
          "Lua has no \"continue\"; write \"goto continue\" and put the label \
           \"::continue::\" at the end of the loop body"
        "\"continue\" is not a Lua statement"
  | _ ->
      error ~from:e.erange.start
        ~help:
          "only a function call or an assignment can stand alone as a statement"
        "this expression is not a statement"

let chunk () =
  let b = block Chunk None in
  skip ();
  b

let parse ?trace source =
  src := source;
  Hashtbl.reset lexical_errors;
  P.Ref.set unclosed_hints [];
  P.Ref.set else_ifs [];
  let result =
    P.parse ?trace ~max_depth:100_000 ~diagnostics:errors source chunk
  in
  let diags =
    List.map
      (fun (d : _ P.Diagnostics.entry) -> d.value)
      (P.Diagnostics.to_list errors)
    @ Hashtbl.fold (fun _ (e, _) acc -> e :: acc) lexical_errors []
  in
  match result with Ok b -> (Some b, diags) | Error e -> (None, diags @ [ e ])

let error_at source (r : range) ?notes ?help msg =
  match
    P.parse ~consume:`Partial source (fun () ->
        if r.start > 0 then ignore (P.take_span r.start);
        if r.stop > r.start then ignore (P.take_span (r.stop - r.start));
        (error ~from:r.start ?notes ?help msg : unit)
    )
  with
  | Error e ->
      e
  | Ok () ->
      assert false
