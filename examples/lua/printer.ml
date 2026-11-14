open Ast

exception Error_node

let prio_left = function
  | Or ->
      1
  | And ->
      2
  | Lt | Gt | Le | Ge | Ne | Eq ->
      3
  | Bor ->
      4
  | Bxor ->
      5
  | Band ->
      6
  | Shl | Shr ->
      7
  | Concat ->
      9
  | Add | Sub ->
      10
  | Mul | Div | Idiv | Mod ->
      11
  | Pow ->
      14

let prio_right = function Concat -> 8 | Pow -> 13 | op -> prio_left op
let unary_prio = 12

let left_parens op child =
  match child.edesc with
  | Binop (c, _, _) ->
      prio_left op > prio_right c
  | Unop _ ->
      prio_left op > unary_prio
  | _ ->
      false

let right_parens op child =
  match child.edesc with
  | Binop (c, _, _) ->
      prio_left c <= prio_right op
  | _ ->
      false

let operand_parens child =
  match child.edesc with
  | Binop (c, _, _) ->
      prio_left c <= unary_prio
  | _ ->
      false

let is_name s =
  s <> ""
  && Lexer.is_alpha s.[0]
  && String.for_all Lexer.is_alnum s
  && not (Lexer.is_reserved s)

let quote_string s =
  let b = Buffer.create (String.length s + 2) in
  Buffer.add_char b '"';
  String.iter
    (function
      | '"' ->
          Buffer.add_string b "\\\""
      | '\\' ->
          Buffer.add_string b "\\\\"
      | '\n' ->
          Buffer.add_string b "\\n"
      | c when Char.code c < 32 || Char.code c > 126 ->
          Buffer.add_string b (Printf.sprintf "\\%03d" (Char.code c))
      | c ->
          Buffer.add_char b c
      )
    s;
  Buffer.add_char b '"';
  Buffer.contents b

let explicit = ref false

let rec expr b ind e =
  match e.edesc with
  | (Binop _ | Unop _) when !explicit ->
      Buffer.add_char b '(';
      operator b ind e;
      Buffer.add_char b ')'
  | _ ->
      operator b ind e

and operator b ind e =
  let add = Buffer.add_string b in
  match e.edesc with
  | Nil ->
      add "nil"
  | True ->
      add "true"
  | False ->
      add "false"
  | Vararg ->
      add "..."
  | Number (_, text) ->
      add text
  | String s ->
      add (quote_string s)
  | Function f ->
      add "function";
      funcbody b ind f
  | Table [] ->
      add "{}"
  | Table fields ->
      add "{";
      List.iteri
        (fun i f ->
          if i > 0 then add ", ";
          field b ind f
        )
        fields;
      add "}"
  | Binop (op, l, r) ->
      wrap b ind (left_parens op l) l;
      add (" " ^ binop_text op ^ " ");
      wrap b ind (right_parens op r) r
  | Unop (op, x) ->
      let inner = Buffer.create 16 in
      wrap inner ind (operand_parens x) x;
      let text = Buffer.contents inner in
      add
        ( match op with
        | Not ->
            "not "
        | Neg when text <> "" && text.[0] = '-' ->
            "- "
        | Neg | Len | Bnot ->
            unop_text op
        );
      add text
  | Name n ->
      add n
  | Index (o, { edesc = String k; _ }) when is_name k ->
      prefix b ind o;
      add ("." ^ k)
  | Index (o, k) ->
      prefix b ind o;
      add "[";
      expr b ind k;
      add "]"
  | Call (f, args) ->
      prefix b ind f;
      exprs_paren b ind args
  | Method_call (o, m, args) ->
      prefix b ind o;
      add (":" ^ m);
      exprs_paren b ind args
  | Paren x ->
      add "(";
      expr b ind x;
      add ")"
  | Bad_expr ->
      raise Error_node

and wrap b ind parens e =
  if parens then begin
    Buffer.add_char b '(';
    expr b ind e;
    Buffer.add_char b ')'
  end else
    expr b ind e

and prefix b ind e =
  match e.edesc with
  | Name _ | Index _ | Call _ | Method_call _ | Paren _ ->
      expr b ind e
  | _ ->
      wrap b ind true e

and exprs b ind es =
  List.iteri
    (fun i e ->
      if i > 0 then Buffer.add_string b ", ";
      expr b ind e
    )
    es

and exprs_paren b ind es =
  Buffer.add_char b '(';
  exprs b ind es;
  Buffer.add_char b ')'

and field b ind = function
  | Positional e ->
      expr b ind e
  | Named (n, e) ->
      Buffer.add_string b (n ^ " = ");
      expr b ind e
  | Keyed (k, v) ->
      Buffer.add_char b '[';
      expr b ind k;
      Buffer.add_string b "] = ";
      expr b ind v
  | Bad_field ->
      raise Error_node

and funcbody b ind f =
  let params =
    List.map (function Param p -> p | Bad_param -> raise Error_node) f.params
    @
    if f.vararg then
      [ "..." ]
    else
      []
  in
  Buffer.add_string b ("(" ^ String.concat ", " params ^ ")");
  block b (ind + 1) f.body;
  Buffer.add_string b ("\n" ^ String.make (2 * ind) ' ' ^ "end")

and block b ind stats =
  List.iter
    (fun s ->
      let line = Buffer.create 64 in
      stat line ind s;
      let text = Buffer.contents line in
      Buffer.add_string b ("\n" ^ String.make (2 * ind) ' ');
      if text <> "" && text.[0] = '(' then Buffer.add_char b ';';
      Buffer.add_string b text
    )
    stats

and stat b ind s =
  let add = Buffer.add_string b in
  let pad = String.make (2 * ind) ' ' in
  let body stats =
    block b (ind + 1) stats;
    add ("\n" ^ pad ^ "end")
  in
  match s.sdesc with
  | Local (vars, es) ->
      let var = function
        | Var (n, None) ->
            n
        | Var (n, Some a) ->
            n ^ " <" ^ attrib_text a ^ ">"
        | Bad_var ->
            raise Error_node
      in
      add ("local " ^ String.concat ", " (List.map var vars));
      if es <> [] then begin
        add " = ";
        exprs b ind es
      end
  | Assign (targets, es) ->
      exprs b ind targets;
      add " = ";
      exprs b ind es
  | Call_stat e ->
      expr b ind e
  | Do stats ->
      add "do";
      body stats
  | While (c, stats) ->
      add "while ";
      expr b ind c;
      add " do";
      body stats
  | Repeat (stats, c) ->
      add "repeat";
      block b (ind + 1) stats;
      add ("\n" ^ pad ^ "until ");
      expr b ind c
  | If (branches, else_) ->
      List.iteri
        (fun i (c, stats) ->
          add
            ( if i = 0 then
                "if "
              else
                "\n" ^ pad ^ "elseif "
            );
          expr b ind c;
          add " then";
          block b (ind + 1) stats
        )
        branches;
      Option.iter
        (fun stats ->
          add ("\n" ^ pad ^ "else");
          block b (ind + 1) stats
        )
        else_;
      add ("\n" ^ pad ^ "end")
  | Numeric_for (v, start, stop, step, stats) ->
      add ("for " ^ v ^ " = ");
      exprs b ind (start :: stop :: Option.to_list step);
      add " do";
      body stats
  | Generic_for (vars, es, stats) ->
      add ("for " ^ String.concat ", " vars ^ " in ");
      exprs b ind es;
      add " do";
      body stats
  | Function_stat (path, meth, f) ->
      add ("function " ^ String.concat "." path);
      Option.iter (fun m -> add (":" ^ m)) meth;
      funcbody b ind f
  | Local_function (n, f) ->
      add ("local function " ^ n);
      funcbody b ind f
  | Return [] ->
      add "return"
  | Return es ->
      add "return ";
      exprs b ind es
  | Break ->
      add "break"
  | Goto l ->
      add ("goto " ^ l)
  | Label l ->
      add ("::" ^ l ^ "::")
  | Bad_stat ->
      raise Error_node

let to_string ?(parens = false) chunk =
  explicit := parens;
  let b = Buffer.create 4096 in
  block b 0 chunk;
  let text = Buffer.contents b in
  let text =
    if String.length text > 0 && text.[0] = '\n' then
      String.sub text 1 (String.length text - 1)
    else
      text
  in
  text ^ "\n"
