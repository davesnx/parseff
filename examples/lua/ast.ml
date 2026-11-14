type range = Parseff.range = { start : int; stop : int }
type unop = Neg | Not | Len | Bnot

type binop =
  | Or
  | And
  | Lt
  | Gt
  | Le
  | Ge
  | Ne
  | Eq
  | Bor
  | Bxor
  | Band
  | Shl
  | Shr
  | Concat
  | Add
  | Sub
  | Mul
  | Div
  | Idiv
  | Mod
  | Pow

type number = Int of int64 | Float of float
type attrib = Const | Close

type expr = { edesc : edesc; erange : range }

and edesc =
  | Nil
  | True
  | False
  | Vararg
  | Number of number * string
  | String of string
  | Function of funcbody
  | Table of field list
  | Binop of binop * expr * expr
  | Unop of unop * expr
  | Name of string
  | Index of expr * expr
  | Call of expr * expr list
  | Method_call of expr * string * expr list
  | Paren of expr
  | Bad_expr

and field =
  | Positional of expr
  | Named of string * expr
  | Keyed of expr * expr
  | Bad_field

and param = Param of string | Bad_param
and funcbody = { params : param list; vararg : bool; body : block }
and block = stat list
and stat = { sdesc : sdesc; srange : range }
and local = Var of string * attrib option | Bad_var

and sdesc =
  | Local of local list * expr list
  | Assign of expr list * expr list
  | Call_stat of expr
  | Do of block
  | While of expr * block
  | Repeat of block * expr
  | If of (expr * block) list * block option
  | Numeric_for of string * expr * expr * expr option * block
  | Generic_for of string list * expr list * block
  | Function_stat of string list * string option * funcbody
  | Local_function of string * funcbody
  | Return of expr list
  | Break
  | Goto of string
  | Label of string
  | Bad_stat

let binop_text = function
  | Or ->
      "or"
  | And ->
      "and"
  | Lt ->
      "<"
  | Gt ->
      ">"
  | Le ->
      "<="
  | Ge ->
      ">="
  | Ne ->
      "~="
  | Eq ->
      "=="
  | Bor ->
      "|"
  | Bxor ->
      "~"
  | Band ->
      "&"
  | Shl ->
      "<<"
  | Shr ->
      ">>"
  | Concat ->
      ".."
  | Add ->
      "+"
  | Sub ->
      "-"
  | Mul ->
      "*"
  | Div ->
      "/"
  | Idiv ->
      "//"
  | Mod ->
      "%"
  | Pow ->
      "^"

let unop_text = function Neg -> "-" | Not -> "not" | Len -> "#" | Bnot -> "~"
let attrib_text = function Const -> "const" | Close -> "close"

let rec dump_expr b e =
  let add = Buffer.add_string b in
  let list f xs =
    add "(";
    List.iteri
      (fun i x ->
        if i > 0 then add " ";
        f b x
      )
      xs;
    add ")"
  in
  match e.edesc with
  | Nil ->
      add "nil"
  | True ->
      add "true"
  | False ->
      add "false"
  | Vararg ->
      add "..."
  | Number (Int i, _) ->
      add (Printf.sprintf "(int %Ld)" i)
  | Number (Float f, _) ->
      add (Printf.sprintf "(float %h)" f)
  | String s ->
      add (Printf.sprintf "(string %S)" s)
  | Function f ->
      add "(function ";
      dump_funcbody b f;
      add ")"
  | Table fields ->
      add "(table ";
      list dump_field fields;
      add ")"
  | Binop (op, l, r) ->
      add (Printf.sprintf "(%s " (binop_text op));
      dump_expr b l;
      add " ";
      dump_expr b r;
      add ")"
  | Unop (op, x) ->
      add (Printf.sprintf "(%s " (unop_text op));
      dump_expr b x;
      add ")"
  | Name n ->
      add (Printf.sprintf "(name %s)" n)
  | Index (o, k) ->
      add "(index ";
      dump_expr b o;
      add " ";
      dump_expr b k;
      add ")"
  | Call (f, args) ->
      add "(call ";
      dump_expr b f;
      add " ";
      list dump_expr args;
      add ")"
  | Method_call (o, m, args) ->
      add "(method ";
      dump_expr b o;
      add (" " ^ m ^ " ");
      list dump_expr args;
      add ")"
  | Paren x ->
      add "(paren ";
      dump_expr b x;
      add ")"
  | Bad_expr ->
      add "<error>"

and dump_field b = function
  | Positional e ->
      dump_expr b e
  | Named (n, e) ->
      Buffer.add_string b ("(" ^ n ^ " = ");
      dump_expr b e;
      Buffer.add_string b ")"
  | Keyed (k, v) ->
      Buffer.add_string b "([";
      dump_expr b k;
      Buffer.add_string b "] = ";
      dump_expr b v;
      Buffer.add_string b ")"
  | Bad_field ->
      Buffer.add_string b "<error>"

and dump_funcbody ?(indent = 0) b f =
  let params =
    List.map (function Param p -> p | Bad_param -> "<error>") f.params
    @
    if f.vararg then
      [ "..." ]
    else
      []
  in
  Buffer.add_string b ("(" ^ String.concat " " params ^ ")");
  dump_block b (indent + 1) f.body

and dump_block b indent stats =
  List.iter
    (fun s ->
      Buffer.add_char b '\n';
      dump_stat b indent s
    )
    stats

and dump_stat b indent s =
  let add = Buffer.add_string b in
  let pad = String.make (2 * indent) ' ' in
  let exprs es =
    add "(";
    List.iteri
      (fun i e ->
        if i > 0 then add " ";
        dump_expr b e
      )
      es;
    add ")"
  in
  let nested name body =
    add (pad ^ "(" ^ name);
    dump_block b (indent + 1) body;
    add ")"
  in
  match s.sdesc with
  | Local (vars, es) ->
      let var = function
        | Var (n, None) ->
            n
        | Var (n, Some a) ->
            n ^ " <" ^ attrib_text a ^ ">"
        | Bad_var ->
            "<error>"
      in
      add (pad ^ "(local (" ^ String.concat ", " (List.map var vars) ^ ") ");
      exprs es;
      add ")"
  | Assign (targets, es) ->
      add (pad ^ "(assign ");
      exprs targets;
      add " ";
      exprs es;
      add ")"
  | Call_stat e ->
      add pad;
      dump_expr b e
  | Do body ->
      nested "do" body
  | While (c, body) ->
      add (pad ^ "(while ");
      dump_expr b c;
      dump_block b (indent + 1) body;
      add ")"
  | Repeat (body, c) ->
      add (pad ^ "(repeat");
      dump_block b (indent + 1) body;
      add ("\n" ^ pad ^ "  until ");
      dump_expr b c;
      add ")"
  | If (branches, else_) ->
      add (pad ^ "(if");
      List.iter
        (fun (c, body) ->
          add ("\n" ^ pad ^ "  (cond ");
          dump_expr b c;
          dump_block b (indent + 2) body;
          add ")"
        )
        branches;
      Option.iter
        (fun body ->
          add ("\n" ^ pad ^ "  (else");
          dump_block b (indent + 2) body;
          add ")"
        )
        else_;
      add ")"
  | Numeric_for (v, start, stop, step, body) ->
      add (pad ^ "(for " ^ v ^ " ");
      exprs (start :: stop :: Option.to_list step);
      dump_block b (indent + 1) body;
      add ")"
  | Generic_for (vars, es, body) ->
      add (pad ^ "(for-in (" ^ String.concat " " vars ^ ") ");
      exprs es;
      dump_block b (indent + 1) body;
      add ")"
  | Function_stat (path, meth, f) ->
      add (pad ^ "(function " ^ String.concat "." path);
      Option.iter (fun m -> add (":" ^ m)) meth;
      add " ";
      dump_funcbody ~indent b f;
      add ")"
  | Local_function (n, f) ->
      add (pad ^ "(local-function " ^ n ^ " ");
      dump_funcbody ~indent b f;
      add ")"
  | Return es ->
      add (pad ^ "(return ");
      exprs es;
      add ")"
  | Break ->
      add (pad ^ "(break)")
  | Goto l ->
      add (pad ^ "(goto " ^ l ^ ")")
  | Label l ->
      add (pad ^ "(label " ^ l ^ ")")
  | Bad_stat ->
      add (pad ^ "<error>")

let dump_statement s =
  let b = Buffer.create 256 in
  dump_block b 1 [ s ];
  Buffer.contents b
