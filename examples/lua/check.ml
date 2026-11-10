open Ast

type issue = {
  at : range;
  msg : string;
  notes : (range * string) list;
  help : string option;
}

type label = { lname : string; lat : range; lnactvar : int }
type pending = { gname : string; gat : range; mutable gnactvar : int }
type blk = { firstlabel : int; firstgoto : int; bnactvar : int; isloop : bool }
type var = { vname : string; vattrib : attrib option; vat : range; vctc : bool }

type fs = {
  labels : label Dynarray.t;
  gotos : pending Dynarray.t;
  vars : var Dynarray.t;
  upvals : string Dynarray.t;
  mutable blocks : blk list;
  vararg : bool;
  parent : fs option;
  opener : range option;
  mutable broken : bool;
  mutable over_locals : bool;
  mutable over_upvals : bool;
}

let max_vars = 200
let max_upvals = 255
let max_levels = 200
let base_levels = 1

exception Too_deep of range

let nesting source chunk =
  let n = String.length source in
  let enter c at =
    let c = c + 1 in
    if c >= max_levels then raise (Too_deep at);
    c
  in
  let char_after i =
    let k = Lexer.skip_raw source i in
    if k < n then
      source.[k]
    else
      '\000'
  in
  let method_args_char (o : expr) =
    let colon = Lexer.skip_raw source o.erange.stop in
    let name = Lexer.skip_raw source (colon + 1) in
    char_after (Lexer.ident_end source name)
  in
  let rec expr c e = node (enter c e.erange) e
  and node f e =
    match e.edesc with
    | Nil | True | False | Vararg | Number _ | String _ | Name _ | Bad_expr ->
        ()
    | Binop (_, l, r) ->
        node f l;
        node (enter f r.erange) r
    | Unop (_, x) ->
        node (enter f x.erange) x
    | Paren x ->
        expr f x
    | Call (fn, args) ->
        node f fn;
        arguments f (char_after fn.erange.stop) args
    | Method_call (o, _, args) ->
        node f o;
        arguments f (method_args_char o) args
    | Index (o, k) -> (
        node f o;
        match k.edesc with
        | String _ when Lexer.is_alpha source.[k.erange.start] ->
            ()
        | _ ->
            expr f k
      )
    | Table fields ->
        table f fields
    | Function fb ->
        block f fb.body
  and arguments f opening args =
    match (opening, args) with
    | '(', _ ->
        List.iter (expr f) args
    | '{', [ { edesc = Table fields; _ } ] ->
        table f fields
    | _ ->
        ()
  and table f fields =
    List.iter
      (function
        | Positional v | Named (_, v) ->
            expr f v
        | Keyed (k, v) ->
            expr f k;
            expr f v
        | Bad_field ->
            ()
        )
      fields
  and block c stats =
    ignore
      (List.fold_left
         (fun run s ->
           match s.sdesc with
           | Label _ ->
               stat (c + run) s;
               run + 1
           | _ ->
               stat c s;
               0
         )
         0 stats
      )
  and stat c s =
    let c = enter c s.srange in
    match s.sdesc with
    | Local (_, es) | Return es ->
        List.iter (expr c) es
    | Local_function (_, fb) | Function_stat (_, _, fb) ->
        block c fb.body
    | Assign (targets, es) ->
        let last =
          List.fold_left
            (fun (level, i) t ->
              node level t;
              if i > 0 then
                (enter level t.erange, i + 1)
              else
                (level, i + 1)
            )
            (c, 0) targets
          |> fst
        in
        List.iter (expr last) es
    | Call_stat e ->
        node c e
    | Do b ->
        block c b
    | While (cond, b) ->
        expr c cond;
        block c b
    | Repeat (b, cond) ->
        block c b;
        expr c cond
    | If (branches, else_) ->
        List.iter
          (fun (cond, b) ->
            expr c cond;
            block c b
          )
          branches;
        Option.iter (block c) else_
    | Numeric_for (_, a, b, step, body) ->
        List.iter (expr c) (a :: b :: Option.to_list step);
        block c body
    | Generic_for (_, es, body) ->
        List.iter (expr c) es;
        block c body
    | Break | Goto _ | Label _ | Bad_stat ->
        ()
  in
  match block base_levels chunk with
  | () ->
      None
  | exception Too_deep at ->
      Some at

let find_last p a =
  let rec go i =
    if i < 0 then
      None
    else
      let x = Dynarray.get a i in
      if p x then
        Some x
      else
        go (i - 1)
  in
  go (Dynarray.length a - 1)

let run source chunk =
  let issues = ref [] in
  let all_labels = ref [] in
  let loc i =
    let l = Parseff.location_of_position source i in
    Printf.sprintf "%d:%d" l.line l.col
  in
  let add ?(notes = []) ?help at msg =
    issues := { at; msg; notes; help } :: !issues
  in
  let new_fs ?opener parent vararg =
    {
      labels = Dynarray.create ();
      gotos = Dynarray.create ();
      vars = Dynarray.create ();
      upvals = Dynarray.create ();
      blocks = [];
      vararg;
      parent;
      opener;
      broken = false;
      over_locals = false;
      over_upvals = false;
    }
  in
  let in_function fs =
    match fs.opener with Some r -> [ (r, "in this function") ] | None -> []
  in
  let enter fs isloop =
    fs.blocks <-
      {
        firstlabel = Dynarray.length fs.labels;
        firstgoto = Dynarray.length fs.gotos;
        bnactvar = Dynarray.length fs.vars;
        isloop;
      }
      :: fs.blocks
  in
  let declare ?(ctc = false) fs vname vattrib vat =
    if Dynarray.length fs.vars + 1 > max_vars && not fs.over_locals then begin
      fs.over_locals <- true;
      add vat ~notes:(in_function fs)
        ~help:
          "every active local of the function counts, with its parameters, its \
           <const> locals, and the hidden control variables of each enclosing \
           for loop (3 for a numeric for, 4 for a generic for)"
        (Printf.sprintf
           "too many local variables in one function (the limit is %d)" max_vars
        )
    end;
    Dynarray.add_last fs.vars { vname; vattrib; vat; vctc = ctc }
  in
  let rec lookup fs name =
    match find_last (fun v -> v.vname = name) fs.vars with
    | Some v ->
        if v.vctc then
          `Const
        else
          `Local
    | None ->
        if Dynarray.exists (String.equal name) fs.upvals then
          `Upvalue
        else
          begin match fs.parent with
          | None ->
              `Global
          | Some p -> (
              match lookup p name with
              | `Local | `Upvalue ->
                  `Upvalue
              | (`Const | `Global) as kind ->
                  kind
            )
        end
  in
  let rec capture fs name at =
    match find_last (fun v -> v.vname = name) fs.vars with
    | Some v ->
        if v.vctc then
          `Const
        else
          `Local
    | None ->
        if Dynarray.exists (String.equal name) fs.upvals then
          `Upvalue
        else
          begin match fs.parent with
          | None ->
              `Global
          | Some p -> (
              match capture p name at with
              | `Local | `Upvalue ->
                  if
                    Dynarray.length fs.upvals + 1 > max_upvals
                    && not fs.over_upvals
                  then begin
                    fs.over_upvals <- true;
                    add at ~notes:(in_function fs)
                      ~help:
                        "each local of an enclosing function that this \
                         function uses counts, and _ENV counts when it uses a \
                         global"
                      (Printf.sprintf
                         "too many upvalues in one function (the limit is %d)"
                         max_upvals
                      )
                  end;
                  Dynarray.add_last fs.upvals name;
                  `Upvalue
              | (`Const | `Global) as kind ->
                  kind
            )
        end
  in
  let use fs name at =
    match capture fs name at with
    | `Global ->
        ignore (capture fs "_ENV" at)
    | `Const | `Local | `Upvalue ->
        ()
  in
  let rec constant fs e =
    match e.edesc with
    | Nil | True | False | Number _ | String _ ->
        true
    | Paren x | Unop (Not, x) ->
        constant fs x
    | Unop (Neg, { edesc = Number (Float f, _); _ }) ->
        (not (Float.is_nan f)) && f <> 0.
    | Unop ((Neg | Bnot), { edesc = Number (Int _, _); _ }) ->
        true
    | Name n ->
        lookup fs n = `Const
    | _ ->
        false
  in
  let remove_goto fs i =
    for j = i to Dynarray.length fs.gotos - 2 do
      Dynarray.set fs.gotos j (Dynarray.get fs.gotos (j + 1))
    done;
    ignore (Dynarray.pop_last fs.gotos)
  in
  let solve fs (l : label) =
    let bl = List.hd fs.blocks in
    let i = ref bl.firstgoto in
    while !i < Dynarray.length fs.gotos do
      let g = Dynarray.get fs.gotos !i in
      if g.gname = l.lname then begin
        if g.gnactvar < l.lnactvar then begin
          let v = Dynarray.get fs.vars g.gnactvar in
          add g.gat
            ~notes:
              [
                (v.vat, Printf.sprintf "local '%s' is declared here" v.vname);
                (l.lat, "the label is here");
              ]
            ~help:
              "a \"goto\" cannot jump forward into the scope of a local; move \
               the label before the declaration, or put the declaration in a \
               \"do ... end\" block"
            (Printf.sprintf "goto '%s' jumps into the scope of local '%s'"
               g.gname v.vname
            )
        end;
        remove_goto fs !i
      end else
        incr i
    done
  in
  let create_label fs lname lat ~last =
    let lnactvar =
      if last then
        (List.hd fs.blocks).bnactvar
      else
        Dynarray.length fs.vars
    in
    let l = { lname; lat; lnactvar } in
    Dynarray.add_last fs.labels l;
    solve fs l
  in
  let find_label fs name = find_last (fun l -> l.lname = name) fs.labels in
  let undefined fs (g : pending) =
    if g.gname = "break" then
      add g.gat
        ~help:
          "\"break\" leaves the innermost \"while\", \"for\" or \"repeat\" \
           loop of the same function"
        "\"break\" outside a loop"
    else if not fs.broken then
      let help =
        match List.find_opt (fun l -> l.lname = g.gname) !all_labels with
        | Some l ->
            Some
              (Printf.sprintf
                 "the label at %s is not visible here: a label is visible in \
                  its block and the nested blocks, but not in nested functions"
                 (loc l.lat.start)
              )
        | None ->
            None
      in
      add g.gat ?help (Printf.sprintf "no visible label '%s' for goto" g.gname)
  in
  let leave fs =
    match fs.blocks with
    | [] ->
        assert false
    | bl :: rest ->
        Dynarray.truncate fs.vars bl.bnactvar;
        if bl.isloop then
          create_label fs "break" { start = 0; stop = 0 } ~last:false;
        Dynarray.truncate fs.labels bl.firstlabel;
        fs.blocks <- rest;
        if rest <> [] then
          for i = bl.firstgoto to Dynarray.length fs.gotos - 1 do
            (Dynarray.get fs.gotos i).gnactvar <- bl.bnactvar
          done
        else
          for i = bl.firstgoto to Dynarray.length fs.gotos - 1 do
            undefined fs (Dynarray.get fs.gotos i)
          done
  in
  let rec resolve fs name =
    match find_last (fun v -> v.vname = name) fs.vars with
    | Some v ->
        Some v
    | None ->
        Option.bind fs.parent (fun p -> resolve p name)
  in
  let check_assign fs name at =
    match resolve fs name with
    | Some { vattrib = Some a; vat; _ } ->
        add at
          ~notes:[ (vat, Printf.sprintf "declared <%s> here" (attrib_text a)) ]
          (Printf.sprintf "attempt to assign to const variable '%s'" name)
    | Some _ | None ->
        ()
  in
  let rec expr fs e =
    match e.edesc with
    | Nil | True | False | Number _ | String _ ->
        ()
    | Name n ->
        use fs n e.erange
    | Vararg ->
        if not fs.vararg then
          add e.erange
            ~help:"add \"...\" to the parameter list of the enclosing function"
            "cannot use '...' outside a vararg function"
    | Function f ->
        funcbody fs f ~self:false ~opener:e.erange
    | Table fields ->
        List.iter
          (function
            | Positional v | Named (_, v) ->
                expr fs v
            | Keyed (k, v) ->
                expr fs k;
                expr fs v
            | Bad_field ->
                fs.broken <- true
            )
          fields
    | Binop (_, a, b) ->
        expr fs a;
        expr fs b
    | Unop (_, a) | Paren a ->
        expr fs a
    | Index (a, b) ->
        expr fs a;
        expr fs b
    | Call (f, args) | Method_call (f, _, args) ->
        expr fs f;
        List.iter (expr fs) args
    | Bad_expr ->
        fs.broken <- true
  and funcbody parent f ~self ~opener =
    let fs = new_fs ~opener (Some parent) f.vararg in
    enter fs false;
    if self then declare fs "self" None opener;
    List.iter
      (function
        | Param (p, r) ->
            declare fs p None r
        | Bad_param ->
            fs.broken <- true;
            declare fs "" None opener
        )
      f.params;
    block fs f.body ~repeat:false;
    leave fs
  and scoped fs isloop body ~repeat =
    enter fs isloop;
    block fs body ~repeat;
    leave fs
  and block fs stats ~repeat =
    let rec go = function
      | [] ->
          ()
      | s :: rest ->
          let last =
            (not repeat)
            && List.for_all
                 (fun s -> match s.sdesc with Label _ -> true | _ -> false)
                 rest
          in
          stat fs s ~last;
          go rest
    in
    go stats
  and stat fs s ~last =
    match s.sdesc with
    | Local (vars, es) ->
        List.iter (expr fs) es;
        let ctc =
          match (List.rev vars, List.rev es) with
          | Var (_, Some Const, _) :: _, e :: _ ->
              List.length vars = List.length es && constant fs e
          | _ ->
              false
        in
        let count = List.length vars in
        List.iteri
          (fun i -> function
            | Var (n, a, r) ->
                declare ~ctc:(ctc && i = count - 1) fs n a r
            | Bad_var ->
                fs.broken <- true;
                declare fs "" None s.srange
            )
          vars
    | Local_function (n, f) ->
        declare fs n None s.srange;
        funcbody fs f ~self:false ~opener:s.srange
    | Assign (targets, es) ->
        List.iter
          (fun t ->
            (match t.edesc with Name n -> check_assign fs n t.erange | _ -> ());
            expr fs t
          )
          targets;
        List.iter (expr fs) es
    | Call_stat e ->
        expr fs e
    | Do b ->
        scoped fs false b ~repeat:false
    | While (c, b) ->
        expr fs c;
        enter fs true;
        scoped fs false b ~repeat:false;
        leave fs
    | Repeat (b, c) ->
        enter fs true;
        enter fs false;
        block fs b ~repeat:true;
        expr fs c;
        leave fs;
        leave fs
    | If (branches, else_) ->
        List.iter
          (fun (c, b) ->
            expr fs c;
            scoped fs false b ~repeat:false
          )
          branches;
        Option.iter (fun b -> scoped fs false b ~repeat:false) else_
    | Numeric_for (v, a, b, c, body) ->
        enter fs true;
        List.iter (expr fs) (a :: b :: Option.to_list c);
        for _ = 1 to 3 do
          declare fs "(for state)" None s.srange
        done;
        enter fs false;
        declare fs v None s.srange;
        scoped fs false body ~repeat:false;
        leave fs;
        leave fs
    | Generic_for (vs, es, body) ->
        enter fs true;
        List.iter (expr fs) es;
        if vs = [] then fs.broken <- true;
        for _ = 1 to 4 do
          declare fs "(for state)" None s.srange
        done;
        enter fs false;
        List.iter (fun v -> declare fs v None s.srange) vs;
        scoped fs false body ~repeat:false;
        leave fs;
        leave fs
    | Function_stat (path, meth, f) ->
        use fs (List.hd path) s.srange;
        ( match (path, meth) with
        | [ n ], None ->
            check_assign fs n s.srange
        | _ ->
            ()
        );
        funcbody fs f ~self:(meth <> None) ~opener:s.srange
    | Return es ->
        List.iter (expr fs) es
    | Break ->
        Dynarray.add_last fs.gotos
          {
            gname = "break";
            gat = s.srange;
            gnactvar = Dynarray.length fs.vars;
          }
    | Goto name -> (
        match find_label fs name with
        | Some _ ->
            ()
        | None ->
            Dynarray.add_last fs.gotos
              {
                gname = name;
                gat = s.srange;
                gnactvar = Dynarray.length fs.vars;
              }
      )
    | Label name ->
        ( match find_label fs name with
        | Some l ->
            add s.srange
              ~notes:[ (l.lat, "first defined here") ]
              (Printf.sprintf "label '%s' is already defined at %s" name
                 (loc l.lat.start)
              )
        | None ->
            ()
        );
        all_labels :=
          { lname = name; lat = s.srange; lnactvar = 0 } :: !all_labels;
        create_label fs name s.srange ~last
    | Bad_stat ->
        fs.broken <- true
  in
  let main = new_fs None true in
  Dynarray.add_last main.upvals "_ENV";
  enter main false;
  block main chunk ~repeat:false;
  leave main;
  ( match nesting source chunk with
  | Some at ->
      add at
        ~help:
          "luac counts a level for each enclosing statement and expression, \
           and for each operand of a chain such as a .. b .. c, and stops with \
           \"C stack overflow\"; move part of it into a local variable or a \
           function"
        (Printf.sprintf "too deeply nested (the limit is %d levels)" max_levels)
  | None ->
      ()
  );
  List.rev !issues
