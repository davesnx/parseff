type value =
  | Unit
  | Chr of char
  | Str of string
  | Int of int
  | List of value list
  | Pair of value * value
  | Opt of value option
  | Range of int * int
  | Bin of value * value * value
  | Pre of value * value
  | Post of value * value
  | Cap_ok of value
  | Cap_err of string

type g =
  | Eps
  | Char of char
  | Consume of string
  | Any
  | Soft_error
  | Seq of g * g
  | Or of g * g
  | One_of of g list
  | Optional of g
  | Many of int * g
  | Sep_by of int * g * g
  | End_by of int * g * g
  | Fold_left of g * g * bool
  | Fold_right of g * g * bool
  | Count of int * g
  | Look_ahead of g
  | Commit
  | Cut
  | Context of string * bool * g
  | Expect of g
  | With_range of g
  | Recognize of g
  | Expression of g * op list list
  | Capture of g
  | Capture_release of g
  | Warn of int
  | Set of int
  | Get
  | Fail_hard
  | Raise
  | Try of g * g
  | Rec of g

and op = Infix_l of g | Infix_r of g | Prefix of g | Postfix of g

let rec show_value = function
  | Unit ->
      "()"
  | Chr c ->
      Printf.sprintf "%C" c
  | Str s ->
      Printf.sprintf "%S" s
  | Int n ->
      string_of_int n
  | List l ->
      "[" ^ String.concat "; " (List.map show_value l) ^ "]"
  | Pair (a, b) ->
      "(" ^ show_value a ^ ", " ^ show_value b ^ ")"
  | Opt None ->
      "None"
  | Opt (Some v) ->
      "Some " ^ show_value v
  | Range (a, b) ->
      Printf.sprintf "%d..%d" a b
  | Bin (o, a, b) ->
      Printf.sprintf "Bin(%s, %s, %s)" (show_value o) (show_value a)
        (show_value b)
  | Pre (o, a) ->
      Printf.sprintf "Pre(%s, %s)" (show_value o) (show_value a)
  | Post (o, a) ->
      Printf.sprintf "Post(%s, %s)" (show_value o) (show_value a)
  | Cap_ok v ->
      "Cap_ok " ^ show_value v
  | Cap_err s ->
      Printf.sprintf "Cap_err %S" s

let rec show_g = function
  | Eps ->
      "Eps"
  | Char c ->
      Printf.sprintf "Char %C" c
  | Consume s ->
      Printf.sprintf "Consume %S" s
  | Any ->
      "Any"
  | Soft_error ->
      "Soft_error"
  | Seq (a, b) ->
      Printf.sprintf "Seq (%s, %s)" (show_g a) (show_g b)
  | Or (a, b) ->
      Printf.sprintf "Or (%s, %s)" (show_g a) (show_g b)
  | One_of l ->
      "One_of [" ^ String.concat "; " (List.map show_g l) ^ "]"
  | Optional a ->
      Printf.sprintf "Optional (%s)" (show_g a)
  | Many (n, a) ->
      Printf.sprintf "Many (%d, %s)" n (show_g a)
  | Sep_by (n, a, b) ->
      Printf.sprintf "Sep_by (%d, %s, %s)" n (show_g a) (show_g b)
  | End_by (n, a, b) ->
      Printf.sprintf "End_by (%d, %s, %s)" n (show_g a) (show_g b)
  | Fold_left (a, b, o) ->
      Printf.sprintf "Fold_left (%s, %s, %b)" (show_g a) (show_g b) o
  | Fold_right (a, b, o) ->
      Printf.sprintf "Fold_right (%s, %s, %b)" (show_g a) (show_g b) o
  | Count (n, a) ->
      Printf.sprintf "Count (%d, %s)" n (show_g a)
  | Look_ahead a ->
      Printf.sprintf "Look_ahead (%s)" (show_g a)
  | Commit ->
      "Commit"
  | Cut ->
      "Cut"
  | Context (n, d, a) ->
      Printf.sprintf "Context (%S, %b, %s)" n d (show_g a)
  | Expect a ->
      Printf.sprintf "Expect (%s)" (show_g a)
  | With_range a ->
      Printf.sprintf "With_range (%s)" (show_g a)
  | Recognize a ->
      Printf.sprintf "Recognize (%s)" (show_g a)
  | Expression (a, t) ->
      let op = function
        | Infix_l g ->
            "Infix_l (" ^ show_g g ^ ")"
        | Infix_r g ->
            "Infix_r (" ^ show_g g ^ ")"
        | Prefix g ->
            "Prefix (" ^ show_g g ^ ")"
        | Postfix g ->
            "Postfix (" ^ show_g g ^ ")"
      in
      let level l = "[" ^ String.concat "; " (List.map op l) ^ "]" in
      Printf.sprintf "Expression (%s, [%s])" (show_g a)
        (String.concat "; " (List.map level t))
  | Capture a ->
      Printf.sprintf "Capture (%s)" (show_g a)
  | Capture_release a ->
      Printf.sprintf "Capture_release (%s)" (show_g a)
  | Warn k ->
      Printf.sprintf "Warn %d" k
  | Set k ->
      Printf.sprintf "Set %d" k
  | Get ->
      "Get"
  | Fail_hard ->
      "Fail_hard"
  | Raise ->
      "Raise"
  | Try (a, b) ->
      Printf.sprintf "Try (%s, %s)" (show_g a) (show_g b)
  | Rec a ->
      Printf.sprintf "Rec (%s)" (show_g a)

let max_depth = 8
let step_budget = 4_000

exception Boom
exception Over_budget

type Parseff.custom_error += Soft

let show_error (e : Parseff.error) =
  let contexts =
    List.map
      (fun (c : Parseff.context) -> Printf.sprintf "%s@%d" c.name c.start)
      e.contexts
  in
  let message = Parseff.error_message e in
  let message =
    match Re.exec_opt (Re.compile (Re.str ", found ")) message with
    | Some g ->
        String.sub message 0 (Re.Group.start g 0)
    | None ->
        message
  in
  Printf.sprintf "%d %s [%s]" e.range.start message (String.concat ", " contexts)

(* Interpreter (a): the real combinators. *)

type cx = {
  diags : int Parseff.Diagnostics.t;
  cell : int Parseff.Ref.t;
  mutable steps : int;
}

let rec run cx g =
  cx.steps <- cx.steps + 1;
  if cx.steps > step_budget then raise Over_budget;
  match g with
  | Eps ->
      Unit
  | Char c ->
      Chr (Parseff.char c)
  | Consume s ->
      Str (Parseff.consume s)
  | Any ->
      Chr (Parseff.any_char ())
  | Soft_error ->
      Parseff.error Soft
  | Seq (a, b) ->
      let x = run cx a in
      let y = run cx b in
      Pair (x, y)
  | Or (a, b) ->
      Parseff.or_ (fun () -> run cx a) (fun () -> run cx b) ()
  | One_of l ->
      Parseff.one_of (List.map (fun g () -> run cx g) l) ()
  | Optional a ->
      Opt (Parseff.optional (fun () -> run cx a) ())
  | Many (n, a) ->
      List (Parseff.many ~at_least:n (fun () -> run cx a) ())
  | Sep_by (n, a, s) ->
      List
        (Parseff.sep_by ~at_least:n (fun () -> run cx a) (fun () -> run cx s) ())
  | End_by (n, a, s) ->
      List
        (Parseff.end_by ~at_least:n (fun () -> run cx a) (fun () -> run cx s) ())
  | Fold_left (a, o, d) ->
      Parseff.fold_left
        (fun () -> run cx a)
        (combine cx o)
        ?otherwise:
          ( if d then
              Some (Str "otherwise")
            else
              None
          )
        ()
  | Fold_right (a, o, d) ->
      Parseff.fold_right
        (fun () -> run cx a)
        (combine cx o)
        ?otherwise:
          ( if d then
              Some (Str "otherwise")
            else
              None
          )
        ()
  | Count (n, a) ->
      List (Parseff.count n (fun () -> run cx a) ())
  | Look_ahead a ->
      Parseff.look_ahead (fun () -> run cx a)
  | Commit ->
      Parseff.commit ();
      Unit
  | Cut ->
      Parseff.cut ();
      Unit
  | Context (name, delimited, a) ->
      Parseff.context ~delimited name (fun () -> run cx a)
  | Expect a ->
      Parseff.expect "thing" (fun () -> run cx a)
  | With_range a ->
      let v, r = Parseff.with_range (fun () -> run cx a) in
      Pair (v, Range (r.start, r.stop))
  | Recognize a ->
      Str (Parseff.recognize (fun () -> run cx a))
  | Expression (atom, table) ->
      let op = function
        | Infix_l t ->
            Parseff.infix_left (fun () ->
                let o = run cx t in
                fun a b -> Bin (o, a, b)
            )
        | Infix_r t ->
            Parseff.infix_right (fun () ->
                let o = run cx t in
                fun a b -> Bin (o, a, b)
            )
        | Prefix t ->
            Parseff.prefix (fun () ->
                let o = run cx t in
                fun a -> Pre (o, a)
            )
        | Postfix t ->
            Parseff.postfix (fun () ->
                let o = run cx t in
                fun a -> Post (o, a)
            )
      in
      Parseff.expression
        ~operand:(fun () -> run cx atom)
        (List.map (List.map op) table)
        ()
  | Capture a -> (
      match Parseff.capture (fun () -> run cx a) with
      | Ok v ->
          Cap_ok v
      | Error e ->
          Cap_err (show_error e)
    )
  | Capture_release a -> (
      match Parseff.capture ~release:true (fun () -> run cx a) with
      | Ok v ->
          Cap_ok v
      | Error e ->
          Cap_err (show_error e)
    )
  | Warn k ->
      Parseff.warn cx.diags k;
      Unit
  | Set k ->
      Parseff.Ref.set cx.cell k;
      Unit
  | Get ->
      Int (Parseff.Ref.get cx.cell)
  | Fail_hard ->
      Parseff.fail "hard"
  | Raise ->
      raise Boom
  | Try (a, b) -> (
      try run cx a with Boom -> run cx b
    )
  | Rec a ->
      Parseff.rec_ (fun () -> run cx a)

and combine cx o () =
  let v = run cx o in
  fun a b -> Bin (v, a, b)

type cls = Success | Soft | Hard of int | Depth of int | Window | Escaped

type outcome = {
  cls : cls;
  result : (value, string) result;
  diags : (int * int) list;
  cell : int;
  trace : string list;
}

let show_event = function
  | Parseff.Enter { name; pos } ->
      Printf.sprintf "enter %s@%d" name pos
  | Return { name; start; stop } ->
      Printf.sprintf "exit %s@%d..%d" name start stop
  | Fail { name; start; pos } ->
      Printf.sprintf "fail %s@%d from %d" name pos start
  | Captured { start; error } ->
      Printf.sprintf "captured @%d: %s" start (show_error error)

let window_exceeded = "backtrack window exceeded"

type feed = Function | Seq | Bigstring

let chunked feed input sizes =
  let pos = ref 0 in
  let turn = ref 0 in
  let next len =
    let chunk = List.nth sizes (!turn mod List.length sizes) in
    incr turn;
    let n = min (min chunk len) (String.length input - !pos) in
    let p = !pos in
    pos := p + n;
    (p, n)
  in
  match feed with
  | Function ->
      Parseff.Source.of_function ?initial_buffer_size:Buf_size_env.buf_size
        (fun buf off len ->
          let p, n = next len in
          Bytes.blit_string input p buf off n;
          n
      )
  | Seq ->
      let rec seq () =
        if !pos >= String.length input then
          Seq.Nil
        else
          let p, n = next max_int in
          Seq.Cons (String.sub input p n, seq)
      in
      Parseff.Source.of_seq seq
  | Bigstring ->
      let shift = 3 in
      let len = String.length input in
      let buf =
        Bigarray.Array1.create Bigarray.char Bigarray.c_layout (len + 5)
      in
      Bigarray.Array1.fill buf '#';
      String.iteri (fun i c -> buf.{shift + i} <- c) input;
      Parseff.Source.of_bigstring_chunks (fun () ->
          if !pos >= len then
            None
          else
            let p, n = next max_int in
            Some (buf, shift + p, n)
      )

type runner = Memory | Stream of feed * int list * int

let run_real ?(traced = false) ~partial runner g input =
  let cx =
    {
      diags = Parseff.Diagnostics.create ();
      cell = Parseff.Ref.create 0;
      steps = 0;
    }
  in
  let events = ref [] in
  let trace =
    if traced then
      Some (fun e -> events := show_event e :: !events)
    else
      None
  in
  let p () =
    let v = run cx g in
    if partial then
      Pair (v, Int (Parseff.position ()))
    else
      v
  in
  let consume =
    if partial then
      `Partial
    else
      `All
  in
  let cls, result =
    match
      match runner with
      | Memory ->
          Parseff.parse ~max_depth ~consume ~diagnostics:cx.diags ?trace input p
      | Stream (feed, sizes, backtrack_window) ->
          Parseff.parse_source ~max_depth ~backtrack_window ~consume
            ~diagnostics:cx.diags ?trace (chunked feed input sizes) p
    with
    | Ok v ->
        (Success, Ok v)
    | Error { kind = Backtrack_window_exceeded _; _ } ->
        (Window, Error window_exceeded)
    | Error ({ kind = Failed "hard"; _ } as e) ->
        (Hard e.range.start, Error (show_error e))
    | Error ({ kind = Depth_limit_exceeded _; _ } as e) ->
        (Depth e.range.start, Error (show_error e))
    | Error e ->
        (Soft, Error (show_error e))
    | exception Boom ->
        (Escaped, Error "Boom escaped")
  in
  {
    cls;
    result;
    diags =
      List.map
        (fun (d : int Parseff.Diagnostics.entry) -> (d.pos, d.value))
        (Parseff.Diagnostics.to_list cx.diags);
    cell = Parseff.Ref.get cx.cell;
    trace = List.rev !events;
  }

(* Interpreter (b): a pure backtracking model. A state is a position, the
   diagnostics recorded so far (newest first) and the value of the one Ref
   cell. Every backtracking point saves the state and gets back to it
   explicitly. A frame is a [bool ref] committed flag, so [commit] and [cut]
   can seal the frames that are open around them. A [capture] is a barrier
   that goes back to its start. A [capture ~release:true] frame holds the state
   it goes back to, which [commit] and [cut] replace. A [cut] that replaces it
   also gives that state to the sealed frames above the [capture]: a frame that
   then drops a step without progress goes back to it, so nothing recorded
   before the cut is lost. *)

type mst = { pos : int; diags : (int * int) list; cell : int }

exception M_fail of mst
exception M_boom of mst
exception M_hard of int * mst
exception M_depth of int * mst

(* The first step of the right operand of an [infix_right] whose operator and
   operand consumed nothing stops at an [infix_right] operator that consumes
   nothing, so that the pair does not nest until the depth limit. *)
exception Expression_stop

type frame =
  | Flag of {
      committed : bool ref;
      kept : mst option ref;
      start : int;
      last : bool;
    }
  | Barrier of int
  | Captured of mst ref
  | Pin of int

type env = {
  frames : frame list;
  nframes : int;
  ctx : (int * bool ref) option;
  depth : int;
}

type model = { input : string; mutable msteps : int; mutable span : int }

let push f env =
  { env with frames = f :: env.frames; nframes = env.nframes + 1 }

let flag ?(last = false) env st =
  let committed = ref false in
  ( committed,
    push (Flag { committed; kept = ref None; start = st.pos; last }) env
  )

let back env st =
  match env.frames with
  | Flag { kept = { contents = Some s }; _ } :: _ ->
      { s with pos = st.pos }
  | _ ->
      st

let rec commit_top st = function
  | Pin _ :: rest ->
      commit_top st rest
  | Flag f :: _ ->
      f.committed := true
  | Captured saved :: _ ->
      saved := st
  | Barrier _ :: _ | [] ->
      ()

let seal ?captures frames n =
  let rec go above fs n =
    n <= 0
    ||
    match fs with
    | Barrier _ :: _ ->
        false
    | Captured saved :: rest -> (
        match captures with
        | Some st ->
            saved := st;
            List.iter (fun kept -> kept := Some st) above;
            go above rest (n - 1)
        | None ->
            false
      )
    | Flag f :: rest ->
        f.committed := true;
        go (f.kept :: above) rest (n - 1)
    | Pin _ :: rest ->
        go above rest (n - 1)
    | [] ->
        true
  in
  go [] frames n

(* [parse_source] must keep the bytes from the oldest position that a frame
   can still rewind to. A read that needs the bytes up to [need] checks the
   window while fewer than [need] bytes are buffered, so it fails only if
   [need - 1 - oldest] reaches the window. *)
let read m env st need =
  let oldest =
    List.fold_left
      (fun oldest -> function
        | Barrier start | Pin start ->
            min oldest start
        | Captured saved ->
            min oldest !saved.pos
        | Flag { committed; start; last; _ } ->
            if !committed || last then
              oldest
            else
              min oldest start
        )
      st.pos env.frames
  in
  m.span <- max m.span (need - 1 - oldest)

let rec_ env st k =
  if env.depth >= max_depth then raise (M_depth (st.pos, st));
  k { env with depth = env.depth + 1 } st

let choice env left right st =
  let f, envf = flag env st in
  match left envf st with
  | r ->
      r
  | exception M_fail s ->
      if !f then raise (M_fail s);
      right (snd (flag ~last:true env st)) st

let repeat env body acc st =
  let rec loop acc st =
    let f, envf = flag env st in
    match body envf st with
    | v, st' ->
        if st'.pos = st.pos then
          (List.rev acc, back envf st)
        else
          loop (v :: acc) st'
    | exception M_fail s ->
        if !f then raise (M_fail s);
        (List.rev acc, st)
  in
  loop acc st

let rec required env body acc n st =
  if n <= 0 then
    (acc, st)
  else
    let v, st = body env st in
    required env body (v :: acc) (n - 1) st

let many env n body st =
  let acc, st = required env body [] n st in
  repeat env body acc st

let then_ a b env st =
  let v, st = a env st in
  let _, st = b env st in
  (v, st)

let rec ev m env g st =
  m.msteps <- m.msteps + 1;
  if m.msteps > step_budget then raise Over_budget;
  let len = String.length m.input in
  match g with
  | Eps ->
      (Unit, st)
  | Char c ->
      read m env st (st.pos + 1);
      if st.pos < len && m.input.[st.pos] = c then
        (Chr c, { st with pos = st.pos + 1 })
      else
        raise (M_fail st)
  | Consume s ->
      let n = String.length s in
      read m env st (st.pos + n);
      if st.pos + n <= len && String.sub m.input st.pos n = s then
        (Str s, { st with pos = st.pos + n })
      else
        raise (M_fail st)
  | Any ->
      read m env st (st.pos + 1);
      if st.pos < len then
        (Chr m.input.[st.pos], { st with pos = st.pos + 1 })
      else
        raise (M_fail st)
  | Soft_error ->
      raise (M_fail st)
  | Seq (a, b) ->
      let x, st = ev m env a st in
      let y, st = ev m env b st in
      (Pair (x, y), st)
  | Or (a, b) ->
      choice env (fun env -> ev m env a) (fun env -> ev m env b) st
  | One_of [] ->
      raise (M_fail st)
  | One_of [ a ] ->
      ev m env a st
  | One_of (a :: rest) ->
      choice env (fun env -> ev m env a) (fun env -> ev m env (One_of rest)) st
  | Optional a -> (
      let f, envf = flag env st in
      match ev m envf a st with
      | v, st ->
          (Opt (Some v), st)
      | exception M_fail s ->
          if !f then raise (M_fail s);
          (Opt None, st)
    )
  | Many (n, a) ->
      let l, st = many env n (fun env -> ev m env a) st in
      (List l, st)
  | Sep_by (0, a, s) -> (
      let f, envf = flag env st in
      match ev m envf a st with
      | v, st ->
          let l, st = repeat env (sep_then m a s) [ v ] st in
          (List l, st)
      | exception M_fail failed ->
          if !f then raise (M_fail failed);
          (List [], st)
    )
  | Sep_by (n, a, s) ->
      let v, st = ev m env a st in
      let l, st = many env (n - 1) (sep_then m a s) st in
      (List (v :: l), st)
  | End_by (n, a, s) ->
      let l, st =
        many env n (then_ (fun env -> ev m env a) (fun env -> ev m env s)) st
      in
      (List l, st)
  | Fold_left (a, o, d) ->
      let one env st =
        let acc, st = ev m env a st in
        let rec loop acc st =
          let f, env' = flag env st in
          match
            let ov, st1 = ev m env' o st in
            let rhs, st2 = ev m env' a st1 in
            (Bin (ov, acc, rhs), st2)
          with
          | v, st' ->
              if st'.pos = st.pos then
                (acc, back env' st)
              else
                loop v st'
          | exception M_fail s ->
              if !f then raise (M_fail s);
              (acc, st)
        in
        loop acc st
      in
      otherwise env d one st
  | Fold_right (a, o, d) ->
      let rec one env st =
        let first, st = ev m env a st in
        choice env
          (fun env st ->
            let ov, st = ev m env o st in
            let rhs, st = one env st in
            (Bin (ov, first, rhs), st)
          )
          (fun _ st -> (first, st))
          st
      in
      otherwise env d one st
  | Count (n, a) ->
      let acc, st = required env (fun env -> ev m env a) [] n st in
      (List (List.rev acc), st)
  | Look_ahead a -> (
      match ev m (push (Barrier st.pos) env) a st with
      | v, _ ->
          (v, st)
      | exception M_fail _ ->
          raise (M_fail st)
      | exception M_boom _ ->
          raise (M_boom st)
      | exception M_hard (p, _) ->
          raise (M_hard (p, st))
      | exception M_depth (p, _) ->
          raise (M_depth (p, st))
    )
  | Commit ->
      commit_top st env.frames;
      (Unit, st)
  | Cut ->
      ( match env.ctx with
      | None ->
          ignore (seal ~captures:st env.frames env.nframes)
      | Some (floor, cut) ->
          if seal ~captures:st env.frames (env.nframes - floor) then cut := true
      );
      (Unit, st)
  | Context (_, _, a) -> (
      let cut = ref false in
      match ev m { env with ctx = Some (env.nframes, cut) } a st with
      | r ->
          r
      | exception M_fail s ->
          if !cut then ignore (seal env.frames env.nframes);
          raise (M_fail s)
    )
  | Expect a ->
      ev m env a st
  | With_range a ->
      let v, st' = ev m env a st in
      (Pair (v, Range (st.pos, st'.pos)), st')
  | Recognize a ->
      let _, st' = ev m (push (Pin st.pos) env) a st in
      (Str (String.sub m.input st.pos (st'.pos - st.pos)), st')
  | Expression (atom, table) ->
      expression m env atom table st
  | Capture a -> (
      match ev m (push (Barrier st.pos) env) a st with
      | v, st ->
          (Cap_ok v, st)
      | exception (M_fail _ | M_hard _) ->
          (Cap_err "", st)
    )
  | Capture_release a -> (
      let saved = ref st in
      match ev m (push (Captured saved) env) a st with
      | v, st ->
          (Cap_ok v, st)
      | exception (M_fail _ | M_hard _) ->
          (Cap_err "", !saved)
    )
  | Warn k ->
      (Unit, { st with diags = (st.pos, k) :: st.diags })
  | Set k ->
      (Unit, { st with cell = k })
  | Get ->
      (Int st.cell, st)
  | Fail_hard ->
      raise (M_hard (st.pos, st))
  | Raise ->
      raise (M_boom st)
  | Try (a, b) -> (
      match ev m env a st with r -> r | exception M_boom s -> ev m env b s
    )
  | Rec a ->
      rec_ env st (fun env -> ev m env a)

and sep_then m a s env st =
  let _, st = ev m env s st in
  ev m env a st

and otherwise env d one st =
  if d then
    choice env one (fun _ st -> (Str "otherwise", st)) st
  else
    one env st

and expression m env atom table st =
  let prefixed =
    List.exists (List.exists (function Prefix _ -> true | _ -> false)) table
  in
  let rec parse_levels env levels st =
    let lhs, st = operand env st in
    loop env levels lhs st (-1)
  and operand env st =
    if not prefixed then
      ev m env atom st
    else
      let f, envf = flag env st in
      let rec try_levels = function
        | [] ->
            None
        | ops :: rest ->
            try_ops rest ops
      and try_ops rest = function
        | [] ->
            try_levels rest
        | Prefix t :: ops -> (
            match
              let ov, s1 = ev m envf t st in
              if s1.pos = st.pos then raise (M_fail s1);
              let v, s2 = rec_ envf s1 (fun env -> parse_levels env rest) in
              (Pre (ov, v), s2)
            with
            | r ->
                Some r
            | exception M_fail s ->
                if !f then raise (M_fail s);
                try_ops rest ops
          )
        | _ :: ops ->
            try_ops rest ops
      in
      match try_levels table with Some r -> r | None -> ev m env atom st
  and loop env levels lhs st stop_at =
    match levels with
    | [] ->
        (lhs, st)
    | _ :: _ -> (
        let f, envr = flag env st in
        let rec try_levels = function
          | [] ->
              raise (M_fail st)
          | ops :: rest as here ->
              try_ops here rest ops
        and try_ops here rest = function
          | [] ->
              try_levels rest
          | Prefix _ :: ops ->
              try_ops here rest ops
          | op :: ops -> (
              match
                match op with
                | Infix_l t ->
                    let ov, s1 = ev m envr t st in
                    let rhs, s2 = parse_levels envr rest s1 in
                    (Bin (ov, lhs, rhs), s2)
                | Infix_r t ->
                    let ov, s1 = ev m envr t st in
                    if s1.pos = stop_at then raise Expression_stop;
                    let rhs, s2 =
                      rec_ envr s1 (fun env s1 ->
                          let o, s2 = operand env s1 in
                          loop env here o s2
                            ( if s2.pos = st.pos then
                                st.pos
                              else
                                -1
                            )
                      )
                    in
                    (Bin (ov, lhs, rhs), s2)
                | Postfix t ->
                    let ov, s1 = ev m envr t st in
                    (Post (ov, lhs), s1)
                | Prefix _ ->
                    assert false
              with
              | r ->
                  r
              | exception M_fail s ->
                  if !f then raise (M_fail s);
                  try_ops here rest ops
            )
        in
        match try_levels levels with
        | v, st' ->
            if st'.pos = st.pos then
              (lhs, back envr st)
            else
              loop env levels v st' (-1)
        | exception M_fail s ->
            if !f then raise (M_fail s);
            (lhs, st)
        | exception Expression_stop ->
            (lhs, back envr st)
      )
  in
  parse_levels env table st

type model_outcome =
  | M_success of value * mst
  | M_soft of mst
  | M_hard_at of int * mst
  | M_depth_at of int * mst
  | M_escaped of mst

let run_model ~partial g input =
  let m = { input; msteps = 0; span = 0 } in
  let env = { frames = []; nframes = 0; ctx = None; depth = 0 } in
  let outcome =
    match ev m env g { pos = 0; diags = []; cell = 0 } with
    | v, st ->
        if partial then
          M_success (Pair (v, Int st.pos), st)
        else begin
          read m env st (st.pos + 1);
          if st.pos = String.length input then
            M_success (v, st)
          else
            M_soft st
        end
    | exception M_fail st ->
        M_soft st
    | exception M_hard (p, st) ->
        M_hard_at (p, st)
    | exception M_depth (p, st) ->
        M_depth_at (p, st)
    | exception M_boom st ->
        M_escaped st
  in
  (outcome, m.span)

let rec erase = function
  | Cap_err _ ->
      Cap_err ""
  | (Unit | Chr _ | Str _ | Int _ | Range _) as v ->
      v
  | List l ->
      List (List.map erase l)
  | Pair (a, b) ->
      Pair (erase a, erase b)
  | Opt o ->
      Opt (Option.map erase o)
  | Bin (o, a, b) ->
      Bin (erase o, erase a, erase b)
  | Pre (o, a) ->
      Pre (erase o, erase a)
  | Post (o, a) ->
      Post (erase o, erase a)
  | Cap_ok v ->
      Cap_ok (erase v)

let show_model = function
  | M_success (v, _) ->
      "Ok " ^ show_value v
  | M_soft _ ->
      "a failure"
  | M_hard_at (p, _) ->
      Printf.sprintf "fail \"hard\" at %d" p
  | M_depth_at (p, _) ->
      Printf.sprintf "depth limit at %d" p
  | M_escaped _ ->
      "Boom escaped"

let model_state = function
  | M_success (_, st)
  | M_soft st
  | M_hard_at (_, st)
  | M_depth_at (_, st)
  | M_escaped st ->
      st

let agrees mo (o : outcome) =
  let st = model_state mo in
  List.rev st.diags = o.diags
  && st.cell = o.cell
  &&
  match (mo, o.cls, o.result) with
  | M_success (v, _), Success, Ok v' ->
      v = erase v'
  | M_soft _, Soft, _ | M_escaped _, Escaped, _ ->
      true
  | M_hard_at (p, _), Hard p', _ | M_depth_at (p, _), Depth p', _ ->
      p = p'
  | _ ->
      false

let show_diags l =
  String.concat "; " (List.map (fun (p, k) -> Printf.sprintf "%d:%d" p k) l)

let show_outcome (o : outcome) =
  Printf.sprintf "%s, diagnostics [%s], cell %d, trace [%s]"
    ( match o.result with
    | Ok v ->
        "Ok " ^ show_value v
    | Error e ->
        "Error " ^ e
    )
    (show_diags o.diags) o.cell
    (String.concat "; " o.trace)

let alphabet = [| 'a'; 'b'; 'c' |]
let int rs n = Random.State.int rs n
let letter rs = alphabet.(int rs (Array.length alphabet))

let leaf rs =
  match int rs 22 with
  | 0 | 1 | 2 ->
      Eps
  | 3 | 4 | 5 | 6 | 7 | 8 ->
      Char (letter rs)
  | 9 | 10 ->
      Consume (String.init (1 + int rs 2) (fun _ -> letter rs))
  | 11 ->
      Any
  | 12 ->
      Soft_error
  | 13 | 14 ->
      Commit
  | 15 ->
      Cut
  | 16 | 17 ->
      Warn (int rs 10)
  | 18 | 19 ->
      Set (int rs 10)
  | 20 ->
      Get
  | _ -> (
      match int rs 3 with 0 -> Fail_hard | 1 -> Raise | _ -> Get
    )

let rec gen rs size =
  if size <= 1 then
    leaf rs
  else
    let n = size - 1 in
    let h = max 1 (n / 2) in
    match int rs 30 with
    | 0 | 1 | 2 | 3 | 4 ->
        Seq (gen rs h, gen rs h)
    | 5 | 6 | 7 ->
        Or (gen rs h, gen rs h)
    | 8 ->
        One_of (List.init (int rs 4) (fun _ -> gen rs (max 1 (n / 3))))
    | 9 ->
        Optional (gen rs n)
    | 10 | 11 ->
        Many (int rs 2, gen rs n)
    | 12 ->
        Sep_by (int rs 3, gen rs h, gen rs h)
    | 13 ->
        End_by (int rs 2, gen rs h, gen rs h)
    | 14 ->
        Fold_left (gen rs h, gen rs h, int rs 2 = 0)
    | 15 ->
        Fold_right (gen rs h, Seq (Char (letter rs), gen rs h), int rs 2 = 0)
    | 16 ->
        Count (int rs 3, gen rs n)
    | 17 ->
        Look_ahead (gen rs n)
    | 18 | 19 ->
        Context
          ( ( if int rs 2 = 0 then
                "x"
              else
                "y"
            ),
            int rs 2 = 0,
            gen rs n
          )
    | 20 ->
        Expect (gen rs n)
    | 21 ->
        With_range (gen rs n)
    | 22 ->
        Recognize (gen rs n)
    | 23 | 24 ->
        gen_expression rs n
    | 25 ->
        if int rs 2 = 0 then
          Capture (gen rs n)
        else
          Capture_release (gen rs n)
    | 26 ->
        Try (gen rs h, gen rs h)
    | 27 ->
        Rec (gen rs n)
    | _ ->
        leaf rs

(* A prefix token that seals a frame with commit or cut and then consumes
   nothing makes [parse] raise [Failure "Parseff: internal error: failure
   without a recorded position"], so prefix tokens neither commit nor cut. *)
and unseal = function
  | Commit | Cut ->
      Eps
  | ( Eps
    | Char _
    | Consume _
    | Any
    | Soft_error
    | Warn _
    | Set _
    | Get
    | Fail_hard
    | Raise ) as g ->
      g
  | Seq (a, b) ->
      Seq (unseal a, unseal b)
  | Or (a, b) ->
      Or (unseal a, unseal b)
  | One_of l ->
      One_of (List.map unseal l)
  | Optional a ->
      Optional (unseal a)
  | Many (n, a) ->
      Many (n, unseal a)
  | Sep_by (n, a, b) ->
      Sep_by (n, unseal a, unseal b)
  | End_by (n, a, b) ->
      End_by (n, unseal a, unseal b)
  | Fold_left (a, b, d) ->
      Fold_left (unseal a, unseal b, d)
  | Fold_right (a, b, d) ->
      Fold_right (unseal a, unseal b, d)
  | Count (n, a) ->
      Count (n, unseal a)
  | Look_ahead a ->
      Look_ahead (unseal a)
  | Context (name, d, a) ->
      Context (name, d, unseal a)
  | Expect a ->
      Expect (unseal a)
  | With_range a ->
      With_range (unseal a)
  | Recognize a ->
      Recognize (unseal a)
  | Expression (a, t) ->
      let op = function
        | Infix_l g ->
            Infix_l (unseal g)
        | Infix_r g ->
            Infix_r (unseal g)
        | Prefix g ->
            Prefix (unseal g)
        | Postfix g ->
            Postfix (unseal g)
      in
      Expression (unseal a, List.map (List.map op) t)
  | Capture a ->
      Capture (unseal a)
  | Capture_release a ->
      Capture_release (unseal a)
  | Try (a, b) ->
      Try (unseal a, unseal b)
  | Rec a ->
      Rec (unseal a)

and gen_expression rs n =
  let token () =
    if int rs 3 = 0 then
      gen rs (max 1 (n / 4))
    else
      Char (letter rs)
  in
  let op () =
    match int rs 4 with
    | 0 ->
        Infix_l (token ())
    | 1 ->
        Infix_r (token ())
    | 2 ->
        Prefix (unseal (token ()))
    | _ ->
        Postfix (token ())
  in
  let table =
    List.init (int rs 4) (fun _ -> List.init (1 + int rs 2) (fun _ -> op ()))
  in
  Expression (gen rs (max 1 (n / 3)), table)

let gen_input rs = String.init (int rs 8) (fun _ -> letter rs)

let seeds = [ 1; 2; 3; 4; 5; 6; 7; 8 ]
let cases_per_seed = 2_000
let large_window = 65_536

let streams =
  let sizes =
    [
      ([ 1 ], large_window);
      ([ 1; 2; 3 ], large_window);
      ([ 1 ], 2);
      ([ 2 ], 3);
      ([ 3 ], 4);
      ([ 1; 3 ], 8);
      ([ 8 ], large_window);
      ([ 8 ], 2);
      ([ 8 ], 5);
    ]
  in
  List.map (fun (s, w) -> (Function, s, w)) sizes
  @ List.map (fun (s, w) -> (Seq, s, w)) sizes
  @ List.map (fun (s, w) -> (Bigstring, s, w)) sizes
  @ [ (Bigstring, [ 0; 2; 0; 0; 3 ], large_window) ]
  @ [ (Seq, [ 0; 2; 0; 0; 3 ], large_window); (Seq, [ 0; 1; 3 ], 3) ]

let show_stream (feed, sizes, window) =
  Printf.sprintf "parse_source, %s, chunks [%s], backtrack_window %d"
    ( match feed with
    | Function ->
        "of_function"
    | Seq ->
        "of_seq"
    | Bigstring ->
        "of_bigstring_chunks"
    )
    (String.concat "; " (List.map string_of_int sizes))
    window

let report seed case g input what =
  Alcotest.failf
    "seed %d, case %d (replay with Random.State.make [| %d; %d |])\n\
     grammar: %s\n\
     input: %S\n\
     %s"
    seed case seed case (show_g g) input what

type stats = {
  mutable checked : int;
  mutable skipped : int;
  mutable successes : int;
}

let check_case stats seed case =
  let rs = Random.State.make [| seed; case |] in
  let g = gen rs (2 + int rs 14) in
  let input = gen_input rs in
  let partial = int rs 2 = 0 in
  match
    let mo, span = run_model ~partial g input in
    let memory = run_real ~partial Memory g input in
    let traced = run_real ~traced:true ~partial Memory g input in
    let streamed =
      List.map
        (fun ((feed, sizes, window) as s) ->
          ( s,
            run_real ~traced:true ~partial
              (Stream (feed, sizes, window))
              g input
          )
        )
        streams
    in
    (mo, span, memory, traced, streamed)
  with
  | exception Over_budget ->
      stats.skipped <- stats.skipped + 1
  | exception exn ->
      report seed case g input ("raised " ^ Printexc.to_string exn)
  | mo, span, memory, traced, streamed ->
      stats.checked <- stats.checked + 1;
      if memory.cls = Success then stats.successes <- stats.successes + 1;
      let against name (o : outcome) =
        if not (agrees mo o) then
          report seed case g input
            (Printf.sprintf "model: %s, diagnostics [%s], cell %d\n%s: %s"
               (show_model mo)
               (show_diags (List.rev (model_state mo).diags))
               (model_state mo).cell name (show_outcome o)
            )
      in
      against "parse" memory;
      let untraced o = { o with trace = [] } in
      if untraced traced <> memory then
        report seed case g input
          (Printf.sprintf "parse: %s\nparse ~trace: %s" (show_outcome memory)
             (show_outcome traced)
          );
      List.iter
        (fun (((_, _, window) as s), (o : outcome)) ->
          if window = large_window then against (show_stream s) (untraced o);
          if o.cls = Window then
            begin if window > span then
              report seed case g input
                (Printf.sprintf
                   "%s: window exceeded, but the model rewinds at most %d bytes"
                   (show_stream s) span
                )
          end
          else if o <> traced then
            report seed case g input
              (Printf.sprintf "parse: %s\n%s: %s" (show_outcome traced)
                 (show_stream s) (show_outcome o)
              )
        )
        streamed

let test_random_grammars () =
  let stats = { checked = 0; skipped = 0; successes = 0 } in
  List.iter
    (fun seed ->
      for case = 0 to cases_per_seed - 1 do
        check_case stats seed case
      done
    )
    seeds;
  let total = List.length seeds * cases_per_seed in
  if stats.skipped * 20 > total then
    Alcotest.failf "%d of %d cases ran out of steps" stats.skipped total;
  if stats.successes * 5 < stats.checked then
    Alcotest.failf "only %d of %d cases parsed" stats.successes stats.checked

let () =
  Alcotest.run "Differential"
    [
      ( "random grammars",
        [
          Alcotest.test_case "model, parse and parse_source agree" `Quick
            test_random_grammars;
        ]
      );
    ]
