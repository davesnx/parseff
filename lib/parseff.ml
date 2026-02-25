type span = {
  buf : string;
  off : int;
  len : int;
}

let[@inline always] span_to_string s =
  if s.len = 0 then ""
  else String.sub s.buf s.off s.len

type _ Effect.t +=
  | Consume : string -> string Effect.t
  | Satisfy : (char -> bool) * string -> char Effect.t
  | Match_re : Re.re -> string Effect.t
  | Choose : (unit -> 'a) * (unit -> 'a) -> 'a Effect.t
  | Fail : string -> 'a Effect.t
  | Look_ahead : (unit -> 'a) -> 'a Effect.t
  | End_of_input : unit Effect.t
  | Take_while : (char -> bool) -> string Effect.t
  | Skip_while : (char -> bool) -> unit Effect.t
  | Greedy_many : (unit -> 'a) -> 'a list Effect.t
  | Skip_while_then_char : (char -> bool) * char -> unit Effect.t
  | Fused_sep_take : (char -> bool) * char * (char -> bool) -> string Effect.t
  | Sep_by_take : (char -> bool) * char * (char -> bool) -> string list Effect.t
  | Take_while_span : (char -> bool) -> span Effect.t
  | Sep_by_take_span : (char -> bool) * char * (char -> bool) -> span list Effect.t

type 'a result =
  | Ok of 'a * int
  | Error of { pos : int; expected : string }

exception Parse_error of int * string

type state = {
  input : string;
  mutable pos : int;
}

let run input (parser : unit -> 'a) : 'a result =
  let st = { input; pos = 0 } in
  let input_len = String.length input in
  let rec go : 'b. (unit -> 'b) -> 'b result = fun p ->
    Effect.Deep.match_with p ()
      {
        retc = (fun v -> Ok (v, st.pos));
        exnc =
          (function
          | Parse_error (pos, msg) -> Error { pos; expected = msg }
          | e -> raise e);
        effc =
          (fun (type a) (eff : a Effect.t) ->
            match eff with
            | Consume s ->
                Some
                  (fun (k : (a, _) Effect.Deep.continuation) ->
                    let len = String.length s in
                    if st.pos + len <= input_len then begin
                      let inp = st.input in
                      let pos = st.pos in
                      let rec check i =
                        if i >= len then true
                        else if String.unsafe_get inp (pos + i) = String.unsafe_get s i then
                          check (i + 1)
                        else false
                      in
                      if check 0 then begin
                        st.pos <- pos + len;
                        Effect.Deep.continue k s
                      end else
                        Effect.Deep.discontinue k
                          (Parse_error (pos, Printf.sprintf "expected %S" s))
                    end else
                      Effect.Deep.discontinue k
                        (Parse_error (st.pos, Printf.sprintf "expected %S (EOF)" s)))
            | Satisfy (pred, label) ->
                Some
                  (fun k ->
                    if st.pos < input_len then
                      let c = String.unsafe_get st.input st.pos in
                      if pred c then begin
                        st.pos <- st.pos + 1;
                        Effect.Deep.continue k c
                      end else
                        Effect.Deep.discontinue k
                          (Parse_error (st.pos, Printf.sprintf "expected %s" label))
                    else
                      Effect.Deep.discontinue k
                        (Parse_error (st.pos, Printf.sprintf "expected %s (EOF)" label)))
            | Take_while pred ->
                Some
                  (fun k ->
                    let start = st.pos in
                    let inp = st.input in
                    let pos = ref start in
                    while !pos < input_len && pred (String.unsafe_get inp !pos) do
                      pos := !pos + 1
                    done;
                    let end_pos = !pos in
                    st.pos <- end_pos;
                    if end_pos > start then
                      Effect.Deep.continue k (String.sub inp start (end_pos - start))
                    else
                      Effect.Deep.continue k "")
            | Skip_while pred ->
                Some
                  (fun k ->
                    let inp = st.input in
                    let pos = ref st.pos in
                    while !pos < input_len && pred (String.unsafe_get inp !pos) do
                      pos := !pos + 1
                    done;
                    st.pos <- !pos;
                    Effect.Deep.continue k ())
            | Skip_while_then_char (pred, c) ->
                Some
                  (fun k ->
                    let inp = st.input in
                    let pos = ref st.pos in
                    while !pos < input_len && pred (String.unsafe_get inp !pos) do
                      pos := !pos + 1
                    done;
                    if !pos < input_len && String.unsafe_get inp !pos = c then begin
                      st.pos <- !pos + 1;
                      Effect.Deep.continue k ()
                    end else begin
                      st.pos <- !pos;
                      Effect.Deep.discontinue k
                        (Parse_error (!pos, Printf.sprintf "expected %C" c))
                    end)
            | Fused_sep_take (ws_pred, sep_char, take_pred) ->
                Some
                  (fun k ->
                    let inp = st.input in
                    (* Phase 1: skip whitespace *)
                    let pos = ref st.pos in
                    while !pos < input_len && ws_pred (String.unsafe_get inp !pos) do
                      pos := !pos + 1
                    done;
                    (* Phase 2: match separator *)
                    if !pos < input_len && String.unsafe_get inp !pos = sep_char then begin
                      pos := !pos + 1;
                      (* Phase 3: skip whitespace *)
                      while !pos < input_len && ws_pred (String.unsafe_get inp !pos) do
                        pos := !pos + 1
                      done;
                      (* Phase 4: take_while1 *)
                      let start = !pos in
                      while !pos < input_len && take_pred (String.unsafe_get inp !pos) do
                        pos := !pos + 1
                      done;
                      if !pos > start then begin
                        st.pos <- !pos;
                        Effect.Deep.continue k (String.sub inp start (!pos - start))
                      end else begin
                        st.pos <- !pos;
                        Effect.Deep.discontinue k (Parse_error (!pos, "expected value"))
                      end
                    end else begin
                      st.pos <- !pos;
                      Effect.Deep.discontinue k
                        (Parse_error (!pos, Printf.sprintf "expected %C" sep_char))
                    end)
            | Sep_by_take (ws_pred, sep_char, take_pred) ->
                Some
                  (fun k ->
                    let inp = st.input in
                    let pos = ref st.pos in
                    (* Try first element *)
                    let start = !pos in
                    while !pos < input_len && take_pred (String.unsafe_get inp !pos) do
                      pos := !pos + 1
                    done;
                    if !pos <= start then begin
                      (* No first element - return empty list *)
                      st.pos <- !pos;
                      Effect.Deep.continue k []
                    end else begin
                      let acc = ref [String.sub inp start (!pos - start)] in
                      let continue_loop = ref true in
                      while !continue_loop do
                        (* Skip ws *)
                        let sep_pos = ref !pos in
                        while !sep_pos < input_len && ws_pred (String.unsafe_get inp !sep_pos) do
                          sep_pos := !sep_pos + 1
                        done;
                        (* Check separator *)
                        if !sep_pos < input_len && String.unsafe_get inp !sep_pos = sep_char then begin
                          sep_pos := !sep_pos + 1;
                          (* Skip ws *)
                          while !sep_pos < input_len && ws_pred (String.unsafe_get inp !sep_pos) do
                            sep_pos := !sep_pos + 1
                          done;
                          (* Take while *)
                          let elem_start = !sep_pos in
                          while !sep_pos < input_len && take_pred (String.unsafe_get inp !sep_pos) do
                            sep_pos := !sep_pos + 1
                          done;
                          if !sep_pos > elem_start then begin
                            acc := String.sub inp elem_start (!sep_pos - elem_start) :: !acc;
                            pos := !sep_pos
                          end else begin
                            continue_loop := false
                          end
                        end else begin
                          continue_loop := false
                        end
                      done;
                      st.pos <- !pos;
                      Effect.Deep.continue k (List.rev !acc)
                    end)
            | Take_while_span pred ->
                Some
                  (fun k ->
                    let start = st.pos in
                    let inp = st.input in
                    let pos = ref start in
                    while !pos < input_len && pred (String.unsafe_get inp !pos) do
                      pos := !pos + 1
                    done;
                    st.pos <- !pos;
                    Effect.Deep.continue k { buf = inp; off = start; len = !pos - start })
            | Sep_by_take_span (ws_pred, sep_char, take_pred) ->
                Some
                  (fun k ->
                    let inp = st.input in
                    let pos = ref st.pos in
                    (* Try first element *)
                    let start = !pos in
                    while !pos < input_len && take_pred (String.unsafe_get inp !pos) do
                      pos := !pos + 1
                    done;
                    if !pos <= start then begin
                      st.pos <- !pos;
                      Effect.Deep.continue k []
                    end else begin
                      let acc = ref [{ buf = inp; off = start; len = !pos - start }] in
                      let continue_loop = ref true in
                      while !continue_loop do
                        let sep_pos = ref !pos in
                        while !sep_pos < input_len && ws_pred (String.unsafe_get inp !sep_pos) do
                          sep_pos := !sep_pos + 1
                        done;
                        if !sep_pos < input_len && String.unsafe_get inp !sep_pos = sep_char then begin
                          sep_pos := !sep_pos + 1;
                          while !sep_pos < input_len && ws_pred (String.unsafe_get inp !sep_pos) do
                            sep_pos := !sep_pos + 1
                          done;
                          let elem_start = !sep_pos in
                          while !sep_pos < input_len && take_pred (String.unsafe_get inp !sep_pos) do
                            sep_pos := !sep_pos + 1
                          done;
                          if !sep_pos > elem_start then begin
                            acc := { buf = inp; off = elem_start; len = !sep_pos - elem_start } :: !acc;
                            pos := !sep_pos
                          end else
                            continue_loop := false
                        end else
                          continue_loop := false
                      done;
                      st.pos <- !pos;
                      Effect.Deep.continue k (List.rev !acc)
                    end)
            | Match_re re ->
                Some
                  (fun k ->
                    (try
                       let groups = Re.exec ~pos:st.pos re st.input in
                       let match_start = Re.Group.start groups 0 in
                       if match_start <> st.pos then
                         Effect.Deep.discontinue k
                           (Parse_error (st.pos, "regex match failed"))
                       else
                         let matched = Re.Group.get groups 0 in
                         let match_end = Re.Group.stop groups 0 in
                         st.pos <- match_end;
                         Effect.Deep.continue k matched
                     with Not_found ->
                       Effect.Deep.discontinue k
                         (Parse_error (st.pos, "regex match failed"))))
            (* Greedy_many, Choose, and Look_ahead carry existential type
               variables in their GADT constructors that can't unify with the
               continuation's expected type inside effc. Obj.magic erases the
               type mismatch — safe because go/go_shallow return the same
               runtime representation regardless of the type parameter. *)
            | Greedy_many p ->
                Some
                  (fun k ->
                    let acc = ref [] in
                    let saved = ref st.pos in
                    let failed = ref false in
                    while not !failed do
                      saved := st.pos;
                      match go (Obj.magic p) with
                      | Ok (v, pos') ->
                          st.pos <- pos';
                          acc := (Obj.magic v) :: !acc
                      | Error _ ->
                          st.pos <- !saved;
                          failed := true
                    done;
                    Effect.Deep.continue k (Obj.magic (List.rev !acc)))
            | Choose (left, right) ->
                Some
                  (fun k ->
                    let saved = st.pos in
                    match go (Obj.magic left) with
                    | Ok (v, pos') ->
                        st.pos <- pos';
                        Effect.Deep.continue k (Obj.magic v)
                    | Error _ ->
                        st.pos <- saved;
                        (match go (Obj.magic right) with
                        | Ok (v, pos') ->
                            st.pos <- pos';
                            Effect.Deep.continue k (Obj.magic v)
                        | Error e ->
                            Effect.Deep.discontinue k (Parse_error (e.pos, e.expected))))
            | Fail msg ->
                Some
                  (fun k -> Effect.Deep.discontinue k (Parse_error (st.pos, msg)))
            | Look_ahead p ->
                Some
                  (fun k ->
                    let saved = st.pos in
                    match go (Obj.magic p) with
                    | Ok (v, _) ->
                        st.pos <- saved;
                        Effect.Deep.continue k (Obj.magic v)
                    | Error e ->
                        st.pos <- saved;
                        Effect.Deep.discontinue k (Parse_error (e.pos, e.expected)))
            | End_of_input ->
                Some
                  (fun k ->
                    if st.pos = input_len then
                      Effect.Deep.continue k ()
                    else
                      Effect.Deep.discontinue k
                        (Parse_error (st.pos, "expected end of input")))
            | _ -> None);
      }
  in
  go parser

let run_shallow input (parser : unit -> 'a) : 'a result =
  let st = { input; pos = 0 } in
  let input_len = String.length input in
  (* Polymorphic recursion: go_shallow is called for sub-parsers of different
     return types (Greedy_many, Choose, Look_ahead), so drive/drive_exn must
     be polymorphic in both the continuation value type and fiber result type. *)
  let rec go_shallow : type r. (unit -> r) -> r result = fun p ->
    let fiber = Effect.Shallow.fiber p in
    drive fiber ()
  and drive : type a r. (a, r) Effect.Shallow.continuation -> a -> r result =
    fun k v ->
    Effect.Shallow.continue_with k v
      {
        retc = (fun v -> Ok (v, st.pos));
        exnc =
          (function
          | Parse_error (pos, msg) -> Error { pos; expected = msg }
          | e -> raise e);
        effc =
          (fun (type c) (eff : c Effect.t) ->
            match eff with
            | Consume s ->
                Some
                  (fun (k : (c, _) Effect.Shallow.continuation) ->
                    let len = String.length s in
                    if st.pos + len <= input_len then begin
                      let inp = st.input in
                      let pos = st.pos in
                      let rec check i =
                        if i >= len then true
                        else if String.unsafe_get inp (pos + i) = String.unsafe_get s i then
                          check (i + 1)
                        else false
                      in
                      if check 0 then begin
                        st.pos <- pos + len;
                        drive k s
                      end else
                        drive_exn k (Parse_error (pos, Printf.sprintf "expected %S" s))
                    end else
                      drive_exn k (Parse_error (st.pos, Printf.sprintf "expected %S (EOF)" s)))
            | Satisfy (pred, label) ->
                Some
                  (fun k ->
                    if st.pos < input_len then
                      let c = String.unsafe_get st.input st.pos in
                      if pred c then begin
                        st.pos <- st.pos + 1;
                        drive k c
                      end else
                        drive_exn k (Parse_error (st.pos, Printf.sprintf "expected %s" label))
                    else
                      drive_exn k (Parse_error (st.pos, Printf.sprintf "expected %s (EOF)" label)))
            | Take_while pred ->
                Some
                  (fun k ->
                    let start = st.pos in
                    let inp = st.input in
                    let pos = ref start in
                    while !pos < input_len && pred (String.unsafe_get inp !pos) do
                      pos := !pos + 1
                    done;
                    let end_pos = !pos in
                    st.pos <- end_pos;
                    if end_pos > start then
                      drive k (String.sub inp start (end_pos - start))
                    else
                      drive k "")
            | Take_while_span pred ->
                Some
                  (fun k ->
                    let start = st.pos in
                    let inp = st.input in
                    let pos = ref start in
                    while !pos < input_len && pred (String.unsafe_get inp !pos) do
                      pos := !pos + 1
                    done;
                    st.pos <- !pos;
                    drive k { buf = inp; off = start; len = !pos - start })
            | Skip_while pred ->
                Some
                  (fun k ->
                    let inp = st.input in
                    let pos = ref st.pos in
                    while !pos < input_len && pred (String.unsafe_get inp !pos) do
                      pos := !pos + 1
                    done;
                    st.pos <- !pos;
                    drive k ())
            | Skip_while_then_char (pred, c) ->
                Some
                  (fun k ->
                    let inp = st.input in
                    let pos = ref st.pos in
                    while !pos < input_len && pred (String.unsafe_get inp !pos) do
                      pos := !pos + 1
                    done;
                    if !pos < input_len && String.unsafe_get inp !pos = c then begin
                      st.pos <- !pos + 1;
                      drive k ()
                    end else begin
                      st.pos <- !pos;
                      drive_exn k (Parse_error (!pos, Printf.sprintf "expected %C" c))
                    end)
            | Fused_sep_take (ws_pred, sep_char, take_pred) ->
                Some
                  (fun k ->
                    let inp = st.input in
                    let pos = ref st.pos in
                    while !pos < input_len && ws_pred (String.unsafe_get inp !pos) do
                      pos := !pos + 1
                    done;
                    if !pos < input_len && String.unsafe_get inp !pos = sep_char then begin
                      pos := !pos + 1;
                      while !pos < input_len && ws_pred (String.unsafe_get inp !pos) do
                        pos := !pos + 1
                      done;
                      let start = !pos in
                      while !pos < input_len && take_pred (String.unsafe_get inp !pos) do
                        pos := !pos + 1
                      done;
                      if !pos > start then begin
                        st.pos <- !pos;
                        drive k (String.sub inp start (!pos - start))
                      end else begin
                        st.pos <- !pos;
                        drive_exn k (Parse_error (!pos, "expected value"))
                      end
                    end else begin
                      st.pos <- !pos;
                      drive_exn k (Parse_error (!pos, Printf.sprintf "expected %C" sep_char))
                    end)
            | Sep_by_take (ws_pred, sep_char, take_pred) ->
                Some
                  (fun k ->
                    let inp = st.input in
                    let pos = ref st.pos in
                    let start = !pos in
                    while !pos < input_len && take_pred (String.unsafe_get inp !pos) do
                      pos := !pos + 1
                    done;
                    if !pos <= start then begin
                      st.pos <- !pos;
                      drive k []
                    end else begin
                      let acc = ref [String.sub inp start (!pos - start)] in
                      let continue_loop = ref true in
                      while !continue_loop do
                        let sep_pos = ref !pos in
                        while !sep_pos < input_len && ws_pred (String.unsafe_get inp !sep_pos) do
                          sep_pos := !sep_pos + 1
                        done;
                        if !sep_pos < input_len && String.unsafe_get inp !sep_pos = sep_char then begin
                          sep_pos := !sep_pos + 1;
                          while !sep_pos < input_len && ws_pred (String.unsafe_get inp !sep_pos) do
                            sep_pos := !sep_pos + 1
                          done;
                          let elem_start = !sep_pos in
                          while !sep_pos < input_len && take_pred (String.unsafe_get inp !sep_pos) do
                            sep_pos := !sep_pos + 1
                          done;
                          if !sep_pos > elem_start then begin
                            acc := String.sub inp elem_start (!sep_pos - elem_start) :: !acc;
                            pos := !sep_pos
                          end else
                            continue_loop := false
                        end else
                          continue_loop := false
                      done;
                      st.pos <- !pos;
                      drive k (List.rev !acc)
                    end)
            | Sep_by_take_span (ws_pred, sep_char, take_pred) ->
                Some
                  (fun k ->
                    let inp = st.input in
                    let pos = ref st.pos in
                    let start = !pos in
                    while !pos < input_len && take_pred (String.unsafe_get inp !pos) do
                      pos := !pos + 1
                    done;
                    if !pos <= start then begin
                      st.pos <- !pos;
                      drive k []
                    end else begin
                      let acc = ref [{ buf = inp; off = start; len = !pos - start }] in
                      let continue_loop = ref true in
                      while !continue_loop do
                        let sep_pos = ref !pos in
                        while !sep_pos < input_len && ws_pred (String.unsafe_get inp !sep_pos) do
                          sep_pos := !sep_pos + 1
                        done;
                        if !sep_pos < input_len && String.unsafe_get inp !sep_pos = sep_char then begin
                          sep_pos := !sep_pos + 1;
                          while !sep_pos < input_len && ws_pred (String.unsafe_get inp !sep_pos) do
                            sep_pos := !sep_pos + 1
                          done;
                          let elem_start = !sep_pos in
                          while !sep_pos < input_len && take_pred (String.unsafe_get inp !sep_pos) do
                            sep_pos := !sep_pos + 1
                          done;
                          if !sep_pos > elem_start then begin
                            acc := { buf = inp; off = elem_start; len = !sep_pos - elem_start } :: !acc;
                            pos := !sep_pos
                          end else
                            continue_loop := false
                        end else
                          continue_loop := false
                      done;
                      st.pos <- !pos;
                      drive k (List.rev !acc)
                    end)
            | Match_re re ->
                Some
                  (fun k ->
                    (try
                       let groups = Re.exec ~pos:st.pos re st.input in
                       let match_start = Re.Group.start groups 0 in
                       if match_start <> st.pos then
                         drive_exn k (Parse_error (st.pos, "regex match failed"))
                       else
                         let matched = Re.Group.get groups 0 in
                         let match_end = Re.Group.stop groups 0 in
                         st.pos <- match_end;
                         drive k matched
                     with Not_found ->
                       drive_exn k (Parse_error (st.pos, "regex match failed"))))
            | Greedy_many p ->
                Some
                  (fun k ->
                    let acc = ref [] in
                    let saved = ref st.pos in
                    let failed = ref false in
                    while not !failed do
                      saved := st.pos;
                      match go_shallow (Obj.magic p) with
                      | Ok (v, pos') ->
                          st.pos <- pos';
                          acc := (Obj.magic v) :: !acc
                      | Error _ ->
                          st.pos <- !saved;
                          failed := true
                    done;
                    drive k (Obj.magic (List.rev !acc)))
            | Choose (left, right) ->
                Some
                  (fun k ->
                    let saved = st.pos in
                    match go_shallow (Obj.magic left) with
                    | Ok (v, pos') ->
                        st.pos <- pos';
                        drive k (Obj.magic v)
                    | Error _ ->
                        st.pos <- saved;
                        (match go_shallow (Obj.magic right) with
                        | Ok (v, pos') ->
                            st.pos <- pos';
                            drive k (Obj.magic v)
                        | Error e ->
                            drive_exn k (Parse_error (e.pos, e.expected))))
            | Fail msg ->
                Some
                  (fun k -> drive_exn k (Parse_error (st.pos, msg)))
            | Look_ahead p ->
                Some
                  (fun k ->
                    let saved = st.pos in
                    match go_shallow (Obj.magic p) with
                    | Ok (v, _) ->
                        st.pos <- saved;
                        drive k (Obj.magic v)
                    | Error e ->
                        st.pos <- saved;
                        drive_exn k (Parse_error (e.pos, e.expected)))
            | End_of_input ->
                Some
                  (fun k ->
                    if st.pos = input_len then
                      drive k ()
                    else
                      drive_exn k (Parse_error (st.pos, "expected end of input")))
            | _ -> None);
      }
  and drive_exn : type a r. (a, r) Effect.Shallow.continuation -> exn -> r result =
    fun k e ->
    Effect.Shallow.discontinue_with k e
      {
        retc = (fun v -> Ok (v, st.pos));
        exnc =
          (function
          | Parse_error (pos, msg) -> Error { pos; expected = msg }
          | ex -> raise ex);
        effc =
          (fun (type c) (eff : c Effect.t) ->
            match eff with
            | Fail msg ->
                Some (fun (k : (c, _) Effect.Shallow.continuation) ->
                  drive_exn k (Parse_error (st.pos, msg)))
            | _ ->
                Some (fun (k : (c, _) Effect.Shallow.continuation) ->
                  drive_exn k e));
      }
  in
  go_shallow parser

let[@inline] consume s = Effect.perform (Consume s)
let[@inline] satisfy pred label = Effect.perform (Satisfy (pred, label))
let[@inline] char c = satisfy (Char.equal c) (String.make 1 c)
let[@inline] match_re re = Effect.perform (Match_re re)
let[@inline] take_while pred = Effect.perform (Take_while pred)

let[@inline] take_while1 pred label =
  let s = take_while pred in
  if String.length s = 0 then Effect.perform (Fail label)
  else s

let[@inline] skip_while pred = Effect.perform (Skip_while pred)
let[@inline] skip_while_then_char pred c = Effect.perform (Skip_while_then_char (pred, c))

let[@inline] sep_by_take ws_pred sep_char take_pred =
  Effect.perform (Sep_by_take (ws_pred, sep_char, take_pred))

let[@inline] take_while_span pred = Effect.perform (Take_while_span pred)

let[@inline] sep_by_take_span ws_pred sep_char take_pred =
  Effect.perform (Sep_by_take_span (ws_pred, sep_char, take_pred))

let[@inline] fused_sep_take ws_pred sep_char take_pred =
  Effect.perform (Fused_sep_take (ws_pred, sep_char, take_pred))

let[@inline] fail msg = Effect.perform (Fail msg)
let[@inline] end_of_input () = Effect.perform End_of_input
let[@inline] ( <|> ) p q () = Effect.perform (Choose (p, q))
let[@inline] look_ahead p = Effect.perform (Look_ahead p)
let string = consume

let[@inline] many (p : unit -> 'a) () : 'a list =
  Effect.perform (Greedy_many p)

let[@inline] many1 (p : unit -> 'a) () : 'a list =
  let first = p () in
  let rest = many p () in
  first :: rest

let sep_by (p : unit -> 'a) (sep : unit -> 'b) () : 'a list =
  ((fun () ->
     let first = p () in
     let rest =
       many
         (fun () ->
           let _ = sep () in
           p ())
         ()
     in
     first :: rest)
  <|> fun () -> [])
    ()

let sep_by1 (p : unit -> 'a) (sep : unit -> 'b) () : 'a list =
  let first = p () in
  let rest =
    many
      (fun () ->
        let _ = sep () in
        p ())
      ()
  in
  first :: rest

let optional (p : unit -> 'a) () : 'a option =
  ((fun () -> Some (p ())) <|> fun () -> None) ()

let count n (p : unit -> 'a) () : 'a list =
  let rec loop acc i =
    if i <= 0 then List.rev acc else loop (p () :: acc) (i - 1)
  in
  loop [] n

let digit () =
  let c = satisfy (fun c -> c >= '0' && c <= '9') "digit" in
  Char.code c - Char.code '0'

let letter () = satisfy (fun c -> (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')) "letter"
let[@inline always] is_whitespace c = c = ' ' || c = '\t' || c = '\n' || c = '\r'
let[@inline] whitespace () = take_while is_whitespace
let[@inline] whitespace1 () = take_while1 is_whitespace "whitespace"
let[@inline] skip_whitespace () = skip_while is_whitespace

let alphanum () =
  satisfy (fun c -> (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9')) "alphanumeric"

let any_char () = satisfy (fun _ -> true) "any character"
