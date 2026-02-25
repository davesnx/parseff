(** Parser combinators with OCaml 5 algebraic effects *)

(** {1 Effect Types} *)

(** Parser effects - the protocol between parsers and handlers *)
type _ Effect.t +=
  | Consume : string -> string Effect.t
      (** [Consume s] matches the exact literal string [s] *)
  | Satisfy : (char -> bool) * string -> char Effect.t
      (** [Satisfy (pred, label)] matches a character satisfying [pred] *)
  | Match_re : Re.re -> string Effect.t
      (** [Match_re re] matches a regular expression (must be compiled with [Re.compile]) *)
  | Choose : (unit -> 'a) * (unit -> 'a) -> 'a Effect.t
      (** [Choose (left, right)] tries [left], backtracks and tries [right] on failure *)
  | Fail : string -> 'a Effect.t  (** [Fail msg] aborts parsing with an error message *)
  | Look_ahead : (unit -> 'a) -> 'a Effect.t
      (** [Look_ahead p] runs parser [p] without consuming input *)
  | End_of_input : unit Effect.t
      (** [End_of_input] succeeds only if no input remains *)
  | Take_while : (char -> bool) -> string Effect.t
      (** [Take_while pred] consumes characters while [pred] holds, returns matched string *)
  | Skip_while : (char -> bool) -> unit Effect.t
      (** [Skip_while pred] skips characters while [pred] holds *)
  | Greedy_many : (unit -> 'a) -> 'a list Effect.t
      (** [Greedy_many p] applies [p] zero or more times greedily *)
  | Skip_while_then_char : (char -> bool) * char -> unit Effect.t
      (** [Skip_while_then_char (pred, c)] skips chars matching [pred], then matches [c] *)
  | Fused_sep_take : (char -> bool) * char * (char -> bool) -> string Effect.t
      (** [Fused_sep_take (ws_pred, sep_char, take_pred)]
          skip ws, match sep, skip ws, take_while1 - all in one effect *)
  | Sep_by_take : (char -> bool) * char * (char -> bool) -> string list Effect.t
      (** [Sep_by_take (ws_pred, sep_char, take_pred)]
          Parses zero or more separated values entirely in the handler.
          Equivalent to: first_take :: many (ws sep ws take) but in a tight loop. *)

(** {1 Result Types} *)

(** Parse result *)
type 'a result =
  | Ok of 'a * int  (** Success: parsed value and final position *)
  | Error of { pos : int; expected : string }  (** Failure: position and expected token *)

(** Parse error exception *)
exception Parse_error of int * string

(** {1 Internal State} *)

(** Internal parser state *)
type state = {
  input : string;
  mutable pos : int;
}

(** {1 Runner/Handler} *)

(** Run a parser on input *)
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
            | Match_re re ->
                Some
                  (fun k ->
                    (try
                       let groups = Re.exec ~pos:st.pos re st.input in
                       let matched = Re.Group.get groups 0 in
                       let match_end = Re.Group.stop groups 0 in
                       st.pos <- match_end;
                       Effect.Deep.continue k matched
                     with Not_found ->
                       Effect.Deep.discontinue k
                         (Parse_error (st.pos, "regex match failed"))))
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

(** {1 Primitive Combinators} *)

(** [consume s] matches the exact literal string [s] *)
let consume s = Effect.perform (Consume s)

(** [satisfy pred label] matches a character satisfying predicate [pred] *)
let satisfy pred label = Effect.perform (Satisfy (pred, label))

(** [char c] matches the exact character [c] *)
let char c = satisfy (Char.equal c) (String.make 1 c)

(** [match_re re] matches a regular expression *)
let match_re re = Effect.perform (Match_re re)

(** [take_while pred] consumes characters while [pred] holds, returns matched string.
    Always succeeds (returns "" if no characters match). *)
let take_while pred = Effect.perform (Take_while pred)

(** [take_while1 pred label] consumes one or more characters while [pred] holds.
    Fails with [label] if no characters match. *)
let take_while1 pred label =
  let s = take_while pred in
  if String.length s = 0 then Effect.perform (Fail label)
  else s

(** [skip_while pred] skips characters while [pred] holds.
    Always succeeds (skips zero characters if predicate doesn't match). *)
let skip_while pred = Effect.perform (Skip_while pred)

(** [skip_while_then_char pred c] skips characters matching [pred] then matches [c].
    Fused operation - more efficient than [skip_while pred; char c]. *)
let skip_while_then_char pred c = Effect.perform (Skip_while_then_char (pred, c))

(** [sep_by_take ws_pred sep_char take_pred] parses a separated list of values
    entirely in the handler. Returns list of matched strings.
    Equivalent to sep_by (take_while1 take_pred) (ws *> char sep *> ws) but
    handled entirely in a single effect dispatch with zero intermediate allocations. *)
let sep_by_take ws_pred sep_char take_pred =
  Effect.perform (Sep_by_take (ws_pred, sep_char, take_pred))

(** [fused_sep_take ws_pred sep_char take_pred] performs:
    skip whitespace, match separator, skip whitespace, take_while1
    all in a single effect dispatch. Returns the taken string. *)
let fused_sep_take ws_pred sep_char take_pred =
  Effect.perform (Fused_sep_take (ws_pred, sep_char, take_pred))

(** [fail msg] aborts parsing with an error message *)
let fail msg = Effect.perform (Fail msg)

(** [end_of_input ()] succeeds only if no input remains *)
let end_of_input () = Effect.perform End_of_input

(** [(<|>)] is the alternation combinator - tries left, then right on failure *)
let ( <|> ) p q () = Effect.perform (Choose (p, q))

(** [look_ahead p] runs parser [p] without consuming input *)
let look_ahead p = Effect.perform (Look_ahead p)

(** [string s] is an alias for [consume s] *)
let string = consume

(** {1 Repetition Combinators} *)

(** [many p] applies parser [p] zero or more times *)
let many (p : unit -> 'a) () : 'a list =
  Effect.perform (Greedy_many p)

(** [many1 p] applies parser [p] one or more times *)
let many1 (p : unit -> 'a) () : 'a list =
  let first = p () in
  let rest = many p () in
  first :: rest

(** [sep_by p sep] parses zero or more occurrences of [p] separated by [sep] *)
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

(** [sep_by1 p sep] parses one or more occurrences of [p] separated by [sep] *)
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

(** [optional p] optionally applies parser [p] *)
let optional (p : unit -> 'a) () : 'a option =
  ((fun () -> Some (p ())) <|> fun () -> None) ()

(** [count n p] applies parser [p] exactly [n] times *)
let count n (p : unit -> 'a) () : 'a list =
  let rec loop acc i =
    if i <= 0 then List.rev acc else loop (p () :: acc) (i - 1)
  in
  loop [] n

(** {1 Convenience Combinators} *)

(** [digit ()] parses a decimal digit and returns its integer value *)
let digit () =
  let c = satisfy (fun c -> c >= '0' && c <= '9') "digit" in
  Char.code c - Char.code '0'

(** [letter ()] parses an ASCII letter *)
let letter () = satisfy (fun c -> (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')) "letter"

let is_whitespace c = c = ' ' || c = '\t' || c = '\n' || c = '\r'

(** [whitespace ()] parses zero or more whitespace characters *)
let whitespace () = take_while is_whitespace

(** [whitespace1 ()] parses one or more whitespace characters *)
let whitespace1 () = take_while1 is_whitespace "whitespace"

(** [skip_whitespace ()] skips zero or more whitespace characters (returns unit) *)
let skip_whitespace () = skip_while is_whitespace

(** [alphanum ()] parses an alphanumeric character *)
let alphanum () =
  satisfy (fun c -> (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9')) "alphanumeric"

(** [any_char ()] parses any character *)
let any_char () = satisfy (fun _ -> true) "any character"
