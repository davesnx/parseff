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
  (* Recursive runner for nested handler calls (supports backtracking) *)
  let rec go (p : unit -> 'a) : 'a result =
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
                    if st.pos + len <= input_len then
                      let here = String.sub st.input st.pos len in
                      if here = s then (
                        st.pos <- st.pos + len;
                        Effect.Deep.continue k s)
                      else
                        Effect.Deep.discontinue k
                          (Parse_error (st.pos, Printf.sprintf "expected %S" s))
                    else
                      Effect.Deep.discontinue k
                        (Parse_error (st.pos, Printf.sprintf "expected %S (EOF)" s)))
            | Satisfy (pred, label) ->
                Some
                  (fun k ->
                    if st.pos < input_len then
                      let c = String.get st.input st.pos in
                      if pred c then (
                        st.pos <- st.pos + 1;
                        Effect.Deep.continue k c)
                      else
                        Effect.Deep.discontinue k
                          (Parse_error (st.pos, Printf.sprintf "expected %s" label))
                    else
                      Effect.Deep.discontinue k
                        (Parse_error (st.pos, Printf.sprintf "expected %s (EOF)" label)))
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
            | Choose (left, right) ->
                Some
                  (fun k ->
                    let saved = st.pos in
                    match go (Obj.magic left) with
                    | Ok (v, pos') ->
                        st.pos <- pos';
                        Effect.Deep.continue k (Obj.magic v)
                    | Error _ ->
                        (* Backtrack - restore cursor and try right branch *)
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
  let rec loop acc =
    ((fun () ->
        let v = p () in
        loop (v :: acc))
    <|> fun () -> List.rev acc)
      ()
  in
  loop []

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

(** [whitespace ()] parses zero or more whitespace characters *)
let whitespace () =
  let re = Re.compile (Re.Posix.re "[ \t\n\r]*") in
  match_re re

(** [whitespace1 ()] parses one or more whitespace characters *)
let whitespace1 () =
  let re = Re.compile (Re.Posix.re "[ \t\n\r]+") in
  match_re re

(** [alphanum ()] parses an alphanumeric character *)
let alphanum () =
  satisfy (fun c -> (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9')) "alphanumeric"

(** [any_char ()] parses any character *)
let any_char () = satisfy (fun _ -> true) "any character"
