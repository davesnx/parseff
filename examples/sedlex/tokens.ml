(* Glue between a lexer and parseff. The lexer runs on demand, and its tokens
   stay in an array so that a rewind finds them again without lexing twice.
   The parser position stays a byte offset: it is always the start of the
   current token, or a position before the first token. *)

open Parseff

type 'tok token = { value : 'tok; start : int; stop : int }

type 'tok t = {
  mutable tokens : 'tok token array;
  mutable len : int;
  mutable next : unit -> 'tok token;
  eof : 'tok;
  show : 'tok -> string;
  move : int -> unit;
  mutable last : int;
}

(* [next ()] returns the next token. After the input, it must return [eof]
   with [start = stop = length]. [move] goes to a token start; [skip_to] does
   not read input, so a lexer that reads a stream ahead of parseff needs a
   [move] that does. *)
let create ?(move = skip_to) ~show ~eof next =
  { tokens = [||]; len = 0; next; eof; show; move; last = 0 }

let lexed t = t.len

let push t tok =
  if t.len = Array.length t.tokens then begin
    let tokens = Array.make (max 16 (2 * t.len)) tok in
    Array.blit t.tokens 0 tokens 0 t.len;
    t.tokens <- tokens
  end;
  t.tokens.(t.len) <- tok;
  t.len <- t.len + 1

(* Lexes until a token starts at or after [pos], or the input ends. *)
let fill t pos =
  while
    t.len = 0
    ||
    let tok = t.tokens.(t.len - 1) in
    tok.start < pos && tok.value <> t.eof
  do
    push t (t.next ())
  done

(* The index of the first token that starts at or after [pos]. Positions only
   change by [skip_to] and backtracking, so the last answer is usually still
   right and is checked first. *)
let index_at t pos =
  fill t pos;
  let i = t.last in
  if
    i < t.len && t.tokens.(i).start >= pos
    && (i = 0 || t.tokens.(i - 1).start < pos)
  then
    i
  else
    let rec search lo hi =
      if lo >= hi then
        lo
      else
        let mid = (lo + hi) / 2 in
        if t.tokens.(mid).start >= pos then
          search lo mid
        else
          search (mid + 1) hi
    in
    let i = search 0 (t.len - 1) in
    t.last <- i;
    i

let peek t =
  let tok = t.tokens.(index_at t (position ())) in
  t.move tok.start;
  tok

(* To the start of the next token, so a combinator that starts here starts
   where the next token check fails. *)
let advance t =
  let i = index_at t (position ()) in
  fill t t.tokens.(i).stop;
  let i = min (i + 1) (t.len - 1) in
  t.last <- i;
  t.move t.tokens.(i).start

(* Reads the token, and one byte after it, before failing: on a stream, the
   error can then quote the token or say that the input ended. *)
let unexpected t tok label =
  let _ =
    look_ahead (fun () ->
        t.move tok.stop;
        peek_char ()
    )
  in
  expected_token ~upto:tok.stop label

let eat t value =
  let tok = peek t in
  if tok.value = value then
    advance t
  else
    unexpected t tok (t.show value)

(* [optional] keeps the failure, so a later failure at the same token also
   lists [value]. *)
let accept t value =
  match optional (fun () -> eat t value) with Some () -> true | None -> false

let select t label f =
  let tok = peek t in
  match f tok.value with
  | Some v ->
      advance t;
      v
  | None ->
      unexpected t tok label

(* Drops the tokens from the position on and lexes the rest with [next], for a
   token that depends on parser state. Tokens before the position stay. *)
let relex t next =
  let i = index_at t (position ()) in
  t.len <- i;
  t.last <- 0;
  t.next <- next

(* The end of the token before the position, so ranges leave out the
   whitespace and comments that [advance] skipped. *)
let last_stop t =
  let i = index_at t (position ()) in
  if i = 0 then
    position ()
  else
    t.tokens.(i - 1).stop
