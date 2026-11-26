(* Glue between a lexer and parseff. The lexer runs on demand, and its tokens
   stay in an array so that a rewind finds them again without lexing twice.
   The parser position stays a byte offset: it is always the start of the
   current token, or a position before the first token.

   Tokens are keyed by position only, so the lexer must not depend on what the
   parser expects, as a regular expression against division does: after a
   rewind, a token lexed for one reading would be reused for the other.
   [examples/single_pass] reads such tokens where the grammar needs them. *)

open Parseff

type 'tok token = { value : 'tok; start : int; stop : int }

type 'tok t = {
  mutable tokens : 'tok token array;
  mutable len : int;
  next : unit -> 'tok token;
  eof : 'tok;
  show : 'tok -> string;
  mutable last : int;
}

(* [next ()] returns the next token. After the input, it must return [eof]
   with [start = stop = length]. *)
let create ~show ~eof next =
  { tokens = [||]; len = 0; next; eof; show; last = 0 }

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
  skip_to tok.start;
  tok

(* To the start of the next token, so a combinator that starts here starts
   where the next token check fails. *)
let advance t =
  let i = index_at t (position ()) in
  fill t t.tokens.(i).stop;
  let i = min (i + 1) (t.len - 1) in
  t.last <- i;
  skip_to t.tokens.(i).start

let unexpected tok label = expected_token ~upto:tok.stop label

let eat t value =
  let tok = peek t in
  if tok.value = value then
    advance t
  else
    unexpected tok (t.show value)

let select t label f =
  let tok = peek t in
  match f tok.value with
  | Some v ->
      advance t;
      v
  | None ->
      unexpected tok label
