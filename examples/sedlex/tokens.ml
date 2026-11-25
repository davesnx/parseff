(* Glue between a lexer that ran over the whole input and parseff. The parser
   position stays a byte offset: it is always the start of the current token,
   or a position before the first token. *)

open Parseff

type 'tok token = { value : 'tok; start : int; stop : int }

type 'tok t = {
  tokens : 'tok token array;
  show : 'tok -> string;
  mutable last : int;
}

(* The last token must end the input, with [start = stop = length]. *)
let create ~show tokens = { tokens; show; last = 0 }

(* The index of the first token that starts at or after [pos]. Positions only
   change by [skip_to] and backtracking, so the last answer is usually still
   right and is checked first. *)
let index_at t pos =
  let i = t.last in
  if t.tokens.(i).start >= pos && (i = 0 || t.tokens.(i - 1).start < pos) then
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
    let i = search 0 (Array.length t.tokens - 1) in
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
  let i = min (i + 1) (Array.length t.tokens - 1) in
  t.last <- i;
  skip_to t.tokens.(i).start

let eat t value =
  let tok = peek t in
  if tok.value = value then
    advance t
  else
    expected_token ~upto:tok.stop (t.show value)

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
      expected_token ~upto:tok.stop label

(* The end of the token before the position, so ranges leave out the
   whitespace and comments that [advance] skipped. *)
let last_stop t =
  let i = index_at t (position ()) in
  if i = 0 then
    position ()
  else
    t.tokens.(i - 1).stop
