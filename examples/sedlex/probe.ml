(* Small parsers that show how token errors combine with parseff's
   combinators. [probe.exe SCENARIO INPUT] runs one of them on [INPUT]. *)

open Parseff

type tokens = { tokens : Lexer.t array }

let index_at t pos =
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
  search 0 (Array.length t.tokens - 1)

let token_at t pos = t.tokens.(index_at t pos)

let peek t =
  let tok = token_at t (position ()) in
  skip_to tok.start;
  tok

(* Moves past [tok] and the whitespace and comments after it, to the start of
   the next token. A combinator that starts after [advance] then starts where
   the next token check fails, so [expect], [require], and [list] see that
   failure as their own. *)
let advance t (tok : Lexer.t) = skip_to (token_at t tok.stop).start

(* The end of the token before the position. Ranges end here, not after the
   whitespace that [advance] skipped. *)
let last_stop t =
  let i = index_at t (position ()) in
  if i = 0 then
    position ()
  else
    t.tokens.(i - 1).stop

let unexpected (tok : Lexer.t) expected = expected_token ~upto:tok.stop expected

let eat t expected_token label =
  let current_token = peek t in
  if current_token.token = expected_token then
    advance t current_token
  else
    unexpected current_token label

let name t =
  let tok = peek t in
  match tok.token with
  | Ident name ->
      advance t tok;
      name
  | _ ->
      unexpected tok "a name"

let eat_in t () = eat t In "'in'"
let eat_rparen t () = eat t Rparen "')'"

let scenario t = function
  | "or" ->
      or_ (eat_in t) (eat_rparen t)
  | "expect" ->
      expect "the keyword in" (eat_in t)
  | "expect-after" ->
      let _ = name t in
      expect "the keyword in" (eat_in t)
  | "mixed" ->
      or_ (eat_in t) (fun () -> literal "foo")
  | "mixed-deeper" ->
      or_ (eat_in t) (fun () -> literal "baz")
  | "require" ->
      let _ = name t in
      require (eat_in t) (fun _ -> ());
      let _ = name t in
      ()
  | "range" ->
      let _, r = with_range (fun () -> name t) in
      Printf.printf "range %d-%d\n" r.start r.stop
  | "token-range" ->
      let start = (peek t).start in
      let _ = name t in
      Printf.printf "range %d-%d\n" start (last_stop t)
  | "context" ->
      let _ = name t in
      context "pair" (fun () ->
          let _ = name t in
          eat_in t ()
      )
  | s ->
      prerr_endline ("unknown scenario " ^ s);
      exit 2

let () =
  match Sys.argv with
  | [| _; s; input |] -> (
      let t = { tokens = Lexer.tokenize input } in
      let diagnostics = Diagnostics.create () in
      let result =
        parse ~consume:`Partial ~diagnostics input (fun () -> scenario t s)
      in
      List.iter
        (fun (d : Diagnostics.entry) ->
          print_endline ("recorded: " ^ render_error d.value)
        )
        (Diagnostics.to_list diagnostics);
      match result with
      | Ok () ->
          print_endline "ok"
      | Error e ->
          print_endline (render_error e)
    )
  | _ ->
      prerr_endline "usage: probe.exe SCENARIO INPUT";
      exit 2
