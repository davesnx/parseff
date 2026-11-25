(* Small parsers that show how token errors combine with parseff's
   combinators. [probe.exe SCENARIO INPUT] runs one of them on [INPUT]. *)

open Parseff

type custom_error += Unexpected of { expected : string; found : string }

let show_error = function
  | Unexpected { expected; found } ->
      Printf.sprintf "expected %s, found %s" expected found
  | _ ->
      "unknown error"

type tokens = { input : string; tokens : Lexer.t array }

let token_at t pos =
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
  t.tokens.(search 0 (Array.length t.tokens - 1))

let peek t =
  let tok = token_at t (position ()) in
  skip_to tok.start;
  tok

let advance (t : Lexer.t) = skip_to t.stop

let unexpected t (tok : Lexer.t) expected =
  error ~from:tok.start ~upto:tok.stop
    (Unexpected { expected; found = Lexer.describe t.input tok })

let eat t expected_token label =
  let current_token = peek t in
  if current_token.token = expected_token then
    advance current_token
  else
    unexpected t current_token label

let name t =
  let tok = peek t in
  match tok.token with
  | Ident name ->
      advance tok;
      name
  | _ ->
      unexpected t tok "a name"

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
  | "require" ->
      let _ = name t in
      require (eat_in t) (fun _ -> ());
      let _ = name t in
      ()
  | "range" ->
      let _, r = with_range (fun () -> name t) in
      Printf.printf "range %d-%d\n" r.start r.stop
  | s ->
      prerr_endline ("unknown scenario " ^ s);
      exit 2

let () =
  match Sys.argv with
  | [| _; s; input |] -> (
      let t = { input; tokens = Lexer.tokenize input } in
      let diagnostics = Diagnostics.create () in
      let result =
        parse ~consume:`Partial ~diagnostics input (fun () -> scenario t s)
      in
      List.iter
        (fun (d : Diagnostics.entry) ->
          print_endline ("recorded: " ^ render_error ~custom:show_error d.value)
        )
        (Diagnostics.to_list diagnostics);
      match result with
      | Ok () ->
          print_endline "ok"
      | Error e ->
          print_endline (render_error ~custom:show_error e)
    )
  | _ ->
      prerr_endline "usage: probe.exe SCENARIO INPUT";
      exit 2
