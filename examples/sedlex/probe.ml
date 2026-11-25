(* Small parsers that show how token errors combine with parseff's
   combinators. [probe.exe SCENARIO INPUT] runs one of them on [INPUT]. *)

open Parseff

let name t =
  Tokens.select t "a name" (function Lexer.Ident name -> Some name | _ -> None)

let eat_in t () = Tokens.eat t Lexer.In
let eat_rparen t () = Tokens.eat t Lexer.Rparen

(* An [accept] that records nothing when the token does not match. *)
let accept_silent t value =
  (Tokens.peek t).value = value
  &&
  ( Tokens.advance t;
    true
  )

(* [(a + b + c)]: names joined by [+] in parentheses. *)
let sum t accept =
  Tokens.eat t Lexer.Lparen;
  let _ = name t in
  while accept t Lexer.Plus do
    let _ = name t in
    ()
  done;
  Tokens.eat t Rparen

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
      let start = (Tokens.peek t).start in
      let _ = name t in
      Printf.printf "range %d-%d\n" start (Tokens.last_stop t)
  | "sum-accept" ->
      sum t Tokens.accept
  | "sum-silent" ->
      sum t accept_silent
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
      let t = Tokens.create ~show:Lexer.show (Lexer.tokenize input) in
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
