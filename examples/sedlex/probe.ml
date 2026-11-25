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

(* One stream of [size]-byte chunks, read by sedlex and by parseff. Each keeps
   its own cursor into the chunks read so far. *)
type tee = { input : string; size : int; chunks : string Dynarray.t }

let reader tee =
  let i = ref 0 in
  fun () ->
    let pos = Dynarray.length tee.chunks * tee.size in
    if !i = Dynarray.length tee.chunks && pos < String.length tee.input then
      Dynarray.add_last tee.chunks
        (String.sub tee.input pos (min tee.size (String.length tee.input - pos)));
    if !i < Dynarray.length tee.chunks then begin
      incr i;
      Some (Dynarray.get tee.chunks (!i - 1))
    end else
      None

let char_reader tee =
  let next = reader tee in
  let chunk = ref "" and pos = ref 0 in
  let rec gen () =
    if !pos < String.length !chunk then begin
      incr pos;
      Some !chunk.[!pos - 1]
    end else
      match next () with
      | Some c ->
          chunk := c;
          pos := 0;
          gen ()
      | None ->
          None
  in
  gen

(* [skip_to] that reads input up to [target] first. *)
let pull_to target =
  let n = ref (target - position ()) in
  if !n > 0 then
    skip_while (fun _ ->
        decr n;
        !n >= 0
    )

(* All the tokens of [input], for comparison with the number lexed. *)
let count_tokens input =
  let next = Lexer.tokens input in
  let rec loop n =
    if (next ()).value = Lexer.Eof then
      n + 1
    else
      loop (n + 1)
  in
  loop 0

let () =
  match Sys.argv with
  | [| _; s; input |] ->
      let mode, s =
        match String.split_on_char ':' s with
        | [ mode; s ] ->
            (mode, s)
        | _ ->
            ("", s)
      in
      let tee = { input; size = 2; chunks = Dynarray.create () } in
      let t =
        match mode with
        | "stream" ->
            Tokens.create ~show:Lexer.show ~eof:Lexer.Eof
              (Lexer.stream_tokens (char_reader tee))
        | "stream-pull" ->
            Tokens.create ~move:pull_to ~show:Lexer.show ~eof:Lexer.Eof
              (Lexer.stream_tokens (char_reader tee))
        | _ ->
            Tokens.create ~show:Lexer.show ~eof:Lexer.Eof (Lexer.tokens input)
      in
      let count = mode = "count" in
      let diagnostics = Diagnostics.create () in
      let run () = scenario t s in
      let result =
        if mode = "" || count then
          parse ~consume:`Partial ~diagnostics input run
        else
          Source.parse ~consume:`Partial ~diagnostics
            (Source.of_chunks (reader tee))
            run
      in
      List.iter
        (fun (d : Diagnostics.entry) ->
          print_endline ("recorded: " ^ render_error d.value)
        )
        (Diagnostics.to_list diagnostics);
      ( match result with
      | Ok () ->
          print_endline "ok"
      | Error e ->
          print_endline (render_error e)
      );
      if count then
        Printf.printf "lexed %d of %d tokens\n" (Tokens.lexed t)
          (count_tokens input)
  | _ ->
      prerr_endline "usage: probe.exe SCENARIO INPUT";
      exit 2
