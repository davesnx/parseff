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

(* [operand (/ operand)*] then the end, where an operand is a name or a
   regular expression. Prints the operands it read. *)
let divisions t operand =
  let rec loop acc =
    if Tokens.accept t Lexer.Slash then
      loop (acc ^ " / " ^ operand ())
    else
      acc
  in
  let s = loop (operand ()) in
  Tokens.eat t Eof;
  print_endline s

let regex_text t =
  Tokens.select t "a regular expression" (function
    | Lexer.Regex r ->
        Some r
    | _ ->
        None
    )

(* The cached tokens were lexed with [/] as division. Lexes the regular
   expression on its own, then goes on with the first cached token after
   it. *)
let operand_resync input t () =
  let tok = Tokens.peek t in
  match tok.value with
  | Lexer.Slash -> (
      match Lexer.tokens ~from:tok.start ~regex:true input () with
      | { value = Lexer.Regex r; stop; _ } ->
          skip_to stop;
          r
      | _ ->
          name t
    )
  | _ ->
      name t

(* Lexes again from the [/], in the mode where it starts a regular
   expression. *)
let operand_relex input t () =
  let tok = Tokens.peek t in
  match tok.value with
  | Lexer.Slash ->
      Tokens.relex t (Lexer.tokens ~from:tok.start ~regex:true input);
      regex_text t
  | _ ->
      name t

let slash_names t =
  Tokens.eat t Lexer.Slash;
  let a = name t in
  Tokens.eat t Lexer.Slash;
  let b = name t in
  Tokens.eat t Eof;
  print_endline ("/ " ^ a ^ " / " ^ b)

let scenario input t = function
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
  | "regex-resync" ->
      divisions t (operand_resync input t)
  | "regex-relex" ->
      divisions t (operand_relex input t)
  | "regex-backtrack" ->
      or_
        (fun () ->
          print_endline (operand_relex input t ());
          Tokens.eat t Eof
        )
        (fun () -> slash_names t)
  | "regex-backtrack-relex" ->
      or_
        (fun () ->
          print_endline (operand_relex input t ());
          Tokens.eat t Eof
        )
        (fun () ->
          Tokens.relex t (Lexer.tokens ~from:(position ()) input);
          slash_names t
        )
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
      let run () = scenario input t s in
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
