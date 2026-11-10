module P = Parseff

let read_file path =
  match In_channel.with_open_bin path In_channel.input_all with
  | text ->
      text
  | exception Sys_error msg ->
      let prefix = path ^ ": " in
      let reason =
        if String.starts_with ~prefix msg then
          String.sub msg (String.length prefix)
            (String.length msg - String.length prefix)
        else
          msg
      in
      Printf.eprintf "main.exe: cannot read %s: %s\n" path reason;
      exit 1

let prepare s =
  let b = Bytes.of_string s in
  let n = Bytes.length b in
  let i =
    if n >= 3 && String.sub s 0 3 = "\xEF\xBB\xBF" then begin
      Bytes.fill b 0 3 ' ';
      3
    end else
      0
  in
  if i < n && s.[i] = '#' then begin
    let j = ref i in
    while !j < n && s.[!j] <> '\n' do
      Bytes.set b !j ' ';
      incr j
    done
  end;
  Bytes.to_string b

let custom = function Lexer.Lua { msg; _ } -> msg | _ -> "unknown error"

let generic_help source (e : P.error) =
  let s = source in
  let i = e.range.start in
  let n = String.length s in
  let at k =
    if k < n then
      s.[k]
    else
      '\000'
  in
  match (e.kind, Lexer.tok_at s i) with
  | _, Sym "!" when at (i + 1) = '=' ->
      Some "Lua writes \"not equal\" as '~='"
  | _, Sym "!" ->
      Some "Lua writes logical negation as 'not'"
  | _, Sym "&" when at (i + 1) = '&' ->
      Some "Lua writes logical \"and\" as 'and'"
  | _, Sym "|" when at (i + 1) = '|' ->
      Some "Lua writes logical \"or\" as 'or'"
  | Expected { items = x, xs; _ }, Word w
    when Lexer.is_reserved w
         && List.exists
              (function P.Label "name" -> true | _ -> false)
              (x :: xs) ->
      Some
        (Printf.sprintf
           "'%s' is a reserved word in Lua and cannot be used as a name" w
        )
  | _ ->
      None

let help_of source (e : P.error) =
  match e.kind with
  | Custom (Lexer.Lua { help; _ }) ->
      help
  | _ ->
      generic_help source e

let gutter text =
  let lines = String.split_on_char '\n' text in
  let width line =
    let k = ref 0 in
    while !k < String.length line && line.[!k] = ' ' do
      incr k
    done;
    if
      !k > 0
      && !k + 1 < String.length line
      && (line.[!k] = '|' || line.[!k] = '*')
      && line.[!k + 1] = ' '
    then
      Some !k
    else
      None
  in
  match List.find_map width lines with
  | Some k ->
      String.make k ' '
  | None ->
      "  "

let render ~name ~source e =
  let base = P.render_error ~name ~source ~custom e in
  match help_of source e with
  | None ->
      base
  | Some h ->
      base ^ "\n" ^ gutter base ^ "* help: " ^ h

let analyse source =
  let block, parse_errors = Parser.parse source in
  let check_errors =
    match block with
    | Some b ->
        List.map
          (fun (i : Check.issue) ->
            let notes =
              List.map (fun (r, text) -> { P.at = Some r; text }) i.notes
            in
            Parser.error_at source i.at ~notes ?help:i.help i.msg
          )
          (Check.run source b)
    | None ->
        []
  in
  let all =
    List.stable_sort
      (fun (a : P.error) (b : P.error) -> compare a.range.start b.range.start)
      (parse_errors @ check_errors)
  in
  let same (a : P.error) (b : P.error) = a.range = b.range && a.kind = b.kind in
  let all =
    List.fold_left
      (fun kept e ->
        if List.exists (same e) kept then
          kept
        else
          e :: kept
      )
      [] all
    |> List.rev
  in
  (block, all)

let max_shown = 50

let report ~name ~source errors =
  List.iteri
    (fun i e ->
      if i < max_shown then prerr_endline (render ~name ~source e ^ "\n")
    )
    errors;
  let n = List.length errors in
  if n > max_shown then
    Printf.eprintf "%d errors, %d shown\n" n max_shown
  else
    Printf.eprintf "%d error%s\n" n
      ( if n = 1 then
          ""
        else
          "s"
      )

let usage () =
  prerr_endline "usage: main.exe (check|print|explicit|ast|trace) FILE";
  exit 2

let () =
  match Sys.argv with
  | [| _; cmd; file |] -> (
      let source = prepare (read_file file) in
      match cmd with
      | "check" ->
          let _, errors = analyse source in
          if errors = [] then
            exit 0
          else begin
            report ~name:file ~source errors;
            exit 1
          end
      | "ast" -> (
          let block, errors = analyse source in
          Option.iter (fun b -> print_string (Ast.dump b)) block;
          match errors with
          | [] ->
              exit 0
          | _ ->
              report ~name:file ~source errors;
              exit 1
        )
      | ("print" | "explicit") as cmd -> (
          match analyse source with
          | Some b, [] ->
              print_string (Printer.to_string ~parens:(cmd = "explicit") b);
              exit 0
          | _, errors ->
              report ~name:file ~source errors;
              exit 1
        )
      | "trace" ->
          let _ =
            Parser.parse ~trace:(P.print_trace Format.std_formatter) source
          in
          exit 0
      | _ ->
          usage ()
    )
  | _ ->
      usage ()
