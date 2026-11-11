module P = Parseff

let custom = function Lexer.Lua { msg; _ } -> msg | _ -> "unknown error"

let help_of (e : P.error) =
  match e.kind with Custom (Lexer.Lua { help; _ }) -> help | _ -> None

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

let with_help base help =
  match help with
  | None ->
      base
  | Some h ->
      base ^ "\n" ^ gutter base ^ "* help: " ^ h

let render ~name ?source e =
  with_help (P.render_error ~name ?source ~custom e) (help_of e)

let max_shown = 50
let backtrack_window = 64 * 1024 * 1024

let read_file path = In_channel.with_open_bin path In_channel.input_all

let cannot_read path msg =
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

type tee = {
  mutable data : Bytes.t;
  mutable lo : int;
  mutable hi : int;
  mutable base : int;
  mutable line : int;
}

let tee = { data = Bytes.create 65536; lo = 0; hi = 0; base = 0; line = 1 }

let tee_read ic buf off len =
  let n = input ic buf off len in
  if n > 0 then begin
    let live = tee.hi - tee.lo in
    if tee.hi + n > Bytes.length tee.data then begin
      let data =
        if 2 * (live + n) > Bytes.length tee.data then
          Bytes.create (2 * (live + n))
        else
          tee.data
      in
      Bytes.blit tee.data tee.lo data 0 live;
      tee.data <- data;
      tee.lo <- 0;
      tee.hi <- live
    end;
    Bytes.blit buf off tee.data tee.hi n;
    tee.hi <- tee.hi + n
  end;
  n

let tee_trim (base, line) =
  if base > tee.base then begin
    tee.lo <- tee.lo + min (base - tee.base) (tee.hi - tee.lo);
    tee.base <- base;
    tee.line <- line
  end

let render_window ~name (e : P.error) =
  let stop = tee.base + (tee.hi - tee.lo) in
  let inside (r : P.range) = r.start >= tee.base && r.stop <= stop in
  match e.kind with
  | Custom (Lexer.Lua { msg; help; _ }) when inside e.range ->
      let shift = tee.base - (tee.line - 1) in
      let text =
        String.make (tee.line - 1) '\n'
        ^ Bytes.sub_string tee.data tee.lo (tee.hi - tee.lo)
      in
      let rebase (r : P.range) =
        { P.start = r.start - shift; stop = r.stop - shift }
      in
      let notes =
        List.map
          (fun (n : P.note) ->
            match n.at with
            | Some r when inside r ->
                { n with at = Some (rebase r) }
            | _ ->
                { n with at = None }
          )
          e.notes
      in
      render ~name ~source:text
        (Parser.error_at text (rebase e.range) ~notes ?help msg)
  | _ ->
      render ~name e

let analyse path ~on_statement ~on_errors =
  Parser.reset ();
  let checker = Check.create ~loc:Lexer.loc in
  let run source =
    Seq.iter
      (fun r ->
        let errors =
          List.map
            (fun (d : _ P.Diagnostics.entry) -> d.value)
            (P.Diagnostics.to_list Parser.errors)
        in
        let errors =
          match r with
          | Ok stats ->
              List.iter
                (fun s ->
                  checker.statement s;
                  on_statement s
                )
                stats;
              errors
          | Error e ->
              errors @ [ e ]
        in
        on_errors (errors @ Lexer.take_fresh_lexical ())
      )
      (P.parse_source_each ~max_depth:100_000 ~backtrack_window
         ~diagnostics:Parser.errors source Parser.top
      )
  in
  ( try
      if path = "-" then
        run (P.Source.of_function (tee_read stdin))
      else
        In_channel.with_open_bin path (fun ic -> run (P.Source.of_channel ic))
    with Sys_error msg -> cannot_read path msg
  );
  checker.finish ()

let same (a : P.error) (b : P.error) = a.range = b.range && a.kind = b.kind

let dedupe errors =
  List.fold_left
    (fun kept e ->
      if List.exists (same e) kept then
        kept
      else
        e :: kept
    )
    [] errors
  |> List.rev

type collected = {
  mutable errors : P.error list;
  mutable rendered : (int * string) list;
  mutable count : int;
  seen : (int * int * string, unit) Hashtbl.t;
}

let collect path c errors =
  if path = "-" then begin
    List.iter
      (fun (e : P.error) ->
        let key = (e.range.start, e.range.stop, P.error_message ~custom e) in
        if not (Hashtbl.mem c.seen key) then begin
          Hashtbl.replace c.seen key ();
          c.count <- c.count + 1;
          if c.count <= max_shown then
            c.rendered <-
              (e.range.start, render_window ~name:"stdin" e) :: c.rendered
        end
      )
      errors;
    tee_trim (Lexer.window ())
  end else
    c.errors <- List.rev_append errors c.errors

let report path c issues =
  let name =
    if path = "-" then
      "stdin"
    else
      path
  in
  let source =
    if path = "-" then
      None
    else
      match read_file path with s -> Some s | exception Sys_error _ -> None
  in
  let rendered, total =
    match source with
    | Some source ->
        let check_errors =
          List.map
            (fun (i : Check.issue) ->
              let notes =
                List.map (fun (r, text) -> { P.at = Some r; text }) i.notes
              in
              Parser.error_at source i.at ~notes ?help:i.help i.msg
            )
            issues
        in
        let all = dedupe (List.rev c.errors @ check_errors) in
        ( List.map
            (fun (e : P.error) -> (e.range.start, render ~name ~source e))
            all,
          List.length all
        )
    | None ->
        let plain =
          if path = "-" then
            []
          else
            List.map
              (fun (e : P.error) -> (e.range.start, render ~name e))
              (dedupe (List.rev c.errors))
        in
        let issues =
          List.map
            (fun (i : Check.issue) ->
              let sep =
                if String.starts_with ~prefix:"byte" i.where then
                  ": "
                else
                  ":"
              in
              ( i.at.start,
                with_help
                  (Printf.sprintf "%s%s%s: [error] %s" name sep i.where i.msg)
                  i.help
              )
            )
            issues
        in
        let all = List.rev c.rendered @ plain @ issues in
        (all, List.length plain + List.length issues + c.count)
  in
  let rendered = List.stable_sort (fun (a, _) (b, _) -> compare a b) rendered in
  List.iteri
    (fun i (_, text) -> if i < max_shown then prerr_endline (text ^ "\n"))
    rendered;
  if total > max_shown then
    Printf.eprintf "%d errors, %d shown\n" total max_shown
  else
    Printf.eprintf "%d error%s\n" total
      ( if total = 1 then
          ""
        else
          "s"
      )

let run path ~on_statement =
  let c = { errors = []; rendered = []; count = 0; seen = Hashtbl.create 16 } in
  let issues = analyse path ~on_statement ~on_errors:(collect path c) in
  (c, issues)

let failed (c, issues) = c.errors <> [] || c.count > 0 || issues <> []

let usage () =
  prerr_endline "usage: main.exe (check|print|explicit|ast) FILE (- for stdin)";
  exit 2

let () =
  match Sys.argv with
  | [| _; cmd; path |] ->
      let finish result =
        if failed result then begin
          report path (fst result) (snd result);
          exit 1
        end else
          exit 0
      in
      begin match cmd with
      | "check" ->
          finish (run path ~on_statement:ignore)
      | "ast" ->
          let opened = ref false in
          let open_chunk () =
            if not !opened then begin
              opened := true;
              print_string "(chunk"
            end
          in
          let result =
            run path ~on_statement:(fun s ->
                open_chunk ();
                print_string (Ast.dump_statement s)
            )
          in
          open_chunk ();
          print_string ")\n";
          finish result
      | ("print" | "explicit") as cmd ->
          let parens = cmd = "explicit" in
          finish
            (run path ~on_statement:(fun s ->
                 print_string (Printer.to_string ~parens [ s ])
             )
            )
      | _ ->
          usage ()
      end
  | _ ->
      usage ()
