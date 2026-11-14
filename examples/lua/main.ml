let custom = function Lexer.Lua { msg; _ } -> msg | _ -> "unknown error"

let render ~name ?source e = Parseff.render_error ~name ?source ~custom e

let max_shown = 50
let backtrack_window = 1024 * 1024

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

let render_window ~name (e : Parseff.error) =
  let stop = tee.base + (tee.hi - tee.lo) in
  let inside (r : Parseff.range) = r.start >= tee.base && r.stop <= stop in
  match e.kind with
  | Custom (Lexer.Lua { msg; _ }) when inside e.range ->
      let shift = tee.base - (tee.line - 1) in
      let text =
        String.make (tee.line - 1) '\n'
        ^ Bytes.sub_string tee.data tee.lo (tee.hi - tee.lo)
      in
      let rebase (r : Parseff.range) =
        { Parseff.start = r.start - shift; stop = r.stop - shift }
      in
      let notes =
        List.map
          (fun (n : Parseff.note) ->
            match n.at with
            | Some r when inside r ->
                { n with at = Some (rebase r) }
            | _ ->
                { n with at = None }
          )
          e.notes
      in
      render ~name ~source:text
        (Parser.error_at text (rebase e.range) ~notes ?help:e.help msg)
  | _ ->
      render ~name e

let analyse path ~on_statement ~on_errors =
  Parser.reset ();
  let run source =
    Seq.iter
      (fun r ->
        let errors =
          List.map
            (fun (d : _ Parseff.Diagnostics.entry) -> d.value)
            (Parseff.Diagnostics.to_list Parser.errors)
        in
        let errors =
          match r with
          | Ok stat ->
              Option.iter on_statement stat;
              errors
          | Error e ->
              errors @ [ e ]
        in
        on_errors (errors @ Lexer.take_fresh_lexical ())
      )
      (Parseff.parse_source_each ~max_depth:100_000 ~backtrack_window
         ~diagnostics:Parser.errors source Parser.top
      )
  in
  try
    if path = "-" then
      run (Parseff.Source.of_function (tee_read stdin))
    else
      In_channel.with_open_bin path (fun ic ->
          run (Parseff.Source.of_channel ic)
      )
  with Sys_error msg -> cannot_read path msg

let dedupe errors =
  let seen = Hashtbl.create 64 in
  List.filter
    (fun (e : Parseff.error) ->
      let key = (e.range, e.kind) in
      if Hashtbl.mem seen key then
        false
      else begin
        Hashtbl.replace seen key ();
        true
      end
    )
    errors

type collected = {
  mutable errors : Parseff.error list;
  mutable rendered : (int * string) list;
  mutable count : int;
  seen : (int * int * string, unit) Hashtbl.t;
}

let collect path c errors =
  if path = "-" then begin
    List.iter
      (fun (e : Parseff.error) ->
        let key =
          (e.range.start, e.range.stop, Parseff.error_message ~custom e)
        in
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

let report path c =
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
        let all =
          List.stable_sort
            (fun (a : Parseff.error) (b : Parseff.error) ->
              compare a.range.start b.range.start
            )
            (dedupe (List.rev c.errors))
        in
        ( List.map
            (fun (e : Parseff.error) -> (e.range.start, render ~name ~source e))
            (List.filteri (fun i _ -> i < max_shown) all),
          List.length all
        )
    | None ->
        let plain =
          if path = "-" then
            []
          else
            List.map
              (fun (e : Parseff.error) -> (e.range.start, render ~name e))
              (dedupe (List.rev c.errors))
        in
        let all = List.rev c.rendered @ plain in
        (all, List.length plain + c.count)
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
  analyse path ~on_statement ~on_errors:(collect path c);
  c

let failed c = c.errors <> [] || c.count > 0

let usage () =
  prerr_endline "usage: main.exe (check|print|explicit|ast) FILE (- for stdin)";
  exit 2

let () =
  match Sys.argv with
  | [| _; cmd; path |] ->
      let finish result =
        if failed result then begin
          report path result;
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
                 match Printer.to_string ~parens [ s ] with
                 | text ->
                     print_string text
                 | exception Printer.Error_node ->
                     ()
             )
            )
      | _ ->
          usage ()
      end
  | _ ->
      usage ()
