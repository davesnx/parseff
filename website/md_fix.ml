(* Rewrites of the markdown that odoc generates for the website.

   md_fix.exe code-spans FILE
     Gives an inline code span that contains a backtick, such as
     `~consume:`Partial`, a longer delimiter: `` ~consume:`Partial ``.

   md_fix.exe relative-links FILE SECTION DOC_DIR
     In a page of SECTION (api or guides), points a link to a page of the other
     section, (./name.md), at ../guides/name.md or ../api/name.md.

   Both leave fenced code blocks unchanged and edit FILE in place. *)

let is_upper c = c >= 'A' && c <= 'Z'

let read_file path = In_channel.with_open_bin path In_channel.input_all

let write_file path contents =
  Out_channel.with_open_bin path (fun oc ->
      Out_channel.output_string oc contents
  )

(* Applies [f] to every line outside fenced code blocks. *)
let map_prose_lines f contents =
  let in_fence = ref false in
  String.split_on_char '\n' contents
  |> List.map (fun line ->
      if String.starts_with ~prefix:"```" line then (
        in_fence := not !in_fence;
        line
      ) else if !in_fence then
        line
      else
        f line
  )
  |> String.concat "\n"

let longest_backtick_run s =
  let best = ref 0 and run = ref 0 in
  String.iter
    (fun c ->
      if c = '`' then (
        incr run;
        best := max !best !run
      ) else
        run := 0
    )
    s;
  !best

(* A span opens at a backtick that does not follow another backtick. Its content
   is one or more characters, where a backtick counts only when an uppercase
   letter follows it. It closes at the first backtick that is followed by
   neither a backtick nor an uppercase letter. *)
let fix_code_spans_line line =
  let len = String.length line in
  let at i =
    if i < len then
      Some line.[i]
    else
      None
  in
  let content_char j =
    match at j with
    | Some '`' -> (
        match at (j + 1) with Some c -> is_upper c | None -> false
      )
    | Some _ ->
        true
    | None ->
        false
  in
  let closes j =
    at j = Some '`'
    &&
    match at (j + 1) with
    | Some c ->
        c <> '`' && not (is_upper c)
    | None ->
        true
  in
  let rec span_end j =
    if closes j then
      Some j
    else if content_char j then
      span_end (j + 1)
    else
      None
  in
  let buf = Buffer.create (len + 8) in
  let rec go i =
    if i >= len then
      ()
    else
      let opens = line.[i] = '`' && (i = 0 || line.[i - 1] <> '`') in
      match
        if opens && content_char (i + 1) then
          span_end (i + 2)
        else
          None
      with
      | Some j ->
          let content = String.sub line (i + 1) (j - i - 1) in
          let n = longest_backtick_run content in
          ( if n = 0 then
              Buffer.add_string buf ("`" ^ content ^ "`")
            else
              let d = String.make (n + 1) '`' in
              Buffer.add_string buf (d ^ " " ^ content ^ " " ^ d)
          );
          go (j + 1)
      | None ->
          Buffer.add_char buf line.[i];
          go (i + 1)
  in
  go 0;
  Buffer.contents buf

(* Rewrites (./NAME.md) and (./NAME.md#anchor) when NAME.md is in [others]. *)
let fix_relative_links_line ~others ~prefix line =
  let len = String.length line in
  let name_char c = c <> '/' && c <> '#' && c <> '?' && c <> ')' in
  let buf = Buffer.create (len + 16) in
  let rec go i =
    if i >= len then
      ()
    else if i + 3 <= len && String.sub line i 3 = "(./" then (
      let start = i + 3 in
      let stop = ref start in
      while !stop < len && name_char line.[!stop] do
        incr stop
      done;
      let name = String.sub line start (!stop - start) in
      let anchor_end =
        if String.length name < 4 || not (String.ends_with ~suffix:".md" name)
        then
          None
        else if !stop < len && line.[!stop] = ')' then
          Some !stop
        else if !stop < len && line.[!stop] = '#' then
          String.index_from_opt line !stop ')'
        else
          None
      in
      match anchor_end with
      | Some close ->
          let anchor = String.sub line !stop (close - !stop) in
          if List.mem name others then
            Buffer.add_string buf ("(" ^ prefix ^ "/" ^ name ^ anchor ^ ")")
          else
            Buffer.add_string buf (String.sub line i (close - i + 1));
          go (close + 1)
      | None ->
          Buffer.add_char buf line.[i];
          go (i + 1)
    ) else (
      Buffer.add_char buf line.[i];
      go (i + 1)
    )
  in
  go 0;
  Buffer.contents buf

let pages_of_dir dir =
  Sys.readdir dir |> Array.to_list
  |> List.filter_map (fun f ->
      if Filename.check_suffix f ".mld" then
        Some (Filename.chop_suffix f ".mld" ^ ".md")
      else
        None
  )

let edit file f = write_file file (map_prose_lines f (read_file file))

let () =
  match Array.to_list Sys.argv |> List.tl with
  | [ "code-spans"; file ] ->
      edit file fix_code_spans_line
  | [ "relative-links"; file; section; doc_dir ] ->
      let other_dir, prefix =
        match section with
        | "api" ->
            (Filename.concat doc_dir "guides", "../guides")
        | "guides" ->
            (doc_dir, "../api")
        | _ ->
            prerr_endline
              ("md_fix.exe: expected section 'api' or 'guides', got '" ^ section
             ^ "'"
              );
            exit 1
      in
      let others = pages_of_dir other_dir in
      edit file (fix_relative_links_line ~others ~prefix)
  | _ ->
      prerr_endline
        "usage: md_fix.exe code-spans FILE\n\
        \       md_fix.exe relative-links FILE SECTION DOC_DIR";
      exit 2
