(* md_fix.exe FILE SECTION DOC_DIR

   Rewrites the links in the markdown that odoc generates for a page of SECTION
   (api or guides), outside fenced code blocks, and edits FILE in place:

   - The website has no module pages, so a link to one, such as
     [`Parseff.many`](./Parseff.md#val-many), becomes its text.
   - odoc writes every page to one directory, so a link to a page of the other
     section, (./name.md), points at ../guides/name.md or ../api/name.md. *)

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

let has_prefix_at line i prefix =
  i + String.length prefix <= String.length line
  && String.sub line i (String.length prefix) = prefix

let backtick_run line i =
  let j = ref i in
  while !j < String.length line && line.[!j] = '`' do
    incr j
  done;
  !j - i

(* The index of the next run of exactly [n] backticks from [i]. *)
let rec closing_run line i n =
  match String.index_from_opt line i '`' with
  | None ->
      None
  | Some j ->
      let m = backtick_run line j in
      if m = n then
        Some j
      else
        closing_run line (j + m) n

(* The index of the ] that ends a link text starting at [i], skipping code
   spans. *)
let rec link_text_end line i =
  if i >= String.length line then
    None
  else
    match line.[i] with
    | ']' ->
        Some i
    | '[' ->
        None
    | '`' -> (
        let n = backtick_run line i in
        match closing_run line (i + n) n with
        | Some j ->
            link_text_end line (j + n)
        | None ->
            None
      )
    | _ ->
        link_text_end line (i + 1)

(* The index of the ) that ends a link destination starting at [i]. A space
   means a title or no ) at all, and such a link is left unchanged. *)
let rec destination_end line i =
  if i >= String.length line then
    None
  else
    match line.[i] with
    | ')' ->
        Some i
    | ' ' ->
        None
    | _ ->
        destination_end line (i + 1)

let module_page line j =
  has_prefix_at line j "](./Parseff.md" || has_prefix_at line j "](./Parseff-"

(* Replaces [TEXT](./Parseff.md...) and [TEXT](./Parseff-...) with TEXT. *)
let unlink_module_pages line =
  let len = String.length line in
  let buf = Buffer.create len in
  let rec go i =
    if i >= len then
      ()
    else
      let link =
        if line.[i] <> '[' then
          None
        else
          match link_text_end line (i + 1) with
          | Some j when module_page line j ->
              Option.map (fun close -> (j, close)) (destination_end line (j + 2))
          | _ ->
              None
      in
      match link with
      | Some (j, close) ->
          Buffer.add_string buf (String.sub line (i + 1) (j - i - 1));
          go (close + 1)
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
    else if has_prefix_at line i "(./" then (
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

let () =
  match Array.to_list Sys.argv |> List.tl with
  | [ file; section; doc_dir ] ->
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
      let fix line =
        fix_relative_links_line ~others ~prefix (unlink_module_pages line)
      in
      write_file file (map_prose_lines fix (read_file file))
  | _ ->
      prerr_endline "usage: md_fix.exe FILE SECTION DOC_DIR";
      exit 2
