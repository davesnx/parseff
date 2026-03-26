let has_env name =
  match Sys.getenv_opt name with Some _ -> true | None -> false

let env_truthy name =
  match Sys.getenv_opt name with
  | None ->
      false
  | Some value ->
      let value = String.lowercase_ascii value in
      value <> "" && value <> "0" && value <> "false" && value <> "no"

let stdout_supports_color () =
  try Unix.isatty (Unix.descr_of_out_channel stdout) with _ -> false

let term_supports_color () =
  match Sys.getenv_opt "TERM" with
  | Some "dumb" | None ->
      false
  | Some _ ->
      true

let colors_enabled =
  lazy
    (let force = env_truthy "CLICOLOR_FORCE" in
     let disabled =
       has_env "NO_COLOR" || Sys.getenv_opt "CLICOLOR" = Some "0"
     in
     force
     || ((not disabled) && term_supports_color () && stdout_supports_color ())
    )

let render code text =
  if Lazy.force colors_enabled then
    Printf.sprintf "\027[%sm%s\027[0m" code text
  else
    text

let banner_text text = render "1;36" text
let rule_text text = render "36" text
let section_text text = render "1;33" text
let label_text text = render "1;32" text
let header_text text = render "1;34" text
let path_text text = render "1;35" text

let print_blank_lines count =
  for _ = 1 to count do
    print_endline ""
  done

let print_heading ?(before = 1) ?(after = 1) ?(underline = '=') ~render_title
    title =
  print_blank_lines before;
  print_endline (render_title title);
  print_endline (rule_text (String.make (String.length title) underline));
  print_blank_lines after

let print_banner title =
  print_heading ~before:2 ~after:1 ~underline:'=' ~render_title:banner_text
    title

let print_section ?(before = 1) ?(underline = '-') title =
  print_heading ~before ~after:1 ~underline ~render_title:section_text title

let print_label_value name value =
  Printf.printf "%s %s\n" (label_text (name ^ ":")) value

let print_notice name value =
  Printf.printf "%s %s\n" (header_text (name ^ ":")) value

let print_paths ?(label = "Artifacts") first second =
  Printf.printf "%s %s and %s\n"
    (header_text (label ^ ":"))
    (path_text first) (path_text second)
