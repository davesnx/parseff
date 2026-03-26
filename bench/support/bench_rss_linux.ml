type snapshot = { vm_rss_kib : int option; vm_hwm_kib : int option }

let empty = { vm_rss_kib = None; vm_hwm_kib = None }

let parse_kib line =
  let len = String.length line in
  let rec skip_non_digits i =
    if i >= len then
      None
    else
      match String.unsafe_get line i with
      | '0' .. '9' ->
          Some i
      | _ ->
          skip_non_digits (i + 1)
  in
  match skip_non_digits 0 with
  | None ->
      None
  | Some start ->
      let rec take_digits i =
        if i < len then
          match String.unsafe_get line i with
          | '0' .. '9' ->
              take_digits (i + 1)
          | _ ->
              i
        else
          i
      in
      Some (int_of_string (String.sub line start (take_digits start - start)))

let line_value prefix line =
  if String.starts_with ~prefix line then
    parse_kib line
  else
    None

let take () =
  if Sys.os_type <> "Unix" || not (Sys.file_exists "/proc/self/status") then
    empty
  else
    let ic = open_in "/proc/self/status" in
    Fun.protect
      ~finally:(fun () -> close_in_noerr ic)
      (fun () ->
        let rss = ref None in
        let hwm = ref None in
        ( try
            while true do
              let line = input_line ic in
              ( match line_value "VmRSS:" line with
              | Some kib ->
                  rss := Some kib
              | None ->
                  ()
              );
              match line_value "VmHWM:" line with
              | Some kib ->
                  hwm := Some kib
              | None ->
                  ()
            done
          with End_of_file -> ()
        );
        { vm_rss_kib = !rss; vm_hwm_kib = !hwm }
      )
