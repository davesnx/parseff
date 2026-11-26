let () =
  match Sys.argv with
  | [| _; input |] -> (
      match Parseff.parse input Lang.program with
      | Ok e ->
          Printf.printf "%s = %g\n" (Lang.to_string e) (Lang.eval [] e)
      | Error e ->
          print_endline (Parseff.render_error ~custom:Lang.show_error e)
    )
  | _ ->
      prerr_endline "usage: main.exe EXPRESSION";
      exit 2
