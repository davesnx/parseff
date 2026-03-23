type route = Home | About | Terms | Blog_home | Blog_article of string

let slug_re = Re.compile (Re.Posix.re ".+")

let home () =
  let _ = Parseff.consume "/" in
  Parseff.end_of_input ();
  Home

let about () =
  let _ = Parseff.consume "/about" in
  Parseff.end_of_input ();
  About

let terms () =
  let _ = Parseff.consume "/legal/terms" in
  Parseff.end_of_input ();
  Terms

let blog_home () =
  let _ = Parseff.consume "/blog" in
  Parseff.end_of_input ();
  Blog_home

let blog_article () =
  let _ = Parseff.consume "/blog/" in
  let slug = Parseff.match_regex slug_re in
  Parseff.end_of_input ();
  Blog_article slug

let route () = Parseff.one_of [ home; about; terms; blog_home; blog_article ] ()

let route_to_string = function
  | Home ->
      "Home"
  | About ->
      "About"
  | Terms ->
      "Terms"
  | Blog_home ->
      "Blog_home"
  | Blog_article slug ->
      Printf.sprintf "Blog_article(%s)" slug

let () =
  let input = Sys.argv.(1) in
  match Parseff.parse input route with
  | Ok result ->
      Printf.printf "%-25s -> %s\n" input (route_to_string result)
  | Error { pos; error = `Expected exp } ->
      Printf.printf "%-25s -> Error at %d: %s\n" input pos exp
  | Error { pos; error = `Unexpected_end_of_input } ->
      Printf.printf "%-25s -> Unexpected end of input at %d\n" input pos
  | Error _ ->
      Printf.printf "%-25s -> Unknown error\n" input
