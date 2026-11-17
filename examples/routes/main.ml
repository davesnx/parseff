type route = Home | About | Terms | Blog_home | Blog_article of string

let home () =
  Parseff.literal "/";
  Parseff.end_of_input ();
  Home

let about () =
  Parseff.literal "/about";
  Parseff.end_of_input ();
  About

let terms () =
  Parseff.literal "/legal/terms";
  Parseff.end_of_input ();
  Terms

let blog_home () =
  Parseff.literal "/blog";
  Parseff.end_of_input ();
  Blog_home

let blog_article () =
  Parseff.literal "/blog/";
  let slug = Parseff.take_while ~at_least:1 ~label:"slug" (fun _ -> true) in
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
  | Error e ->
      Printf.printf "%-25s -> Error at %d: %s\n" input e.range.start
        (Parseff.error_message e)
