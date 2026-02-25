type route = Home | About | Terms | BlogHome | BlogArticle of string

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
  BlogHome

let blog_article () =
  let _ = Parseff.consume "/blog/" in
  let slug = Parseff.match_regex slug_re in
  Parseff.end_of_input ();
  BlogArticle slug

let route () = Parseff.one_of [ home; about; terms; blog_home; blog_article ] ()

let route_to_string = function
  | Home -> "Home"
  | About -> "About"
  | Terms -> "Terms"
  | BlogHome -> "BlogHome"
  | BlogArticle slug -> Printf.sprintf "BlogArticle(%s)" slug

let () =
  let test_cases =
    [ ("/", Home)
    ; ("/about", About)
    ; ("/legal/terms", Terms)
    ; ("/blog", BlogHome)
    ; ("/blog/hello-world", BlogArticle "hello-world")
    ; ("/blog/ocaml-effects", BlogArticle "ocaml-effects")
    ]
  in
  Printf.printf "Route Parser Examples\n";
  Printf.printf "=====================\n\n";
  List.iter
    (fun (input, expected) ->
      match Parseff.parse input route with
      | Ok (result, _) ->
          let matches = result = expected in
          Printf.printf "✓ %-25s -> %s %s\n" input (route_to_string result)
            (if matches
             then ""
             else Printf.sprintf "(expected %s)" (route_to_string expected))
      | Error { pos; error= `Expected exp } ->
          Printf.printf "✗ %-25s -> Error at %d: %s\n" input pos exp
      | Error _ -> Printf.printf "✗ %-25s -> Unknown error\n" input)
    test_cases
