open Parseff

type route =
  | Home
  | About
  | Terms
  | BlogHome
  | BlogArticle of string

(** Parse home route: / *)
let home () =
  let _ = consume "/" in
  end_of_input ();
  Home

(** Parse about route: /about *)
let about () =
  let _ = consume "/about" in
  end_of_input ();
  About

(** Parse terms route: /legal/terms *)
let terms () =
  let _ = consume "/legal/terms" in
  end_of_input ();
  Terms

(** Parse blog home: /blog *)
let blog_home () =
  let _ = consume "/blog" in
  end_of_input ();
  BlogHome

(** Parse blog article: /blog/slug *)
let blog_article () =
  let _ = consume "/blog/" in
  let re = Re.compile (Re.Posix.re ".+") in
  let slug = match_re re in
  end_of_input ();
  BlogArticle slug

(** Top-level route parser *)
let route () = (home <|> about <|> terms <|> blog_home <|> blog_article) ()

let route_to_string = function
  | Home -> "Home"
  | About -> "About"
  | Terms -> "Terms"
  | BlogHome -> "BlogHome"
  | BlogArticle slug -> Printf.sprintf "BlogArticle(%s)" slug

let () =
  let test_cases =
    [
      ("/", Home);
      ("/about", About);
      ("/legal/terms", Terms);
      ("/blog", BlogHome);
      ("/blog/hello-world", BlogArticle "hello-world");
      ("/blog/ocaml-effects", BlogArticle "ocaml-effects");
    ]
  in
  Printf.printf "Route Parser Examples\n";
  Printf.printf "=====================\n\n";
  List.iter
    (fun (input, expected) ->
      match run input route with
      | Ok (result, _) ->
          let matches = result = expected in
          Printf.printf "✓ %-25s -> %s %s\n" input (route_to_string result)
            (if matches then "" else Printf.sprintf "(expected %s)" (route_to_string expected))
      | Error { pos; expected = exp } ->
          Printf.printf "✗ %-25s -> Error at %d: %s\n" input pos exp)
    test_cases
