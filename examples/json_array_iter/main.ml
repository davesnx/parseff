type json =
  | Null
  | Bool of bool
  | Number of float
  | String of string
  | Array of json list
  | Object of (string * json) list

let ws () =
  Parseff.skip_while (fun c -> c = ' ' || c = '\n' || c = '\t' || c = '\r')

let token c =
  let _ = Parseff.char c in
  ws ()

let string_body () =
  let b = Buffer.create 16 in
  let rec go () =
    Buffer.add_string b (Parseff.take_while (fun c -> c <> '"' && c <> '\\'));
    match Parseff.any_char () with
    | '"' ->
        Buffer.contents b
    | _ ->
        Buffer.add_char b (Parseff.any_char ());
        go ()
  in
  go ()

let string_lit () =
  let _ = Parseff.char '"' in
  string_body ()

let number () =
  Parseff.take_while ~at_least:1
    (fun c ->
      (c >= '0' && c <= '9')
      || c = '-' || c = '+' || c = '.' || c = 'e' || c = 'E'
    )
    ~label:"number"
  |> float_of_string

let rec value () =
  Parseff.rec_ (fun () ->
      let v =
        match Parseff.peek_char () with
        | Some '{' ->
            token '{';
            let fields =
              Parseff.sep_by
                (fun () ->
                  let k = string_lit () in
                  ws ();
                  token ':';
                  (k, value ())
                )
                (fun () -> token ',')
                ()
            in
            let _ = Parseff.char '}' in
            Object fields
        | Some '[' ->
            token '[';
            let xs = Parseff.sep_by value (fun () -> token ',') () in
            let _ = Parseff.char ']' in
            Array xs
        | Some '"' ->
            String (string_lit ())
        | Some 't' ->
            let _ = Parseff.consume "true" in
            Bool true
        | Some 'f' ->
            let _ = Parseff.consume "false" in
            Bool false
        | Some 'n' ->
            let _ = Parseff.consume "null" in
            Null
        | _ ->
            Number (number ())
      in
      ws ();
      v
  )

let top_array f () =
  ws ();
  token '[';
  Parseff.sep_by_iter value (fun () -> token ',') f ();
  let _ = Parseff.char ']' in
  ws ()

let element i =
  Printf.sprintf
    {|{"id": %d, "name": "item \"%d\"", "tags": ["a", "b"], "score": %d.5, "ok": true, "none": null}|}
    i i (i mod 100)

let generate count =
  let next = ref 0 in
  let b = Buffer.create 65_536 in
  Parseff.Source.of_chunks (fun () ->
      if !next > count then
        None
      else begin
        Buffer.clear b;
        if !next = 0 then Buffer.add_string b "[\n";
        while !next < count && Buffer.length b < 65_000 do
          if !next > 0 then Buffer.add_string b ",\n";
          Buffer.add_string b (element !next);
          incr next
        done;
        if !next = count then begin
          Buffer.add_string b "\n]\n";
          incr next
        end;
        Some (Buffer.contents b)
      end
  )

let () =
  let count = int_of_string Sys.argv.(1) in
  let elements = ref 0 and ids = ref 0 in
  let on_element = function
    | Object (("id", Number id) :: _) ->
        incr elements;
        ids := !ids + int_of_float id
    | _ ->
        failwith "unexpected element"
  in
  match
    Parseff.parse_source ~backtrack_window:4096 (generate count)
      (top_array on_element)
  with
  | Ok () ->
      Printf.printf "elements=%d sum_of_ids=%d\n" !elements !ids
  | Error e ->
      Printf.printf "error at %d: %s\n" e.range.start (Parseff.error_message e)
