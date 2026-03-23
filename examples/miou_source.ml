(* Miou + parseff: parallel log parsing across domains.

   Three batches of structured log entries are parsed simultaneously on
   separate Miou domains using Miou.parallel *)

type level = Info | Warn | Error

let string_of_level = function
  | Info ->
      "INFO"
  | Warn ->
      "WARN"
  | Error ->
      "ERROR"

let level_rank = function Error -> 0 | Warn -> 1 | Info -> 2

type log_entry = {
  timestamp : string;
  level : level;
  tag : string;
  message : string;
}

let parse_timestamp () = Parseff.take_while (fun c -> c <> ' ')

let parse_level () =
  Parseff.one_of
    [
      (fun () ->
        let _ = Parseff.consume "INFO" in
        Info
      );
      (fun () ->
        let _ = Parseff.consume "WARN" in
        Warn
      );
      (fun () ->
        let _ = Parseff.consume "ERROR" in
        Error
      );
    ]
    ()

let parse_tag () =
  let _ = Parseff.char '[' in
  let name = Parseff.take_while (fun c -> c <> ']') in
  let _ = Parseff.char ']' in
  name

let entry () =
  let timestamp = parse_timestamp () in
  Parseff.skip_whitespace ();
  let level = parse_level () in
  Parseff.skip_whitespace ();
  let tag = parse_tag () in
  let _ = Parseff.char ' ' in
  let message = Parseff.take_while (fun c -> c <> '\n') in
  ignore (Parseff.optional (fun () -> Parseff.char '\n') ());
  { timestamp; level; tag; message }

let parse_exn text =
  let parser () = Parseff.many entry () in
  match Parseff.parse text parser with Ok es -> es | Error _ -> assert false

let () =
  Miou.run ~domains:3 (fun () ->
      let batches =
        [
          "2024-01-15T10:30:00 INFO  [http] GET /index.html 200\n\
           2024-01-15T10:30:01 INFO  [http] GET /style.css 200\n\
           2024-01-15T10:30:02 WARN  [http] GET /missing.png 404\n";
          "2024-01-15T11:00:00 ERROR [db] Connection refused\n\
           2024-01-15T11:00:05 INFO  [db] Reconnected successfully\n\
           2024-01-15T11:00:06 INFO  [http] POST /api/data 201\n";
          "2024-01-15T12:00:00 WARN  [auth] Token near expiry\n\
           2024-01-15T12:00:30 ERROR [auth] Token expired\n\
           2024-01-15T12:01:00 INFO  [auth] Token refreshed\n";
        ]
      in

      let all_entries =
        Miou.parallel parse_exn batches
        |> List.concat_map (function Ok es -> es | Error exn -> raise exn)
      in

      let sorted =
        List.sort
          (fun a b -> compare (level_rank a.level) (level_rank b.level))
          all_entries
      in
      Printf.printf "Parsed %d log entries across 3 domains:\n"
        (List.length sorted);
      List.iter
        (fun e ->
          Printf.printf "  %s %-5s [%s] %s\n" e.timestamp
            (string_of_level e.level) e.tag e.message
        )
        sorted
  )
