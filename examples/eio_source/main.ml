open Eio.Std

let entry () =
  let key = Parseff.take_while (fun c -> c <> '=') in
  let _ = Parseff.char '=' in
  let value = Parseff.take_while (fun c -> c <> '\n') in
  ignore (Parseff.optional (fun () -> Parseff.char '\n'));
  (key, value)

let config () = Parseff.many entry

let () =
  Eio_main.run (fun env ->
      let clock = Eio.Stdenv.clock env in

      Switch.run (fun sw ->
          let promise =
            Fiber.fork_promise ~sw (fun () ->
                Eio.Time.sleep clock 0.05;
                "db_host=localhost\ndb_port=5432\ndb_name=myapp\n"
            )
          in

          let fetched = ref false in
          let source =
            Parseff.Source.of_chunks (fun () ->
                if !fetched then
                  None
                else begin
                  fetched := true;
                  Some (Promise.await_exn promise)
                end
            )
          in

          match Parseff.Source.parse source config with
          | Ok pairs ->
              List.iter (fun (k, v) -> Printf.printf "%s=%s\n" k v) pairs
          | Error _ ->
              Printf.eprintf "parse error\n"
      )
  )
