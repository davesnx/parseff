(* Eio + parseff: Source.of_chunks that awaits an Eio promise.

   The callback inside Source.of_chunks performs an Eio effect
   (Promise.await) which suspends the fiber until data arrives.
   This works because parseff's deep effect handlers forward
   unrecognised effects to the outer Eio scheduler. *)

open Eio.Std

let entry () =
  let key = Parseff.take_while (fun c -> c <> '=') in
  let _ = Parseff.char '=' in
  let value = Parseff.take_while (fun c -> c <> '\n') in
  ignore (Parseff.optional (fun () -> Parseff.char '\n') ());
  (key, value)

let config () = Parseff.many entry ()

let () =
  Eio_main.run (fun env ->
      let clock = Eio.Stdenv.clock env in

      Switch.run (fun sw ->
          (* Kick off an async fetch — in real code this would be an HTTP call. *)
          let promise =
            Fiber.fork_promise ~sw (fun () ->
                Eio.Time.sleep clock 0.05;
                "db_host=localhost\ndb_port=5432\ndb_name=myapp\n"
            )
          in

          (* Source.of_chunks that awaits the Eio promise on first read. *)
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

          match Parseff.parse_source source config with
          | Ok pairs ->
              List.iter (fun (k, v) -> Printf.printf "%s=%s\n" k v) pairs
          | Error _ ->
              Printf.eprintf "parse error\n"
      )
  )
