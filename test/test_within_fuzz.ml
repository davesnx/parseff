(* Differential fuzzing of [within]: random parser programs from a small
   grammar run on random byte strings.

   - Frame oracle: [take k; within n body] on [input] behaves like [body] on the
     [n] bytes after [k] (shifted by [k]), plus the documented short-frame
     error.
   - Runner oracle: [parse], [Source.parse] (several chunk sizes and windows)
     and [Push.parse] agree on values, errors and diagnostics.
   - Render oracle: under [parse], [render_error e] equals
     [render_error ~source:input e] for the error of the parse (an error built
     inside a frame shows only the frame's bytes, on purpose).

   Environment: WITHIN_FUZZ_SEEDS (a-b or a,b,c), WITHIN_FUZZ_CASES per seed,
   WITHIN_FUZZ_SIZE (program size, default 16), WITHIN_FUZZ_CASE=seed:case to
   replay one case, WITHIN_FUZZ_DUMP=file to write one line per case (to diff
   two builds of the library), WITHIN_FUZZ_STATS to print counts. Long run:
   WITHIN_FUZZ_SEEDS=100-499 WITHIN_FUZZ_CASES=20000
   _build/default/test/test_within_fuzz.exe *)

type g =
  | Eps
  | Char of char
  | Lit of bool * string
  | Any
  | Satisfy
  | Take of bool * int
  | Scan of scan
  | Until of bool * int option * string
  | Peek of int option
  | Eoi
  | Pos
  | Loc
  | Utf8 of int
  | Bin of int
  | Num of int
  | Keyword of string
  | Ident
  | Token of string
  | Peek_skip of g
  | Skip_into of g * int * g
  | Fail
  | Fail_from of int
  | Raise
  | Commit
  | Cut
  | Iter_take of int
  | Sep_by_take of bool
  | Skip_nested
  | Seq of g * g
  | Or of g * g
  | One_of of g list
  | Many of int option * g
  | Sep_by of g * g
  | Optional of g
  | Look_ahead of g
  | Not_followed_by of g
  | Peek_with of g
  | Recognize of bool * g
  | With_range of g
  | Capture of bool * g
  | Recover of g * g
  | Require of g
  | List of g * g option
  | Rec of g
  | Expression of g * g
  | Precedence of g
  | Context of bool * g
  | Expect of g
  | Within of int * g
  | Try of g * g
  | Count of int * g
  | Iter_many of g
  | Fold of g * g

and scan = {
  op : int;
  pred : int;
  set : bool;
  at_least : int option;
  at_most : int option;
}

let rec show = function
  | Eps ->
      "Eps"
  | Char c ->
      Printf.sprintf "Char %C" c
  | Lit (cs, s) ->
      Printf.sprintf "Lit (%b, %S)" cs s
  | Any ->
      "Any"
  | Satisfy ->
      "Satisfy"
  | Take (s, n) ->
      Printf.sprintf "Take (%b, %d)" s n
  | Scan s ->
      let o = function
        | None ->
            "None"
        | Some n ->
            Printf.sprintf "Some %d" n
      in
      Printf.sprintf
        "Scan { op = %d; pred = %d; set = %b; at_least = %s; at_most = %s }"
        s.op s.pred s.set (o s.at_least) (o s.at_most)
  | Until (t, k, s) ->
      Printf.sprintf "Until (%b, %s, %S)" t
        (match k with None -> "None" | Some n -> Printf.sprintf "Some %d" n)
        s
  | Peek None ->
      "Peek None"
  | Peek (Some n) ->
      Printf.sprintf "Peek (Some %d)" n
  | Eoi ->
      "Eoi"
  | Pos ->
      "Pos"
  | Loc ->
      "Loc"
  | Utf8 k ->
      Printf.sprintf "Utf8 %d" k
  | Bin k ->
      Printf.sprintf "Bin %d" k
  | Num k ->
      Printf.sprintf "Num %d" k
  | Keyword s ->
      Printf.sprintf "Keyword %S" s
  | Ident ->
      "Ident"
  | Token s ->
      Printf.sprintf "Token %S" s
  | Peek_skip a ->
      Printf.sprintf "Peek_skip (%s)" (show a)
  | Skip_into (a, n, b) ->
      Printf.sprintf "Skip_into (%s, %d, %s)" (show a) n (show b)
  | Fail ->
      "Fail"
  | Fail_from k ->
      Printf.sprintf "Fail_from %d" k
  | Raise ->
      "Raise"
  | Commit ->
      "Commit"
  | Cut ->
      "Cut"
  | Iter_take n ->
      Printf.sprintf "Iter_take %d" n
  | Sep_by_take s ->
      Printf.sprintf "Sep_by_take %b" s
  | Skip_nested ->
      "Skip_nested"
  | Seq (a, b) ->
      Printf.sprintf "Seq (%s, %s)" (show a) (show b)
  | Or (a, b) ->
      Printf.sprintf "Or (%s, %s)" (show a) (show b)
  | One_of l ->
      "One_of [" ^ String.concat "; " (List.map show l) ^ "]"
  | Many (k, a) ->
      Printf.sprintf "Many (%s, %s)"
        (match k with None -> "None" | Some n -> Printf.sprintf "Some %d" n)
        (show a)
  | Sep_by (a, b) ->
      Printf.sprintf "Sep_by (%s, %s)" (show a) (show b)
  | Optional a ->
      Printf.sprintf "Optional (%s)" (show a)
  | Look_ahead a ->
      Printf.sprintf "Look_ahead (%s)" (show a)
  | Not_followed_by a ->
      Printf.sprintf "Not_followed_by (%s)" (show a)
  | Peek_with a ->
      Printf.sprintf "Peek_with (%s)" (show a)
  | Recognize (s, a) ->
      Printf.sprintf "Recognize (%b, %s)" s (show a)
  | With_range a ->
      Printf.sprintf "With_range (%s)" (show a)
  | Capture (r, a) ->
      Printf.sprintf "Capture (%b, %s)" r (show a)
  | Recover (a, b) ->
      Printf.sprintf "Recover (%s, %s)" (show a) (show b)
  | Require a ->
      Printf.sprintf "Require (%s)" (show a)
  | List (a, None) ->
      Printf.sprintf "List (%s, None)" (show a)
  | List (a, Some b) ->
      Printf.sprintf "List (%s, Some (%s))" (show a) (show b)
  | Rec a ->
      Printf.sprintf "Rec (%s)" (show a)
  | Expression (a, b) ->
      Printf.sprintf "Expression (%s, %s)" (show a) (show b)
  | Precedence a ->
      Printf.sprintf "Precedence (%s)" (show a)
  | Context (d, a) ->
      Printf.sprintf "Context (%b, %s)" d (show a)
  | Expect a ->
      Printf.sprintf "Expect (%s)" (show a)
  | Within (n, a) ->
      Printf.sprintf "Within (%d, %s)" n (show a)
  | Try (a, b) ->
      Printf.sprintf "Try (%s, %s)" (show a) (show b)
  | Count (n, a) ->
      Printf.sprintf "Count (%d, %s)" n (show a)
  | Iter_many a ->
      Printf.sprintf "Iter_many (%s)" (show a)
  | Fold (a, b) ->
      Printf.sprintf "Fold (%s, %s)" (show a) (show b)

(* Interpreter *)

exception Boom
exception Over_budget

let max_steps = 3_000
let preds =
  [|
    (fun c -> c = 'a');
    (fun c -> (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z'));
    (fun c -> c <> '\n');
    (fun _ -> true);
    (fun c -> c >= '0' && c <= '9');
    (fun c -> c <> ';' && c <> ')');
  |]

let sets = Array.map Parseff.Charset.of_predicate preds
let upreds = [| Parseff.Utf8.is_whitespace; (fun u -> Uchar.to_int u > 0x7F) |]

type cx = {
  shift : int;  (** added to every position shown, for the frame oracle *)
  input : string;  (** the whole input, to check render_error *)
  check_render : bool;
  mutable steps : int;
  mutable bad_render : string option;
}

let cut_found m =
  let sep = ", found " in
  let n = String.length sep in
  let rec go i =
    if i + n > String.length m then
      m
    else if String.sub m i n = sep then
      String.sub m 0 i
    else
      go (i + 1)
  in
  go 0

let render_check cx (e : Parseff.error) =
  let plain = Parseff.render_error e in
  (* Known, not about frames: with a context at end of input, the stored
     excerpt elides a single blank line that ~source shows; a context that
     starts on a line after the error gets a wrong line. *)
  let known =
    (e.contexts <> [] && e.range.start = String.length cx.input)
    || List.exists
         (fun (c : Parseff.context) -> c.start > e.range.start)
         e.contexts
  in
  if cx.check_render && cx.bad_render = None && not known then begin
    let full = Parseff.render_error ~source:cx.input e in
    if plain <> full then
      cx.bad_render <-
        Some
          (Printf.sprintf "render_error:\n%s\nrender_error ~source:\n%s" plain
             full
          )
  end

(* [full] keeps the range stop and the found text: they can differ between
   runners, but not between two string parses. *)
let show_error ~full cx (e : Parseff.error) =
  let m = Parseff.error_message e in
  let contexts =
    List.map
      (fun (c : Parseff.context) ->
        Printf.sprintf "%s@%d" c.name (c.start + cx.shift)
      )
      e.contexts
  in
  if full then
    Printf.sprintf "%d-%d %s [%s]" (e.range.start + cx.shift)
      (e.range.stop + cx.shift) m
      (String.concat "," contexts)
  else
    Printf.sprintf "%d %s [%s]" (e.range.start + cx.shift) (cut_found m)
      (String.concat "," contexts)

let pos cx p = string_of_int (p + cx.shift)

let rec run ~full cx g =
  cx.steps <- cx.steps + 1;
  if cx.steps > max_steps then raise Over_budget;
  let run = run ~full cx in
  match g with
  | Eps ->
      "()"
  | Char c ->
      String.make 1 (Parseff.char c)
  | Lit (cs, s) ->
      Parseff.literal ~case_sensitive:cs s;
      s
  | Any ->
      String.make 1 (Parseff.any_char ())
  | Satisfy ->
      String.make 1 (Parseff.satisfy ~label:"letter" preds.(1))
  | Take (false, n) ->
      Parseff.take n
  | Take (true, n) ->
      Parseff.Slice.to_string (Parseff.Slice.take n)
  | Scan { op; pred; set; at_least; at_most } -> (
      let set =
        if set then
          Some sets.(pred)
        else
          None
      in
      let p = preds.(pred) in
      match op with
      | 0 ->
          Parseff.take_while ?at_least ?at_most ?set p
      | 1 ->
          Parseff.skip_while ?at_least ?at_most ?set p;
          "skip"
      | 2 ->
          Parseff.Slice.to_string
            (Parseff.Slice.take_while ?at_least ?at_most ?set p)
      | 3 ->
          let b = Buffer.create 8 in
          Parseff.Iter.take_while ?at_most ?set p (fun s ->
              Buffer.add_string b (Parseff.Slice.to_string s)
          );
          Buffer.contents b
      | 4 ->
          Parseff.Utf8.take_while ?at_least ?at_most upreds.(pred mod 2)
      | 5 ->
          Parseff.Utf8.skip_while ?at_least ?at_most upreds.(pred mod 2);
          "uskip"
      | _ ->
          Parseff.Slice.to_string
            (Parseff.Utf8.Slice.take_while ?at_least ?at_most
               upreds.(pred mod 2)
            )
    )
  | Until (true, at_most, t) ->
      Parseff.take_until ?at_most t
  | Until (false, at_most, t) ->
      Parseff.skip_until ?at_most t;
      "until"
  | Peek ahead -> (
      match Parseff.peek_char ?ahead () with
      | None ->
          "None"
      | Some c ->
          Printf.sprintf "%C" c
    )
  | Eoi ->
      Parseff.end_of_input ();
      "eoi"
  | Pos ->
      pos cx (Parseff.position ())
  | Loc ->
      let l = Parseff.location () in
      Printf.sprintf "%d:%d:%d" l.offset l.line l.byte_col
  | Utf8 k -> (
      match k with
      | 0 ->
          string_of_int (Uchar.to_int (Parseff.Utf8.any_char ()))
      | 1 ->
          string_of_int (Uchar.to_int (Parseff.Utf8.letter ()))
      | 2 ->
          Parseff.Utf8.whitespace ()
      | 3 ->
          Parseff.Utf8.skip_whitespace ();
          "uws"
      | _ ->
          string_of_int (Uchar.to_int (Parseff.Utf8.char (Uchar.of_int 0xE9)))
    )
  | Bin k -> (
      match k with
      | 0 ->
          string_of_int (Parseff.any_uint8 ())
      | 1 ->
          string_of_int (Parseff.BE.any_uint16 ())
      | 2 ->
          string_of_int (Parseff.LE.any_uint24 ())
      | 3 ->
          string_of_int (Parseff.BE.any_uint32 ())
      | 4 ->
          Int64.to_string (Parseff.LE.any_int64 ())
      | 5 ->
          string_of_float (Parseff.BE.any_double ())
      | 6 ->
          string_of_int (Parseff.any_varint ())
      | 7 ->
          Parseff.BE.int16 0x6161;
          "int16"
      | 8 ->
          Parseff.LE.int32 0x61616161l;
          "int32"
      | _ ->
          Parseff.BE.int64 0x6161616161616161L;
          "int64"
    )
  | Num k -> (
      match k with
      | 0 ->
          string_of_int (Parseff.integer ())
      | 1 ->
          string_of_float (Parseff.number ())
      | 2 ->
          string_of_int (Parseff.hex_integer ())
      | 3 ->
          String.make 1 (Parseff.digit ())
      | 4 ->
          Parseff.whitespace ()
      | 5 ->
          Parseff.skip_whitespace ();
          "ws"
      | _ ->
          Parseff.lexeme (fun () -> String.make 1 (Parseff.letter ()))
    )
  | Keyword s ->
      Parseff.keyword s;
      s
  | Ident ->
      Parseff.identifier
        ~reserved:(fun s -> s = "ab")
        preds.(1)
        (fun c -> preds.(1) c || preds.(4) c)
  | Token s ->
      let word, stop =
        Parseff.peek_with (fun () -> Parseff.take_while preds.(1))
      in
      if word = s then begin
        Parseff.advance_to stop;
        word
      end else
        Parseff.expected_token ~upto:stop (Printf.sprintf "%S" s)
  | Peek_skip a ->
      let v, stop = Parseff.peek_with (fun () -> run a) in
      Parseff.advance_to stop;
      v
  | Skip_into (a, n, b) ->
      let _, stop = Parseff.peek_with (fun () -> run a) in
      Parseff.within n (fun () ->
          Parseff.advance_to stop;
          run b
      )
  | Fail ->
      Parseff.fail "message"
  | Fail_from k ->
      Parseff.fail ~from:(max 0 (Parseff.position () - k)) "from"
  | Raise ->
      raise Boom
  | Commit ->
      Parseff.commit ();
      "commit"
  | Cut ->
      Parseff.cut ();
      "cut"
  | Iter_take n ->
      let b = Buffer.create 8 in
      Parseff.Iter.take n (fun s ->
          Buffer.add_string b (Parseff.Slice.to_string s)
      );
      Buffer.contents b
  | Sep_by_take false ->
      String.concat "|"
        (Parseff.sep_by_take ~ws:(fun c -> c = ' ') ~sep:',' preds.(1))
  | Sep_by_take true ->
      String.concat "|"
        (List.map Parseff.Slice.to_string
           (Parseff.Slice.sep_by_take ~ws:(fun c -> c = ' ') ~sep:',' preds.(1))
        )
  | Skip_nested ->
      Parseff.skip_nested [ ('(', ')'); ('[', ']') ] (fun c -> c = ';');
      "nested"
  | Seq (a, b) ->
      let x = run a in
      let y = run b in
      x ^ "," ^ y
  | Or (a, b) ->
      Parseff.or_ (fun () -> run a) (fun () -> run b)
  | One_of l ->
      Parseff.one_of (List.map (fun g () -> run g) l)
  | Many (at_most, a) ->
      "[" ^ String.concat ";" (Parseff.many ?at_most (fun () -> run a)) ^ "]"
  | Sep_by (a, b) ->
      "["
      ^ String.concat ";" (Parseff.sep_by (fun () -> run a) (fun () -> run b))
      ^ "]"
  | Optional a -> (
      match Parseff.optional (fun () -> run a) with
      | None ->
          "none"
      | Some v ->
          "some " ^ v
    )
  | Look_ahead a ->
      "la " ^ Parseff.look_ahead (fun () -> run a)
  | Not_followed_by a ->
      Parseff.not_followed_by (fun () -> run a);
      "nfb"
  | Peek_with a ->
      let v, stop = Parseff.peek_with (fun () -> run a) in
      v ^ "@" ^ pos cx stop
  | Recognize (false, a) ->
      "<" ^ Parseff.recognize (fun () -> run a) ^ ">"
  | Recognize (true, a) ->
      "<"
      ^ Parseff.Slice.to_string (Parseff.Slice.recognize (fun () -> run a))
      ^ ">"
  | With_range a ->
      let v, r = Parseff.with_range (fun () -> run a) in
      Printf.sprintf "%s{%s-%s}" v (pos cx r.start) (pos cx r.stop)
  | Capture (release, a) -> (
      match Parseff.capture ~release (fun () -> run a) with
      | Ok v ->
          "ok " ^ v
      | Error e ->
          "err " ^ show_error ~full cx e
    )
  | Recover (a, sync) ->
      Parseff.recover
        ~sync:(fun () -> ignore (run sync))
        (fun () -> run a)
        (fun e -> "hole " ^ show_error ~full cx e)
  | Require a ->
      Parseff.require
        (fun () -> run a)
        (fun e -> "missing " ^ show_error ~full cx e)
  | List (a, sep) ->
      let sep = Option.map (fun s () -> ignore (run s)) sep in
      "{"
      ^ String.concat ";"
          (Parseff.list ?sep
             ~close:(fun () -> Parseff.peek_char () = Some ')')
             ~sync:(fun () ->
               Parseff.skip_while (fun c -> c <> ';' && c <> ')');
               ignore (Parseff.optional (fun () -> Parseff.char ';'))
             )
             (fun () -> run a)
             (fun e -> "hole " ^ show_error ~full cx e)
          )
      ^ "}"
  | Rec a ->
      Parseff.rec_ (fun () -> run a)
  | Expression (atom, op) ->
      Parseff.expression
        ~operand:(fun () -> run atom)
        [
          [
            Parseff.infix_left (fun () ->
                ignore (Parseff.char '+');
                fun a b -> "(" ^ a ^ "+" ^ b ^ ")"
            );
          ];
          [
            Parseff.infix_right (fun () ->
                let o = run op in
                fun a b -> "(" ^ a ^ o ^ b ^ ")"
            );
          ];
          [
            Parseff.prefix (fun () ->
                ignore (Parseff.char '-');
                fun a -> "-" ^ a
            );
          ];
        ]
  | Precedence atom ->
      Parseff.precedence
        ~operator:(fun () ->
          match Parseff.peek_char () with
          | Some '+' ->
              Some
                ( 1,
                  Parseff.infix_left (fun () ->
                      ignore (Parseff.any_char ());
                      fun a b -> "(" ^ a ^ "+" ^ b ^ ")"
                  )
                )
          | Some '*' ->
              Some
                ( 2,
                  Parseff.infix_right (fun () ->
                      ignore (Parseff.any_char ());
                      fun a b -> "(" ^ a ^ "*" ^ b ^ ")"
                  )
                )
          | _ ->
              None
        )
        (fun () -> run atom)
  | Context (delimited, a) ->
      Parseff.context ~delimited "ctx" (fun () -> run a)
  | Expect a ->
      Parseff.expect "thing" (fun () -> run a)
  | Within (n, a) ->
      "w(" ^ Parseff.within n (fun () -> run a) ^ ")"
  | Try (a, b) -> (
      try run a with Boom -> "caught " ^ run b
    )
  | Count (n, a) ->
      "#" ^ String.concat ";" (Parseff.count n (fun () -> run a))
  | Iter_many a ->
      let b = Buffer.create 8 in
      Parseff.Iter.many (fun () -> run a) (fun v -> Buffer.add_string b v);
      Buffer.contents b
  | Fold (a, o) ->
      Parseff.fold_left
        (fun () -> run a)
        (fun () ->
          let x = run o in
          fun a b -> "(" ^ a ^ x ^ b ^ ")"
        )

(* Generator *)

let int rs n = Random.State.int rs n
let pick rs a = a.(int rs (Array.length a))
let bytes = "aaaabcAZ0129+-*;,()[] \n\t\xc3\xa9\xe2\x80\x83\xff\x00\x80"
let letters = "aab0+;("

let gen_input rs =
  let len =
    match int rs 4 with 0 -> int rs 6 | 1 | 2 -> int rs 20 | _ -> int rs 48
  in
  let run_of = ref 0 and run_char = ref 'a' in
  String.init len (fun _ ->
      if !run_of > 0 then begin
        decr run_of;
        !run_char
      end else if int rs 6 = 0 then begin
        run_of := int rs 12;
        run_char := bytes.[int rs (String.length bytes)];
        !run_char
      end else if int rs 10 = 0 then
        Char.chr (int rs 256)
      else
        bytes.[int rs (String.length bytes)]
  )

let gen_string rs =
  match int rs 6 with
  | 0 ->
      String.make (1 + int rs 12) 'a'
  | 1 ->
      "\xc3\xa9"
  | _ ->
      String.init
        (1 + int rs 3)
        (fun _ -> letters.[int rs (String.length letters)])

let gen_len rs =
  match int rs 12 with
  | 0 ->
      -1
  | 1 ->
      0
  | 2 ->
      (* Not [max_int]: [pos + n] saturates there, so the count of missing
         bytes depends on the position. *)
      max_int / 2
  | 3 ->
      9 + int rs 16
  | 4 ->
      30 + int rs 30
  | _ ->
      1 + int rs 8

let gen_limit rs =
  if int rs 3 = 0 then
    Some (int rs 10)
  else
    None

let leaf ~loc rs =
  match int rs 40 with
  | 0 | 1 ->
      Eps
  | 2 | 3 | 4 ->
      Char letters.[int rs (String.length letters)]
  | 5 | 6 ->
      Lit (int rs 4 > 0, gen_string rs)
  | 7 ->
      Any
  | 8 ->
      Satisfy
  | 9 | 10 ->
      Take (int rs 2 = 0, gen_len rs)
  | 11 | 12 | 13 | 14 | 15 ->
      let at_least =
        if int rs 3 = 0 then
          Some (int rs 4)
        else
          None
      in
      let at_most =
        match (at_least, gen_limit rs) with
        | Some l, Some m ->
            Some (l + m)
        | _, m ->
            m
      in
      Scan
        {
          op = int rs 7;
          pred = int rs (Array.length preds);
          set = int rs 2 = 0;
          at_least;
          at_most;
        }
  | 16 | 17 ->
      Until (int rs 2 = 0, gen_limit rs, gen_string rs)
  | 18 | 19 ->
      Peek
        ( if int rs 3 = 0 then
            None
          else
            Some (int rs 12)
        )
  | 20 ->
      Eoi
  | 21 ->
      if loc && int rs 2 = 0 then
        Loc
      else
        Pos
  | 22 | 23 ->
      Utf8 (int rs 5)
  | 24 | 25 ->
      Bin (int rs 10)
  | 26 | 27 ->
      Num (int rs 7)
  | 28 ->
      Keyword (pick rs [| "ab"; "a"; "aaaaaaaaa"; "b0" |])
  | 29 ->
      Ident
  | 30 ->
      Token (pick rs [| "ab"; "a"; "aaaaaa" |])
  | 31 ->
      if int rs 2 = 0 || not loc then
        Fail
      else
        Fail_from (int rs 4)
  | 32 ->
      Raise
  | 33 ->
      Commit
  | 34 ->
      Cut
  | 35 ->
      Iter_take (gen_len rs)
  | 36 ->
      Sep_by_take (int rs 2 = 0)
  | 37 ->
      Skip_nested
  | _ ->
      Char 'a'

let rec gen ~loc rs size =
  if size <= 1 then
    leaf ~loc rs
  else
    let n = size - 1 in
    let h = max 1 (n / 2) in
    let gen = gen ~loc rs in
    match int rs 44 with
    | 0 | 1 | 2 | 3 | 4 | 5 ->
        Seq (gen h, gen h)
    | 6 | 7 | 8 ->
        Or (gen h, gen h)
    | 9 ->
        One_of (List.init (int rs 4) (fun _ -> gen (max 1 (n / 3))))
    | 10 | 11 ->
        Many (gen_limit rs, gen n)
    | 12 ->
        Sep_by (gen h, gen h)
    | 13 ->
        Optional (gen n)
    | 14 ->
        Look_ahead (gen n)
    | 15 ->
        Not_followed_by (gen n)
    | 16 ->
        Peek_with (gen n)
    | 17 ->
        Peek_skip (gen n)
    | 18 ->
        Skip_into (gen h, gen_len rs, gen h)
    | 19 ->
        Recognize (int rs 2 = 0, gen n)
    | 20 ->
        With_range (gen n)
    | 21 ->
        Capture (int rs 2 = 0, gen n)
    | 22 ->
        Recover (gen h, gen h)
    | 23 ->
        Require (gen n)
    | 24 ->
        List
          ( gen h,
            if int rs 2 = 0 then
              Some (gen h)
            else
              None
          )
    | 25 ->
        Rec (gen n)
    | 26 ->
        Expression (gen h, gen (max 1 (h / 2)))
    | 27 ->
        Precedence (gen n)
    | 28 ->
        Context (int rs 2 = 0, gen n)
    | 29 ->
        Expect (gen n)
    | 30 | 31 | 32 | 33 | 34 | 35 ->
        Within (gen_len rs, gen n)
    | 36 ->
        Try (gen h, gen h)
    | 37 ->
        Count (int rs 3, gen n)
    | 38 ->
        Iter_many (gen n)
    | 39 ->
        Fold (gen h, gen h)
    | _ ->
        leaf ~loc rs

(* Runners *)

type runner =
  | Memory
  | Chunks of int * int  (** chunk size, backtrack window *)
  | Push of int  (** chunk size *)

let runners =
  [
    Chunks (1, 65_536);
    Chunks (2, 65_536);
    Chunks (3, 65_536);
    Chunks (7, 65_536);
    Chunks (4096, 65_536);
    Chunks (1, 4);
    Chunks (3, 6);
    Chunks (4096, 3);
    Push 1;
    Push 3;
  ]

let show_runner = function
  | Memory ->
      "parse"
  | Chunks (c, w) ->
      Printf.sprintf "Source.parse (chunks of %d, window %d)" c w
  | Push c ->
      Printf.sprintf "Push.parse (chunks of %d)" c

let source ~chunk input =
  let pos = ref 0 in
  Parseff.Source.of_function (fun buf off len ->
      let n = min (min chunk len) (String.length input - !pos) in
      Bytes.blit_string input !pos buf off n;
      pos := !pos + n;
      n
  )

let push ~chunk ~diagnostics ~partial input p =
  let p () =
    let v = p () in
    if not partial then Parseff.end_of_input ();
    v
  in
  let t = Parseff.Push.parse ?diagnostics p in
  let len = String.length input in
  let rec go at =
    if at >= len then
      Result.map fst (Parseff.Push.finish t)
    else
      let n = min chunk (len - at) in
      match Parseff.Push.feed t (String.sub input at n) with
      | Parseff.Push.Await ->
          go (at + n)
      | Parseff.Push.Done r ->
          Result.map fst r
  in
  go 0

type outcome = { result : string; diags : string list }

let is_window = function
  | Error { Parseff.kind = Backtrack_window_exceeded _; _ } ->
      true
  | _ ->
      false

(* [None] when the program ran out of steps or hit the window. *)
let outcome ?(shift = 0) ?(check_render = false) ~full ~diags ~partial runner g
    input =
  let cx = { shift; input; check_render; steps = 0; bad_render = None } in
  let d =
    if diags then
      Some (Parseff.Diagnostics.create ())
    else
      None
  in
  let p () =
    let v = run ~full cx g in
    if partial then
      v ^ "@" ^ pos cx (Parseff.position ())
    else
      v
  in
  let consume =
    if partial then
      `Partial
    else
      `All
  in
  match
    match runner with
    | Memory ->
        Parseff.parse ~consume ?diagnostics:d input p
    | Chunks (chunk, window) ->
        Parseff.Source.parse ~backtrack_window:window ~consume ?diagnostics:d
          (source ~chunk input) p
    | Push chunk ->
        push ~chunk ~diagnostics:d ~partial input p
  with
  | exception Over_budget ->
      None
  | exception Boom ->
      Some ({ result = "Boom"; diags = [] }, cx.bad_render)
  | exception (Invalid_argument m | Failure m) ->
      Some ({ result = "raised " ^ m; diags = [] }, cx.bad_render)
  | r when is_window r ->
      None
  | r ->
      let result =
        match r with
        | Ok v ->
            "Ok " ^ v
        | Error e ->
            render_check cx e;
            "Error " ^ show_error ~full cx e
      in
      let diags =
        match d with
        | None ->
            []
        | Some d ->
            List.map
              (fun (x : Parseff.Diagnostics.entry) ->
                show_error ~full cx x.value
              )
              (Parseff.Diagnostics.to_list d)
      in
      Some ({ result; diags }, cx.bad_render)

let show_outcome o =
  Printf.sprintf "%s\n  diagnostics: [%s]" o.result (String.concat "; " o.diags)

exception Mismatch of string

let mismatch fmt = Printf.ksprintf (fun s -> raise (Mismatch s)) fmt

type stats = {
  mutable cases : int;
  mutable skipped : int;
  mutable ok : int;
  mutable frame_cases : int;
}

let dump = ref None
let size = ref 16

(* Runner oracle and render oracle. *)
let check_runners stats g input ~diags ~partial =
  match
    outcome ~check_render:true ~full:true ~diags ~partial Memory g input
  with
  | None ->
      stats.skipped <- stats.skipped + 1
  | Some (_, Some bad) ->
      mismatch "%s" bad
  | Some (full, None) ->
      stats.cases <- stats.cases + 1;
      if String.starts_with ~prefix:"Ok" full.result then
        stats.ok <- stats.ok + 1;
      let memory =
        match outcome ~full:false ~diags ~partial Memory g input with
        | Some (o, _) ->
            o
        | None ->
            assert false
      in
      let lines = ref [ show_outcome full ] in
      List.iter
        (fun r ->
          match outcome ~full:false ~diags ~partial r g input with
          | None ->
              lines := "window" :: !lines
          | Some (o, _) ->
              lines := show_outcome o :: !lines;
              (* A stream reads a byte to check the end, so it can raise
                 [Invalid_argument] from [advance_to] where [parse] does not. *)
              if o <> memory then
                mismatch "parse:\n  %s\n%s:\n  %s" (show_outcome memory)
                  (show_runner r) (show_outcome o)
        )
        runners;
      Option.iter
        (fun oc ->
          Printf.fprintf oc "%S" (String.concat " | " (List.rev !lines))
        )
        !dump

(* Frame oracle: [take k; within n body] against [body] on the frame bytes. *)
let check_frame stats ~k ~n body input ~diags =
  let len = String.length input in
  if k <= len && n >= 0 then begin
    let g = Seq (Take (false, k), Within (n, body)) in
    let frame_len = min n (len - k) in
    let sub = String.sub input k frame_len in
    match
      ( outcome ~full:true ~diags ~partial:true Memory g input,
        outcome ~shift:k ~full:true ~diags ~partial:false Memory body sub
      )
    with
    | None, _ | _, None ->
        ()
    | Some (real, _), Some (inner, _) ->
        stats.frame_cases <- stats.frame_cases + 1;
        Option.iter
          (fun oc -> Printf.fprintf oc " frame %S" (show_outcome real))
          !dump;
        let prefix = String.sub input 0 k in
        let contains s sub =
          let n = String.length sub in
          let rec go i =
            i + n <= String.length s && (String.sub s i n = sub || go (i + 1))
          in
          go 0
        in
        (* The missing bytes merge with what the payload expected at the end,
           and a [fail] or a limit failure there wins over them. *)
        let short real =
          String.starts_with ~prefix:"Error " real.result
          &&
          if contains real.result " expected " then
            String.starts_with
              ~prefix:(Printf.sprintf "Error %d-%d " len len)
              real.result
            && (contains real.result (Printf.sprintf " %d byte" (k + n - len))
               || contains real.result " more alternatives"
               )
          else
            true
        in
        let expected =
          if String.starts_with ~prefix:"Ok " inner.result then
            if k + n > len then
              if short real then
                real
              else
                {
                  inner with
                  result =
                    Printf.sprintf
                      "Error %d-%d expected %d bytes, found end of input []" len
                      len
                      (k + n - len);
                }
            else
              {
                inner with
                result =
                  Printf.sprintf "Ok %s,w(%s)@%d" prefix
                    (String.sub inner.result 3 (String.length inner.result - 3))
                    (k + n);
              }
          else
            inner
        in
        if real <> expected then
          mismatch
            "frame of %d bytes at %d, frame bytes %S\n\
            \  within: %s\n\
            \  expected: %s"
            n k sub (show_outcome real) (show_outcome expected)
  end

let run_case stats seed case =
  let rs = Random.State.make [| seed; case |] in
  let input = gen_input rs in
  let diags = int rs 2 = 0 in
  let partial = int rs 3 = 0 in
  let g = gen ~loc:true rs (2 + int rs !size) in
  let body = gen ~loc:false rs (1 + int rs (!size * 3 / 4)) in
  let k = int rs (String.length input + 1) in
  let n =
    match int rs 4 with
    | 0 ->
        String.length input - k + int rs 4
    | _ ->
        int rs (String.length input - k + 1)
  in
  let report what =
    Alcotest.failf
      "seed %d, case %d\n\
       input: %S\n\
       program: %s\n\
       frame: take %d; within %d (%s)\n\
       %s"
      seed case input (show g) k n (show body) what
  in
  Option.iter (fun oc -> Printf.fprintf oc "%d %d " seed case) !dump;
  match
    check_runners stats g input ~diags ~partial;
    check_frame stats ~k ~n body input ~diags
  with
  | () ->
      Option.iter (fun oc -> output_char oc '\n') !dump
  | exception Mismatch m when !dump <> None ->
      Option.iter (fun oc -> Printf.fprintf oc " MISMATCH %S\n" m) !dump
  | exception Mismatch m ->
      report m
  | exception e ->
      report ("raised " ^ Printexc.to_string e)

let env_int name default =
  match Sys.getenv_opt name with Some s -> int_of_string s | None -> default

let seeds () =
  match Sys.getenv_opt "WITHIN_FUZZ_SEEDS" with
  | None ->
      [ 1; 2; 3; 4 ]
  | Some s -> (
      match String.split_on_char '-' s with
      | [ a; b ] ->
          List.init
            (int_of_string b - int_of_string a + 1)
            (fun i -> int_of_string a + i)
      | _ ->
          List.map int_of_string (String.split_on_char ',' s)
    )

let fuzz () =
  let cases = env_int "WITHIN_FUZZ_CASES" 5_000 in
  size := env_int "WITHIN_FUZZ_SIZE" 16;
  dump := Option.map open_out (Sys.getenv_opt "WITHIN_FUZZ_DUMP");
  let stats = { cases = 0; skipped = 0; ok = 0; frame_cases = 0 } in
  match Sys.getenv_opt "WITHIN_FUZZ_CASE" with
  | Some s ->
      Scanf.sscanf s "%d:%d" (run_case stats)
  | None ->
      List.iter
        (fun seed ->
          for case = 0 to cases - 1 do
            run_case stats seed case
          done
        )
        (seeds ());
      Option.iter close_out !dump;
      if Sys.getenv_opt "WITHIN_FUZZ_STATS" <> None then
        Printf.printf "cases %d, skipped %d, ok %d, frame cases %d\n%!"
          stats.cases stats.skipped stats.ok stats.frame_cases;
      if stats.skipped * 10 > stats.cases then
        Alcotest.failf "%d of %d cases ran out of steps" stats.skipped
          stats.cases;
      if stats.ok * 10 < stats.cases then
        Alcotest.failf "only %d of %d cases parsed" stats.ok stats.cases

let () =
  Alcotest.run "Within fuzz"
    [
      ( "random programs",
        [
          Alcotest.test_case "frames, runners and render_error agree" `Quick
            fuzz;
        ]
      );
    ]
