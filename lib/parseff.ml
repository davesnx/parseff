type span = { buf : string; off : int; len : int }

let[@inline always] span_to_string s =
  if s.len = 0 then "" else String.sub s.buf s.off s.len

type _ Effect.t +=
  | Consume : string -> string Effect.t
  | Satisfy : (char -> bool) * string -> char Effect.t
  | Position : int Effect.t
  | Match_re : Re.re -> string Effect.t
  | Choose : (unit -> 'a) * (unit -> 'a) -> 'a Effect.t
  | Fail : string -> 'a Effect.t
  | Error_val : 'e -> 'a Effect.t
  | Look_ahead : (unit -> 'a) -> 'a Effect.t
  | End_of_input : unit Effect.t
  | Take_while : (char -> bool) -> string Effect.t
  | Skip_while : (char -> bool) -> unit Effect.t
  | Greedy_many : (unit -> 'a) -> 'a list Effect.t
  | Skip_while_then_char : (char -> bool) * char -> unit Effect.t
  | Fused_sep_take : (char -> bool) * char * (char -> bool) -> string Effect.t
  | Sep_by_take : (char -> bool) * char * (char -> bool) -> string list Effect.t
  | Take_while_span : (char -> bool) -> span Effect.t
  | Sep_by_take_span :
      (char -> bool) * char * (char -> bool)
      -> span list Effect.t
  | Rec_ : (unit -> 'a) -> 'a Effect.t

type ('a, 'e) result = Ok of 'a | Error of { pos : int; error : 'e }

exception Parse_error of int * string
exception User_error of int * Obj.t
exception Depth_limit_exceeded of int * string

type state = { mutable input : string; mutable pos : int }

(* Polymorphic record so handle_exn can be called at different types inside the
   polymorphic-recursive [go]. A plain function parameter would fix 'a. *)
type 'e exn_handler = { handle : 'a. exn -> ('a, 'e) result }

(* {{{ State-manipulation helpers. Each operates on [state] + [input_len],
   either returning a value or raising [Parse_error]. Shared by deep and shallow
   handlers. *)

let[@inline] handle_consume st input_len s =
  let len = String.length s in
  if st.pos + len <= input_len then begin
    let inp = st.input in
    let pos = st.pos in
    let rec check i =
      if i >= len then true
      else if String.unsafe_get inp (pos + i) = String.unsafe_get s i then
        check (i + 1)
      else false
    in
    if check 0 then begin
      st.pos <- pos + len;
      s
    end
    else raise (Parse_error (pos, Printf.sprintf "expected %S" s))
  end
  else raise (Parse_error (st.pos, Printf.sprintf "expected %S (EOF)" s))

let[@inline] handle_satisfy st input_len pred label =
  if st.pos < input_len then
    let c = String.unsafe_get st.input st.pos in
    if pred c then begin
      st.pos <- st.pos + 1;
      c
    end
    else raise (Parse_error (st.pos, Printf.sprintf "expected %s" label))
  else raise (Parse_error (st.pos, Printf.sprintf "expected %s (EOF)" label))

let[@inline] handle_take_while st input_len pred =
  let start = st.pos in
  let inp = st.input in
  let pos = ref start in
  while !pos < input_len && pred (String.unsafe_get inp !pos) do
    pos := !pos + 1
  done;
  let end_pos = !pos in
  st.pos <- end_pos;
  if end_pos > start then String.sub inp start (end_pos - start) else ""

let[@inline] handle_take_while_span st input_len pred =
  let start = st.pos in
  let inp = st.input in
  let pos = ref start in
  while !pos < input_len && pred (String.unsafe_get inp !pos) do
    pos := !pos + 1
  done;
  st.pos <- !pos;
  { buf = inp; off = start; len = !pos - start }

let[@inline] handle_skip_while st input_len pred =
  let inp = st.input in
  let pos = ref st.pos in
  while !pos < input_len && pred (String.unsafe_get inp !pos) do
    pos := !pos + 1
  done;
  st.pos <- !pos

let[@inline] handle_skip_while_then_char st input_len pred c =
  let inp = st.input in
  let pos = ref st.pos in
  while !pos < input_len && pred (String.unsafe_get inp !pos) do
    pos := !pos + 1
  done;
  if !pos < input_len && String.unsafe_get inp !pos = c then st.pos <- !pos + 1
  else begin
    st.pos <- !pos;
    raise (Parse_error (!pos, Printf.sprintf "expected %C" c))
  end

let[@inline] handle_fused_sep_take st input_len ws_pred sep_char take_pred =
  let inp = st.input in
  let pos = ref st.pos in
  while !pos < input_len && ws_pred (String.unsafe_get inp !pos) do
    pos := !pos + 1
  done;
  if !pos < input_len && String.unsafe_get inp !pos = sep_char then begin
    pos := !pos + 1;
    while !pos < input_len && ws_pred (String.unsafe_get inp !pos) do
      pos := !pos + 1
    done;
    let start = !pos in
    while !pos < input_len && take_pred (String.unsafe_get inp !pos) do
      pos := !pos + 1
    done;
    if !pos > start then begin
      st.pos <- !pos;
      String.sub inp start (!pos - start)
    end
    else begin
      st.pos <- !pos;
      raise (Parse_error (!pos, "expected value"))
    end
  end
  else begin
    st.pos <- !pos;
    raise (Parse_error (!pos, Printf.sprintf "expected %C" sep_char))
  end

let[@inline] handle_sep_by_take st input_len ws_pred sep_char take_pred =
  let inp = st.input in
  let pos = ref st.pos in
  let start = !pos in
  while !pos < input_len && take_pred (String.unsafe_get inp !pos) do
    pos := !pos + 1
  done;
  if !pos <= start then begin
    st.pos <- !pos;
    []
  end
  else begin
    let acc = ref [ String.sub inp start (!pos - start) ] in
    let continue_loop = ref true in
    while !continue_loop do
      let sep_pos = ref !pos in
      while !sep_pos < input_len && ws_pred (String.unsafe_get inp !sep_pos) do
        sep_pos := !sep_pos + 1
      done;
      if !sep_pos < input_len && String.unsafe_get inp !sep_pos = sep_char then begin
        sep_pos := !sep_pos + 1;
        while
          !sep_pos < input_len && ws_pred (String.unsafe_get inp !sep_pos)
        do
          sep_pos := !sep_pos + 1
        done;
        let elem_start = !sep_pos in
        while
          !sep_pos < input_len && take_pred (String.unsafe_get inp !sep_pos)
        do
          sep_pos := !sep_pos + 1
        done;
        if !sep_pos > elem_start then begin
          acc := String.sub inp elem_start (!sep_pos - elem_start) :: !acc;
          pos := !sep_pos
        end
        else continue_loop := false
      end
      else continue_loop := false
    done;
    st.pos <- !pos;
    List.rev !acc
  end

let[@inline] handle_sep_by_take_span st input_len ws_pred sep_char take_pred =
  let inp = st.input in
  let pos = ref st.pos in
  let start = !pos in
  while !pos < input_len && take_pred (String.unsafe_get inp !pos) do
    pos := !pos + 1
  done;
  if !pos <= start then begin
    st.pos <- !pos;
    []
  end
  else begin
    let acc = ref [ { buf = inp; off = start; len = !pos - start } ] in
    let continue_loop = ref true in
    while !continue_loop do
      let sep_pos = ref !pos in
      while !sep_pos < input_len && ws_pred (String.unsafe_get inp !sep_pos) do
        sep_pos := !sep_pos + 1
      done;
      if !sep_pos < input_len && String.unsafe_get inp !sep_pos = sep_char then begin
        sep_pos := !sep_pos + 1;
        while
          !sep_pos < input_len && ws_pred (String.unsafe_get inp !sep_pos)
        do
          sep_pos := !sep_pos + 1
        done;
        let elem_start = !sep_pos in
        while
          !sep_pos < input_len && take_pred (String.unsafe_get inp !sep_pos)
        do
          sep_pos := !sep_pos + 1
        done;
        if !sep_pos > elem_start then begin
          acc :=
            { buf = inp; off = elem_start; len = !sep_pos - elem_start } :: !acc;
          pos := !sep_pos
        end
        else continue_loop := false
      end
      else continue_loop := false
    done;
    st.pos <- !pos;
    List.rev !acc
  end

let[@inline] handle_match_regex st re =
  try
    let groups = Re.exec ~pos:st.pos re st.input in
    let match_start = Re.Group.start groups 0 in
    if match_start <> st.pos then
      raise (Parse_error (st.pos, "regex match failed"))
    else
      let matched = Re.Group.get groups 0 in
      let match_end = Re.Group.stop groups 0 in
      st.pos <- match_end;
      matched
  with Not_found -> raise (Parse_error (st.pos, "regex match failed"))

let[@inline] handle_end_of_input st input_len =
  if st.pos <> input_len then
    raise (Parse_error (st.pos, "expected end of input"))

(* }}} *)

(* {{{ Deep handler *)

let run_deep (type e) ~max_depth (handler : e exn_handler) input
    (parser : unit -> 'a) : ('a, e) result =
  let st = { input; pos = 0 } in
  let input_len = String.length input in
  let nest_depth = ref 0 in
  let rec go : 'b. (unit -> 'b) -> ('b, e) result =
   fun p ->
    match p () with
    | v -> Ok v
    | exception exn -> handler.handle exn
    | effect Position, k -> Effect.Deep.continue k st.pos
    | effect Consume s, k -> (
        match handle_consume st input_len s with
        | v -> Effect.Deep.continue k v
        | exception exn -> Effect.Deep.discontinue k exn)
    | effect Satisfy (pred, label), k -> (
        match handle_satisfy st input_len pred label with
        | v -> Effect.Deep.continue k v
        | exception exn -> Effect.Deep.discontinue k exn)
    | effect Take_while pred, k ->
        Effect.Deep.continue k (handle_take_while st input_len pred)
    | effect Take_while_span pred, k ->
        Effect.Deep.continue k (handle_take_while_span st input_len pred)
    | effect Skip_while pred, k ->
        handle_skip_while st input_len pred;
        Effect.Deep.continue k ()
    | effect Skip_while_then_char (pred, c), k -> (
        match handle_skip_while_then_char st input_len pred c with
        | () -> Effect.Deep.continue k ()
        | exception exn -> Effect.Deep.discontinue k exn)
    | effect Fused_sep_take (ws_pred, sep_char, take_pred), k -> (
        match handle_fused_sep_take st input_len ws_pred sep_char take_pred with
        | v -> Effect.Deep.continue k v
        | exception exn -> Effect.Deep.discontinue k exn)
    | effect Sep_by_take (ws_pred, sep_char, take_pred), k ->
        Effect.Deep.continue k
          (handle_sep_by_take st input_len ws_pred sep_char take_pred)
    | effect Sep_by_take_span (ws_pred, sep_char, take_pred), k ->
        Effect.Deep.continue k
          (handle_sep_by_take_span st input_len ws_pred sep_char take_pred)
    | effect Match_re re, k -> (
        match handle_match_regex st re with
        | v -> Effect.Deep.continue k v
        | exception exn -> Effect.Deep.discontinue k exn)
    | effect Greedy_many p, k ->
        let acc = ref [] in
        let saved = ref st.pos in
        let failed = ref false in
        while not !failed do
          saved := st.pos;
          match go (Obj.magic p) with
          | Ok v -> acc := Obj.magic v :: !acc
          | Error _ ->
              st.pos <- !saved;
              failed := true
        done;
        Effect.Deep.continue k (Obj.magic (List.rev !acc))
    | effect Choose (left, right), k -> (
        let saved = st.pos in
        match go (Obj.magic left) with
        | Ok v -> Effect.Deep.continue k (Obj.magic v)
        | Error _ -> (
            st.pos <- saved;
            match go (Obj.magic right) with
            | Ok v -> Effect.Deep.continue k (Obj.magic v)
            | Error e ->
                Effect.Deep.discontinue k (User_error (e.pos, Obj.repr e.error))
            ))
    | effect Fail msg, k ->
        Effect.Deep.discontinue k (Parse_error (st.pos, msg))
    | effect Error_val e, k ->
        Effect.Deep.discontinue k (User_error (st.pos, Obj.repr e))
    | effect Look_ahead p, k -> (
        let saved = st.pos in
        match go (Obj.magic p) with
        | Ok v ->
            st.pos <- saved;
            Effect.Deep.continue k (Obj.magic v)
        | Error e ->
            st.pos <- saved;
            Effect.Deep.discontinue k (User_error (e.pos, Obj.repr e.error)))
    | effect Rec_ p, k ->
        if !nest_depth >= max_depth then
          Effect.Deep.discontinue k
            (Depth_limit_exceeded
               ( st.pos,
                 Printf.sprintf "maximum nesting depth %d exceeded" max_depth ))
        else begin
          incr nest_depth;
          match go (Obj.magic p) with
          | Ok v ->
              decr nest_depth;
              Effect.Deep.continue k (Obj.magic v)
          | Error e ->
              decr nest_depth;
              Effect.Deep.discontinue k (User_error (e.pos, Obj.repr e.error))
        end
    | effect End_of_input, k -> (
        match handle_end_of_input st input_len with
        | () -> Effect.Deep.continue k ()
        | exception exn -> Effect.Deep.discontinue k exn)
  in
  go parser

let parse ?(max_depth = 128) input (parser : unit -> 'a) : ('a, 'e) result =
  match
    run_deep ~max_depth
      {
        handle =
          (function
          | Parse_error (pos, msg) -> Error { pos; error = `Expected msg }
          | User_error (pos, obj) -> Error { pos; error = Obj.obj obj }
          | e -> raise e);
      }
      input parser
  with
  | result -> result
  | exception Depth_limit_exceeded (pos, msg) ->
      Error { pos; error = `Expected msg }

(* }}} *)

(* {{{ Source-based streaming *)

type source = {
  mutable input : string;
  mutable input_len : int;
  read : bytes -> int -> int -> int;
  mutable eof : bool;
  tmp : bytes;
}

let default_buf_size = 4096

let try_refill src =
  if src.eof then false
  else
    let n = src.read src.tmp 0 (Bytes.length src.tmp) in
    if n = 0 then begin
      src.eof <- true;
      false
    end
    else begin
      let old = src.input in
      let old_len = src.input_len in
      let new_len = old_len + n in
      let buf = Bytes.create new_len in
      Bytes.blit_string old 0 buf 0 old_len;
      Bytes.blit src.tmp 0 buf old_len n;
      src.input <- Bytes.unsafe_to_string buf;
      src.input_len <- new_len;
      true
    end

let ensure_bytes src pos needed =
  let rec loop () =
    if pos + needed <= src.input_len then true
    else if not (try_refill src) then false
    else loop ()
  in
  loop ()

let handle_take_while_source src st pred =
  let start = st.pos in
  let continue = ref true in
  while !continue do
    if st.pos < src.input_len then begin
      if pred (String.unsafe_get src.input st.pos) then st.pos <- st.pos + 1
      else continue := false
    end
    else if try_refill src then ()
    else continue := false
  done;
  st.input <- src.input;
  if st.pos > start then String.sub src.input start (st.pos - start) else ""

let handle_take_while_span_source src st pred =
  let start = st.pos in
  let continue = ref true in
  while !continue do
    if st.pos < src.input_len then begin
      if pred (String.unsafe_get src.input st.pos) then st.pos <- st.pos + 1
      else continue := false
    end
    else if try_refill src then ()
    else continue := false
  done;
  st.input <- src.input;
  { buf = src.input; off = start; len = st.pos - start }

let handle_skip_while_source src st pred =
  let continue = ref true in
  while !continue do
    if st.pos < src.input_len then begin
      if pred (String.unsafe_get src.input st.pos) then st.pos <- st.pos + 1
      else continue := false
    end
    else if try_refill src then ()
    else continue := false
  done;
  st.input <- src.input

let handle_skip_while_then_char_source src st pred c =
  handle_skip_while_source src st pred;
  ignore (ensure_bytes src st.pos 1);
  if st.pos < src.input_len && String.unsafe_get src.input st.pos = c then
    st.pos <- st.pos + 1
  else raise (Parse_error (st.pos, Printf.sprintf "expected %C" c))

let handle_fused_sep_take_source src st ws_pred sep_char take_pred =
  handle_skip_while_source src st ws_pred;
  ignore (ensure_bytes src st.pos 1);
  if st.pos < src.input_len && String.unsafe_get src.input st.pos = sep_char
  then begin
    st.pos <- st.pos + 1;
    handle_skip_while_source src st ws_pred;
    let start = st.pos in
    let continue = ref true in
    while !continue do
      if st.pos < src.input_len then begin
        if take_pred (String.unsafe_get src.input st.pos) then
          st.pos <- st.pos + 1
        else continue := false
      end
      else if try_refill src then ()
      else continue := false
    done;
    st.input <- src.input;
    if st.pos > start then String.sub src.input start (st.pos - start)
    else raise (Parse_error (st.pos, "expected value"))
  end
  else raise (Parse_error (st.pos, Printf.sprintf "expected %C" sep_char))

let handle_sep_by_take_source src st ws_pred sep_char take_pred =
  let start = st.pos in
  let continue_take = ref true in
  while !continue_take do
    if st.pos < src.input_len then begin
      if take_pred (String.unsafe_get src.input st.pos) then
        st.pos <- st.pos + 1
      else continue_take := false
    end
    else if try_refill src then ()
    else continue_take := false
  done;
  st.input <- src.input;
  if st.pos <= start then []
  else begin
    let acc = ref [ String.sub src.input start (st.pos - start) ] in
    let continue_loop = ref true in
    while !continue_loop do
      let saved_pos = st.pos in
      handle_skip_while_source src st ws_pred;
      ignore (ensure_bytes src st.pos 1);
      if st.pos < src.input_len && String.unsafe_get src.input st.pos = sep_char
      then begin
        st.pos <- st.pos + 1;
        handle_skip_while_source src st ws_pred;
        let elem_start = st.pos in
        let ct = ref true in
        while !ct do
          if st.pos < src.input_len then begin
            if take_pred (String.unsafe_get src.input st.pos) then
              st.pos <- st.pos + 1
            else ct := false
          end
          else if try_refill src then ()
          else ct := false
        done;
        st.input <- src.input;
        if st.pos > elem_start then
          acc := String.sub src.input elem_start (st.pos - elem_start) :: !acc
        else begin
          st.pos <- saved_pos;
          st.input <- src.input;
          continue_loop := false
        end
      end
      else begin
        st.pos <- saved_pos;
        st.input <- src.input;
        continue_loop := false
      end
    done;
    List.rev !acc
  end

let handle_sep_by_take_span_source src st ws_pred sep_char take_pred =
  let start = st.pos in
  let continue_take = ref true in
  while !continue_take do
    if st.pos < src.input_len then begin
      if take_pred (String.unsafe_get src.input st.pos) then
        st.pos <- st.pos + 1
      else continue_take := false
    end
    else if try_refill src then ()
    else continue_take := false
  done;
  st.input <- src.input;
  if st.pos <= start then []
  else begin
    let acc = ref [ { buf = src.input; off = start; len = st.pos - start } ] in
    let continue_loop = ref true in
    while !continue_loop do
      let saved_pos = st.pos in
      handle_skip_while_source src st ws_pred;
      ignore (ensure_bytes src st.pos 1);
      if st.pos < src.input_len && String.unsafe_get src.input st.pos = sep_char
      then begin
        st.pos <- st.pos + 1;
        handle_skip_while_source src st ws_pred;
        let elem_start = st.pos in
        let ct = ref true in
        while !ct do
          if st.pos < src.input_len then begin
            if take_pred (String.unsafe_get src.input st.pos) then
              st.pos <- st.pos + 1
            else ct := false
          end
          else if try_refill src then ()
          else ct := false
        done;
        st.input <- src.input;
        if st.pos > elem_start then
          acc :=
            { buf = src.input; off = elem_start; len = st.pos - elem_start }
            :: !acc
        else begin
          st.pos <- saved_pos;
          st.input <- src.input;
          continue_loop := false
        end
      end
      else begin
        st.pos <- saved_pos;
        st.input <- src.input;
        continue_loop := false
      end
    done;
    List.rev !acc
  end

let handle_match_regex_source src st re =
  let rec loop () =
    try
      let groups = Re.exec ~pos:st.pos re src.input in
      let match_start = Re.Group.start groups 0 in
      let match_end = Re.Group.stop groups 0 in
      if match_start <> st.pos then
        raise (Parse_error (st.pos, "regex match failed"))
      else if match_end = src.input_len && not src.eof then begin
        ignore (try_refill src);
        st.input <- src.input;
        loop ()
      end
      else begin
        let matched = Re.Group.get groups 0 in
        st.pos <- match_end;
        st.input <- src.input;
        matched
      end
    with Not_found ->
      if st.pos >= src.input_len && not src.eof then begin
        ignore (try_refill src);
        st.input <- src.input;
        loop ()
      end
      else raise (Parse_error (st.pos, "regex match failed"))
  in
  loop ()

let handle_end_of_input_source src st =
  if st.pos = src.input_len && not src.eof then ignore (try_refill src);
  if st.pos <> src.input_len then
    raise (Parse_error (st.pos, "expected end of input"))

let run_deep_source (type e) ~max_depth (handler : e exn_handler) (src : source)
    (parser : unit -> 'a) : ('a, e) result =
  let st = { input = src.input; pos = 0 } in
  let nest_depth = ref 0 in
  let sync_to_source () = st.input <- src.input in
  let rec go : 'b. (unit -> 'b) -> ('b, e) result =
   fun p ->
    match p () with
    | v -> Ok v
    | exception exn -> handler.handle exn
    | effect Position, k -> Effect.Deep.continue k st.pos
    | effect Consume s, k -> (
        try
          ignore (ensure_bytes src st.pos (String.length s));
          sync_to_source ();
          match handle_consume st src.input_len s with
          | v -> Effect.Deep.continue k v
          | exception exn -> Effect.Deep.discontinue k exn
        with exn -> Effect.Deep.discontinue k exn)
    | effect Satisfy (pred, label), k -> (
        try
          ignore (ensure_bytes src st.pos 1);
          sync_to_source ();
          match handle_satisfy st src.input_len pred label with
          | v -> Effect.Deep.continue k v
          | exception exn -> Effect.Deep.discontinue k exn
        with exn -> Effect.Deep.discontinue k exn)
    | effect Take_while pred, k ->
        Effect.Deep.continue k (handle_take_while_source src st pred)
    | effect Take_while_span pred, k ->
        Effect.Deep.continue k (handle_take_while_span_source src st pred)
    | effect Skip_while pred, k ->
        handle_skip_while_source src st pred;
        Effect.Deep.continue k ()
    | effect Skip_while_then_char (pred, c), k -> (
        match handle_skip_while_then_char_source src st pred c with
        | () -> Effect.Deep.continue k ()
        | exception exn -> Effect.Deep.discontinue k exn)
    | effect Fused_sep_take (ws_pred, sep_char, take_pred), k -> (
        match
          handle_fused_sep_take_source src st ws_pred sep_char take_pred
        with
        | v -> Effect.Deep.continue k v
        | exception exn -> Effect.Deep.discontinue k exn)
    | effect Sep_by_take (ws_pred, sep_char, take_pred), k ->
        Effect.Deep.continue k
          (handle_sep_by_take_source src st ws_pred sep_char take_pred)
    | effect Sep_by_take_span (ws_pred, sep_char, take_pred), k ->
        Effect.Deep.continue k
          (handle_sep_by_take_span_source src st ws_pred sep_char take_pred)
    | effect Match_re re, k -> (
        match handle_match_regex_source src st re with
        | v -> Effect.Deep.continue k v
        | exception exn -> Effect.Deep.discontinue k exn)
    | effect Greedy_many p, k ->
        let acc = ref [] in
        let saved = ref st.pos in
        let failed = ref false in
        while not !failed do
          saved := st.pos;
          match go (Obj.magic p) with
          | Ok v -> acc := Obj.magic v :: !acc
          | Error _ ->
              st.pos <- !saved;
              sync_to_source ();
              failed := true
        done;
        Effect.Deep.continue k (Obj.magic (List.rev !acc))
    | effect Choose (left, right), k -> (
        let saved = st.pos in
        match go (Obj.magic left) with
        | Ok v -> Effect.Deep.continue k (Obj.magic v)
        | Error _ -> (
            st.pos <- saved;
            sync_to_source ();
            match go (Obj.magic right) with
            | Ok v -> Effect.Deep.continue k (Obj.magic v)
            | Error e ->
                Effect.Deep.discontinue k (User_error (e.pos, Obj.repr e.error))
            ))
    | effect Fail msg, k ->
        Effect.Deep.discontinue k (Parse_error (st.pos, msg))
    | effect Error_val e, k ->
        Effect.Deep.discontinue k (User_error (st.pos, Obj.repr e))
    | effect Look_ahead p, k -> (
        let saved = st.pos in
        match go (Obj.magic p) with
        | Ok v ->
            st.pos <- saved;
            sync_to_source ();
            Effect.Deep.continue k (Obj.magic v)
        | Error e ->
            st.pos <- saved;
            sync_to_source ();
            Effect.Deep.discontinue k (User_error (e.pos, Obj.repr e.error)))
    | effect Rec_ p, k ->
        if !nest_depth >= max_depth then
          Effect.Deep.discontinue k
            (Depth_limit_exceeded
               ( st.pos,
                 Printf.sprintf "maximum nesting depth %d exceeded" max_depth ))
        else begin
          incr nest_depth;
          match go (Obj.magic p) with
          | Ok v ->
              decr nest_depth;
              Effect.Deep.continue k (Obj.magic v)
          | Error e ->
              decr nest_depth;
              Effect.Deep.discontinue k (User_error (e.pos, Obj.repr e.error))
        end
    | effect End_of_input, k -> (
        match handle_end_of_input_source src st with
        | () -> Effect.Deep.continue k ()
        | exception exn -> Effect.Deep.discontinue k exn)
  in
  go parser

module Source = struct
  type t = source

  let of_string s =
    {
      input = s;
      input_len = String.length s;
      read = (fun _ _ _ -> 0);
      eof = true;
      tmp = Bytes.empty;
    }

  let of_channel ?(buf_size = default_buf_size) ic =
    {
      input = "";
      input_len = 0;
      read = (fun buf off len -> input ic buf off len);
      eof = false;
      tmp = Bytes.create buf_size;
    }

  let of_function read =
    {
      input = "";
      input_len = 0;
      read;
      eof = false;
      tmp = Bytes.create default_buf_size;
    }
end

let parse_source ?(max_depth = 128) (src : Source.t) (parser : unit -> 'a) :
    ('a, 'e) result =
  match
    run_deep_source ~max_depth
      {
        handle =
          (function
          | Parse_error (pos, msg) -> Error { pos; error = `Expected msg }
          | User_error (pos, obj) -> Error { pos; error = Obj.obj obj }
          | e -> raise e);
      }
      src parser
  with
  | result -> result
  | exception Depth_limit_exceeded (pos, msg) ->
      Error { pos; error = `Expected msg }

(* }}} *)

(* {{{ Combinators *)

let[@inline] consume s = Effect.perform (Consume s)
let[@inline] satisfy pred ~label = Effect.perform (Satisfy (pred, label))
let[@inline] char c = satisfy (Char.equal c) ~label:(String.make 1 c)
let[@inline] match_regex re = Effect.perform (Match_re re)
let[@inline] take_while pred = Effect.perform (Take_while pred)

let[@inline] take_while1 pred ~label =
  let s = take_while pred in
  if String.length s = 0 then Effect.perform (Fail label) else s

let[@inline] skip_while pred = Effect.perform (Skip_while pred)

let[@inline] skip_while_then_char pred c =
  Effect.perform (Skip_while_then_char (pred, c))

let[@inline] sep_by_take ws_pred sep_char take_pred =
  Effect.perform (Sep_by_take (ws_pred, sep_char, take_pred))

let[@inline] take_while_span pred = Effect.perform (Take_while_span pred)

let[@inline] sep_by_take_span ws_pred sep_char take_pred =
  Effect.perform (Sep_by_take_span (ws_pred, sep_char, take_pred))

let[@inline] fused_sep_take ws_pred sep_char take_pred =
  Effect.perform (Fused_sep_take (ws_pred, sep_char, take_pred))

let[@inline] fail msg = Effect.perform (Fail msg)
let[@inline] error e = Effect.perform (Error_val e)
let[@inline] position () = Effect.perform Position
let[@inline] end_of_input () = Effect.perform End_of_input
let[@inline] or_ p q () = Effect.perform (Choose (p, q))
let[@inline] look_ahead p = Effect.perform (Look_ahead p)
let[@inline] rec_ p = Effect.perform (Rec_ p)
let[@inline] many (p : unit -> 'a) () : 'a list = Effect.perform (Greedy_many p)

let[@inline] many1 (p : unit -> 'a) () : 'a list =
  let first = p () in
  let rest = many p () in
  first :: rest

let sep_by (p : unit -> 'a) (sep : unit -> 'b) () : 'a list =
  or_
    (fun () ->
      let first = p () in
      let rest =
        many
          (fun () ->
            let _ = sep () in
            p ())
          ()
      in
      first :: rest)
    (fun () -> [])
    ()

let sep_by1 (p : unit -> 'a) (sep : unit -> 'b) () : 'a list =
  let first = p () in
  let rest =
    many
      (fun () ->
        let _ = sep () in
        p ())
      ()
  in
  first :: rest

let between (open_ : unit -> 'a) (close_ : unit -> 'b) (p : unit -> 'c) () : 'c
    =
  let _ = open_ () in
  let value = p () in
  let _ = close_ () in
  value

let end_by (p : unit -> 'a) (sep : unit -> 'b) () : 'a list =
  many
    (fun () ->
      let value = p () in
      let _ = sep () in
      value)
    ()

let end_by1 (p : unit -> 'a) (sep : unit -> 'b) () : 'a list =
  let first =
    let value = p () in
    let _ = sep () in
    value
  in
  first :: end_by p sep ()

let chainl1 (p : unit -> 'a) (op : unit -> 'a -> 'a -> 'a) () : 'a =
  let first = p () in
  let rest =
    many
      (fun () ->
        let f = op () in
        let rhs = p () in
        (f, rhs))
      ()
  in
  List.fold_left (fun acc (f, rhs) -> f acc rhs) first rest

let chainl (p : unit -> 'a) (op : unit -> 'a -> 'a -> 'a) (default : 'a) () : 'a
    =
  or_ (chainl1 p op) (fun () -> default) ()

let rec chainr1 (p : unit -> 'a) (op : unit -> 'a -> 'a -> 'a) () : 'a =
  let first = p () in
  or_
    (fun () ->
      let f = op () in
      let rhs = chainr1 p op () in
      f first rhs)
    (fun () -> first)
    ()

let chainr (p : unit -> 'a) (op : unit -> 'a -> 'a -> 'a) (default : 'a) () : 'a
    =
  or_ (chainr1 p op) (fun () -> default) ()

let optional (p : unit -> 'a) () : 'a option =
  or_ (fun () -> Some (p ())) (fun () -> None) ()

let count n (p : unit -> 'a) () : 'a list =
  let rec loop acc i =
    if i <= 0 then List.rev acc else loop (p () :: acc) (i - 1)
  in
  loop [] n

let digit () =
  let c = satisfy (fun c -> c >= '0' && c <= '9') ~label:"digit" in
  Char.code c - Char.code '0'

let letter () =
  satisfy
    (fun c -> (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z'))
    ~label:"letter"

let[@inline always] is_whitespace c =
  c = ' ' || c = '\t' || c = '\n' || c = '\r'

let[@inline] whitespace () = take_while is_whitespace
let[@inline] whitespace1 () = take_while1 is_whitespace ~label:"whitespace"
let[@inline] skip_whitespace () = skip_while is_whitespace

let alphanum () =
  satisfy
    (fun c ->
      (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9'))
    ~label:"alphanumeric"

let any_char () = satisfy (fun _ -> true) ~label:"any character"
let expect msg p = try p () with _ -> fail msg

let one_of parsers () =
  let rec try_all = function
    | [] -> fail "no alternative matched"
    | [ p ] -> p ()
    | p :: rest -> or_ p (fun () -> try_all rest) ()
  in
  try_all parsers

let one_of_labeled labeled_parsers () =
  let labels = List.map fst labeled_parsers in
  let parsers = List.map snd labeled_parsers in
  try one_of parsers ()
  with _ ->
    let expected = String.concat ", " labels in
    fail (Printf.sprintf "expected one of: %s" expected)

(* }}} *)
