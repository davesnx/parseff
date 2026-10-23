type span = { buf : string; off : int; len : int }

let one_char_strings = Array.init 256 (fun i -> String.make 1 (Char.chr i))

let[@inline always] string_slice inp off len total_len =
  if len = 0 then
    ""
  else if len = 1 then
    Array.unsafe_get one_char_strings (Char.code (String.unsafe_get inp off))
  else if off = 0 && len = total_len then
    inp
  else
    String.sub inp off len

let[@inline always] span_to_string s =
  string_slice s.buf s.off s.len (String.length s.buf)

type location = { offset : int; line : int; col : int }

type endian = Big | Little

type ('a, 'e) result = Ok of 'a | Error of { pos : int; error : 'e }
type 'd diagnostic = { pos : int; diagnostic : 'd }

type ('e, 'd) error_with_diagnostics = {
  pos : int;
  error : 'e;
  diagnostics : 'd diagnostic list;
}

type ('a, 'e, 'd) result_with_diagnostics =
  ('a * 'd diagnostic list, ('e, 'd) error_with_diagnostics) Stdlib.result

type error_msg =
  | Msg of string
  | Expected_string of string
  | Expected_char of char
  | Expected_label of string
  | Expected_value
  | Composed of error_msg * error_msg
  | No_alternative

let rec format_error_msg = function
  | Msg s ->
      s
  | Expected_string s ->
      "expected \"" ^ String.escaped s ^ "\""
  | Expected_char c ->
      "expected '" ^ String.make 1 c ^ "'"
  | Expected_label l ->
      "expected " ^ l
  | Expected_value ->
      "expected value"
  | Composed (l, r) ->
      format_error_msg l ^ " or " ^ format_error_msg r
  | No_alternative ->
      "no alternative matched"

exception Parse_error of int * error_msg
exception Unexpected_eof of int
exception User_error of int * Obj.t
exception User_failure of int * string
exception Propagated_error of int * Obj.t
exception Depth_limit_exceeded of int * string

type line_index = {
  mutable starts : int array;
  mutable count : int;
  mutable scanned_up_to : int;
  mutable line_base : int;
}

type rewind_frame_kind = Choice | Repeat | Barrier | Last_branch

type rewind_frame = {
  kind : rewind_frame_kind;
  start_pos : int;
  saved_diagnostics_rev : (int * Obj.t) list;
  mutable committed : bool;
}

type backtrack_window_error = { limit : int; oldest : int }

exception Backtrack_window_exceeded of int * backtrack_window_error

type source = {
  initial : string;
  read : bytes -> int -> int -> int;
  mutable eof : bool;
  tmp : bytes;
  mutable window : int;
}

(* Absolute offset [p] is [input.[p - base]]. Bytes at absolute offsets
   [base, input_len) are buffered. [input] is never mutated, only replaced, so
   slices taken from it stay valid after a refill. Memory input has
   [base = 0] and no source. *)
type state = {
  mutable input : string;
  mutable pos : int;
  mutable input_len : int;
  mutable base : int;
  max_depth : int;
  mutable current_depth : int;
  mutable diagnostics_rev : (int * Obj.t) list;
  mutable line_index : line_index option;
  source : source option;
  mutable rewind_frames : rewind_frame list;
  mutable active : bool;
}

let[@inline always] imin (a : int) b =
  if a < b then
    a
  else
    b
let[@inline always] imax (a : int) b =
  if a > b then
    a
  else
    b
let default_buf_size = 4096
let default_backtrack_window = 65_536

let check_backtrack_window w =
  if w < 2 then
    invalid_arg "Parseff.parse_source: backtrack_window must be at least 2"

module Source = struct
  type t = source

  let make ~initial ~read ~eof ~tmp : t =
    { initial; read; eof; tmp; window = default_backtrack_window }

  let of_string s =
    make ~initial:s ~read:(fun _ _ _ -> 0) ~eof:true ~tmp:Bytes.empty

  let of_channel ?(buf_size = default_buf_size) ic =
    if buf_size < 1 then
      invalid_arg "Parseff.Source.of_channel: buf_size must be at least 1";
    make ~initial:""
      ~read:(fun buf off len -> input ic buf off len)
      ~eof:false ~tmp:(Bytes.create buf_size)

  let of_function read =
    make ~initial:"" ~read ~eof:false ~tmp:(Bytes.create default_buf_size)

  let of_chunks read_chunk =
    let pending = ref "" in
    let pending_pos = ref 0 in
    of_function (fun buf off len ->
        if !pending_pos < String.length !pending then begin
          let available = String.length !pending - !pending_pos in
          let n = min len available in
          Bytes.blit_string !pending !pending_pos buf off n;
          pending_pos := !pending_pos + n;
          n
        end else
          let rec next () =
            match read_chunk () with
            | None ->
                0
            | Some s when String.length s = 0 ->
                next ()
            | Some chunk ->
                pending := chunk;
                pending_pos := 0;
                let n = min len (String.length chunk) in
                Bytes.blit_string chunk 0 buf off n;
                pending_pos := n;
                n
          in
          next ()
    )

  let of_seq seq =
    let r = ref seq in
    of_chunks (fun () ->
        match !r () with
        | Seq.Nil ->
            None
        | Seq.Cons (s, rest) ->
            r := rest;
            Some s
    )
end

module State = struct
  type t = state

  let make ~input ~max_depth ~source : t =
    {
      input;
      pos = 0;
      input_len = String.length input;
      base = 0;
      max_depth;
      current_depth = 0;
      diagnostics_rev = [];
      line_index = None;
      source;
      rewind_frames = [];
      active = false;
    }

  let for_input ~input ~max_depth = make ~input ~max_depth ~source:None

  let for_source ~(src : source) ~max_depth ~backtrack_window =
    src.window <- backtrack_window;
    make ~input:src.initial ~max_depth ~source:(Some src)

  let none = for_input ~input:"" ~max_depth:0

  let key : t Domain.DLS.key = Domain.DLS.new_key (fun () -> none)

  type slot = { owner : int; state : t }

  let cache : slot ref = ref { owner = -1; state = none }

  let[@inline always] install (t : t) =
    Domain.DLS.set key t;
    cache := { owner = (Domain.self () :> int); state = t }

  let[@inline always] current () =
    let slot = !cache in
    if slot.owner = (Domain.self () :> int) then
      slot.state
    else begin
      let t = Domain.DLS.get key in
      cache := { owner = (Domain.self () :> int); state = t };
      t
    end

  let run (t : t) f =
    let previous = current () in
    install t;
    t.active <- true;
    let leave () =
      t.active <- false;
      install
        ( if previous.active then
            previous
          else
            none
        )
    in
    match f () with
    | v ->
        leave ();
        v
    | exception exn ->
        let bt = Printexc.get_raw_backtrace () in
        leave ();
        Printexc.raise_with_backtrace exn bt

  let[@inline always] get_diagnostics_rev (t : t) = t.diagnostics_rev

  let push_rewind_frame (t : t) kind =
    let frame =
      {
        kind;
        start_pos = t.pos;
        saved_diagnostics_rev = t.diagnostics_rev;
        committed = false;
      }
    in
    t.rewind_frames <- frame :: t.rewind_frames;
    frame

  let pop_rewind_frame (t : t) frame =
    match t.rewind_frames with
    | top :: rest when top == frame ->
        t.rewind_frames <- rest
    | _ ->
        invalid_arg "Parseff.State.pop_rewind_frame"

  let commit_nearest (t : t) =
    let loop = function
      | [] ->
          ()
      | { kind = Barrier; _ } :: _ ->
          ()
      | frame :: _ ->
          frame.committed <- true
    in
    loop t.rewind_frames

  let reclaim_floor (t : t) ~keep_from =
    let rec go floor = function
      | [] ->
          floor
      | frame :: rest ->
          if
            (frame.kind = Barrier
            || (frame.kind <> Last_branch && not frame.committed)
            )
            && frame.start_pos < floor
          then
            go frame.start_pos rest
          else
            go floor rest
    in
    go keep_from t.rewind_frames
end

exception Not_in_parse_context

let () =
  Printexc.register_printer (function
    | Not_in_parse_context ->
        Some
          "Parseff: parser called outside of a parse context. Parser \
           combinators can only be used inside a function passed to \
           Parseff.parse, Parseff.parse_until_end, Parseff.parse_source, or \
           Parseff.parse_source_until_end."
    | _ ->
        None
    )

let[@inline always] get_state () =
  let st = State.current () in
  if st == State.none then raise Not_in_parse_context;
  st

let create_line_index () =
  let starts = Array.make 64 0 in
  starts.(0) <- 0;
  { starts; count = 1; scanned_up_to = 0; line_base = 0 }

let extend_line_index idx ?(base_offset = 0) input up_to =
  let i = ref idx.scanned_up_to in
  while !i < up_to do
    let rel = !i - base_offset in
    if
      rel >= 0
      && rel < String.length input
      && String.unsafe_get input rel = '\n'
    then begin
      if idx.count >= Array.length idx.starts then begin
        let new_arr = Array.make (idx.count * 2) 0 in
        Array.blit idx.starts 0 new_arr 0 idx.count;
        idx.starts <- new_arr
      end;
      idx.starts.(idx.count) <- !i + 1;
      idx.count <- idx.count + 1
    end;
    incr i
  done;
  idx.scanned_up_to <- imax idx.scanned_up_to up_to

let find_line idx offset =
  let lo = ref 0 and hi = ref (idx.count - 1) in
  while !lo < !hi do
    let mid = !lo + ((!hi - !lo + 1) / 2) in
    if idx.starts.(mid) <= offset then
      lo := mid
    else
      hi := mid - 1
  done;
  !lo

let ensure_line_index_preserved_through st up_to =
  if up_to > 0 then begin
    let idx =
      match st.line_index with
      | Some idx ->
          idx
      | None ->
          let idx = create_line_index () in
          st.line_index <- Some idx;
          idx
    in
    extend_line_index idx ~base_offset:st.base st.input up_to;
    let line = find_line idx up_to in
    if line > 0 then begin
      Array.blit idx.starts line idx.starts 0 (idx.count - line);
      idx.count <- idx.count - line;
      idx.line_base <- idx.line_base + line
    end
  end

let[@inline never] handle_location st =
  let idx =
    match st.line_index with
    | Some idx ->
        idx
    | None ->
        let idx = create_line_index () in
        st.line_index <- Some idx;
        idx
  in
  let up_to = min st.pos st.input_len in
  extend_line_index idx ~base_offset:st.base st.input up_to;
  let line_idx = find_line idx st.pos in
  {
    offset = st.pos;
    line = idx.line_base + line_idx + 1;
    col = st.pos - idx.starts.(line_idx) + 1;
  }

let[@inline never] append st ~floor chunk n =
  let floor = imax 0 (floor - 1) in
  let cur_len = st.input_len - st.base in
  let drop = imin (imax 0 (floor - st.base)) cur_len in
  if drop > 0 then ensure_line_index_preserved_through st floor;
  let kept = cur_len - drop in
  let buf = Bytes.create (kept + n) in
  Bytes.blit_string st.input drop buf 0 kept;
  Bytes.blit chunk 0 buf kept n;
  st.input <- Bytes.unsafe_to_string buf;
  st.base <- st.base + drop;
  st.input_len <- st.input_len + n

let[@inline never] read_source st src buf off len =
  let n = src.read buf off len in
  State.install st;
  if n < 0 || n > len then
    invalid_arg
      (Printf.sprintf "Parseff.Source: read returned %d, expected 0 to %d" n len);
  n

let[@inline never] rec fill st ~keep_from need =
  match st.source with
  | None ->
      false
  | Some src ->
      if st.input_len >= need then
        true
      else if src.eof then
        false
      else
        let floor = State.reclaim_floor st ~keep_from in
        let limit = src.window in
        let max_bytes =
          imin (limit - (st.input_len - floor)) (Bytes.length src.tmp)
        in
        if max_bytes <= 0 then
          raise (Backtrack_window_exceeded (need, { limit; oldest = floor }))
        else
          let n = read_source st src src.tmp 0 max_bytes in
          if n = 0 then begin
            src.eof <- true;
            false
          end else begin
            append st ~floor src.tmp n;
            fill st ~keep_from need
          end

let[@inline always] have st n =
  n <= st.input_len - st.pos
  || st.source != None
     && fill st ~keep_from:st.pos
          ( if n > max_int - st.pos then
              max_int
            else
              st.pos + n
          )

let no_keep = max_int

let[@inline never] refill_at st keep_from p =
  match st.source with
  | None ->
      false
  | Some _ ->
      let keep = imin keep_from p in
      st.pos <- keep;
      fill st ~keep_from:keep (st.input_len + 1)

let[@inline always] char_at st pos = String.unsafe_get st.input (pos - st.base)

let[@inline always] slice st off len =
  string_slice st.input (off - st.base) len (st.input_len - st.base)

let[@inline always] span st off len =
  match st.source with
  | None ->
      { buf = st.input; off; len }
  | Some _ ->
      let s = slice st off len in
      { buf = s; off = 0; len = String.length s }

(* Polymorphic record so handle_exn can be called at different types. *)
type 'e exn_handler = { handle : 'a. exn -> ('a, 'e) result } [@@unboxed]

let[@inline never] handle_consume st s =
  let len = String.length s in
  if len = 1 then begin
    (* O5: Special-case single-char strings -- skip loop overhead *)
      let c = String.unsafe_get s 0 in
      if have st 1 then
        if char_at st st.pos = c then begin
          st.pos <- st.pos + 1;
          s
        end else
          raise (Parse_error (st.pos, Expected_string s))
      else
        raise (Unexpected_eof st.pos)
  end else begin
    let complete = have st len in
    let pos = st.pos in
    let avail =
      if complete then
        len
      else
        st.input_len - pos
    in
    let rec check i =
      if i >= avail then
        true
      else if char_at st (pos + i) = String.unsafe_get s i then
        check (i + 1)
      else
        false
    in
    if not (check 0) then
      raise (Parse_error (pos, Expected_string s))
    else if complete then begin
      st.pos <- pos + len;
      s
    end else
      raise (Unexpected_eof pos)
  end

let[@inline] handle_satisfy st pred label =
  if have st 1 then
    let c = char_at st st.pos in
    if pred c then begin
      st.pos <- st.pos + 1;
      c
    end else
      raise (Parse_error (st.pos, Expected_label label))
  else
    raise (Unexpected_eof st.pos)

(* dedicated char handler it avoids closure + String.make allocation *)
let[@inline] handle_match_char st c =
  if have st 1 then
    let ch = char_at st st.pos in
    if ch = c then begin
      st.pos <- st.pos + 1;
      c
    end else
      raise (Parse_error (st.pos, Expected_char c))
  else
    raise (Unexpected_eof st.pos)

let[@inline never] scan_while_refilling st ~keep_from start pred =
  let p = ref start in
  let continue_loop = ref true in
  while !continue_loop do
    let base = st.base in
    let inp = st.input in
    let len = st.input_len - base in
    let i = ref (!p - base) in
    while !i < len && pred (String.unsafe_get inp !i) do
      incr i
    done;
    p := !i + base;
    if !i < len || not (refill_at st keep_from !p) then continue_loop := false
  done;
  !p

let[@inline always] scan_while st ~keep_from start pred =
  let base = st.base in
  let inp = st.input in
  let len = st.input_len - base in
  let i = ref (start - base) in
  while !i < len && pred (String.unsafe_get inp !i) do
    incr i
  done;
  if !i < len then
    !i + base
  else
    match st.source with
    | None ->
        !i + base
    | Some _ ->
        scan_while_refilling st ~keep_from (!i + base) pred

let[@inline always] handle_take_while st pred =
  let start = st.pos in
  let end_pos = scan_while st ~keep_from:start start pred in
  st.pos <- end_pos;
  slice st start (end_pos - start)

let[@inline always] handle_take_while_span st pred =
  let start = st.pos in
  let end_pos = scan_while st ~keep_from:start start pred in
  st.pos <- end_pos;
  span st start (end_pos - start)

let[@inline always] handle_skip_while st pred =
  st.pos <- scan_while st ~keep_from:no_keep st.pos pred

let[@inline never] handle_skip_while_then_char st pred c =
  let pos = scan_while st ~keep_from:no_keep st.pos pred in
  if pos < st.input_len && char_at st pos = c then begin
    st.pos <- pos + 1
  end else begin
    st.pos <- pos;
    if pos >= st.input_len then
      raise (Unexpected_eof pos)
    else
      raise (Parse_error (pos, Expected_char c))
  end

let[@inline never] handle_fused_sep_take st ws_pred sep_char take_pred =
  let pos = scan_while st ~keep_from:no_keep st.pos ws_pred in
  if pos < st.input_len && char_at st pos = sep_char then begin
    let pos2 = scan_while st ~keep_from:no_keep (pos + 1) ws_pred in
    let end_pos = scan_while st ~keep_from:pos2 pos2 take_pred in
    if end_pos > pos2 then begin
      st.pos <- end_pos;
      slice st pos2 (end_pos - pos2)
    end else begin
      st.pos <- end_pos;
      if end_pos >= st.input_len then
        raise (Unexpected_eof end_pos)
      else
        raise (Parse_error (end_pos, Expected_value))
    end
  end else begin
    st.pos <- pos;
    if pos >= st.input_len then
      raise (Unexpected_eof pos)
    else
      raise (Parse_error (pos, Expected_char sep_char))
  end

let[@inline always] handle_sep_by_take st ws_pred sep_char take_pred =
  let start = st.pos in
  let first_end = scan_while st ~keep_from:start start take_pred in
  if first_end <= start then begin
    st.pos <- first_end;
    []
  end else begin
    let acc = ref [ slice st start (first_end - start) ] in
    let pos = ref first_end in
    let continue_loop = ref true in
    while !continue_loop do
      let ws_end = scan_while st ~keep_from:!pos !pos ws_pred in
      if ws_end < st.input_len && char_at st ws_end = sep_char then begin
        let after_ws = scan_while st ~keep_from:!pos (ws_end + 1) ws_pred in
        let elem_end = scan_while st ~keep_from:!pos after_ws take_pred in
        if elem_end > after_ws then begin
          acc := slice st after_ws (elem_end - after_ws) :: !acc;
          pos := elem_end
        end else
          continue_loop := false
      end else
        continue_loop := false
    done;
    st.pos <- !pos;
    List.rev !acc
  end

let[@inline always] handle_sep_by_take_span st ws_pred sep_char take_pred =
  let start = st.pos in
  let first_end = scan_while st ~keep_from:start start take_pred in
  if first_end <= start then begin
    st.pos <- first_end;
    []
  end else begin
    let acc = ref [ span st start (first_end - start) ] in
    let pos = ref first_end in
    let continue_loop = ref true in
    while !continue_loop do
      let ws_end = scan_while st ~keep_from:!pos !pos ws_pred in
      if ws_end < st.input_len && char_at st ws_end = sep_char then begin
        let after_ws = scan_while st ~keep_from:!pos (ws_end + 1) ws_pred in
        let elem_end = scan_while st ~keep_from:!pos after_ws take_pred in
        if elem_end > after_ws then begin
          acc := span st after_ws (elem_end - after_ws) :: !acc;
          pos := elem_end
        end else
          continue_loop := false
      end else
        continue_loop := false
    done;
    st.pos <- !pos;
    List.rev !acc
  end

let regex_failed = Msg "regex match failed"
let invalid_utf8_msg = Msg "invalid UTF-8"

let[@inline never] buffer_for_regex st =
  match st.source with
  | Some src when (not src.eof) && st.input_len - st.pos < src.window / 2 ->
      let floor = State.reclaim_floor st ~keep_from:st.pos in
      let room = src.window - (st.input_len - floor) in
      if room <= 0 then
        Some (st.pos + (src.window / 2), { limit = src.window; oldest = floor })
      else begin
        let chunk = Bytes.create room in
        let got = ref 0 in
        let want = imin room ((src.window / 2) - (st.input_len - st.pos)) in
        while !got < want && not src.eof do
          let n = read_source st src chunk !got (room - !got) in
          if n = 0 then
            src.eof <- true
          else
            got := !got + n
        done;
        if !got > 0 then append st ~floor chunk !got;
        None
      end
  | _ ->
      None

let[@inline never] handle_match_regex st re =
  let pinned = buffer_for_regex st in
  let raise_if_pinned () =
    match pinned with
    | Some (need, info) ->
        raise (Backtrack_window_exceeded (need, info))
    | None ->
        ()
  in
  match Re.exec ~pos:(st.pos - st.base) re st.input with
  | groups ->
      let match_start = st.base + Re.Group.start groups 0 in
      let match_end = st.base + Re.Group.stop groups 0 in
      if match_start <> st.pos then begin
        raise_if_pinned ();
        raise (Parse_error (st.pos, regex_failed))
      end else begin
        if match_end >= st.input_len then raise_if_pinned ();
        st.pos <- match_end;
        Re.Group.get groups 0
      end
  | exception Not_found ->
      raise_if_pinned ();
      if st.pos >= st.input_len then
        raise (Unexpected_eof st.pos)
      else
        raise (Parse_error (st.pos, regex_failed))

let expected_end_of_input = Msg "expected end of input"

let[@inline never] handle_any_uint8 st =
  if have st 1 then begin
    let v = String.get_uint8 st.input (st.pos - st.base) in
    st.pos <- st.pos + 1;
    v
  end else
    raise (Unexpected_eof st.pos)

let[@inline never] handle_any_int8 st =
  if have st 1 then begin
    let v = String.get_int8 st.input (st.pos - st.base) in
    st.pos <- st.pos + 1;
    v
  end else
    raise (Unexpected_eof st.pos)

let[@inline never] handle_any_int16 st endian =
  if have st 2 then begin
    let pos = st.pos - st.base in
    let v =
      match endian with
      | Big ->
          String.get_int16_be st.input pos
      | Little ->
          String.get_int16_le st.input pos
    in
    st.pos <- st.pos + 2;
    v
  end else
    raise (Unexpected_eof st.pos)

let[@inline never] handle_any_int32 st endian =
  if have st 4 then begin
    let pos = st.pos - st.base in
    let v =
      match endian with
      | Big ->
          String.get_int32_be st.input pos
      | Little ->
          String.get_int32_le st.input pos
    in
    st.pos <- st.pos + 4;
    v
  end else
    raise (Unexpected_eof st.pos)

let[@inline never] handle_any_int64 st endian =
  if have st 8 then begin
    let pos = st.pos - st.base in
    let v =
      match endian with
      | Big ->
          String.get_int64_be st.input pos
      | Little ->
          String.get_int64_le st.input pos
    in
    st.pos <- st.pos + 8;
    v
  end else
    raise (Unexpected_eof st.pos)

let[@inline never] handle_take st n =
  if have st n then begin
    let s = slice st st.pos n in
    st.pos <- st.pos + n;
    s
  end else
    raise (Unexpected_eof st.pos)

exception Invalid_utf8 of int

let[@inline] uchar_label u =
  let i = Uchar.to_int u in
  if i < 0x80 then
    Printf.sprintf "%C" (Char.chr i)
  else
    Printf.sprintf "U+%04X" i

let[@inline] utf8_sequence_length lead =
  if lead < 0xC2 then
    1
  else if lead < 0xE0 then
    2
  else if lead < 0xF0 then
    3
  else if lead < 0xF5 then
    4
  else
    1

let[@inline never] utf8_fill st keep_from p =
  match st.source with
  | None ->
      ()
  | Some _ ->
      let keep = imin keep_from p in
      st.pos <- keep;
      if fill st ~keep_from:keep (p + 1) then begin
        let needed = utf8_sequence_length (Char.code (char_at st p)) in
        if needed > 1 then ignore (fill st ~keep_from:keep (p + needed))
      end

let[@inline] decode_uchar st ~keep_from p =
  if p + 4 > st.input_len then utf8_fill st keep_from p;
  if p >= st.input_len then raise (Unexpected_eof p);
  let d = String.get_utf_8_uchar st.input (p - st.base) in
  if not (Uchar.utf_decode_is_valid d) then raise (Invalid_utf8 p);
  d

let[@inline never] handle_satisfy_uchar st pred label =
  let d = decode_uchar st ~keep_from:st.pos st.pos in
  let u = Uchar.utf_decode_uchar d in
  if pred u then begin
    st.pos <- st.pos + Uchar.utf_decode_length d;
    u
  end else
    raise (Parse_error (st.pos, Expected_label label))

let[@inline never] scan_uchars inp start limit pred =
  let pos = ref start in
  let continue_loop = ref true in
  while !pos < limit && !continue_loop do
    let b = Char.code (String.unsafe_get inp !pos) in
    if b < 0x80 then
      begin if pred (Uchar.unsafe_of_int b) then
        incr pos
      else
        continue_loop := false
    end
    else begin
      let d = String.get_utf_8_uchar inp !pos in
      if not (Uchar.utf_decode_is_valid d) then raise (Invalid_utf8 !pos);
      if pred (Uchar.utf_decode_uchar d) then
        pos := !pos + Uchar.utf_decode_length d
      else
        continue_loop := false
    end
  done;
  !pos

let[@inline never] scan_while_uchar_refilling st ~keep_from start pred =
  let p = ref start in
  let continue_loop = ref true in
  while !continue_loop do
    let base = st.base in
    let limit = st.input_len - base - 3 in
    let i =
      try scan_uchars st.input (!p - base) limit pred
      with Invalid_utf8 i -> raise (Invalid_utf8 (i + base))
    in
    p := i + base;
    if i < limit then
      continue_loop := false
    else begin
      utf8_fill st keep_from !p;
      if !p < st.input_len then begin
        let d = String.get_utf_8_uchar st.input (!p - st.base) in
        if not (Uchar.utf_decode_is_valid d) then raise (Invalid_utf8 !p);
        if pred (Uchar.utf_decode_uchar d) then
          p := !p + Uchar.utf_decode_length d
        else
          continue_loop := false
      end else
        continue_loop := false
    end
  done;
  !p

let[@inline always] scan_while_uchar st ~keep_from start pred =
  match st.source with
  | None ->
      scan_uchars st.input start st.input_len pred
  | Some _ ->
      scan_while_uchar_refilling st ~keep_from start pred

let[@inline never] handle_take_while_uchar st pred =
  let start = st.pos in
  let end_pos = scan_while_uchar st ~keep_from:start start pred in
  st.pos <- end_pos;
  slice st start (end_pos - start)

let[@inline never] handle_take_while_span_uchar st pred =
  let start = st.pos in
  let end_pos = scan_while_uchar st ~keep_from:start start pred in
  st.pos <- end_pos;
  span st start (end_pos - start)

let[@inline never] handle_skip_while_uchar st pred =
  st.pos <- scan_while_uchar st ~keep_from:no_keep st.pos pred

let[@inline never] handle_skip_while_then_uchar st pred term =
  let pos = scan_while_uchar st ~keep_from:no_keep st.pos pred in
  if pos >= st.input_len then begin
    st.pos <- pos;
    raise (Unexpected_eof pos)
  end;
  let d = decode_uchar st ~keep_from:pos pos in
  let u = Uchar.utf_decode_uchar d in
  if Uchar.equal u term then
    st.pos <- pos + Uchar.utf_decode_length d
  else begin
    st.pos <- pos;
    raise (Parse_error (pos, Msg ("expected " ^ uchar_label term)))
  end

let[@inline always] pos_of_exn = function
  | Parse_error (p, _)
  | Unexpected_eof p
  | User_error (p, _)
  | Propagated_error (p, _) ->
      p
  | _ ->
      -1

let[@inline] compose_branch_errors left_exn right_exn =
  let lpos = pos_of_exn left_exn in
  let rpos = pos_of_exn right_exn in
  if lpos <> rpos then
    let exn =
      if lpos > rpos then
        left_exn
      else
        right_exn
    in
    match exn with
    | User_error (pos, obj) ->
        Propagated_error (pos, obj)
    | _ ->
        exn
  else
    match (left_exn, right_exn) with
    | Parse_error (_, lmsg), Parse_error (pos, rmsg) ->
        Parse_error (pos, Composed (lmsg, rmsg))
    | Parse_error _, Unexpected_eof _ ->
        left_exn
    | Unexpected_eof _, Parse_error _ ->
        right_exn
    | Unexpected_eof pos, Unexpected_eof _ ->
        Unexpected_eof pos
    | _, User_error (pos, obj) ->
        (* in case of being different exn, we 2nd user_error is propagated *)
        Propagated_error (pos, obj)
    | _, right ->
        right

let[@inline] turn_invalid_utf8_to_parse_error f =
  try f () with Invalid_utf8 pos -> raise (Parse_error (pos, invalid_utf8_msg))

let[@inline always] restore_rewind_frame st frame =
  st.pos <- frame.start_pos;
  st.diagnostics_rev <- frame.saved_diagnostics_rev

let[@inline] consume s =
  let st = get_state () in
  handle_consume st s

let[@inline] satisfy pred ~label =
  let st = get_state () in
  handle_satisfy st pred label

let[@inline] char c =
  let st = get_state () in
  handle_match_char st c

let[@inline] peek_char () =
  let st = get_state () in
  if have st 1 then
    Some (char_at st st.pos)
  else
    None

let[@inline] position () =
  let st = get_state () in
  st.pos

let[@inline] location () =
  let st = get_state () in
  handle_location st

let[@inline] end_of_input () =
  let st = get_state () in
  ignore (have st 1);
  if st.pos <> st.input_len then
    raise (Parse_error (st.pos, expected_end_of_input))

let take_while ?(at_least = 0) ?label pred =
  let st = get_state () in
  let s = handle_take_while st pred in
  if at_least > 0 && String.length s < at_least then begin
    raise
      (Parse_error
         (st.pos, Msg (match label with Some l -> l | None -> "take_while"))
      )
  end else
    s

let[@inline] skip_while pred =
  let st = get_state () in
  handle_skip_while st pred

let[@inline] skip_while_then_char pred c =
  let st = get_state () in
  handle_skip_while_then_char st pred c

let[@inline] take_while_span pred =
  let st = get_state () in
  handle_take_while_span st pred

let[@inline] sep_by_take ws_pred sep_char take_pred =
  let st = get_state () in
  handle_sep_by_take st ws_pred sep_char take_pred

let[@inline] sep_by_take_span ws_pred sep_char take_pred =
  let st = get_state () in
  handle_sep_by_take_span st ws_pred sep_char take_pred

let[@inline] fused_sep_take ws_pred sep_char take_pred =
  let st = get_state () in
  handle_fused_sep_take st ws_pred sep_char take_pred

let[@inline] match_regex re =
  let st = get_state () in
  handle_match_regex st re

let[@inline] fail msg =
  let st = get_state () in
  raise (User_failure (st.pos, msg))

let[@inline] error e =
  let st = get_state () in
  raise (User_error (st.pos, Obj.repr e))

let[@inline] warn diagnostic =
  let st = get_state () in
  st.diagnostics_rev <- (st.pos, Obj.repr diagnostic) :: st.diagnostics_rev

let[@inline] warn_at ~pos diagnostic =
  let st = get_state () in
  st.diagnostics_rev <- (pos, Obj.repr diagnostic) :: st.diagnostics_rev

let[@inline] commit () =
  let st = get_state () in
  State.commit_nearest st

let take n =
  if n < 0 then fail "Parseff.take: count must be non-negative";
  if n = 0 then
    ""
  else begin
    let st = get_state () in
    handle_take st n
  end

let or_ left right () =
  let st = get_state () in
  let frame = State.push_rewind_frame st Choice in
  match left () with
  | v ->
      State.pop_rewind_frame st frame;
      v
  | exception (User_failure _ as exn) ->
      State.pop_rewind_frame st frame;
      raise exn
  | exception left_exn -> (
      if pos_of_exn left_exn < 0 then begin
        State.pop_rewind_frame st frame;
        raise left_exn
      end;
      if frame.committed then begin
        State.pop_rewind_frame st frame;
        raise left_exn
      end;
      restore_rewind_frame st frame;
      State.pop_rewind_frame st frame;
      let right_frame = State.push_rewind_frame st Last_branch in
      match right () with
      | v ->
          State.pop_rewind_frame st right_frame;
          v
      | exception (User_failure _ as exn) ->
          State.pop_rewind_frame st right_frame;
          raise exn
      | exception right_exn ->
          State.pop_rewind_frame st right_frame;
          if pos_of_exn right_exn < 0 then raise right_exn;
          if right_frame.committed then
            raise right_exn
          else
            raise (compose_branch_errors left_exn right_exn)
    )

let look_ahead p =
  let st = get_state () in
  let frame = State.push_rewind_frame st Barrier in
  match p () with
  | v ->
      restore_rewind_frame st frame;
      State.pop_rewind_frame st frame;
      v
  | exception exn ->
      restore_rewind_frame st frame;
      State.pop_rewind_frame st frame;
      raise exn

let rec_ p =
  let st = get_state () in
  if st.current_depth >= st.max_depth then
    raise
      (Depth_limit_exceeded
         ( st.pos,
           Printf.sprintf "maximum nesting depth %d exceeded" st.max_depth
         )
      );
  st.current_depth <- st.current_depth + 1;
  match p () with
  | v ->
      st.current_depth <- st.current_depth - 1;
      v
  | exception exn ->
      st.current_depth <- st.current_depth - 1;
      raise exn

let many ?(at_least = 0) (p : unit -> 'a) () : 'a list =
  let st = get_state () in
  if at_least <= 0 then begin
    let acc = ref [] in
    let continue_ref = ref true in
    while !continue_ref do
      let frame = State.push_rewind_frame st Repeat in
      let start = st.pos in
      match p () with
      | v ->
          State.pop_rewind_frame st frame;
          if st.pos = start then
            continue_ref := false
          else
            acc := v :: !acc
      | exception (User_failure _ as exn) ->
          State.pop_rewind_frame st frame;
          raise exn
      | exception exn ->
          State.pop_rewind_frame st frame;
          if pos_of_exn exn < 0 then raise exn;
          if frame.committed then
            raise exn
          else begin
            restore_rewind_frame st frame;
            continue_ref := false
          end
    done;
    List.rev !acc
  end else begin
    let rec collect_required_rev acc n =
      if n <= 0 then
        acc
      else
        collect_required_rev (p () :: acc) (n - 1)
    in
    let required_rev = collect_required_rev [] at_least in
    (* Now greedily collect the rest *)
    let acc = ref [] in
    let continue_ref = ref true in
    while !continue_ref do
      let frame = State.push_rewind_frame st Repeat in
      let start = st.pos in
      match p () with
      | v ->
          State.pop_rewind_frame st frame;
          if st.pos = start then
            continue_ref := false
          else
            acc := v :: !acc
      | exception (User_failure _ as exn) ->
          State.pop_rewind_frame st frame;
          raise exn
      | exception exn ->
          State.pop_rewind_frame st frame;
          if pos_of_exn exn < 0 then raise exn;
          if frame.committed then
            raise exn
          else begin
            restore_rewind_frame st frame;
            continue_ref := false
          end
    done;
    List.rev_append required_rev (List.rev !acc)
  end

let sep_by ?(at_least = 0) (p : unit -> 'a) (sep : unit -> 'b) () : 'a list =
  let st = get_state () in
  if at_least <= 0 then (
    let start_frame = State.push_rewind_frame st Repeat in
    match p () with
    | first ->
        State.pop_rewind_frame st start_frame;
        let acc = ref [ first ] in
        let continue_ref = ref true in
        while !continue_ref do
          let frame = State.push_rewind_frame st Repeat in
          let start = st.pos in
          try
            let _ = sep () in
            let v = p () in
            State.pop_rewind_frame st frame;
            if st.pos = start then
              continue_ref := false
            else
              acc := v :: !acc
          with
          | User_failure _ as exn ->
              State.pop_rewind_frame st frame;
              raise exn
          | exn ->
              State.pop_rewind_frame st frame;
              if pos_of_exn exn < 0 then raise exn;
              if frame.committed then
                raise exn
              else begin
                restore_rewind_frame st frame;
                continue_ref := false
              end
        done;
        List.rev !acc
    | exception (User_failure _ as exn) ->
        State.pop_rewind_frame st start_frame;
        raise exn
    | exception exn ->
        State.pop_rewind_frame st start_frame;
        if pos_of_exn exn < 0 then raise exn;
        if start_frame.committed then
          raise exn
        else begin
          restore_rewind_frame st start_frame;
          []
        end
  ) else
    let first = p () in
    let sep_then_p () =
      let _ = sep () in
      p ()
    in
    let rest = many ~at_least:(at_least - 1) sep_then_p () in
    first :: rest

let between (open_ : unit -> 'a) (close_ : unit -> 'b) (p : unit -> 'c) () : 'c
    =
  let _ = open_ () in
  let value = p () in
  let _ = close_ () in
  value

let end_by ?(at_least = 0) (p : unit -> 'a) (sep : unit -> 'b) () : 'a list =
  many ~at_least
    (fun () ->
      let value = p () in
      let _ = sep () in
      value
    )
    ()

let fold_left_one_or_more (p : unit -> 'a) (op : unit -> 'a -> 'a -> 'a) () : 'a
    =
  let st = get_state () in
  let acc = ref (p ()) in
  let continue_ref = ref true in
  while !continue_ref do
    let frame = State.push_rewind_frame st Repeat in
    let start = st.pos in
    match
      let f = op () in
      let rhs = p () in
      State.pop_rewind_frame st frame;
      (f, rhs)
    with
    | f, rhs ->
        if st.pos = start then
          continue_ref := false
        else
          acc := f !acc rhs
    | exception (User_failure _ as exn) ->
        State.pop_rewind_frame st frame;
        raise exn
    | exception exn ->
        State.pop_rewind_frame st frame;
        if pos_of_exn exn < 0 then raise exn;
        if frame.committed then
          raise exn
        else begin
          restore_rewind_frame st frame;
          continue_ref := false
        end
  done;
  !acc

let fold_left (p : unit -> 'a) (op : unit -> 'a -> 'a -> 'a) ?otherwise () : 'a
    =
  match otherwise with
  | None ->
      fold_left_one_or_more p op ()
  | Some d ->
      or_ (fold_left_one_or_more p op) (fun () -> d) ()

let rec fold_right_one_or_more (p : unit -> 'a) (op : unit -> 'a -> 'a -> 'a) ()
    : 'a =
  let first = p () in
  or_
    (fun () ->
      let f = op () in
      let rhs = fold_right_one_or_more p op () in
      f first rhs
    )
    (fun () -> first)
    ()

let fold_right (p : unit -> 'a) (op : unit -> 'a -> 'a -> 'a) ?otherwise () : 'a
    =
  match otherwise with
  | None ->
      fold_right_one_or_more p op ()
  | Some d ->
      or_ (fold_right_one_or_more p op) (fun () -> d) ()

let optional (p : unit -> 'a) () : 'a option =
  or_ (fun () -> Some (p ())) (fun () -> None) ()

let count n (p : unit -> 'a) () : 'a list =
  let rec loop acc i =
    if i <= 0 then
      List.rev acc
    else
      loop (p () :: acc) (i - 1)
  in
  loop [] n

let expect msg p =
  let err_msg = Msg msg in
  try p () with
  | Parse_error (pos, _) ->
      raise (Parse_error (pos, err_msg))
  | Unexpected_eof pos ->
      raise (Parse_error (pos, err_msg))

let catch p handler = try p () with User_failure (_pos, msg) -> handler msg

let one_of parsers () =
  let rec try_all = function
    | [] ->
        let st = get_state () in
        raise (Parse_error (st.pos, No_alternative))
    | [ p ] ->
        p ()
    | p :: rest ->
        or_ p (fun () -> try_all rest) ()
  in
  try_all parsers

let one_of_labeled labeled_parsers () =
  let parsers = List.map snd labeled_parsers in
  try one_of parsers ()
  with Parse_error (pos, _) | Unexpected_eof pos ->
    let expected = String.concat ", " (List.map fst labeled_parsers) in
    raise
      (Parse_error (pos, Msg (Printf.sprintf "expected one of: %s" expected)))

let digit () =
  let c = satisfy (fun c -> c >= '0' && c <= '9') ~label:"digit" in
  Char.code c - Char.code '0'

let letter () =
  satisfy
    (fun c -> (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z'))
    ~label:"letter"

let[@inline always] is_whitespace c =
  c = ' ' || c = '\t' || c = '\n' || c = '\r'

let whitespace ?(at_least = 0) () =
  take_while ~at_least ~label:"whitespace" is_whitespace
let[@inline] skip_whitespace () = skip_while is_whitespace

let alphanum () =
  satisfy
    (fun c ->
      (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9')
    )
    ~label:"alphanumeric"

let any_char () = satisfy (fun _ -> true) ~label:"any character"

let[@inline] binary_any_uint8 () =
  let st = get_state () in
  handle_any_uint8 st

let[@inline] binary_any_int8 () =
  let st = get_state () in
  handle_any_int8 st

let[@inline] binary_any_int16 endian () =
  let st = get_state () in
  handle_any_int16 st endian

let[@inline] binary_any_int32 endian () =
  let st = get_state () in
  handle_any_int32 st endian

let[@inline] binary_any_int64 endian () =
  let st = get_state () in
  handle_any_int64 st endian

let expect_int16 any_fn expected =
  if expected < -0x8000 || expected > 0xFFFF then
    invalid_arg "Parseff.int16: value must be in -32768..65535";
  let st = get_state () in
  let start = st.pos in
  let actual = any_fn () land 0xFFFF in
  let expected = expected land 0xFFFF in
  if actual <> expected then
    raise
      (Parse_error
         ( start,
           Msg
             (Printf.sprintf "expected int16 0x%04X, got 0x%04X" expected actual)
         )
      )

let expect_int32 any_fn expected =
  let st = get_state () in
  let start = st.pos in
  let actual = any_fn () in
  if actual <> expected then
    raise
      (Parse_error
         ( start,
           Msg
             (Printf.sprintf "expected int32 0x%08lX, got 0x%08lX" expected
                actual
             )
         )
      )

let expect_int64 any_fn expected =
  let st = get_state () in
  let start = st.pos in
  let actual = any_fn () in
  if actual <> expected then
    raise
      (Parse_error
         ( start,
           Msg
             (Printf.sprintf "expected int64 0x%016LX, got 0x%016LX" expected
                actual
             )
         )
      )

module BE = struct
  let any_uint8 = binary_any_uint8
  let any_int8 = binary_any_int8
  let[@inline] any_int16 () = binary_any_int16 Big ()

  let[@inline] any_uint16 () =
    let v = binary_any_int16 Big () in
    v land 0xFFFF

  let[@inline] any_int32 () = binary_any_int32 Big ()
  let[@inline] any_int64 () = binary_any_int64 Big ()
  let[@inline] any_float () = Int32.float_of_bits (any_int32 ())
  let[@inline] any_double () = Int64.float_of_bits (any_int64 ())
  let int16 = expect_int16 any_int16
  let int32 = expect_int32 any_int32
  let int64 = expect_int64 any_int64
end

module LE = struct
  let any_uint8 = binary_any_uint8
  let any_int8 = binary_any_int8
  let[@inline] any_int16 () = binary_any_int16 Little ()

  let[@inline] any_uint16 () =
    let v = binary_any_int16 Little () in
    v land 0xFFFF

  let[@inline] any_int32 () = binary_any_int32 Little ()
  let[@inline] any_int64 () = binary_any_int64 Little ()
  let[@inline] any_float () = Int32.float_of_bits (any_int32 ())
  let[@inline] any_double () = Int64.float_of_bits (any_int64 ())
  let int16 = expect_int16 any_int16
  let int32 = expect_int32 any_int32
  let int64 = expect_int64 any_int64
end

module Utf8 = struct
  let[@inline] satisfy pred ~label =
    let st = get_state () in
    turn_invalid_utf8_to_parse_error (fun () ->
        handle_satisfy_uchar st pred label
    )

  let[@inline] char u = satisfy (Uchar.equal u) ~label:(uchar_label u)

  let[@inline] any_char () = satisfy (fun _ -> true) ~label:"any character"

  let take_while ?(at_least = 0) ?label pred =
    let st = get_state () in
    let s =
      turn_invalid_utf8_to_parse_error (fun () ->
          handle_take_while_uchar st pred
      )
    in
    if at_least > 0 then begin
      let count = ref 0 in
      let i = ref 0 in
      let len = String.length s in
      while !i < len do
        let d = String.get_utf_8_uchar s !i in
        i := !i + Uchar.utf_decode_length d;
        incr count
      done;
      if !count < at_least then begin
        raise
          (Parse_error
             ( st.pos,
               Msg (match label with Some l -> l | None -> "take_while")
             )
          )
      end else
        s
    end else
      s

  let[@inline] skip_while pred =
    let st = get_state () in
    turn_invalid_utf8_to_parse_error (fun () -> handle_skip_while_uchar st pred)

  let[@inline] take_while_span pred =
    let st = get_state () in
    turn_invalid_utf8_to_parse_error (fun () ->
        handle_take_while_span_uchar st pred
    )

  let[@inline] skip_while_then_char pred u =
    let st = get_state () in
    turn_invalid_utf8_to_parse_error (fun () ->
        handle_skip_while_then_uchar st pred u
    )

  let[@inline] is_whitespace u = Uucp.White.is_white_space u

  let letter () = satisfy Uucp.Alpha.is_alphabetic ~label:"letter"

  let digit () =
    let u =
      satisfy
        (fun u ->
          let i = Uchar.to_int u in
          i >= 0x30 && i <= 0x39
        )
        ~label:"digit"
    in
    Uchar.to_int u - 0x30

  let alphanum () =
    satisfy
      (fun u ->
        Uucp.Alpha.is_alphabetic u
        ||
        let i = Uchar.to_int u in
        i >= 0x30 && i <= 0x39
      )
      ~label:"alphanumeric"

  let whitespace ?(at_least = 0) () =
    take_while ~at_least ~label:"whitespace" Uucp.White.is_white_space

  let skip_whitespace () = skip_while Uucp.White.is_white_space
end

let default_handler : _ exn_handler =
  {
    handle =
      (function
      | Parse_error (pos, msg) ->
          Error { pos; error = `Expected (format_error_msg msg) }
      | Unexpected_eof pos ->
          Error { pos; error = `Unexpected_end_of_input }
      | User_error (pos, obj) ->
          Error { pos; error = Obj.obj obj }
      | Propagated_error (pos, obj) ->
          Error { pos; error = Obj.obj obj }
      | e ->
          raise e
      );
  }

let to_diagnostics diagnostics_rev =
  List.rev_map
    (fun (pos, diagnostic) -> { pos; diagnostic = Obj.obj diagnostic })
    diagnostics_rev

let attach_diagnostics result diagnostics =
  match result with
  | Ok value ->
      Stdlib.Ok (value, diagnostics)
  | Error { pos; error } ->
      Stdlib.Error { pos; error; diagnostics }

let parse ?(max_depth = 128) input (parser : unit -> 'a) : ('a, 'e) result =
  let st = State.for_input ~input ~max_depth in
  let result =
    State.run st @@ fun () ->
    match parser () with
    | v ->
        Ok v
    | exception User_failure (pos, msg) ->
        Error { pos; error = `Failure msg }
    | exception Depth_limit_exceeded (pos, msg) ->
        Error { pos; error = `Depth_limit_exceeded msg }
    | exception exn ->
        default_handler.handle exn
  in
  result

let parse_until_end ?(max_depth = 128) input (parser : unit -> 'a) :
    ( 'a,
      [> `Expected of string | `Failure of string | `Unexpected_end_of_input ],
      'd
    )
    result_with_diagnostics =
  let st = State.for_input ~input ~max_depth in
  let result =
    State.run st @@ fun () ->
    match
      let v = parser () in
      end_of_input ();
      v
    with
    | v ->
        Ok v
    | exception User_failure (pos, msg) ->
        Error { pos; error = `Failure msg }
    | exception Depth_limit_exceeded (pos, msg) ->
        Error { pos; error = `Depth_limit_exceeded msg }
    | exception exn ->
        default_handler.handle exn
  in
  let diagnostics = to_diagnostics (State.get_diagnostics_rev st) in
  attach_diagnostics result diagnostics

let parse_source ?(max_depth = 128)
    ?(backtrack_window = default_backtrack_window) (src : Source.t)
    (parser : unit -> 'a) :
    ( 'a,
      [> `Expected of string
      | `Failure of string
      | `Unexpected_end_of_input
      | `Backtrack_window_exceeded of backtrack_window_error
      | `Depth_limit_exceeded of string ]
    )
    result =
  check_backtrack_window backtrack_window;
  let st = State.for_source ~src ~max_depth ~backtrack_window in
  let result =
    State.run st @@ fun () ->
    match parser () with
    | v ->
        Ok v
    | exception Backtrack_window_exceeded (pos, info) ->
        Error { pos; error = `Backtrack_window_exceeded info }
    | exception User_failure (pos, msg) ->
        Error { pos; error = `Failure msg }
    | exception Depth_limit_exceeded (pos, msg) ->
        Error { pos; error = `Depth_limit_exceeded msg }
    | exception exn ->
        default_handler.handle exn
  in
  result

let parse_source_until_end ?(max_depth = 128)
    ?(backtrack_window = default_backtrack_window) (src : Source.t)
    (parser : unit -> 'a) :
    ( 'a,
      [> `Expected of string
      | `Failure of string
      | `Unexpected_end_of_input
      | `Backtrack_window_exceeded of backtrack_window_error
      | `Depth_limit_exceeded of string ],
      'd
    )
    result_with_diagnostics =
  check_backtrack_window backtrack_window;
  let st = State.for_source ~src ~max_depth ~backtrack_window in
  let result =
    State.run st @@ fun () ->
    match
      let v = parser () in
      end_of_input ();
      v
    with
    | v ->
        Ok v
    | exception Backtrack_window_exceeded (pos, info) ->
        Error { pos; error = `Backtrack_window_exceeded info }
    | exception User_failure (pos, msg) ->
        Error { pos; error = `Failure msg }
    | exception Depth_limit_exceeded (pos, msg) ->
        Error { pos; error = `Depth_limit_exceeded msg }
    | exception exn ->
        default_handler.handle exn
  in
  let diagnostics = to_diagnostics (State.get_diagnostics_rev st) in
  attach_diagnostics result diagnostics

let location_of_position input pos =
  let idx = create_line_index () in
  extend_line_index idx input (min pos (String.length input));
  let line_idx = find_line idx pos in
  { offset = pos; line = line_idx + 1; col = pos - idx.starts.(line_idx) + 1 }
