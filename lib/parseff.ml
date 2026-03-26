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
}

type rewind_frame_kind = Choice | Repeat | Barrier

type rewind_frame = {
  kind : rewind_frame_kind;
  start_pos : int;
  saved_diagnostics_rev : (int * Obj.t) list;
  mutable committed : bool;
}

type backtrack_window_error = { limit : int; oldest : int; current : int }

exception Backtrack_window_exceeded of backtrack_window_error

type source = {
  mutable input : string;
  mutable input_len : int;
  mutable base_offset : int;
  read : bytes -> int -> int -> int;
  mutable eof : bool;
  tmp : bytes;
}

type state = {
  mutable input : string;
  mutable pos : int;
  mutable input_len : int;
  mutable max_depth : int;
  mutable current_depth : int;
  mutable diagnostics_rev : (int * Obj.t) list;
  mutable line_index : line_index option;
  mutable source : source option;
  mutable rewind_frames : rewind_frame list;
  mutable source_pins : int list;
  mutable backtrack_window : int option;
  mutable active : bool;
}

let default_buf_size = 4096
let default_backtrack_window = 65_536

module Source = struct
  type t = source

  let[@inline always] make ~input ~input_len ~base_offset ~read ~eof ~tmp : t =
    { input; input_len; base_offset; read; eof; tmp }

  let[@inline always] get_input (t : t) = t.input
  let[@inline always] get_input_len (t : t) = t.base_offset + t.input_len
  let[@inline always] get_base_offset (t : t) = t.base_offset
  let[@inline always] relative_pos (t : t) pos = pos - t.base_offset
  let[@inline always] is_eof (t : t) = t.eof
  let[@inline always] char_at (t : t) pos =
    String.unsafe_get t.input (relative_pos t pos)
  let[@inline always] sub (t : t) off len =
    string_slice t.input (relative_pos t off) len t.input_len

  let[@inline always] span (t : t) off len =
    let s = sub t off len in
    { buf = s; off = 0; len = String.length s }

  let[@inline always] sync_state_input st (t : t) = st.input <- t.input

  let[@inline always] sync_state_input_and_len st (t : t) =
    st.input <- t.input;
    st.input_len <- get_input_len t

  let[@inline always] set_buffer (t : t) ~base_offset ~input ~input_len =
    t.base_offset <- base_offset;
    t.input <- input;
    t.input_len <- input_len

  let[@inline never] append_refill ?max_bytes (src : t) =
    if src.eof then
      false
    else
      let max_bytes =
        match max_bytes with
        | None ->
            Bytes.length src.tmp
        | Some max_bytes ->
            min max_bytes (Bytes.length src.tmp)
      in
      if max_bytes <= 0 then
        false
      else
        let n = src.read src.tmp 0 max_bytes in
        if n = 0 then begin
          src.eof <- true;
          false
        end else begin
          let old = src.input in
          let old_len = src.input_len in
          let new_len = old_len + n in
          let buf = Bytes.create new_len in
          Bytes.blit_string old 0 buf 0 old_len;
          Bytes.blit src.tmp 0 buf old_len n;
          set_buffer src ~base_offset:src.base_offset
            ~input:(Bytes.unsafe_to_string buf)
            ~input_len:new_len;
          true
        end

  let[@inline never] compact_prefix (src : t) keep_from =
    let drop = keep_from - src.base_offset in
    if drop <= 0 then
      ()
    else if drop >= src.input_len then
      set_buffer src ~base_offset:keep_from ~input:"" ~input_len:0
    else begin
      let new_len = src.input_len - drop in
      let buf = Bytes.create new_len in
      Bytes.blit_string src.input drop buf 0 new_len;
      set_buffer src ~base_offset:keep_from
        ~input:(Bytes.unsafe_to_string buf)
        ~input_len:new_len
    end

  let of_string s =
    make ~input:s ~input_len:(String.length s) ~base_offset:0
      ~read:(fun _ _ _ -> 0)
      ~eof:true ~tmp:Bytes.empty

  let of_channel ?(buf_size = default_buf_size) ic =
    make ~input:"" ~input_len:0 ~base_offset:0
      ~read:(fun buf off len -> input ic buf off len)
      ~eof:false ~tmp:(Bytes.create buf_size)

  let of_function read =
    make ~input:"" ~input_len:0 ~base_offset:0 ~read ~eof:false
      ~tmp:(Bytes.create default_buf_size)

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

  type saved = {
    saved_input : string;
    saved_pos : int;
    saved_input_len : int;
    saved_max_depth : int;
    saved_current_depth : int;
    saved_diagnostics_rev : (int * Obj.t) list;
    saved_line_index : line_index option;
    saved_source : source option;
    saved_rewind_frames : rewind_frame list;
    saved_source_pins : int list;
    saved_backtrack_window : int option;
    saved_active : bool;
  }

  let default_max_depth = 128

  let make () : t =
    {
      input = "";
      pos = 0;
      input_len = 0;
      max_depth = default_max_depth;
      current_depth = 0;
      diagnostics_rev = [];
      line_index = None;
      source = None;
      rewind_frames = [];
      source_pins = [];
      backtrack_window = None;
      active = false;
    }

  let key : t Domain.DLS.key = Domain.DLS.new_key make
  let[@inline always] is_active (t : t) = t.active
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

  let push_source_pin (t : t) pos = t.source_pins <- pos :: t.source_pins

  let pop_source_pin (t : t) =
    match t.source_pins with
    | _ :: rest ->
        t.source_pins <- rest
    | [] ->
        invalid_arg "Parseff.State.pop_source_pin"

  let reclaim_floor (t : t) =
    let floor = ref t.pos in
    List.iter
      (fun frame ->
        if
          (frame.kind = Barrier || not frame.committed)
          && frame.start_pos < !floor
        then
          floor := frame.start_pos
      )
      t.rewind_frames;
    List.iter (fun pos -> if pos < !floor then floor := pos) t.source_pins;
    !floor

  let save (t : t) =
    {
      saved_input = t.input;
      saved_pos = t.pos;
      saved_input_len = t.input_len;
      saved_max_depth = t.max_depth;
      saved_current_depth = t.current_depth;
      saved_diagnostics_rev = t.diagnostics_rev;
      saved_line_index = t.line_index;
      saved_source = t.source;
      saved_rewind_frames = t.rewind_frames;
      saved_source_pins = t.source_pins;
      saved_backtrack_window = t.backtrack_window;
      saved_active = t.active;
    }

  let restore (t : t) saved =
    t.input <- saved.saved_input;
    t.pos <- saved.saved_pos;
    t.input_len <- saved.saved_input_len;
    t.max_depth <- saved.saved_max_depth;
    t.current_depth <- saved.saved_current_depth;
    t.diagnostics_rev <- saved.saved_diagnostics_rev;
    t.line_index <- saved.saved_line_index;
    t.source <- saved.saved_source;
    t.rewind_frames <- saved.saved_rewind_frames;
    t.source_pins <- saved.saved_source_pins;
    t.backtrack_window <- saved.saved_backtrack_window;
    t.active <- saved.saved_active

  let reset (t : t) ~input ~input_len ~max_depth ~source ~backtrack_window =
    t.input <- input;
    t.pos <- 0;
    t.input_len <- input_len;
    t.max_depth <- max_depth;
    t.current_depth <- 0;
    t.diagnostics_rev <- [];
    t.line_index <- None;
    t.source <- source;
    t.rewind_frames <- [];
    t.source_pins <- [];
    t.backtrack_window <- backtrack_window;
    t.active <- true

  let reset_for_input t ~input ~max_depth =
    reset t ~input ~input_len:(String.length input) ~max_depth ~source:None
      ~backtrack_window:None

  let reset_for_source t ~(src : source) ~max_depth ~backtrack_window =
    reset t ~input:(Source.get_input src) ~input_len:(Source.get_input_len src)
      ~max_depth ~source:(Some src) ~backtrack_window:(Some backtrack_window)
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
  let st = Domain.DLS.get State.key in
  if not (State.is_active st) then raise Not_in_parse_context;
  st

let[@inline always] buffer_base st =
  match st.source with Some src -> Source.get_base_offset src | None -> 0

let[@inline always] state_relative_pos st pos = pos - buffer_base st

let[@inline always] state_char_at st pos =
  String.unsafe_get st.input (state_relative_pos st pos)

let[@inline always] state_string_slice st off len input_len =
  string_slice st.input
    (state_relative_pos st off)
    len
    (input_len - buffer_base st)

let[@inline always] state_span st off len =
  match st.source with
  | None ->
      { buf = st.input; off; len }
  | Some _ ->
      let s = state_string_slice st off len st.input_len in
      { buf = s; off = 0; len = String.length s }

(* Polymorphic record so handle_exn can be called at different types. *)
type 'e exn_handler = { handle : 'a. exn -> ('a, 'e) result } [@@unboxed]

let[@inline never] handle_consume st input_len s =
  let len = String.length s in
  if len = 1 then begin
    (* O5: Special-case single-char strings -- skip loop overhead *)
      let c = String.unsafe_get s 0 in
      if st.pos < input_len then
        if state_char_at st st.pos = c then begin
          st.pos <- st.pos + 1;
          s
        end else
          raise (Parse_error (st.pos, Expected_string s))
      else
        raise (Unexpected_eof st.pos)
  end else if st.pos + len <= input_len then begin
    let pos = st.pos in
    let rec check i =
      if i >= len then
        true
      else if state_char_at st (pos + i) = String.unsafe_get s i then
        check (i + 1)
      else
        false
    in
    if check 0 then begin
      st.pos <- pos + len;
      s
    end else
      raise (Parse_error (pos, Expected_string s))
  end else
    raise (Unexpected_eof st.pos)

let[@inline] handle_satisfy st input_len pred label =
  if st.pos < input_len then
    let c = state_char_at st st.pos in
    if pred c then begin
      st.pos <- st.pos + 1;
      c
    end else
      raise (Parse_error (st.pos, Expected_label label))
  else
    raise (Unexpected_eof st.pos)

(* dedicated char handler it avoids closure + String.make allocation *)
let[@inline] handle_match_char st input_len c =
  if st.pos < input_len then
    let ch = state_char_at st st.pos in
    if ch = c then begin
      st.pos <- st.pos + 1;
      c
    end else
      raise (Parse_error (st.pos, Expected_char c))
  else
    raise (Unexpected_eof st.pos)

let[@inline always] scan_while inp start input_len pred =
  let pos = ref start in
  while !pos < input_len && pred (String.unsafe_get inp !pos) do
    pos := !pos + 1
  done;
  !pos

let[@inline always] handle_take_while st input_len pred =
  let start = st.pos in
  let end_pos = scan_while st.input start input_len pred in
  st.pos <- end_pos;
  string_slice st.input start (end_pos - start) input_len

let[@inline always] handle_take_while_span st input_len pred =
  let start = st.pos in
  let end_pos = scan_while st.input start input_len pred in
  st.pos <- end_pos;
  state_span st start (end_pos - start)

let[@inline always] handle_skip_while st input_len pred =
  st.pos <- scan_while st.input st.pos input_len pred

let[@inline never] handle_skip_while_then_char st input_len pred c =
  let inp = st.input in
  let pos = scan_while inp st.pos input_len pred in
  if pos < input_len && String.unsafe_get inp pos = c then begin
    st.pos <- pos + 1
  end else begin
    st.pos <- pos;
    if pos >= input_len then
      raise (Unexpected_eof pos)
    else
      raise (Parse_error (pos, Expected_char c))
  end

let[@inline never] handle_fused_sep_take st input_len ws_pred sep_char take_pred
    =
  let inp = st.input in
  let pos = scan_while inp st.pos input_len ws_pred in
  if pos < input_len && String.unsafe_get inp pos = sep_char then begin
    let pos2 = scan_while inp (pos + 1) input_len ws_pred in
    let end_pos = scan_while inp pos2 input_len take_pred in
    if end_pos > pos2 then begin
      st.pos <- end_pos;
      string_slice inp pos2 (end_pos - pos2) input_len
    end else begin
      st.pos <- end_pos;
      if end_pos >= input_len then
        raise (Unexpected_eof end_pos)
      else
        raise (Parse_error (end_pos, Expected_value))
    end
  end else begin
    st.pos <- pos;
    if pos >= input_len then
      raise (Unexpected_eof pos)
    else
      raise (Parse_error (pos, Expected_char sep_char))
  end

let[@inline never] handle_sep_by_take st input_len ws_pred sep_char take_pred =
  let inp = st.input in
  let start = scan_while inp st.pos input_len ws_pred in
  let first_end = scan_while inp start input_len take_pred in
  if first_end <= start then begin
    st.pos <- first_end;
    []
  end else begin
    let acc = ref [ string_slice inp start (first_end - start) input_len ] in
    let pos = ref first_end in
    let continue_loop = ref true in
    while !continue_loop do
      let ws_end = scan_while inp !pos input_len ws_pred in
      if ws_end < input_len && String.unsafe_get inp ws_end = sep_char then begin
        let after_ws = scan_while inp (ws_end + 1) input_len ws_pred in
        let elem_end = scan_while inp after_ws input_len take_pred in
        if elem_end > after_ws then begin
          acc :=
            string_slice inp after_ws (elem_end - after_ws) input_len :: !acc;
          pos := elem_end
        end else
          continue_loop := false
      end else
        continue_loop := false
    done;
    st.pos <- !pos;
    List.rev !acc
  end

let[@inline] handle_sep_by_take_span st input_len ws_pred sep_char take_pred =
  let inp = st.input in
  let start = scan_while inp st.pos input_len ws_pred in
  let first_end = scan_while inp start input_len take_pred in
  if first_end <= start then begin
    st.pos <- first_end;
    []
  end else begin
    let acc = ref [ { buf = inp; off = start; len = first_end - start } ] in
    let pos = ref first_end in
    let continue_loop = ref true in
    while !continue_loop do
      let ws_end = scan_while inp !pos input_len ws_pred in
      if ws_end < input_len && String.unsafe_get inp ws_end = sep_char then begin
        let after_ws = scan_while inp (ws_end + 1) input_len ws_pred in
        let elem_end = scan_while inp after_ws input_len take_pred in
        if elem_end > after_ws then begin
          acc :=
            { buf = inp; off = after_ws; len = elem_end - after_ws } :: !acc;
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

let[@inline never] handle_match_regex st input_len re =
  try
    let groups = Re.exec ~pos:st.pos re st.input in
    let match_start = Re.Group.start groups 0 in
    if match_start <> st.pos then
      raise (Parse_error (st.pos, regex_failed))
    else
      let matched = Re.Group.get groups 0 in
      let match_end = Re.Group.stop groups 0 in
      st.pos <- match_end;
      matched
  with Not_found ->
    if st.pos >= input_len then
      raise (Unexpected_eof st.pos)
    else
      raise (Parse_error (st.pos, regex_failed))

let expected_end_of_input = Msg "expected end of input"

let[@inline never] handle_any_uint8 st input_len =
  if st.pos < input_len then begin
    let v = String.get_uint8 st.input (state_relative_pos st st.pos) in
    st.pos <- st.pos + 1;
    v
  end else
    raise (Unexpected_eof st.pos)

let[@inline never] handle_any_int8 st input_len =
  if st.pos < input_len then begin
    let v = String.get_int8 st.input (state_relative_pos st st.pos) in
    st.pos <- st.pos + 1;
    v
  end else
    raise (Unexpected_eof st.pos)

let[@inline never] handle_any_int16 st input_len endian =
  if st.pos + 2 <= input_len then begin
    let pos = state_relative_pos st st.pos in
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

let[@inline never] handle_any_int32 st input_len endian =
  if st.pos + 4 <= input_len then begin
    let pos = state_relative_pos st st.pos in
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

let[@inline never] handle_any_int64 st input_len endian =
  if st.pos + 8 <= input_len then begin
    let pos = state_relative_pos st st.pos in
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

let[@inline never] handle_take st input_len n =
  if st.pos + n <= input_len then begin
    let s = state_string_slice st st.pos n input_len in
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

let[@inline] decode_uchar inp pos input_len =
  if pos >= input_len then raise (Unexpected_eof pos);
  let d = String.get_utf_8_uchar inp pos in
  if not (Uchar.utf_decode_is_valid d) then raise (Invalid_utf8 pos);
  d

(* UTF-8 in-memory handlers *)

let[@inline never] handle_satisfy_uchar st input_len pred label =
  let d = decode_uchar st.input st.pos input_len in
  let u = Uchar.utf_decode_uchar d in
  if pred u then begin
    st.pos <- st.pos + Uchar.utf_decode_length d;
    u
  end else
    raise (Parse_error (st.pos, Expected_label label))

let[@inline never] scan_while_uchar inp start input_len pred =
  let pos = ref start in
  let continue_loop = ref true in
  while !pos < input_len && !continue_loop do
    let d = String.get_utf_8_uchar inp !pos in
    if not (Uchar.utf_decode_is_valid d) then raise (Invalid_utf8 !pos);
    if pred (Uchar.utf_decode_uchar d) then
      pos := !pos + Uchar.utf_decode_length d
    else
      continue_loop := false
  done;
  !pos

let[@inline never] handle_take_while_uchar st input_len pred =
  let start = st.pos in
  let end_pos = scan_while_uchar st.input start input_len pred in
  st.pos <- end_pos;
  string_slice st.input start (end_pos - start) input_len

let[@inline never] handle_take_while_span_uchar st input_len pred =
  let start = st.pos in
  let end_pos = scan_while_uchar st.input start input_len pred in
  st.pos <- end_pos;
  state_span st start (end_pos - start)

let[@inline never] handle_skip_while_uchar st input_len pred =
  st.pos <- scan_while_uchar st.input st.pos input_len pred

let[@inline never] handle_skip_while_then_uchar st input_len pred term =
  let inp = st.input in
  let pos = scan_while_uchar inp st.pos input_len pred in
  if pos >= input_len then begin
    st.pos <- pos;
    raise (Unexpected_eof pos)
  end;
  let d = String.get_utf_8_uchar inp pos in
  if not (Uchar.utf_decode_is_valid d) then raise (Invalid_utf8 pos);
  let u = Uchar.utf_decode_uchar d in
  if Uchar.equal u term then
    st.pos <- pos + Uchar.utf_decode_length d
  else begin
    st.pos <- pos;
    raise (Parse_error (pos, Msg ("expected " ^ uchar_label term)))
  end

let create_line_index () =
  let starts = Array.make 64 0 in
  starts.(0) <- 0;
  { starts; count = 1; scanned_up_to = 0 }

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
  idx.scanned_up_to <- up_to

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

let[@inline never] handle_location st input_len =
  let idx =
    match st.line_index with
    | Some idx ->
        idx
    | None ->
        let idx = create_line_index () in
        st.line_index <- Some idx;
        idx
  in
  let up_to = min st.pos input_len in
  extend_line_index idx ~base_offset:(buffer_base st) st.input up_to;
  let line_idx = find_line idx st.pos in
  {
    offset = st.pos;
    line = line_idx + 1;
    col = st.pos - idx.starts.(line_idx) + 1;
  }

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
    extend_line_index idx ~base_offset:(buffer_base st) st.input up_to
  end

let try_refill_source ?required_end st (src : source) =
  if Source.is_eof src then
    false
  else begin
    let floor = State.reclaim_floor st in
    let current_end = Source.get_input_len src in
    let required_end =
      match required_end with
      | Some required_end ->
          required_end
      | None ->
          current_end + 1
    in
    let max_bytes =
      match st.backtrack_window with
      | Some limit ->
          if required_end - floor > limit then
            raise
              (Backtrack_window_exceeded
                 { limit; oldest = floor; current = required_end }
              );
          Some (limit - (current_end - floor))
      | None ->
          None
    in
    if floor > Source.get_base_offset src then begin
      ensure_line_index_preserved_through st floor;
      Source.compact_prefix src floor;
      Source.sync_state_input_and_len st src
    end;
    let ok = Source.append_refill ?max_bytes src in
    if ok then Source.sync_state_input_and_len st src;
    ok
  end

let ensure_source_bytes st src pos needed =
  let rec loop () =
    if pos + needed <= Source.get_input_len src then
      true
    else if not (try_refill_source ~required_end:(pos + needed) st src) then
      false
    else
      loop ()
  in
  loop ()

let ensure_source_utf8_char st src pos =
  if not (ensure_source_bytes st src pos 1) then
    false
  else
    let lead = Char.code (Source.char_at src pos) in
    let needed =
      if lead < 0x80 then
        1
      else if lead < 0xC2 then
        1
      else if lead < 0xE0 then
        2
      else if lead < 0xF0 then
        3
      else if lead < 0xF5 then
        4
      else
        1
    in
    if needed <= 1 then
      true
    else
      ensure_source_bytes st src pos needed

let with_source_pin st pos f =
  State.push_source_pin st pos;
  match f () with
  | v ->
      State.pop_source_pin st;
      v
  | exception exn ->
      State.pop_source_pin st;
      raise exn

let scan_while_source (src : source) st pred =
  let continue = ref true in
  while !continue do
    if st.pos < Source.get_input_len src then
      if pred (Source.char_at src st.pos) then
        st.pos <- st.pos + 1
      else
        continue := false
    else if try_refill_source st src then
      ()
    else
      continue := false
  done;
  Source.sync_state_input st src

let handle_take_while_source src st pred =
  let start = st.pos in
  with_source_pin st start (fun () ->
      scan_while_source src st pred;
      if st.pos > start then
        Source.sub src start (st.pos - start)
      else
        ""
  )

let handle_take_while_span_source src st pred =
  let start = st.pos in
  with_source_pin st start (fun () ->
      scan_while_source src st pred;
      Source.span src start (st.pos - start)
  )

let handle_skip_while_source src st pred = scan_while_source src st pred

let handle_skip_while_then_char_source src st pred c =
  handle_skip_while_source src st pred;
  ignore (ensure_source_bytes st src st.pos 1);
  if st.pos < Source.get_input_len src && Source.char_at src st.pos = c then
    st.pos <- st.pos + 1
  else if st.pos >= Source.get_input_len src then
    raise (Unexpected_eof st.pos)
  else
    raise (Parse_error (st.pos, Expected_char c))

let handle_fused_sep_take_source src st ws_pred sep_char take_pred =
  handle_skip_while_source src st ws_pred;
  ignore (ensure_source_bytes st src st.pos 1);
  if st.pos < Source.get_input_len src && Source.char_at src st.pos = sep_char
  then begin
    st.pos <- st.pos + 1;
    handle_skip_while_source src st ws_pred;
    let start = st.pos in
    with_source_pin st start (fun () ->
        scan_while_source src st take_pred;
        if st.pos > start then
          Source.sub src start (st.pos - start)
        else if st.pos >= Source.get_input_len src then
          raise (Unexpected_eof st.pos)
        else
          raise (Parse_error (st.pos, Expected_value))
    )
  end else if st.pos >= Source.get_input_len src then
    raise (Unexpected_eof st.pos)
  else
    raise (Parse_error (st.pos, Expected_char sep_char))

let handle_sep_by_take_source src st ws_pred sep_char take_pred =
  let start = st.pos in
  let first =
    with_source_pin st start (fun () ->
        scan_while_source src st take_pred;
        if st.pos <= start then
          None
        else
          Some (Source.sub src start (st.pos - start))
    )
  in
  match first with
  | None ->
      []
  | Some first ->
      let acc = ref [ first ] in
      let continue_loop = ref true in
      while !continue_loop do
        let saved_pos = st.pos in
        scan_while_source src st ws_pred;
        ignore (ensure_source_bytes st src st.pos 1);
        if
          st.pos < Source.get_input_len src
          && Source.char_at src st.pos = sep_char
        then begin
          st.pos <- st.pos + 1;
          scan_while_source src st ws_pred;
          let elem_start = st.pos in
          with_source_pin st elem_start (fun () ->
              scan_while_source src st take_pred;
              if st.pos > elem_start then
                acc := Source.sub src elem_start (st.pos - elem_start) :: !acc
              else begin
                st.pos <- saved_pos;
                Source.sync_state_input st src;
                continue_loop := false
              end
          )
        end else begin
          st.pos <- saved_pos;
          Source.sync_state_input st src;
          continue_loop := false
        end
      done;
      List.rev !acc

let handle_sep_by_take_span_source src st ws_pred sep_char take_pred =
  let start = st.pos in
  let first =
    with_source_pin st start (fun () ->
        scan_while_source src st take_pred;
        if st.pos <= start then
          None
        else
          Some (Source.span src start (st.pos - start))
    )
  in
  match first with
  | None ->
      []
  | Some first ->
      let acc = ref [ first ] in
      let continue_loop = ref true in
      while !continue_loop do
        let saved_pos = st.pos in
        scan_while_source src st ws_pred;
        ignore (ensure_source_bytes st src st.pos 1);
        if
          st.pos < Source.get_input_len src
          && Source.char_at src st.pos = sep_char
        then begin
          st.pos <- st.pos + 1;
          scan_while_source src st ws_pred;
          let elem_start = st.pos in
          with_source_pin st elem_start (fun () ->
              scan_while_source src st take_pred;
              if st.pos > elem_start then
                acc := Source.span src elem_start (st.pos - elem_start) :: !acc
              else begin
                st.pos <- saved_pos;
                Source.sync_state_input st src;
                continue_loop := false
              end
          )
        end else begin
          st.pos <- saved_pos;
          Source.sync_state_input st src;
          continue_loop := false
        end
      done;
      List.rev !acc

let handle_match_regex_source (src : source) st re =
  let rec loop () =
    try
      let groups =
        Re.exec ~pos:(Source.relative_pos src st.pos) re (Source.get_input src)
      in
      let base = Source.get_base_offset src in
      let match_start = base + Re.Group.start groups 0 in
      let match_end = base + Re.Group.stop groups 0 in
      if match_start <> st.pos then
        raise (Parse_error (st.pos, regex_failed))
      else if match_end = Source.get_input_len src && not (Source.is_eof src)
      then begin
        ignore (try_refill_source st src);
        Source.sync_state_input st src;
        loop ()
      end else begin
        let matched = Re.Group.get groups 0 in
        st.pos <- match_end;
        Source.sync_state_input st src;
        matched
      end
    with Not_found ->
      if st.pos >= Source.get_input_len src && not (Source.is_eof src) then begin
        ignore (try_refill_source st src);
        Source.sync_state_input st src;
        loop ()
      end else if st.pos >= Source.get_input_len src then
        raise (Unexpected_eof st.pos)
      else
        raise (Parse_error (st.pos, regex_failed))
  in
  loop ()

let handle_end_of_input_source (src : source) st =
  if st.pos = Source.get_input_len src && not (Source.is_eof src) then
    ignore (try_refill_source st src);
  if st.pos <> Source.get_input_len src then
    raise (Parse_error (st.pos, expected_end_of_input))

(* UTF-8 streaming handlers *)

let handle_satisfy_uchar_source src st pred label =
  if not (ensure_source_utf8_char st src st.pos) then
    raise (Unexpected_eof st.pos);
  let d =
    String.get_utf_8_uchar (Source.get_input src)
      (Source.relative_pos src st.pos)
  in
  if not (Uchar.utf_decode_is_valid d) then raise (Invalid_utf8 st.pos);
  let u = Uchar.utf_decode_uchar d in
  if pred u then begin
    st.pos <- st.pos + Uchar.utf_decode_length d;
    Source.sync_state_input st src;
    u
  end else
    raise (Parse_error (st.pos, Expected_label label))

let scan_while_uchar_source src st pred =
  let continue_loop = ref true in
  while !continue_loop do
    if ensure_source_utf8_char st src st.pos then begin
      let d =
        String.get_utf_8_uchar (Source.get_input src)
          (Source.relative_pos src st.pos)
      in
      if not (Uchar.utf_decode_is_valid d) then raise (Invalid_utf8 st.pos);
      if pred (Uchar.utf_decode_uchar d) then
        st.pos <- st.pos + Uchar.utf_decode_length d
      else
        continue_loop := false
    end else
      continue_loop := false
  done;
  Source.sync_state_input st src

let handle_take_while_uchar_source src st pred =
  let start = st.pos in
  with_source_pin st start (fun () ->
      scan_while_uchar_source src st pred;
      if st.pos > start then
        Source.sub src start (st.pos - start)
      else
        ""
  )

let handle_take_while_span_uchar_source src st pred =
  let start = st.pos in
  with_source_pin st start (fun () ->
      scan_while_uchar_source src st pred;
      Source.span src start (st.pos - start)
  )

let handle_skip_while_uchar_source src st pred =
  scan_while_uchar_source src st pred

let handle_skip_while_then_uchar_source src st pred term =
  handle_skip_while_uchar_source src st pred;
  if not (ensure_source_utf8_char st src st.pos) then
    raise (Unexpected_eof st.pos);
  let d =
    String.get_utf_8_uchar (Source.get_input src)
      (Source.relative_pos src st.pos)
  in
  if not (Uchar.utf_decode_is_valid d) then raise (Invalid_utf8 st.pos);
  let u = Uchar.utf_decode_uchar d in
  if Uchar.equal u term then begin
    st.pos <- st.pos + Uchar.utf_decode_length d;
    Source.sync_state_input st src
  end else
    raise (Parse_error (st.pos, Msg ("expected " ^ uchar_label term)))

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
  st.diagnostics_rev <- frame.saved_diagnostics_rev;
  match st.source with Some src -> Source.sync_state_input st src | None -> ()

let[@inline] consume s =
  let st = get_state () in
  match st.source with
  | None ->
      handle_consume st st.input_len s
  | Some src ->
      ignore (ensure_source_bytes st src st.pos (String.length s));
      Source.sync_state_input_and_len st src;
      handle_consume st (Source.get_input_len src) s

let[@inline] satisfy pred ~label =
  let st = get_state () in
  match st.source with
  | None ->
      handle_satisfy st st.input_len pred label
  | Some src ->
      ignore (ensure_source_bytes st src st.pos 1);
      Source.sync_state_input_and_len st src;
      handle_satisfy st (Source.get_input_len src) pred label

let[@inline] char c =
  let st = get_state () in
  match st.source with
  | None ->
      handle_match_char st st.input_len c
  | Some src ->
      ignore (ensure_source_bytes st src st.pos 1);
      Source.sync_state_input_and_len st src;
      handle_match_char st (Source.get_input_len src) c

let[@inline] peek_char () =
  let st = get_state () in
  match st.source with
  | None ->
      if st.pos < st.input_len then
        Some (String.unsafe_get st.input st.pos)
      else
        None
  | Some src ->
      ignore (ensure_source_bytes st src st.pos 1);
      Source.sync_state_input_and_len st src;
      if st.pos < Source.get_input_len src then
        Some (Source.char_at src st.pos)
      else
        None

let[@inline] position () =
  let st = get_state () in
  st.pos

let[@inline] location () =
  let st = get_state () in
  match st.source with
  | None ->
      handle_location st st.input_len
  | Some src ->
      Source.sync_state_input_and_len st src;
      handle_location st (Source.get_input_len src)

let[@inline] end_of_input () =
  let st = get_state () in
  match st.source with
  | None ->
      if st.pos <> st.input_len then
        raise (Parse_error (st.pos, expected_end_of_input))
  | Some src ->
      handle_end_of_input_source src st

let take_while ?(at_least = 0) ?label pred =
  let st = get_state () in
  let s =
    match st.source with
    | None ->
        handle_take_while st st.input_len pred
    | Some src ->
        let r = handle_take_while_source src st pred in
        Source.sync_state_input_and_len st src;
        r
  in
  if at_least > 0 && String.length s < at_least then begin
    raise
      (Parse_error
         (st.pos, Msg (match label with Some l -> l | None -> "take_while"))
      )
  end else
    s

let[@inline] skip_while pred =
  let st = get_state () in
  match st.source with
  | None ->
      handle_skip_while st st.input_len pred
  | Some src ->
      handle_skip_while_source src st pred;
      Source.sync_state_input_and_len st src

let[@inline] skip_while_then_char pred c =
  let st = get_state () in
  match st.source with
  | None ->
      handle_skip_while_then_char st st.input_len pred c
  | Some src ->
      handle_skip_while_then_char_source src st pred c;
      Source.sync_state_input_and_len st src

let[@inline] take_while_span pred =
  let st = get_state () in
  match st.source with
  | None ->
      handle_take_while_span st st.input_len pred
  | Some src ->
      let r = handle_take_while_span_source src st pred in
      Source.sync_state_input_and_len st src;
      r

let[@inline] sep_by_take ws_pred sep_char take_pred =
  let st = get_state () in
  match st.source with
  | None ->
      handle_sep_by_take st st.input_len ws_pred sep_char take_pred
  | Some src ->
      let r = handle_sep_by_take_source src st ws_pred sep_char take_pred in
      Source.sync_state_input_and_len st src;
      r

let[@inline] sep_by_take_span ws_pred sep_char take_pred =
  let st = get_state () in
  match st.source with
  | None ->
      handle_sep_by_take_span st st.input_len ws_pred sep_char take_pred
  | Some src ->
      let r =
        handle_sep_by_take_span_source src st ws_pred sep_char take_pred
      in
      Source.sync_state_input_and_len st src;
      r

let[@inline] fused_sep_take ws_pred sep_char take_pred =
  let st = get_state () in
  match st.source with
  | None ->
      handle_fused_sep_take st st.input_len ws_pred sep_char take_pred
  | Some src ->
      let r = handle_fused_sep_take_source src st ws_pred sep_char take_pred in
      Source.sync_state_input_and_len st src;
      r

let[@inline] match_regex re =
  let st = get_state () in
  match st.source with
  | None ->
      handle_match_regex st st.input_len re
  | Some src ->
      let r = handle_match_regex_source src st re in
      Source.sync_state_input_and_len st src;
      r

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
    match st.source with
    | None ->
        handle_take st st.input_len n
    | Some src ->
        ignore (ensure_source_bytes st src st.pos n);
        Source.sync_state_input_and_len st src;
        handle_take st (Source.get_input_len src) n
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
      match right () with
      | v ->
          State.pop_rewind_frame st frame;
          v
      | exception (User_failure _ as exn) ->
          State.pop_rewind_frame st frame;
          raise exn
      | exception right_exn ->
          State.pop_rewind_frame st frame;
          if pos_of_exn right_exn < 0 then raise right_exn;
          if frame.committed then
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
      match p () with
      | v ->
          State.pop_rewind_frame st frame;
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
      match p () with
      | v ->
          State.pop_rewind_frame st frame;
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
          try
            let _ = sep () in
            let v = p () in
            State.pop_rewind_frame st frame;
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
    try
      let f = op () in
      let rhs = p () in
      State.pop_rewind_frame st frame;
      acc := f !acc rhs
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
  | Propagated_error (pos, _) ->
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
  with
  | Parse_error (pos, _) | Unexpected_eof pos | Propagated_error (pos, _) ->
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
  match st.source with
  | None ->
      handle_any_uint8 st st.input_len
  | Some src ->
      ignore (ensure_source_bytes st src st.pos 1);
      Source.sync_state_input_and_len st src;
      handle_any_uint8 st (Source.get_input_len src)

let[@inline] binary_any_int8 () =
  let st = get_state () in
  match st.source with
  | None ->
      handle_any_int8 st st.input_len
  | Some src ->
      ignore (ensure_source_bytes st src st.pos 1);
      Source.sync_state_input_and_len st src;
      handle_any_int8 st (Source.get_input_len src)

let[@inline] binary_any_int16 endian () =
  let st = get_state () in
  match st.source with
  | None ->
      handle_any_int16 st st.input_len endian
  | Some src ->
      ignore (ensure_source_bytes st src st.pos 2);
      Source.sync_state_input_and_len st src;
      handle_any_int16 st (Source.get_input_len src) endian

let[@inline] binary_any_int32 endian () =
  let st = get_state () in
  match st.source with
  | None ->
      handle_any_int32 st st.input_len endian
  | Some src ->
      ignore (ensure_source_bytes st src st.pos 4);
      Source.sync_state_input_and_len st src;
      handle_any_int32 st (Source.get_input_len src) endian

let[@inline] binary_any_int64 endian () =
  let st = get_state () in
  match st.source with
  | None ->
      handle_any_int64 st st.input_len endian
  | Some src ->
      ignore (ensure_source_bytes st src st.pos 8);
      Source.sync_state_input_and_len st src;
      handle_any_int64 st (Source.get_input_len src) endian

let expect_int16 any_fn expected =
  let actual = any_fn () in
  if actual <> expected then
    let st = get_state () in
    raise
      (Parse_error
         ( st.pos,
           Msg
             (Printf.sprintf "expected int16 0x%04X, got 0x%04X" expected actual)
         )
      )

let expect_int32 any_fn expected =
  let actual = any_fn () in
  if actual <> expected then
    let st = get_state () in
    raise
      (Parse_error
         ( st.pos,
           Msg
             (Printf.sprintf "expected int32 0x%08lX, got 0x%08lX" expected
                actual
             )
         )
      )

let expect_int64 any_fn expected =
  let actual = any_fn () in
  if actual <> expected then
    let st = get_state () in
    raise
      (Parse_error
         ( st.pos,
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
        match st.source with
        | None ->
            handle_satisfy_uchar st st.input_len pred label
        | Some src ->
            let r = handle_satisfy_uchar_source src st pred label in
            Source.sync_state_input_and_len st src;
            r
    )

  let[@inline] char u = satisfy (Uchar.equal u) ~label:(uchar_label u)

  let[@inline] any_char () = satisfy (fun _ -> true) ~label:"any character"

  let take_while ?(at_least = 0) ?label pred =
    let st = get_state () in
    let s =
      turn_invalid_utf8_to_parse_error (fun () ->
          match st.source with
          | None ->
              handle_take_while_uchar st st.input_len pred
          | Some src ->
              let r = handle_take_while_uchar_source src st pred in
              Source.sync_state_input_and_len st src;
              r
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
    turn_invalid_utf8_to_parse_error (fun () ->
        match st.source with
        | None ->
            handle_skip_while_uchar st st.input_len pred
        | Some src ->
            handle_skip_while_uchar_source src st pred;
            Source.sync_state_input_and_len st src
    )

  let[@inline] take_while_span pred =
    let st = get_state () in
    turn_invalid_utf8_to_parse_error (fun () ->
        match st.source with
        | None ->
            handle_take_while_span_uchar st st.input_len pred
        | Some src ->
            let r = handle_take_while_span_uchar_source src st pred in
            Source.sync_state_input_and_len st src;
            r
    )

  let[@inline] skip_while_then_char pred u =
    let st = get_state () in
    turn_invalid_utf8_to_parse_error (fun () ->
        match st.source with
        | None ->
            handle_skip_while_then_uchar st st.input_len pred u
        | Some src ->
            handle_skip_while_then_uchar_source src st pred u;
            Source.sync_state_input_and_len st src
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
  let st = Domain.DLS.get State.key in
  let saved = State.save st in
  State.reset_for_input st ~input ~max_depth;
  let result =
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
  State.restore st saved;
  result

let parse_until_end ?(max_depth = 128) input (parser : unit -> 'a) :
    ( 'a,
      [> `Expected of string | `Failure of string | `Unexpected_end_of_input ],
      'd
    )
    result_with_diagnostics =
  let st = Domain.DLS.get State.key in
  let saved = State.save st in
  State.reset_for_input st ~input ~max_depth;
  let result =
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
  State.restore st saved;
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
  let st = Domain.DLS.get State.key in
  let saved = State.save st in
  State.reset_for_source st ~src ~max_depth ~backtrack_window;
  let result =
    match parser () with
    | v ->
        Ok v
    | exception Backtrack_window_exceeded info ->
        Error { pos = info.current; error = `Backtrack_window_exceeded info }
    | exception User_failure (pos, msg) ->
        Error { pos; error = `Failure msg }
    | exception Depth_limit_exceeded (pos, msg) ->
        Error { pos; error = `Depth_limit_exceeded msg }
    | exception exn ->
        default_handler.handle exn
  in
  State.restore st saved;
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
  let st = Domain.DLS.get State.key in
  let saved = State.save st in
  State.reset_for_source st ~src ~max_depth ~backtrack_window;
  let result =
    match
      let v = parser () in
      end_of_input ();
      v
    with
    | v ->
        Ok v
    | exception Backtrack_window_exceeded info ->
        Error { pos = info.current; error = `Backtrack_window_exceeded info }
    | exception User_failure (pos, msg) ->
        Error { pos; error = `Failure msg }
    | exception Depth_limit_exceeded (pos, msg) ->
        Error { pos; error = `Depth_limit_exceeded msg }
    | exception exn ->
        default_handler.handle exn
  in
  let diagnostics = to_diagnostics (State.get_diagnostics_rev st) in
  State.restore st saved;
  attach_diagnostics result diagnostics

let location_of_position input pos =
  let idx = create_line_index () in
  extend_line_index idx input (min pos (String.length input));
  let line_idx = find_line idx pos in
  { offset = pos; line = line_idx + 1; col = pos - idx.starts.(line_idx) + 1 }
