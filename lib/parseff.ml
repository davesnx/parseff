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

exception Fail
exception User_failure of int * string
exception Depth_limit_exceeded of int * string

type line_index = {
  mutable starts : int array;
  mutable count : int;
  mutable scanned_up_to : int;
  mutable line_base : int;
}

let frame_choice = 0
let frame_repeat = 1
let frame_barrier = 2
let frame_last_branch = 3
let frame_kind_mask = 3
let frame_committed_bit = 4
let frame_stride = 3

type backtrack_window_error = { limit : int; oldest : int }

exception Backtrack_window_exceeded of int * backtrack_window_error

type source = {
  initial : string;
  read : bytes -> int -> int -> int;
  mutable eof : bool;
  tmp : bytes;
  mutable window : int;
}

type far = {
  mutable pos : int;
  mutable n : int;
  mutable k0 : int;
  mutable s0 : string;
  mutable k1 : int;
  mutable s1 : string;
  mutable kinds : int array;
  mutable strs : string array;
  mutable special : int;
  mutable obj : Obj.t;
  mutable fctx : int;
  mutable chain_pos : int;
  mutable chain : (string * int * bool) list;
}

let far_none =
  {
    pos = min_int;
    n = 0;
    k0 = 0;
    s0 = "";
    k1 = 0;
    s1 = "";
    kinds = [||];
    strs = [||];
    special = 0;
    obj = Obj.repr 0;
    fctx = 0;
    chain_pos = min_int;
    chain = [];
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
  mutable diagnostics_len : int;
  mutable far : far;
  mutable ctx_top : int;
  mutable ctx : int array;
  mutable line_index : line_index option;
  source : source option;
  mutable frames : int array;
  mutable frames_top : int;
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
      diagnostics_len = 0;
      far = far_none;
      ctx_top = 0;
      ctx = [||];
      line_index = None;
      source;
      frames = [||];
      frames_top = 0;
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

  let leave (t : t) (previous : t) =
    t.active <- false;
    install
      ( if previous.active then
          previous
        else
          none
      )

  let run (t : t) f =
    let previous = current () in
    install t;
    t.active <- true;
    match f () with
    | v ->
        leave t previous;
        v
    | exception exn ->
        let bt = Printexc.get_raw_backtrace () in
        leave t previous;
        Printexc.raise_with_backtrace exn bt

  let[@inline always] get_diagnostics_rev (t : t) = t.diagnostics_rev

  let push_rewind_frame (t : t) kind =
    let index = t.frames_top in
    let base = index * frame_stride in
    if base + frame_stride > Array.length t.frames then begin
      let grown = Array.make (imax 48 (2 * Array.length t.frames)) 0 in
      Array.blit t.frames 0 grown 0 base;
      t.frames <- grown
    end;
    let frames = t.frames in
    Array.unsafe_set frames base t.pos;
    Array.unsafe_set frames (base + 1) kind;
    Array.unsafe_set frames (base + 2) t.diagnostics_len;
    t.frames_top <- index + 1;
    index

  let pop_rewind_frame (t : t) frame =
    if t.frames_top - 1 = frame then
      t.frames_top <- frame
    else
      invalid_arg "Parseff.State.pop_rewind_frame"

  let[@inline always] frame_committed (t : t) frame =
    Array.unsafe_get t.frames ((frame * frame_stride) + 1)
    land frame_committed_bit
    <> 0

  let[@inline always] set_frame_kind (t : t) frame kind =
    Array.unsafe_set t.frames ((frame * frame_stride) + 1) kind

  let restore_rewind_frame (t : t) frame =
    let base = frame * frame_stride in
    t.pos <- Array.unsafe_get t.frames base;
    let saved = Array.unsafe_get t.frames (base + 2) in
    if t.diagnostics_len > saved then begin
      let rec drop l n =
        if n = 0 then
          l
        else
          match l with _ :: rest -> drop rest (n - 1) | [] -> l
      in
      t.diagnostics_rev <- drop t.diagnostics_rev (t.diagnostics_len - saved);
      t.diagnostics_len <- saved
    end

  let add_diagnostic (t : t) pos diagnostic =
    t.diagnostics_rev <- (pos, diagnostic) :: t.diagnostics_rev;
    t.diagnostics_len <- t.diagnostics_len + 1

  let commit_nearest (t : t) =
    let top = t.frames_top in
    if top > 0 then begin
      let slot = ((top - 1) * frame_stride) + 1 in
      let flags = Array.unsafe_get t.frames slot in
      if flags land frame_kind_mask <> frame_barrier then
        Array.unsafe_set t.frames slot (flags lor frame_committed_bit)
    end

  let seal_frames (t : t) ~floor =
    let i = ref (t.frames_top - 1) in
    let reached = ref true in
    while !reached && !i >= floor do
      let slot = (!i * frame_stride) + 1 in
      let flags = Array.unsafe_get t.frames slot in
      if flags land frame_kind_mask = frame_barrier then
        reached := false
      else begin
        Array.unsafe_set t.frames slot (flags lor frame_committed_bit);
        decr i
      end
    done;
    !reached

  let reclaim_floor (t : t) ~keep_from =
    let frames = t.frames in
    let floor = ref keep_from in
    for i = t.frames_top - 1 downto 0 do
      let base = i * frame_stride in
      let flags = Array.unsafe_get frames (base + 1) in
      let kind = flags land frame_kind_mask in
      let start = Array.unsafe_get frames base in
      if
        (kind = frame_barrier
        || (kind <> frame_last_branch && flags land frame_committed_bit = 0)
        )
        && start < !floor
      then
        floor := start
    done;
    !floor
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
    if up_to <= idx.scanned_up_to then begin
      let line = find_line idx up_to in
      if line > 0 then begin
        Array.blit idx.starts line idx.starts 0 (idx.count - line);
        idx.count <- idx.count - line;
        idx.line_base <- idx.line_base + line
      end
    end else begin
      let input = st.input in
      let base = st.base in
      let stop = imin (up_to - base) (String.length input) in
      let newlines = ref 0 and last = ref (-1) in
      for rel = imax 0 (idx.scanned_up_to - base) to stop - 1 do
        if String.unsafe_get input rel = '\n' then begin
          incr newlines;
          last := rel
        end
      done;
      idx.line_base <- idx.line_base + idx.count - 1 + !newlines;
      idx.starts.(0) <-
        ( if !newlines > 0 then
            base + !last + 1
          else
            idx.starts.(idx.count - 1)
        );
      idx.count <- 1;
      idx.scanned_up_to <- up_to
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

let tag_char = 0
let tag_byte = 1
let tag_literal = 2
let tag_label = 3
let tag_end_of_input = 4
let tag_bytes = 5
let tag_at_least = 6
let far_max_capacity = 62

let[@inline never] far_advance_int st p k =
  let f = st.far in
  if f == far_none then
    st.far <-
      {
        pos = p;
        n = 1;
        k0 = k;
        s0 = "";
        k1 = 0;
        s1 = "";
        kinds = [||];
        strs = [||];
        special = 0;
        obj = Obj.repr 0;
        fctx = st.ctx_top;
        chain_pos = min_int;
        chain = [];
      }
  else begin
    f.pos <- p;
    f.fctx <- st.ctx_top;
    f.special <- 0;
    f.k0 <- k;
    f.n <- 1
  end

let[@inline never] far_advance_str st p k s =
  let f = st.far in
  if f == far_none then
    st.far <-
      {
        pos = p;
        n = 1;
        k0 = k;
        s0 = s;
        k1 = 0;
        s1 = "";
        kinds = [||];
        strs = [||];
        special = 0;
        obj = Obj.repr 0;
        fctx = st.ctx_top;
        chain_pos = min_int;
        chain = [];
      }
  else begin
    f.pos <- p;
    f.fctx <- st.ctx_top;
    f.special <- 0;
    f.k0 <- k;
    if f.s0 != s then f.s0 <- s;
    f.n <- 1
  end

let[@inline never] far_advance_special st p =
  let f = st.far in
  if f == far_none then
    st.far <-
      {
        pos = p;
        n = 1;
        k0 = 0;
        s0 = "";
        k1 = 0;
        s1 = "";
        kinds = [||];
        strs = [||];
        special = 0;
        obj = Obj.repr 0;
        fctx = st.ctx_top;
        chain_pos = min_int;
        chain = [];
      }
  else begin
    f.pos <- p;
    f.fctx <- st.ctx_top;
    f.special <- 0;
    f.n <- 1
  end

let far_item_kind f i =
  if i = 0 then
    f.k0
  else if i = 1 then
    f.k1
  else
    Array.unsafe_get f.kinds (i - 2)

let far_item_str f i =
  if i = 0 then
    f.s0
  else if i = 1 then
    f.s1
  else
    Array.unsafe_get f.strs (i - 2)

let far_set_item f i k s =
  if i = 0 then begin
    f.k0 <- k;
    f.s0 <- s
  end else if i = 1 then begin
    f.k1 <- k;
    f.s1 <- s
  end else begin
    f.kinds.(i - 2) <- k;
    f.strs.(i - 2) <- s
  end

let far_compact f =
  let items = ref [] in
  for i = f.n - 1 downto 0 do
    let k = far_item_kind f i in
    let tag = k land 7 in
    let s =
      if tag = tag_literal || tag = tag_label then
        far_item_str f i
      else
        ""
    in
    items := (k, s) :: !items
  done;
  let items = List.sort_uniq compare !items in
  f.n <- 0;
  List.iteri
    (fun i (k, s) ->
      far_set_item f i k s;
      f.n <- i + 1
    )
    items

let[@inline never] far_push_slow f k s =
  let cap = Array.length f.kinds in
  if cap = 0 then begin
    f.kinds <- [| 0; 0; 0; 0; 0; 0 |];
    f.strs <- [| ""; ""; ""; ""; ""; "" |]
  end else if cap < far_max_capacity then begin
    let ncap = imin ((2 * cap) + 2) far_max_capacity in
    let kinds = Array.make ncap 0 in
    let strs = Array.make ncap "" in
    Array.blit f.kinds 0 kinds 0 cap;
    Array.blit f.strs 0 strs 0 cap;
    f.kinds <- kinds;
    f.strs <- strs
  end else
    far_compact f;
  let i = f.n - 2 in
  if i >= 0 then
    begin if i < Array.length f.kinds then begin
      f.kinds.(i) <- k;
      if f.strs.(i) != s then f.strs.(i) <- s;
      f.n <- f.n + 1
    end
  end
  else begin
    far_set_item f f.n k s;
    f.n <- f.n + 1
  end

let[@inline always] far_push_int f k =
  let n = f.n in
  if n = 1 then begin
    f.k1 <- k;
    f.n <- 2
  end else begin
    let i = n - 2 in
    if i >= 0 && i < Array.length f.kinds then begin
      Array.unsafe_set f.kinds i k;
      f.n <- n + 1
    end else
      far_push_slow f k ""
  end

let[@inline always] far_push_str f k s =
  let n = f.n in
  if n = 1 then begin
    f.k1 <- k;
    if f.s1 != s then f.s1 <- s;
    f.n <- 2
  end else begin
    let i = n - 2 in
    if i >= 0 && i < Array.length f.kinds then begin
      Array.unsafe_set f.kinds i k;
      if Array.unsafe_get f.strs i != s then Array.unsafe_set f.strs i s;
      f.n <- n + 1
    end else
      far_push_slow f k s
  end

let far_relabel f n0 label =
  f.n <- n0;
  if n0 = 0 then begin
    f.k0 <- tag_label;
    f.s0 <- label;
    f.n <- 1
  end else
    far_push_str f tag_label label

let[@inline always] fail_int st p k =
  let f = st.far in
  let fp = f.pos in
  if p < fp then raise_notrace Fail;
  if p = fp then
    far_push_int f k
  else
    far_advance_int st p k;
  raise_notrace Fail

let[@inline always] fail_str st p k s =
  let f = st.far in
  let fp = f.pos in
  if p < fp then raise_notrace Fail;
  if p = fp then
    far_push_str f k s
  else
    far_advance_str st p k s;
  raise_notrace Fail

let[@inline never] fail_int_cold st p k = fail_int st p k
let[@inline never] fail_str_cold st p k s = fail_str st p k s

let special_message = 1
let special_user = 2

let[@inline never] fail_special st p kind obj =
  let f = st.far in
  if p < f.pos then raise_notrace Fail;
  if p > f.pos then far_advance_special st p;
  let f = st.far in
  if f.special = 0 then begin
    f.special <- kind;
    f.obj <- obj
  end;
  raise_notrace Fail

let[@inline always] char_kind c =
  let code = Char.code c in
  if code < 0x80 then
    code lsl 3
  else
    (code lsl 3) lor tag_byte

let[@inline always] bytes_kind n = (imin n (max_int lsr 3) lsl 3) lor tag_bytes

let[@inline always] at_least_kind n =
  (imin n (max_int lsr 3) lsl 3) lor tag_at_least
let label_value = "value"
let label_regex = "regex match"

let[@inline never] far_commit_reset st =
  let f = st.far in
  if f != far_none then begin
    f.pos <- -1;
    f.n <- 0;
    f.special <- 0;
    f.chain_pos <- min_int
  end

let[@inline never] handle_consume st s =
  let len = String.length s in
  if len = 1 then begin
    let c = String.unsafe_get s 0 in
    if have st 1 then
      if char_at st st.pos = c then begin
        st.pos <- st.pos + 1;
        s
      end else
        fail_str st st.pos tag_literal s
    else
      fail_str st st.pos tag_literal s
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
        i
      else if char_at st (pos + i) = String.unsafe_get s i then
        check (i + 1)
      else
        i
    in
    let matched = check 0 in
    if matched < avail || not complete then
      fail_str st (pos + matched) ((matched lsl 3) lor tag_literal) s
    else begin
      st.pos <- pos + len;
      s
    end
  end

let[@inline] handle_satisfy st pred label =
  if have st 1 then
    let c = char_at st st.pos in
    if pred c then begin
      st.pos <- st.pos + 1;
      c
    end else
      fail_str st st.pos tag_label label
  else
    fail_str st st.pos tag_label label

(* dedicated char handler it avoids closure + String.make allocation *)
let[@inline] handle_match_char st c =
  if have st 1 then
    let ch = char_at st st.pos in
    if ch = c then begin
      st.pos <- st.pos + 1;
      c
    end else
      fail_int st st.pos (char_kind c)
  else
    fail_int st st.pos (char_kind c)

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
    fail_int st pos (char_kind c)
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
      fail_str st end_pos tag_label label_value
    end
  end else begin
    st.pos <- pos;
    fail_int st pos (char_kind sep_char)
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

let regex_failed = "regex match failed"
let invalid_utf8_msg = "invalid UTF-8"

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
        fail_special st st.pos special_message (Obj.repr regex_failed)
      end else begin
        if match_end >= st.input_len then raise_if_pinned ();
        st.pos <- match_end;
        Re.Group.get groups 0
      end
  | exception Not_found ->
      raise_if_pinned ();
      if st.pos >= st.input_len then
        fail_str st st.pos tag_label label_regex
      else
        fail_special st st.pos special_message (Obj.repr regex_failed)

let[@inline never] handle_any_uint8 st =
  if have st 1 then begin
    let v = String.get_uint8 st.input (st.pos - st.base) in
    st.pos <- st.pos + 1;
    v
  end else
    fail_int st st.pos (bytes_kind 1)

let[@inline never] handle_any_int8 st =
  if have st 1 then begin
    let v = String.get_int8 st.input (st.pos - st.base) in
    st.pos <- st.pos + 1;
    v
  end else
    fail_int st st.pos (bytes_kind 1)

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
    fail_int st st.pos (bytes_kind 2)

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
    fail_int st st.pos (bytes_kind 4)

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
    fail_int st st.pos (bytes_kind 8)

let[@inline never] handle_take st n =
  if have st n then begin
    let s = slice st st.pos n in
    st.pos <- st.pos + n;
    s
  end else
    fail_int st st.pos (bytes_kind n)

exception Invalid_utf8 of int

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

let[@inline] fill_uchar st ~keep_from p =
  if p + 4 > st.input_len then utf8_fill st keep_from p

let[@inline never] handle_satisfy_uchar st pred label =
  let p = st.pos in
  fill_uchar st ~keep_from:p p;
  if p >= st.input_len then fail_str st p tag_label label;
  let d = String.get_utf_8_uchar st.input (p - st.base) in
  if not (Uchar.utf_decode_is_valid d) then raise (Invalid_utf8 p);
  let u = Uchar.utf_decode_uchar d in
  if pred u then begin
    st.pos <- p + Uchar.utf_decode_length d;
    u
  end else
    fail_str st p tag_label label

let[@inline never] handle_char_uchar st expected =
  let p = st.pos in
  fill_uchar st ~keep_from:p p;
  let kind = Uchar.to_int expected lsl 3 in
  if p >= st.input_len then fail_int st p kind;
  let d = String.get_utf_8_uchar st.input (p - st.base) in
  if not (Uchar.utf_decode_is_valid d) then raise (Invalid_utf8 p);
  let u = Uchar.utf_decode_uchar d in
  if Uchar.equal u expected then begin
    st.pos <- p + Uchar.utf_decode_length d;
    u
  end else
    fail_int st p kind

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
  let kind = Uchar.to_int term lsl 3 in
  if pos >= st.input_len then begin
    st.pos <- pos;
    fail_int st pos kind
  end;
  fill_uchar st ~keep_from:pos pos;
  if pos >= st.input_len then fail_int st pos kind;
  let d = String.get_utf_8_uchar st.input (pos - st.base) in
  if not (Uchar.utf_decode_is_valid d) then raise (Invalid_utf8 pos);
  let u = Uchar.utf_decode_uchar d in
  if Uchar.equal u term then
    st.pos <- pos + Uchar.utf_decode_length d
  else begin
    st.pos <- pos;
    fail_int st pos kind
  end

let[@inline] turn_invalid_utf8_to_parse_error st f =
  try f ()
  with Invalid_utf8 pos ->
    fail_special st pos special_message (Obj.repr invalid_utf8_msg)

let[@inline always] restore_rewind_frame st frame =
  State.restore_rewind_frame st frame

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
  if st.pos <> st.input_len then fail_int_cold st st.pos tag_end_of_input

let take_while ?(at_least = 0) ?label pred =
  let st = get_state () in
  let s = handle_take_while st pred in
  if at_least > 0 && String.length s < at_least then
    begin match label with
    | Some l ->
        fail_str_cold st st.pos tag_label l
    | None ->
        fail_int_cold st st.pos (at_least_kind at_least)
  end
  else
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
  fail_special st st.pos special_user (Obj.repr e)

let[@inline] warn diagnostic =
  let st = get_state () in
  State.add_diagnostic st st.pos (Obj.repr diagnostic)

let[@inline] warn_at ~pos diagnostic =
  let st = get_state () in
  State.add_diagnostic st pos (Obj.repr diagnostic)

let[@inline] commit () =
  let st = get_state () in
  State.commit_nearest st;
  if st.far.pos >= st.pos then far_commit_reset st

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
  let frame = State.push_rewind_frame st frame_choice in
  match left () with
  | v ->
      State.pop_rewind_frame st frame;
      v
  | exception Fail -> (
      if State.frame_committed st frame then begin
        State.pop_rewind_frame st frame;
        raise_notrace Fail
      end;
      restore_rewind_frame st frame;
      State.set_frame_kind st frame frame_last_branch;
      match right () with
      | v ->
          State.pop_rewind_frame st frame;
          v
      | exception Fail ->
          State.pop_rewind_frame st frame;
          raise_notrace Fail
      | exception exn ->
          State.pop_rewind_frame st frame;
          raise exn
    )
  | exception exn ->
      State.pop_rewind_frame st frame;
      raise exn

let look_ahead p =
  let st = get_state () in
  let frame = State.push_rewind_frame st frame_barrier in
  match p () with
  | v ->
      restore_rewind_frame st frame;
      State.pop_rewind_frame st frame;
      v
  | exception exn ->
      restore_rewind_frame st frame;
      State.pop_rewind_frame st frame;
      raise exn

let ctx_stride = 2
let ctx_cut_bit = 1

let[@inline never] ctx_grow st =
  let old = st.ctx in
  let meta = Array.make (imax 16 (2 * Array.length old)) 0 in
  Array.blit old 0 meta 0 (Array.length old);
  st.ctx <- meta

let[@inline never] far_contribute f d name start delimited =
  let entry = (name, start, delimited) in
  if f.chain_pos <> f.pos then begin
    f.chain <- [ entry ];
    f.chain_pos <- f.pos
  end else
    f.chain <- entry :: f.chain;
  f.fctx <- d

let[@inline always] context_exit st d name start delimited =
  let f = st.far in
  if f.fctx = d + 1 then far_contribute f d name start delimited;
  st.ctx_top <- d

let[@inline never] context_exit_cut st d =
  if Array.unsafe_get st.ctx ((d * ctx_stride) + 1) land ctx_cut_bit <> 0 then
    ignore (State.seal_frames st ~floor:0)

let context ?(delimited = false) name p =
  let st = get_state () in
  let d = st.ctx_top in
  if (d + 1) * ctx_stride > Array.length st.ctx then ctx_grow st;
  let start = st.pos in
  let m = d * ctx_stride in
  Array.unsafe_set st.ctx m st.frames_top;
  Array.unsafe_set st.ctx (m + 1) 0;
  st.ctx_top <- d + 1;
  match p () with
  | v ->
      context_exit st d name start delimited;
      v
  | exception Fail ->
      context_exit st d name start delimited;
      context_exit_cut st d;
      raise_notrace Fail
  | exception exn ->
      context_exit st d name start delimited;
      raise exn

let cut () =
  let st = get_state () in
  let d = st.ctx_top in
  if d = 0 then
    ignore (State.seal_frames st ~floor:0)
  else begin
    let m = (d - 1) * ctx_stride in
    let ctx = st.ctx in
    if State.seal_frames st ~floor:(Array.unsafe_get ctx m) then
      Array.unsafe_set ctx (m + 1) (Array.unsafe_get ctx (m + 1) lor ctx_cut_bit)
  end

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
      let frame = State.push_rewind_frame st frame_repeat in
      let start = st.pos in
      match p () with
      | v ->
          State.pop_rewind_frame st frame;
          if st.pos = start then
            continue_ref := false
          else
            acc := v :: !acc
      | exception Fail ->
          State.pop_rewind_frame st frame;
          if State.frame_committed st frame then
            raise_notrace Fail
          else begin
            restore_rewind_frame st frame;
            continue_ref := false
          end
      | exception exn ->
          State.pop_rewind_frame st frame;
          raise exn
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
      let frame = State.push_rewind_frame st frame_repeat in
      let start = st.pos in
      match p () with
      | v ->
          State.pop_rewind_frame st frame;
          if st.pos = start then
            continue_ref := false
          else
            acc := v :: !acc
      | exception Fail ->
          State.pop_rewind_frame st frame;
          if State.frame_committed st frame then
            raise_notrace Fail
          else begin
            restore_rewind_frame st frame;
            continue_ref := false
          end
      | exception exn ->
          State.pop_rewind_frame st frame;
          raise exn
    done;
    List.rev_append required_rev (List.rev !acc)
  end

let sep_by ?(at_least = 0) (p : unit -> 'a) (sep : unit -> 'b) () : 'a list =
  let st = get_state () in
  if at_least <= 0 then (
    let start_frame = State.push_rewind_frame st frame_repeat in
    match p () with
    | first ->
        State.pop_rewind_frame st start_frame;
        let acc = ref [ first ] in
        let continue_ref = ref true in
        while !continue_ref do
          let frame = State.push_rewind_frame st frame_repeat in
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
          | Fail ->
              State.pop_rewind_frame st frame;
              if State.frame_committed st frame then
                raise_notrace Fail
              else begin
                restore_rewind_frame st frame;
                continue_ref := false
              end
          | exn ->
              State.pop_rewind_frame st frame;
              raise exn
        done;
        List.rev !acc
    | exception Fail ->
        State.pop_rewind_frame st start_frame;
        if State.frame_committed st start_frame then
          raise_notrace Fail
        else begin
          restore_rewind_frame st start_frame;
          []
        end
    | exception exn ->
        State.pop_rewind_frame st start_frame;
        raise exn
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
    let frame = State.push_rewind_frame st frame_repeat in
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
    | exception Fail ->
        State.pop_rewind_frame st frame;
        if State.frame_committed st frame then
          raise_notrace Fail
        else begin
          restore_rewind_frame st frame;
          continue_ref := false
        end
    | exception exn ->
        State.pop_rewind_frame st frame;
        raise exn
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

let expect label p =
  let st = get_state () in
  let start = st.pos in
  let n0 =
    if st.far.pos = start then
      st.far.n
    else
      0
  in
  match p () with
  | v ->
      v
  | exception Fail ->
      let f = st.far in
      if f.pos = start && f.n > n0 then far_relabel f n0 label;
      raise_notrace Fail

let catch p handler = try p () with User_failure (_pos, msg) -> handler msg

let one_of parsers () =
  let rec try_all = function
    | [] ->
        let st = get_state () in
        fail_special st st.pos special_message
          (Obj.repr "no alternative matched")
    | [ p ] ->
        p ()
    | p :: rest ->
        or_ p (fun () -> try_all rest) ()
  in
  try_all parsers

let one_of_labeled labeled_parsers () =
  one_of (List.map (fun (label, p) () -> expect label p) labeled_parsers) ()

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
    fail_special st start special_message
      (Obj.repr
         (Printf.sprintf "expected int16 0x%04X, got 0x%04X" expected actual)
      )

let expect_int32 any_fn expected =
  let st = get_state () in
  let start = st.pos in
  let actual = any_fn () in
  if actual <> expected then
    fail_special st start special_message
      (Obj.repr
         (Printf.sprintf "expected int32 0x%08lX, got 0x%08lX" expected actual)
      )

let expect_int64 any_fn expected =
  let st = get_state () in
  let start = st.pos in
  let actual = any_fn () in
  if actual <> expected then
    fail_special st start special_message
      (Obj.repr
         (Printf.sprintf "expected int64 0x%016LX, got 0x%016LX" expected actual)
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
    turn_invalid_utf8_to_parse_error st (fun () ->
        handle_satisfy_uchar st pred label
    )

  let[@inline] char u =
    let st = get_state () in
    turn_invalid_utf8_to_parse_error st (fun () -> handle_char_uchar st u)

  let[@inline] any_char () = satisfy (fun _ -> true) ~label:"any character"

  let take_while ?(at_least = 0) ?label pred =
    let st = get_state () in
    let s =
      turn_invalid_utf8_to_parse_error st (fun () ->
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
      if !count < at_least then
        begin match label with
        | Some l ->
            fail_str_cold st st.pos tag_label l
        | None ->
            fail_int_cold st st.pos (at_least_kind at_least)
      end
      else
        s
    end else
      s

  let[@inline] skip_while pred =
    let st = get_state () in
    turn_invalid_utf8_to_parse_error st (fun () ->
        handle_skip_while_uchar st pred
    )

  let[@inline] take_while_span pred =
    let st = get_state () in
    turn_invalid_utf8_to_parse_error st (fun () ->
        handle_take_while_span_uchar st pred
    )

  let[@inline] skip_while_then_char pred u =
    let st = get_state () in
    turn_invalid_utf8_to_parse_error st (fun () ->
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

let format_char code =
  if code < 0x80 then
    "'" ^ Char.escaped (Char.chr code) ^ "'"
  else begin
    let b = Buffer.create 6 in
    Buffer.add_char b '\'';
    Buffer.add_utf_8_uchar b (Uchar.of_int code);
    Buffer.add_char b '\'';
    Buffer.contents b
  end

let describe_item kind s =
  let payload = kind lsr 3 in
  let tag = kind land 7 in
  if tag = tag_char then
    (1, format_char payload)
  else if tag = tag_byte then
    (2, "\"" ^ String.escaped (String.make 1 (Char.chr payload)) ^ "\"")
  else if tag = tag_literal then
    ( 2,
      "\""
      ^ String.escaped (String.sub s payload (String.length s - payload))
      ^ "\""
    )
  else if tag = tag_label then
    (0, s)
  else if tag = tag_end_of_input then
    (3, "end of input")
  else if tag = tag_bytes then
    ( 0,
      if payload = 1 then
        "1 byte"
      else
        string_of_int payload ^ " bytes"
    )
  else
    ( 0,
      if payload = 1 then
        "at least 1 character"
      else
        "at least " ^ string_of_int payload ^ " characters"
    )

let format_expected f =
  let items = ref [] in
  for i = 0 to f.n - 1 do
    items := describe_item (far_item_kind f i) (far_item_str f i) :: !items
  done;
  let items = List.sort_uniq compare !items in
  match List.map snd items with
  | [] ->
      "expected something else"
  | [ a ] ->
      "expected " ^ a
  | [ a; b ] ->
      "expected " ^ a ^ " or " ^ b
  | first :: rest ->
      let rec join = function
        | [] ->
            ""
        | [ last ] ->
            "or " ^ last
        | x :: tl ->
            x ^ ", " ^ join tl
      in
      "expected " ^ first ^ ", " ^ join rest

let truncated_read f =
  let found = ref false in
  for i = 0 to f.n - 1 do
    if far_item_kind f i land 7 = tag_bytes then found := true
  done;
  !found

let far_error st =
  let f = st.far in
  let pos = f.pos in
  if pos < 0 then
    failwith "Parseff: internal error: failure without a recorded position";
  if f.special = special_user then
    Error { pos; error = Obj.obj f.obj }
  else if f.special = special_message then
    Error { pos; error = `Expected (Obj.obj f.obj : string) }
  else if pos >= st.input_len || truncated_read f then
    Error { pos; error = `Unexpected_end_of_input }
  else
    Error { pos; error = `Expected (format_expected f) }

let far_chain st =
  let f = st.far in
  if f.chain_pos = f.pos then
    f.chain
  else
    []

let parse_failure_frames ?(max_depth = 128) input (parser : unit -> 'a) =
  let st = State.for_input ~input ~max_depth in
  State.run st @@ fun () ->
  match parser () with
  | v ->
      Stdlib.Ok v
  | exception Fail ->
      Stdlib.Error (far_chain (State.current ()))

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
    | exception Fail ->
        far_error (State.current ())
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
    | exception Fail ->
        far_error (State.current ())
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
    | exception Fail ->
        far_error (State.current ())
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
    | exception Fail ->
        far_error (State.current ())
  in
  let diagnostics = to_diagnostics (State.get_diagnostics_rev st) in
  attach_diagnostics result diagnostics

let location_of_position input pos =
  let idx = create_line_index () in
  extend_line_index idx input (min pos (String.length input));
  let line_idx = find_line idx pos in
  { offset = pos; line = line_idx + 1; col = pos - idx.starts.(line_idx) + 1 }
