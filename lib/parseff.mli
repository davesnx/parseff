(** Parser combinator library with an imperative-style API.

    Parseff is a parser combinator library that uses algebraic effects to allow
    an imperative-style API for parsers and ensures

    {1 Quick Start}

    {@ocaml[
      let number () =
        let c = satisfy (fun c -> c >= '0' && c <= '9') ~label:"digit" in
        Char.code c - Char.code '0'

      let ip_address () =
        let a = number () in
        let _ = Parseff.consume "." in
        let b = number () in
        let _ = Parseff.consume "." in
        let c = number () in
        let _ = Parseff.consume "." in
        let d = number () in
        Parseff.end_of_input ();
        (a, b, c, d)

      match Parseff.parse "192.168.1.1" ip_address with
      | Ok (a, b, c, d) -> Printf.printf "Parsed: %d.%d.%d.%d\n" a b c d
      | Error e -> prerr_endline (Parseff.render_error ~name:"address" e)
    ]} *)

(** {1 Span Type} *)

type span = { buf : string; off : int; len : int }
(** A slice of the input string. With [parse] it is zero-copy: [buf] is the
    input string. With [parse_source] it is an owned copy with [off = 0]. Use
    [span_to_string] to materialize when needed. *)

val span_to_string : span -> string
(** [span_to_string s] extracts the string from a span. Only call this when you
    actually need the string value. *)

(** {1 Location Type} *)

type location = { offset : int; line : int; col : int }
(** A source location. [line] and [col] are 1-based. [col] counts bytes from the
    start of the line (not characters — relevant for UTF-8). *)

(** {1 Errors} *)

(** A parse error: what failed, where, and what the parser was doing.

    A failed parse returns one [Error.t]. Match on its [kind] to react to the
    failure, or print it with {!render_error}. *)
module Error : sig
  type range = { start : int; stop : int }
  (** A range of byte offsets, [start] included and [stop] excluded, with
      [start <= stop]. *)

  type expected = private
    | Char of Uchar.t  (** A specific character, printed as ['x']. *)
    | Literal of string  (** A literal string, printed as ["abc"]. *)
    | Label of string
        (** A name given by {!satisfy}, {!take_while}, {!expect} and others. *)
    | End_of_input  (** The end of the input. *)

  type found = private
    | Text of string
        (** The text at the failure position. A token cut at 32 bytes ends with
            [...]. *)
    | End  (** The input ended. *)
    | Short of int
        (** A read of a fixed number of bytes, such as {!val:Parseff.take},
            found fewer bytes than it needed before the end of the input. The
            value is how many bytes were left. *)
    | Value of string
        (** An integer field that differs from the expected one, as the
            hexadecimal value read in the same width, for example [0x0002]. *)

  type frame = private { name : string; start : int; delimited : bool }
  (** A construct that was open at the failure, see {!val:Parseff.context}.
      [start] is the byte offset where it started, [delimited] is true for
      constructs with an opening token. *)

  type note = { at : range option; text : string }
  (** An annotation added with [~notes] in {!val:Parseff.fail} or
      {!val:Parseff.error}. [at = Some r] underlines the bytes [r] in the source
      excerpt with the text next to the underline. [at = None] adds a
      ["* note:"] line under the excerpt. *)

  type position = private { location : location; code_point_col : int option }
  (** Where an error is in the input, known only when the line is known.
      [location] is the line and column of the error in bytes. [code_point_col]
      is the column in code points, [None] if the start of the line is no longer
      buffered (streaming). *)

  type excerpt
  (** Source lines kept with the error so that {!Parseff.render_error} does not
      need the input. *)

  type 'e kind = private
    | Expected of {
        items : expected * expected list;
        found : found;
        more : int;
      }
        (** A primitive failed. [items] is everything that was expected at the
            furthest failure position, sorted and without duplicates, up to 64
            items. [more] is how many further alternatives were expected and are
            not in [items], [0] unless more than 64 were. *)
    | Failure of string
        (** A failure raised with {!val:Parseff.fail}, or
            ["no alternative matched"] when [one_of []] is the only failure at
            the furthest position. Failures of primitives are otherwise
            [Expected]. *)
    | User of 'e  (** A value given to {!val:Parseff.error}. *)
    | Depth_limit_exceeded of { limit : int }
        (** {!val:Parseff.rec_} nested deeper than [max_depth]. *)
    | Backtrack_window_exceeded of { limit : int; oldest : int }
        (** A streaming parser needed more rewind history than the window.
            [oldest] is the earliest position the runtime still needs to rewind
            to and [limit] is the configured [backtrack_window]. The error
            position is the parser position when the overflow happened. *)

  type 'e t = private {
    kind : 'e kind;
    span : range;
        (** The bytes the error is about, never reversed, empty at the end of
            the input. *)
    position : position option;
        (** Line and column of [span.start]. [None] when the line is not known:
            in streaming, when the position is older than the buffered input.
            {!Parseff.render_error} prints the byte offset then. *)
    frames : frame list;  (** Open constructs, outermost first. *)
    notes : note list;
    excerpt : excerpt;
  }

  val message : ?user:('e -> string) -> 'e t -> string
  (** [message e] is the one-line message, for example
      [expected "else", "if", or "while", found 'xyz']. A message lists at most
      8 expected items, then [, or N more alternatives]. [user] prints [User]
      errors, without it the message is [user error (no ~user printer given)].
  *)

  type columns = [ `Bytes | `Code_points ]
end

val render_error :
  ?name:string ->
  ?source:string ->
  ?columns:Error.columns ->
  ?user:('e -> string) ->
  'e Error.t ->
  string
(** [render_error e] prints the error with a source excerpt:

    {v
    input.json:4:5: error: expected ',' or '}', found '"'
    2 |     "config": {
      | ╭─────────────╯
    3 | │     "a": 1
    4 | │     "b": 2
      | ╰─────^
      * while parsing object, from 2:13
      * while parsing object, from 1:1
    v}

    [name] is printed at the start of the header. [source] is the parsed input;
    without it the excerpt kept in the error is used, which is enough for the
    error line and the lines around it, and may miss old lines in streaming.
    When the line of the error is not known, which happens in streaming when the
    position is older than the buffered input, the header is
    [input.json: byte N: error: ...] and there is no excerpt. [columns] chooses
    the column unit of the header, [`Bytes] by default; it applies to the
    columns of [* while parsing] lines too. Carets count code points, and a
    control byte, an invalid UTF-8 byte or a C1 control prints in the excerpt as
    the escape [message] uses, such as [\x1B], with as many carets as the escape
    has characters. A [Depth_limit_exceeded] or [Backtrack_window_exceeded]
    error ends with a [* help:] line. Rows have no trailing spaces and the
    result has no trailing newline. *)

(** {1 Result Types} *)

type ('a, 'e) result = ('a, 'e Error.t) Stdlib.result
(** Parse result. ['a] is the parsed value and ['e] is the type of the values
    given to {!val:error}. A parse that never calls [error] leaves ['e]
    unconstrained.

    When a parse fails, the error is the furthest failure: the failure of any
    primitive that happened at the highest input position, including primitives
    in branches that [or_], [many], [sep_by], [optional] or [fold_*] abandoned.
    The [Expected] kind lists everything that was expected at that position,
    sorted and without duplicates:
    [expected "else", "if", or "while", found 'xyz']. A failure at the end of
    the input says [found end of input] and keeps the expected set.

    {[
      match Parseff.parse input parser with
      | Ok value -> ...
      | Error { kind = User (`Too_large n); _ } -> ...
      | Error e -> prerr_endline (Parseff.render_error ~name:"input" e)
    ]} *)

type 'd diagnostic = { pos : int; diagnostic : 'd }
(** Non-fatal diagnostic emitted during parsing. *)

type ('e, 'd) error_with_diagnostics = {
  error : 'e Error.t;
  diagnostics : 'd diagnostic list;
}

type ('a, 'e, 'd) result_with_diagnostics =
  ('a * 'd diagnostic list, ('e, 'd) error_with_diagnostics) Stdlib.result
(** Parse outcome with diagnostics in both success and failure cases. *)

(** {1 Runner} *)

val parse : ?max_depth:int -> string -> (unit -> 'a) -> ('a, 'e) result
(** [parse ?max_depth input parser] runs [parser] on [input] string.

    [max_depth] limits the nesting depth for parsers that use {!rec_} to mark
    recursive entry points. Defaults to [128]. When exceeded, parsing fails with
    [Error.Depth_limit_exceeded] instead of risking a stack overflow.

    This function does not require consuming the full input; call
    {!end_of_input} in your parser when you want full-consumption behavior.

    Returns [Ok result] on success, or [Error e] on failure, where [e] is an
    {!Error.t}. [e.kind] is [Expected] for a mismatch, [Failure] for
    {!val:fail}, [User] for {!val-error}, [Depth_limit_exceeded] when recursion
    nested deeper than [max_depth], and [Backtrack_window_exceeded] for
    streaming.

    Example:
    {@ocaml[
    match parse "hello" (fun () -> consume "hello") with
    | Ok s ->
        Printf.printf "Matched %S\n" s
    | Error { kind = Failure msg; _ } ->
        Printf.printf "Failed: %s\n" msg
    | Error e ->
        prerr_endline (render_error ~name:"input" e)
    ]} *)

val parse_until_end :
  ?max_depth:int ->
  string ->
  (unit -> 'a) ->
  ('a, 'e, 'd) result_with_diagnostics
(** [parse_until_end ?max_depth input parser] runs [parser] on [input], requires
    that all input is consumed, and returns diagnostics on both success and
    failure.

    Non-fatal diagnostics can be emitted with {!warn} and {!warn_at}. *)

(** {1 Primitive Combinators} *)

val consume : string -> string
(** [consume s] matches the exact literal string [s]. Fails at the first byte
    that differs from [s], expecting the rest of [s]. The failure position is
    the mismatch, not the start of [s]: [consume "ab"] on ["ax"] fails at offset
    [1] and expects ["b"]. When the input ends inside [s], the error says
    [found end of input].

    Example:
    {@ocaml[
    let parser () =
      consume "hello";
      consume " ";
      consume "world"
    ]} *)

val satisfy : (char -> bool) -> label:string -> char
(** [satisfy fun ~label] matches the next character if [fun] returns [true] for
    it. If the character doesn't match or input is empty, fails with [label] in
    the error message.

    Example:
    {@ocaml[
    let vowel () = satisfy (fun c -> String.contains "aeiou" c) ~label:"vowel"
    ]} *)

val char : char -> char
(** [char c] matches the exact character [c].

    Example:
    {@ocaml[
    let comma () = char ','
    ]} *)

val match_regex : ?label:string -> Re.re -> string
(** [match_regex ?label re] matches a regular expression. The regex must be
    compiled with [Re.compile].

    When the regex does not match, the parse fails with [expected <label>] if
    [~label] is given, so the failure merges with the other alternatives
    expected at the same position like any other primitive. Without [~label] the
    label is ["text matching the pattern"].

    In streaming mode, the parser keeps at least half of the backtrack window
    buffered ahead of the current position, unless the source has ended. A match
    is limited to the buffered input, so it cannot extend past the window. The
    byte before the current position stays visible to [Re.bol] and word
    boundaries.

    If the window is full of rewind history, the regex runs on the buffered
    input. A match that ends before the end of the buffer is returned. A failed
    match, or one that reaches the end of the buffer, fails with
    [Error.Backtrack_window_exceeded], because more input could change the
    result.

    The match must start at the current position, but [Re] searches forward when
    it does not. A failed match therefore scans the rest of the input, and a
    loop that fails at each position takes quadratic time.

    Example:
    {@ocaml[
    let identifier () =
      let re = Re.compile (Re.Posix.re "[a-zA-Z_][a-zA-Z0-9_]*") in
      match_regex re
    ]} *)

val take_while : ?at_least:int -> ?label:string -> (char -> bool) -> string
(** [take_while fun] reads characters one by one as long as [fun] returns [true]
    for each one. Returns the matched string (may be empty if the very first
    character doesn't match). Much faster than regex for simple character
    classes.

    When [~at_least] is specified, requires at least that many characters to
    match. Fails with [expected <label>] if fewer characters match, or with
    [expected at least n characters] when [~label] is missing. The failure
    position is the end of the characters that matched.

    Example:
    {@ocaml[
    let digits () = take_while (fun c -> c >= '0' && c <= '9')
    let digits1 () =
      take_while ~at_least:1 (fun c -> c >= '0' && c <= '9') ~label:"digit"
    ]} *)

val skip_while : (char -> bool) -> unit
(** [skip_while fun] advances past characters as long as [fun] returns [true].
    Like {!take_while} but doesn't build a string — use this when you only need
    to move past characters. Always succeeds (skips nothing if the first
    character doesn't match).

    Example:
    {@ocaml[
    let skip_spaces () = skip_while (fun c -> c = ' ')
    ]} *)

val skip_while_then_char : (char -> bool) -> char -> unit
(** [skip_while_then_char fun c] skips characters where [fun] returns [true],
    then matches the exact character [c]. More efficient than calling
    {!skip_while} followed by {!char} separately. *)

val sep_by_take : (char -> bool) -> char -> (char -> bool) -> string list
(** [sep_by_take is_whitespace separator is_value_char] parses a list of values
    separated by [separator]. Whitespace (characters where [is_whitespace]
    returns [true]) is skipped around each separator. Each value consists of
    characters where [is_value_char] returns [true]. Returns a list of matched
    strings. Runs entirely in a single operation for maximum efficiency. *)

val take_while_span : (char -> bool) -> span
(** [take_while_span fun] like {!take_while} but returns a zero-copy {!span}
    instead of allocating a string. No memory allocation until you call
    {!span_to_string}. *)

val sep_by_take_span : (char -> bool) -> char -> (char -> bool) -> span list
(** [sep_by_take_span is_whitespace separator is_value_char] like {!sep_by_take}
    but returns zero-copy {!span}s instead of strings. No [String.sub]
    allocations per element. *)

val fused_sep_take : (char -> bool) -> char -> (char -> bool) -> string
(** [fused_sep_take is_whitespace separator is_value_char] skips whitespace,
    matches [separator], skips whitespace again, then reads one or more
    characters where [is_value_char] returns [true]. All steps run in a single
    operation. Returns the taken string. Much more efficient than calling each
    step separately when parsing separated values. *)

val fail : ?from:int -> ?notes:Error.note list -> string -> 'a
(** [fail ?from ?notes msg] aborts parsing with a [Failure msg] error.

    [~from] makes the error cover the bytes from [from] up to the current
    position instead of a single position, and [~notes] attaches {!Error.note}s.
    {!render_error} underlines the span and prints the notes. This is intended
    for user-initiated validation failures (e.g. a parsed value is out of
    range). In streaming, a [~from] older than the buffered input is kept as the
    position of the error. Its line cannot be known, so {!render_error} prints
    [byte N] instead of a line and column and shows no excerpt, unless [~source]
    is given.

    {@ocaml[
    let byte () =
      let start = position () in
      let n = int_of_string (take_while ~at_least:1 is_digit ~label:"digit") in
      if n > 255 then
        fail ~from:start
          ~notes:[ { at = None; text = "octets are 0 to 255" } ]
          "octet out of range"
      else
        n
    ]}

    Failures from [fail] are not caught by backtracking combinators ({!val:or_},
    {!val:many}, {!val:one_of}, {!val:optional}, {!val:look_ahead}) or
    relabeling combinators ({!val:expect}, {!val:one_of_labeled}). Once raised,
    a failure propagates all the way to the runner.

    For typed errors, see {!val-error}. For errors that should participate in
    backtracking, the parser should signal failure through the parsing
    combinators themselves (e.g. {!consume}, {!satisfy}).

    Example:
    {@ocaml[
    let validate_range n =
      if n >= 0 && n <= 255 then
        n
      else
        fail "number out of range"
    ]} *)

val error : ?from:int -> ?notes:Error.note list -> 'e -> 'a
(** [error ?from ?notes e] aborts parsing with a user-defined typed error value.
    [~from] and [~notes] work as in {!fail}.

    The error is returned as [Error { kind = User e; _ }]. Use polymorphic
    variants for rich error reporting:
    {@ocaml[
      let number () =
        let n = parse_int () in
        if n > 255 then error (`Out_of_range n)
        else if n < 0 then error (`Negative n)
        else n

      match run input number with
      | Ok n -> Printf.printf "Got %d\n" n
      | Error { kind = User (`Out_of_range n); _ } ->
          Printf.printf "%d is too large\n" n
      | Error { kind = User (`Negative n); _ } ->
          Printf.printf "%d is negative\n" n
      | Error _ -> Printf.printf "Parse error\n"
    ]}

    {b Note:} User errors from [error] pass through {!val:expect} and
    {!val:one_of_labeled} without being caught or relabeled. However,
    backtracking combinators ({!val:or_}, {!val:many}, {!val:one_of},
    {!val:optional}, {!val:look_ahead}) will catch and absorb user errors just
    like any other parse failure. An absorbed user error is still reported when
    the parse fails and the error is at the furthest failure position. At the
    same position, a user error wins over [Expected]. Between two user errors at
    the same position, the first one wins.

    For a simpler string-based alternative that also escapes backtracking, see
    {!val:fail}. *)

val warn : 'd -> unit
(** [warn diagnostic] records a non-fatal diagnostic at the current position and
    continues parsing. *)

val warn_at : pos:int -> 'd -> unit
(** [warn_at ~pos diagnostic] records a non-fatal diagnostic at [pos] and
    continues parsing. *)

val position : unit -> int
(** [position ()] returns the current parser offset in bytes from the start of
    the input. *)

val location : unit -> location
(** [location ()] returns the current parser location with line and column. Zero
    cost if never called — the line index is built lazily on first use and
    extended incrementally on subsequent calls. *)

val location_of_position : string -> int -> location
(** [location_of_position input pos] computes line and column for a byte
    position in [input]. Useful for converting error positions after parsing. *)

val end_of_input : unit -> unit
(** [end_of_input ()] succeeds only if no input remains. Use this to ensure the
    entire input has been consumed.

    Example:
    {@ocaml[
    let complete_parser () =
      let result = some_parser () in
      end_of_input ();
      result
    ]} *)

val peek_char : unit -> char option
(** [peek_char ()] returns [Some c] where [c] is the next input character
    without consuming it, or [None] if at end of input. This is more efficient
    than using {!or_} or {!look_ahead} for single-character lookahead since it
    avoids backtracking overhead entirely.

    Example:
    {@ocaml[
    let value () =
      match peek_char () with
      | Some '[' ->
          parse_array ()
      | Some '"' ->
          parse_string ()
      | Some c when c >= '0' && c <= '9' ->
          parse_number ()
      | _ ->
          fail "expected value"
    ]} *)

val or_ : (unit -> 'a) -> (unit -> 'a) -> unit -> 'a
(** [or_] is the alternation combinator. Tries the left parser; if it fails,
    backtracks and tries the right parser.

    When both branches fail, the parse reports the furthest failure. If both
    branches fail at the same position, the expected items are merged into one
    sorted list without duplicates. A failure that is behind another one is
    dropped.

    This means [one_of] (which chains [or_]) naturally produces messages like
    ["expected \"else\", \"if\", or \"while\""], in the same order whatever the
    order of the branches.

    Example:
    {@ocaml[
    let bool_parser () =
      or_
        (fun () ->
          consume "true";
          true
        )
        (fun () ->
          consume "false";
          false
        )
        ()
    ]} *)

val look_ahead : (unit -> 'a) -> 'a
(** [look_ahead parser] runs [parser] without consuming any input. If [parser]
    succeeds, the position stays where it was before — useful for peeking at
    what comes next. Fails if [parser] fails.

    Example:
    {@ocaml[
    let check_next_is_digit () = look_ahead digit
    (* position hasn't moved *)
    ]} *)

val commit : unit -> unit
(** [commit ()] seals the innermost active choice or repetition frame: the
    nearest enclosing {!or_}, {!one_of}, {!many}, {!sep_by}, {!optional} or
    [fold_*] frame.

    After [commit], that frame no longer rewinds on failure, which improves
    error locality and allows streaming sources to drop the buffered input that
    only that frame kept alive. [commit] also forgets the failures recorded at
    or beyond the current position, such as the failure of a previous branch of
    the same [or_]. The error then reports only what the committed branch
    expected.

    [commit ()] does nothing outside any frame. It does nothing inside
    {!look_ahead}, and it does not reach the frames outside the look-ahead.

    [commit ()] does not seal or release outer frames. In
    [many (fun () -> or_ a b ())], a [commit ()] inside [a] seals the [or_]
    only. The [many] frame still rewinds to the start of the current iteration,
    and streaming sources keep that input buffered.

    Example:
    {@ocaml[
    let http_method () =
      or_
        (fun () ->
          let _ = consume "GET " in
          commit ();
          `GET
        )
        (fun () ->
          let _ = consume "POST " in
          commit ();
          `POST
        )
        ()
    ]} *)

val context : ?delimited:bool -> string -> (unit -> 'a) -> 'a
(** [context ?delimited name parser] runs [parser] inside a named construct. The
    name is kept for the error report: a parse that fails inside [parser] can
    say which constructs were open at the furthest failure, outermost first.

    [~delimited:true] marks a construct that has an opening token, such as an
    array or a block. An error renderer can then point to where it was opened.

    A construct is only reported when the failure is strictly past the place
    where it started. A [context] whose parser fails at its first byte, such as
    an array alternative tried at a position that holds no opening bracket, is
    not part of the chain.

    A parser that never calls [context] pays nothing for it. [context] itself
    adds a small cost per call. At the furthest failure only the first group of
    failures keeps its chain: when two failures tie, the chain of the first one
    is reported.

    {!cut} inside [parser] makes the whole construct decided: if [parser] then
    fails, no enclosing {!or_}, {!many}, {!sep_by} or similar combinator
    backtracks over it.

    Example:
    {@ocaml[
    let array_parser () =
      context ~delimited:true "array" (fun () ->
          let _ = consume "[" in
          let xs = sep_by value comma () in
          let _ = consume "]" in
          xs
      )
    ]} *)

val cut : unit -> unit
(** [cut ()] declares that the construct that is running is decided. It seals
    every {!or_}, {!one_of}, {!many}, {!sep_by}, {!optional} and [fold_*] frame
    opened since the innermost enclosing {!context}, so none of them backtracks
    after the cut. It stops at a {!look_ahead}.

    When the enclosing {!context} then fails, the frames outside it are sealed
    too, so an enclosing {!or_}, {!many} or {!sep_by} cannot swallow the
    failure. If the context ends without failing, the outer frames stay as they
    were. Without an enclosing {!context}, [cut ()] seals every frame down to
    the first {!look_ahead}.

    Unlike {!commit}, which seals only the nearest frame, [cut] covers a whole
    construct. It also lets streaming sources drop the input that those frames
    kept alive.

    Example:
    {@ocaml[
    let let_statement () =
      context "statement" (fun () ->
          let _ = consume "let" in
          cut ();
          (* from here on, a failure is an error of this statement *)
          let name = identifier () in
          let _ = consume "=" in
          (name, expression ())
      )
    ]} *)

val rec_ : (unit -> 'a) -> 'a
(** [rec_ parser] marks a recursive entry point for depth tracking. Wrap the
    body of recursive parsers with [rec_] so that {!parse} can enforce
    [max_depth] and fail cleanly instead of overflowing the stack.

    Example:
    {@ocaml[
      let rec json () = Parseff.rec_ (fun () ->
        Parseff.one_of [ array_parser; null_parser; ... ] ()
      )
      and array_parser () =
        let _ = Parseff.consume "\[" in
        let elements = ... json () ... in
        ...
    ]} *)

val expect : string -> (unit -> 'a) -> 'a
(** [expect description parser] runs [parser] and, if it fails where it started,
    replaces what the failure expected with [description]. The error reads
    [expected <description>]. Reads naturally: "expect a dot separator".

    Only parse errors (from {!consume}, {!satisfy}, etc.) at the position where
    [parser] started are relabeled. A failure deeper in the input keeps its own
    message, because it says more than [description]:
    [expect "key" (fun () -> consume "id:"; digit ())] on ["id:x"] reports
    [expected digit] at offset [3], and [expected key] on ["foo"]. A failure at
    the end of the input keeps its expected items and says [found end of input].

    Failures from {!val:fail} propagate unchanged (there is no way to intercept
    them). User errors raised via {!val-error} propagate unchanged — this lets
    you use [expect] around parsers that perform validation without losing the
    structured error:

    {@ocaml[
    let octet () =
      expect "an octet (0-255)" (fun () ->
          let n = number () in
          if n > 255 then
            error (`Out_of_range n)
          else
            n
      )
    (* A non-digit input gives: "expected an octet (0-255)" *)
    (* Input "300" gives: `Out_of_range 300 — not swallowed *)
    ]}

    Example:
    {@ocaml[
    let dot () = expect "a dot separator" (fun () -> char '.')
    let digit_val () = expect "a digit (0-9)" digit
    ]} *)

val one_of : (unit -> 'a) list -> unit -> 'a
(** [one_of parsers] tries each parser in order until one succeeds. When every
    parser fails, the error lists what the parsers expected at the furthest
    position, as for {!or_}. [one_of []] expects nothing: alone it fails with
    ["no alternative matched"], and next to other failures at the same position
    it adds nothing to what they expected.

    Example:
    {@ocaml[
    let keyword () =
      one_of
        [
          (fun () -> consume "if");
          (fun () -> consume "else");
          (fun () -> consume "while");
        ]
        ()
    ]} *)

val one_of_labeled : (string * (unit -> 'a)) list -> unit -> 'a
(** [one_of_labeled labeled_parsers] tries each parser in order. On failure,
    reports all labels in the error message.

    Each branch is wrapped by {!expect} with its label, so only failures at the
    position where [one_of_labeled] started are relabeled. A branch that fails
    deeper keeps its own message. User errors raised via {!val-error} inside any
    branch propagate unchanged.

    Example:
    {@ocaml[
    let literal () =
      one_of_labeled
        [
          ("number", number_parser);
          ("string", string_parser);
          ("boolean", bool_parser);
        ]
        ()
    (* On failure: "expected boolean, number, or string" *)
    ]} *)

(** {1 Repetition Combinators} *)

val many : ?at_least:int -> (unit -> 'a) -> unit -> 'a list
(** [many parser] applies [parser] repeatedly until it fails. Returns a list of
    all successful results. Always succeeds (returns [[]] if [parser] fails
    immediately).

    When [~at_least] is specified, requires at least that many successful
    matches. Fails if [parser] doesn't succeed enough times.

    The loop also stops after an iteration that consumes no input, and that
    iteration's result is dropped.

    Example:
    {@ocaml[
    let digits () = many digit () (* parses "123" -> [1; 2; 3] *)
    let non_empty_digits () = many ~at_least:1 digit ()
    ]} *)

val sep_by : ?at_least:int -> (unit -> 'a) -> (unit -> 'b) -> unit -> 'a list
(** [sep_by element separator] parses zero or more occurrences of [element] with
    [separator] between each pair. Returns a list of the parsed elements.

    When [~at_least] is specified, requires at least that many elements.

    The loop stops after a separator and element that together consume no input,
    and that element is dropped.

    Example:
    {@ocaml[
    let csv_line () =
      sep_by
        (fun () -> match_regex (Re.compile (Re.Posix.re "[^,]+")))
        (fun () -> char ',')
        ()
    let csv_line_nonempty () =
      sep_by ~at_least:1
        (fun () -> match_regex (Re.compile (Re.Posix.re "[^,]+")))
        (fun () -> char ',')
        ()
    ]} *)

val between : (unit -> 'a) -> (unit -> 'b) -> (unit -> 'c) -> unit -> 'c
(** [between open_ close_ parser] parses [open_], then [parser], then [close_],
    and returns the value produced by [parser]. *)

val end_by : ?at_least:int -> (unit -> 'a) -> (unit -> 'b) -> unit -> 'a list
(** [end_by element separator] parses zero or more [element]s, each followed by
    [separator].

    When [~at_least] is specified, requires at least that many elements. The
    loop stops after an iteration that consumes no input, as in {!many}. *)

val fold_left :
  (unit -> 'a) -> (unit -> 'a -> 'a -> 'a) -> ?otherwise:'a -> unit -> 'a
(** [fold_left element op] parses one or more [element] values separated by
    [op], combining them left-associatively. Fails if there are zero [element]
    values.

    When [~otherwise] is specified, returns [otherwise] if there are zero
    [element] values instead of failing.

    The loop stops after an [op] and [element] that together consume no input,
    and that pair is not applied. *)

val fold_right :
  (unit -> 'a) -> (unit -> 'a -> 'a -> 'a) -> ?otherwise:'a -> unit -> 'a
(** [fold_right element op] parses one or more [element] values separated by
    [op], combining them right-associatively. Fails if there are zero [element]
    values.

    When [~otherwise] is specified, returns [otherwise] if there are zero
    [element] values instead of failing. *)

val optional : (unit -> 'a) -> unit -> 'a option
(** [optional parser] tries to apply [parser]. Returns [Some result] if it
    succeeds, or [None] if it fails (without consuming input).

    Example:
    {@ocaml[
    let optional_sign () =
      optional (fun () -> or_ (fun () -> char '-') (fun () -> char '+') ()) ()
    ]} *)

val count : int -> (unit -> 'a) -> unit -> 'a list
(** [count n parser] applies [parser] exactly [n] times. Fails if [parser]
    doesn't succeed [n] times.

    Example:
    {@ocaml[
    let three_digits () = count 3 digit ()
    ]} *)

(** {1 Convenience Combinators} *)

val digit : unit -> int
(** [digit ()] parses a decimal digit (0-9) and returns its integer value.

    Example:
    {@ocaml[
      let d = digit () in  (* parses "7" -> 7 *)
      ...
    ]} *)

val letter : unit -> char
(** [letter ()] parses an ASCII letter (a-z or A-Z). *)

val is_whitespace : char -> bool
(** [is_whitespace c] returns true for whitespace characters (space, tab,
    newline, CR). *)

val whitespace : ?at_least:int -> unit -> string
(** [whitespace ()] parses zero or more whitespace characters (space, tab,
    newline, carriage return). Uses fast character scanning (not regex).

    When [~at_least] is specified, requires at least that many whitespace
    characters. [whitespace ~at_least:1 ()] fails if no whitespace is found. *)

val skip_whitespace : unit -> unit
(** [skip_whitespace ()] skips zero or more whitespace characters (returns
    unit). More efficient than [whitespace] when you don't need the matched
    string. *)

val alphanum : unit -> char
(** [alphanum ()] parses an alphanumeric character (letter or digit). *)

val any_char : unit -> char
(** [any_char ()] parses any character. *)

val take : int -> string
(** [take n] reads exactly [n] bytes and returns them as a string. Fails if
    fewer than [n] bytes remain, with
    [expected 5 bytes, found only 3 (end of input)], or [found end of input]
    when none remain. Raises [Invalid_argument] if [n] is negative.

    Useful for length-prefixed binary fields and fixed-width text fields alike.

    Example:
    {@ocaml[
    let payload () =
      let len = BE.any_uint16 () in
      take len
    ]} *)

(** {1 Streaming} *)

(** Input sources for incremental parsing. A source wraps a readable byte stream
    — a channel, file descriptor, or custom reader — behind a uniform interface.
    The parser pulls data on demand by calling the source's read function;
    existing parser code works unchanged.

    A source is for one parse. [of_string] starts again from the beginning, but
    the other sources continue where the last read stopped, and bytes that the
    first parse read ahead are lost. *)
module Source : sig
  type t

  val of_string : string -> t
  (** [of_string s] creates a source from a complete string. Useful for testing
      streaming code paths with known input. *)

  val of_channel : ?buf_size:int -> in_channel -> t
  (** [of_channel ?buf_size ic] creates a source that reads from [ic].
      [buf_size] controls the internal read buffer (default 4096). Raises
      [Invalid_argument] if [buf_size] is less than [1]. *)

  val of_function : (bytes -> int -> int -> int) -> t
  (** [of_function read] creates a source that calls [read buf off len] to
      obtain up to [len] bytes starting at offset [off] in [buf]. Must return
      the number of bytes actually read; return [0] to signal EOF. A result
      outside [0..len] raises [Invalid_argument] from the parse.

      This is the low-level escape hatch for byte sources that fill a
      pre-allocated buffer, such as [Unix.read] or [Unix.recv]. For most use
      cases, prefer {!of_chunks} or {!of_seq}. *)

  val of_chunks : (unit -> string option) -> t
  (** [of_chunks read] creates a source that calls [read ()] to obtain the next
      chunk of input. Return [Some s] with a non-empty string for data, or
      [None] to signal EOF. Empty strings ([Some ""]) are silently skipped.

      This is the recommended way to wrap effectful or callback-based data
      sources:

      {@ocaml skip[
      (* Eio promise *)
      let fetched = ref false in
      let source = Source.of_chunks (fun () ->
          if !fetched then None
          else begin
            fetched := true;
            Some (Promise.await_exn promise)
          end)
      ]} *)

  val of_seq : string Seq.t -> t
  (** [of_seq seq] creates a source from a lazy sequence of string chunks. Each
      element is a chunk of input; the sequence ending ([Seq.Nil]) signals EOF.

      {@ocaml skip[
      let source = Source.of_seq (List.to_seq [ "chunk1"; "chunk2"; "chunk3" ])
      ]} *)
end

val parse_source :
  ?max_depth:int ->
  ?backtrack_window:int ->
  Source.t ->
  (unit -> 'a) ->
  ('a, 'e) result
(** [parse_source ?max_depth source parser] runs [parser] pulling input from
    [source] on demand. Behaves identically to {!parse} but the input does not
    need to be fully available up front.

    The same parsers work with both [parse] and [parse_source] — no changes
    required. Streaming backtracking is bounded by [backtrack_window] (default:
    [65536] bytes). If the parser needs to keep more rewind history than that,
    parsing fails with [Error.Backtrack_window_exceeded]. Use {!commit} to seal
    branches once the grammar has clearly chosen them.

    [backtrack_window] must be at least [2], because {!match_regex} reads ahead
    half of the window. Smaller values raise [Invalid_argument].

    Only the source's read function may suspend, for example to wait on an Eio
    promise or yield to a scheduler. Two parses on one domain can then run
    concurrently. A parser callback that suspends, such as a [satisfy] predicate
    or a [many] body, is not supported.

    Example:
    {@ocaml[
    let ic = open_in "data.json" in
    let source = Source.of_channel ic in
    let result = parse_source source json in
    close_in ic;
    result
    ]} *)

val parse_source_until_end :
  ?max_depth:int ->
  ?backtrack_window:int ->
  Source.t ->
  (unit -> 'a) ->
  ('a, 'e, 'd) result_with_diagnostics
(** [parse_source_until_end ?max_depth source parser] is the streaming
    equivalent of {!parse_until_end}. It enforces full consumption and returns
    diagnostics on both success and failure.

    Example:
    {@ocaml[
    let ic = open_in "data.json" in
    let source = Source.of_channel ic in
    let outcome = parse_source_until_end source json in
    close_in ic;
    outcome
    ]} *)

(** {1 Binary Parsing}

    Big-endian and little-endian parsers for multi-byte integers, floats, and
    doubles. Pick the module matching your wire format's byte order.

    Single-byte readers ({!any_uint8}, {!any_int8}) are at the top level, since
    endianness does not affect single bytes.

    Example:
    {@ocaml[
    let parse_png_chunk () =
      let length = BE.any_int32 () in
      let chunk_type = take 4 in
      let data = take (Int32.to_int length) in
      (chunk_type, data)

    let parse_wav_size () =
      let _ = consume "RIFF" in
      let size = LE.any_int32 () in
      let _ = consume "WAVE" in
      size
    ]} *)

val any_uint8 : unit -> int
(** Read one byte as an unsigned integer (0–255). *)

val any_int8 : unit -> int
(** Read one byte as a signed integer (−128–127). *)

(** Big-endian binary parsers. *)
module BE : sig
  val any_int16 : unit -> int
  (** Read 2 bytes as a big-endian signed 16-bit integer. *)

  val any_uint16 : unit -> int
  (** Read 2 bytes as a big-endian unsigned 16-bit integer. *)

  val any_int32 : unit -> int32
  (** Read 4 bytes as a big-endian signed 32-bit integer. *)

  val any_int64 : unit -> int64
  (** Read 8 bytes as a big-endian signed 64-bit integer. *)

  val any_float : unit -> float
  (** Read 4 bytes as a big-endian IEEE 754 single-precision float. *)

  val any_double : unit -> float
  (** Read 8 bytes as a big-endian IEEE 754 double-precision float. *)

  val int16 : int -> unit
  (** [int16 i] matches exactly the 2 bytes that encode [i] in big-endian order.
      Accepts values from [-32768] to [65535], so [int16 0xFFFE] and
      [int16 (-2)] match the same bytes. Raises [Invalid_argument] outside that
      range. Fails at the start of the field if the bytes don't match, with
      [expected int16be 0x0001, found 0x0002]. *)

  val int32 : int32 -> unit
  (** [int32 i] matches exactly the 4 bytes that encode [i] in big-endian order.
      Fails at the start of the field if the bytes don't match, with
      [expected int32be 0x00000001, found 0x00000002]. *)

  val int64 : int64 -> unit
  (** [int64 i] matches exactly the 8 bytes that encode [i] in big-endian order.
      Fails at the start of the field if the bytes don't match, with
      [expected int64be 0x0000000000000001, found 0x0000000000000002]. *)
end

(** Little-endian binary parsers. *)
module LE : sig
  val any_int16 : unit -> int
  (** Read 2 bytes as a little-endian signed 16-bit integer. *)

  val any_uint16 : unit -> int
  (** Read 2 bytes as a little-endian unsigned 16-bit integer. *)

  val any_int32 : unit -> int32
  (** Read 4 bytes as a little-endian signed 32-bit integer. *)

  val any_int64 : unit -> int64
  (** Read 8 bytes as a little-endian signed 64-bit integer. *)

  val any_float : unit -> float
  (** Read 4 bytes as a little-endian IEEE 754 single-precision float. *)

  val any_double : unit -> float
  (** Read 8 bytes as a little-endian IEEE 754 double-precision float. *)

  val int16 : int -> unit
  (** [int16 i] matches exactly the 2 bytes that encode [i] in little-endian
      order. Accepts values from [-32768] to [65535]. Raises [Invalid_argument]
      outside that range. Fails at the start of the field if the bytes don't
      match, with [expected int16le 0x0001, found 0x0002]. *)

  val int32 : int32 -> unit
  (** [int32 i] matches exactly the 4 bytes that encode [i] in little-endian
      order. Fails at the start of the field if the bytes don't match, with
      [expected int32le 0x00000001, found 0x00000002]. *)

  val int64 : int64 -> unit
  (** [int64 i] matches exactly the 8 bytes that encode [i] in little-endian
      order. Fails at the start of the field if the bytes don't match, with
      [expected int64le 0x0000000000000001, found 0x0000000000000002]. *)
end

(** {1 UTF-8 Parsing}

    The [Utf8] module provides primitives that operate on Unicode code points
    ([Uchar.t]) instead of raw bytes ([char]). Input is still an OCaml [string],
    but characters are decoded as UTF-8 sequences. Invalid UTF-8 raises a parse
    error.

    Unicode character properties (letter, whitespace, etc.) use the
    {{:https://erratique.ch/software/uucp}uucp} library for full Unicode
    support.

    These primitives can be freely mixed with the byte-level primitives in the
    same parser. Position tracking remains in bytes. *)

module Utf8 : sig
  val satisfy : (Uchar.t -> bool) -> label:string -> Uchar.t
  (** [satisfy pred ~label] decodes the next UTF-8 code point and checks it
      against [pred]. Advances by the number of bytes in the UTF-8 encoding
      (1--4). Fails with [label] if the predicate returns [false], the input
      ends, or the bytes are not valid UTF-8.

      Example:
      {@ocaml skip[
      let cjk () =
        Parseff.Utf8.satisfy
          (fun u -> Uchar.to_int u >= 0x4E00 && Uchar.to_int u <= 0x9FFF)
          ~label:"CJK character"
      ]} *)

  val char : Uchar.t -> Uchar.t
  (** [char u] matches the exact Unicode code point [u].

      Example:
      {@ocaml skip[
      let lambda () = Parseff.Utf8.char (Uchar.of_int 0x03BB) (* λ *)
      ]} *)

  val any_char : unit -> Uchar.t
  (** [any_char ()] decodes and returns the next UTF-8 code point, whatever it
      is. Fails on end of input or invalid UTF-8. *)

  val take_while : ?at_least:int -> ?label:string -> (Uchar.t -> bool) -> string
  (** [take_while pred] decodes code points and consumes them while [pred]
      returns [true]. Returns the matched UTF-8 bytes as a string (may be
      empty). Fails on invalid UTF-8 encountered during scanning.

      When [~at_least] is specified, requires at least that many {e code points}
      (not bytes) to match. Fails with [~label] if fewer match.

      Example:
      {@ocaml skip[
      let unicode_word () =
        Parseff.Utf8.take_while ~at_least:1 Uucp.Alpha.is_alphabetic
          ~label:"letter"
      ]} *)

  val skip_while : (Uchar.t -> bool) -> unit
  (** [skip_while pred] advances past code points while [pred] returns [true].
      Like {!take_while} but returns [unit] — no string allocation. *)

  val take_while_span : (Uchar.t -> bool) -> span
  (** [take_while_span pred] like {!take_while} but returns a zero-copy {!span}.
  *)

  val skip_while_then_char : (Uchar.t -> bool) -> Uchar.t -> unit
  (** [skip_while_then_char pred u] skips code points where [pred] returns
      [true], then matches the exact code point [u]. More efficient than calling
      {!skip_while} followed by {!char} separately. *)

  val is_whitespace : Uchar.t -> bool
  (** [is_whitespace u] returns [true] for Unicode whitespace characters, using
      the full Unicode White_Space property. This includes ASCII whitespace plus
      characters like NO-BREAK SPACE (U+00A0), EM SPACE (U+2003), IDEOGRAPHIC
      SPACE (U+3000), and others. *)

  val letter : unit -> Uchar.t
  (** [letter ()] parses a Unicode alphabetic character. Uses the Unicode
      Alphabetic property via [Uucp.Alpha.is_alphabetic], which covers Latin,
      Greek, Cyrillic, CJK, Arabic, Devanagari, and all other Unicode scripts.

      Example:
      {@ocaml skip[
      (* Matches 'a', 'é', 'λ', '中', 'д', etc. *)
      let l = Parseff.Utf8.letter ()
      ]} *)

  val alphanum : unit -> Uchar.t
  (** [alphanum ()] parses a Unicode alphabetic character or an ASCII digit. *)

  val whitespace : ?at_least:int -> unit -> string
  (** [whitespace ()] parses zero or more Unicode whitespace characters. Returns
      the matched UTF-8 string. Uses the full Unicode White_Space property.

      When [~at_least] is specified, requires at least that many whitespace code
      points. *)

  val skip_whitespace : unit -> unit
  (** [skip_whitespace ()] skips zero or more Unicode whitespace characters.
      More efficient than {!whitespace} when you don't need the matched string.
  *)
end
