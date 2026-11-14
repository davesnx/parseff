(** Parser combinator library with an imperative-style API.

    Parseff is a parser combinator library where parsers are plain [unit -> 'a]
    functions that run in direct style, without monads or effects.

    {1 Quick Start}

    {@ocaml[
    let digit_value () =
      let c = satisfy ~label:"digit" (fun c -> c >= '0' && c <= '9') in
      Char.code c - Char.code '0'

    let ip_address () =
      let a = digit_value () in
      let _ = Parseff.consume "." in
      let b = digit_value () in
      let _ = Parseff.consume "." in
      let c = digit_value () in
      let _ = Parseff.consume "." in
      let d = digit_value () in
      (a, b, c, d)

    let () =
      match Parseff.parse "1.2.3.4" ip_address with
      | Ok (a, b, c, d) ->
          Printf.printf "Parsed: %d.%d.%d.%d\n" a b c d
      | Error e ->
          prerr_endline (Parseff.render_error ~name:"address" e)
    ]} *)

(** {1 Span Type} *)

type span
(** A slice of the input. With [parse] it is zero-copy: it points into the input
    string. With [parse_source] it is an owned copy. Use [span_length] and
    [span_get] to read it without a copy, and [span_to_string] to materialize it
    when needed. *)

val span_to_string : span -> string
(** [span_to_string s] extracts the string from a span. Only call this when you
    actually need the string value. *)

val span_length : span -> int
(** [span_length s] is the number of bytes in [s]. *)

val span_get : span -> int -> char
(** [span_get s i] is the byte at index [i] of [s], counted from the start of
    the span. Raises [Invalid_argument] if [i] is not in
    [0 .. span_length s - 1]. *)

(** {1 Location Type} *)

type location = { offset : int; line : int; col : int }
(** A source location. [line] and [col] are 1-based. [col] counts bytes from the
    start of the line (not characters — relevant for UTF-8). *)

(** {1 Errors} *)

type range = { start : int; stop : int }
(** A range of byte offsets, [start] included and [stop] excluded, with
    [start <= stop]. *)

type expected = private
  | Char of Uchar.t  (** A specific character, printed as ['x']. *)
  | Literal of string  (** A literal string, printed as ["abc"]. *)
  | Label of string
      (** A name given by {!val:satisfy}, {!val:take_while}, {!val:expect} and
          others. *)
  | End_of_input  (** The end of the input. *)

type found = private
  | Text of string
      (** The text at the failure position. A token cut at 32 bytes ends with
          [...]. *)
  | End  (** The input ended. *)
  | Short of int
      (** A read of a fixed number of bytes, such as {!val:take}, found fewer
          bytes than it needed before the end of the input. The value is how
          many bytes were left. *)
  | Value of string
      (** An integer field that differs from the expected one, as the
          hexadecimal value read in the same width, for example [0x0002], or a
          negative count given to {!val:take}, for example [-1]. *)

type context = private { name : string; start : int; delimited : bool }
(** A construct that was open at the failure, see {!val:context}. [start] is the
    byte offset where it started, [delimited] is true for constructs with an
    opening token. *)

type note = { at : range option; text : string }
(** An annotation added with [~notes] in {!val:fail} or {!val:error}.
    [at = Some r] underlines the bytes [r] in the source excerpt with the text
    next to the underline. [at = None] adds a ["* note:"] line under the
    excerpt. *)

type excerpt
(** Source lines kept with the error so that {!render_error} does not need the
    input. *)

type custom_error = ..
(** The type of the values given to {!val:error}. It is extensible: declare your
    own cases with [type Parseff.custom_error += Too_large of int] and match
    them as [Custom (Too_large n)]. *)

(** What failed. These five cases are final until 2.0, so a match on all of them
    needs no [_] case. A new kind of error is a {!custom_error} in [Custom]. *)
type error_kind = private
  | Expected of { items : expected * expected list; found : found; more : int }
      (** A primitive failed. [items] is everything that was expected at the
          furthest failure position, sorted and without duplicates, up to 64
          items. [more] is how many further alternatives were expected and are
          not in [items], [0] unless more than 64 were. *)
  | Failed of string
      (** A failure raised with {!val:fail}, or ["no alternative matched"] when
          [one_of []] is the only failure at the furthest position. Failures of
          primitives are otherwise [Expected]. *)
  | Custom of custom_error  (** A value given to {!val:error}. *)
  | Depth_limit_exceeded of { limit : int }
      (** {!val:rec_} nested deeper than [max_depth], or a {!val:fold_right}
          chain longer than its limit. *)
  | Backtrack_window_exceeded of { limit : int; oldest : int }
      (** A streaming parser needed more rewind history than the window.
          [oldest] is the earliest position the runtime still needs to rewind to
          and [limit] is the configured [backtrack_window]. The error position
          is the parser position when the overflow happened. *)

type error = private {
  kind : error_kind;
  range : range;
      (** The bytes the error is about, never reversed, empty at the end of the
          input. *)
  location : location option;
      (** Line and column of [range.start], 1-based. [location.col] counts
          bytes, while the column {!render_error} prints counts Unicode
          characters, so the two differ on a line with non-ASCII text. [None]
          when the line is not known: in streaming, when the position is older
          than the buffered input. {!render_error} prints the byte offset then.
      *)
  contexts : context list;  (** Open constructs, outermost first. *)
  notes : note list;
  help : string option;
      (** The text given with [~help] in {!val:fail} or {!val:error}. *)
  excerpt : excerpt;
}
(** A parse error: what failed, where, and what the parser was doing.

    A failed parse returns one [error]. Match on its [kind] to react to the
    failure, or print it with {!render_error}. *)

val error_message : ?custom:(custom_error -> string) -> error -> string
(** [error_message e] is the one-line message, for example
    [expected "else", "if", or "while", found 'xyz']. A message lists at most 8
    expected items, then [, or N more alternatives]. [custom] prints [Custom]
    errors, without it the message is [custom error (no ~custom printer given)].
*)

val render_error :
  ?name:string ->
  ?source:string ->
  ?custom:(custom_error -> string) ->
  error ->
  string
(** [render_error e] prints the error with a source excerpt:

    {v
    input.json:4:5: [error] expected ',' or '}', found '"'
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
    [input.json: byte N: [error] ...] and there is no excerpt. The columns of
    the header and of the [* while parsing] lines count Unicode characters (code
    points), while [location.col] counts bytes, so they differ on a line with
    non-ASCII text. In streaming, when the start of the error line is no longer
    buffered and [source] is not given, the header still has the line and the
    character column, because Parseff counts the characters of the line that it
    drops. The excerpt of the error line is then cut on the left with [...], and
    a [* while parsing] line for a context that starts in the dropped part of
    the line says [from byte N]. When the error starts in the dropped part of
    its line, the header column is [location.col], which counts bytes, the
    excerpt shows the kept part of the line, and carets mark only the kept part
    of the range, with no caret row when the whole range is dropped. Carets
    count terminal cells, so a wide character such as [名] takes two and a
    combining mark takes none, and a control byte, an invalid UTF-8 byte or a C1
    control prints in the excerpt as the escape {!val:error_message} uses, such
    as [\x1B], with as many carets as the escape has characters. An error with a
    [help] text ends with a [* help:] line, and so does a [Depth_limit_exceeded]
    or [Backtrack_window_exceeded] error. The rows the renderer adds never end
    with a space, and source text is printed byte for byte. The result has no
    trailing newline. *)

(** {1 Diagnostics} *)

module Diagnostics : sig
  type 'd entry = { pos : int; value : 'd }
  (** A non-fatal diagnostic and the byte offset it refers to. *)

  type 'd t
  (** Collects the diagnostics of one parse. ['d] is the type of the values
      given to {!val:Parseff.warn}. *)

  val create : unit -> 'd t

  val to_list : 'd t -> 'd entry list
  (** Diagnostics in emission order. Available after the parse, on [Ok] and on
      [Error]. *)

  val length : 'd t -> int
  val is_empty : 'd t -> bool

  val clear : 'd t -> unit
  (** Empties the collector. Raises [Invalid_argument] while a parse uses it. *)
end

(** {1 Ref} *)

(** A [ref] whose changes are undone when the parser backtracks.

    Use it for state that the parser builds as it reads, such as a symbol table.
    A {!Ref.set} is undone when its branch backtracks, in the same cases as a
    diagnostic from {!warn}. Only the value in the cell is restored, so store
    immutable values, such as a [Map] or a list. A parse does not reset the
    cell: set the start value before each parse.

    A {!Ref.set} costs more than a write to a plain [ref]. It fits state that
    changes a few times in a construct, such as declarations, more than a write
    for each token.

    {@ocaml[
    let names = Ref.create []

    let declare () =
      let name = take_while ~at_least:1 (fun c -> c >= 'a' && c <= 'z') in
      Ref.set names (name :: Ref.get names)

    let statement () =
      or_
        (fun () ->
          declare ();
          let _ = char '=' in
          `Assign
        )
        (fun () ->
          declare ();
          let _ = char ';' in
          `Declare
        )
        ()
    (* On "abc;", names holds ["abc"] once: the set of the failed branch is
       undone. *)
    ]} *)
module Ref : sig
  type 'a t
  (** A cell that holds a value of type ['a]. *)

  val create : 'a -> 'a t
  (** [create v] is a new cell that holds [v]. *)

  val get : 'a t -> 'a
  (** [get cell] is the value in [cell]. *)

  val set : 'a t -> 'a -> unit
  (** [set cell v] stores [v] in [cell]. Raises [Invalid_argument] when another
      running parse set [cell]: give each parse its own cell. *)
end

(** {1 Trace} *)

(** An event of a traced parse, see the [trace] argument of {!val:parse}.

    Each {!val:context} call gives one [Enter], then the events nested in it,
    then one [Return] or [Fail]. Events also come from branches that are later
    abandoned, so a backtrack shows as an [Enter] before the end of the previous
    event. A [Captured] event shows a failure that {!val:capture},
    {!val:recover} or {!val:sep_by_recover} caught, so the parse continued.
    Positions are absolute byte offsets.

    A 1.x release can add events: end a match on [trace_event] with a [_] case.
*)
type trace_event = private
  | Enter of { name : string; pos : int }
      (** A {!val:context} named [name] started at byte offset [pos]. *)
  | Return of { name : string; start : int; stop : int }
      (** The construct returned. It read the bytes from [start] to [stop]. *)
  | Fail of { name : string; start : int; pos : int }
      (** The construct failed or raised. [pos] is the parser position when it
          ended. The error position is in the {!type:error}. *)
  | Captured of { start : int; error : error }
      (** A {!val:capture}, {!val:recover} or {!val:sep_by_recover} caught
          [error] and went back to [start]: the start of its parser, or the last
          {!val:commit} or {!val:cut} that reached it with [~release:true].
          {!val:require} and {!val:list} give it for each error that they
          record, with [start] where the missing part was inserted. [capture]
          gives the event when it returns [Error error]. [recover] and
          [sep_by_recover] give it after [sync], [~record] and [hole] return, so
          the events of [sync] and [hole] come first. When one of them fails,
          the failure was not recovered and no event is given. *)

val print_trace : Format.formatter -> trace_event -> unit
(** [print_trace ppf] is a [trace] callback that prints each event on its own
    line of [ppf], indented by open constructs. Apply it once for each parse.

    Example:
    {@ocaml[
    let _ =
      parse ~trace:(print_trace Format.std_formatter) "hello" (fun () ->
          consume "hello"
      )
    ]} *)

(** {1 Runner} *)

val parse :
  ?max_depth:int ->
  ?consume:[ `All | `Partial ] ->
  ?diagnostics:'d Diagnostics.t ->
  ?trace:(trace_event -> unit) ->
  string ->
  (unit -> 'a) ->
  ('a, error) Stdlib.result
(** [parse input parser] runs [parser] on [input].

    [consume] is [`All] by default: parsing fails with an
    [Expected End_of_input] error when input remains after the parser returns,
    as if the parser ended with {!end_of_input}. The error is at the offset of
    the first unconsumed byte. Pass [~consume:`Partial] to parse a leading part
    of the input on purpose and ignore the rest, for example to read one record
    from the start of a buffer. With [`Partial], the result does not say where
    the parser stopped; call {!position} at the end of the parser to read it.

    [diagnostics] is the collector given to {!warn}, {!recover} and
    {!sep_by_recover}. The runner empties it before the parser starts. Read it
    with {!Diagnostics.to_list} after the parse, whether the result is [Ok] or
    [Error]. With any other collector, {!warn} records nothing, and {!recover}
    and {!sep_by_recover} do not recover: the parse fails at the first error, as
    without them. Thus the same parser can report every error with
    [~diagnostics] and stop at the first one without it.

    [trace] receives a {!type:trace_event} when a {!val:context} starts, returns
    and fails. {!print_trace} prints the events as a tree. An exception from
    [trace] stops the parse, and [parse] raises it again.

    When a parse fails, the error is the furthest failure: the failure of any
    primitive that happened at the highest input position, including primitives
    in branches that [or_], [many], [sep_by], [optional] or [fold_*] abandoned.
    The [Expected] kind lists everything that was expected at that position,
    sorted and without duplicates:
    [expected "else", "if", or "while", found 'xyz']. A failure at the end of
    the input says [found end of input] and keeps the expected set.

    [max_depth] limits the nesting depth for parsers that use {!rec_} to mark
    recursive entry points. Defaults to [128]. When exceeded, parsing fails with
    [Depth_limit_exceeded] instead of risking a stack overflow.

    Returns [Ok result] on success, or [Error e] on failure, where [e] is an
    {!type:error}. [e.kind] is [Expected] for a mismatch, [Failed] for
    {!val:fail}, [Custom] for {!val-error}, [Depth_limit_exceeded] when
    recursion nested deeper than [max_depth], and [Backtrack_window_exceeded]
    for streaming.

    Example:
    {@ocaml[
    match parse "hello" (fun () -> consume "hello") with
    | Ok s ->
        Printf.printf "Matched %S\n" s
    | Error { kind = Failed msg; _ } ->
        Printf.printf "Failed: %s\n" msg
    | Error e ->
        prerr_endline (render_error ~name:"input" e)
    ]} *)

(** {1 Primitive Combinators} *)

val consume : string -> string
(** [consume s] matches the exact literal string [s]. Fails at the first byte
    that differs from [s], expecting the rest of [s]. The failure position is
    the mismatch, not the start of [s]: [consume "ab"] on ["ax"] fails at offset
    [1] and expects ["b"]. When the input ends inside [s], the error says
    [found end of input]. The one exception is a mismatch inside a multi-byte
    character: it is reported at the start of that character.

    Example:
    {@ocaml[
    let parser () =
      let _ = consume "hello" in
      let _ = consume " " in
      consume "world"
    ]} *)

val satisfy : ?label:string -> (char -> bool) -> char
(** [satisfy ?label fun] matches the next character if [fun] returns [true] for
    it. If the character doesn't match or input is empty, fails with
    [expected <label>]. Without [~label] the label is ["at least 1 character"],
    as for [take_while ~at_least:1].

    Example:
    {@ocaml[
    let vowel () = satisfy ~label:"vowel" (fun c -> String.contains "aeiou" c)
    ]} *)

val char : char -> char
(** [char c] matches the exact character [c].

    Example:
    {@ocaml[
    let comma () = char ','
    ]} *)

type regex
(** A regular expression for {!match_regex}. Build it with {!val:regex}. *)

val regex : Re.t -> regex
(** [regex r] compiles [r] for {!match_regex}, anchored at the position where
    the match starts. Compile once, outside the parser, and reuse the value.

    Example:
    {@ocaml[
    let identifier_re = regex (Re.Posix.re ("[a-zA-Z_]" ^ "[a-zA-Z0-9_]*"))
    ]} *)

val match_regex : ?label:string -> regex -> string
(** [match_regex ?label re] matches [re] at the current position and returns the
    matched text.

    When the regex does not match, the parse fails with [expected <label>] if
    [~label] is given, so the failure merges with the other alternatives
    expected at the same position like any other primitive. Without [~label] the
    label is ["text matching the pattern"].

    A failed match examines only the bytes that the regex needs to fail, not the
    rest of the input. [Re.bol] and word boundaries see the byte before the
    current position.

    In streaming mode, a match and the byte after it must fit in the backtrack
    window, unless the source ends after the match. Otherwise the parse fails
    with [Backtrack_window_exceeded]. A regex that needs later bytes to choose a
    longer match can return the shorter one when the window is small: [a(bc)?]
    on ["abc"] can return ["a"] with a window below [6]. Before it matches, a
    streaming [match_regex] reads ahead up to half the backtrack window, also
    when the match then fails. A regex that can still match with more input, for
    example one with a leading [.*], reads until the window is full and then
    fails with [Backtrack_window_exceeded].

    Example:
    {@ocaml[
    let identifier_re = regex (Re.Posix.re ("[a-zA-Z_]" ^ "[a-zA-Z0-9_]*"))
    let identifier () = match_regex ~label:"identifier" identifier_re
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

val sep_by_take : ws:(char -> bool) -> sep:char -> (char -> bool) -> string list
(** [sep_by_take ~ws ~sep is_value_char] parses a list of values separated by
    [sep]. Characters where [ws] returns [true] are skipped around each
    separator. Each value consists of characters where [is_value_char] returns
    [true]. Returns a list of matched strings. Runs entirely in a single
    operation for maximum efficiency. *)

val take_while_span : (char -> bool) -> span
(** [take_while_span fun] like {!take_while} but returns a zero-copy
    {!type:span} instead of allocating a string. No memory allocation until you
    call {!span_to_string}. *)

val sep_by_take_span :
  ws:(char -> bool) -> sep:char -> (char -> bool) -> span list
(** [sep_by_take_span ~ws ~sep is_value_char] like {!sep_by_take} but returns
    zero-copy {!type:span}s instead of strings. No [String.sub] allocations per
    element. *)

val fail :
  ?from:int -> ?upto:int -> ?notes:note list -> ?help:string -> string -> 'a
(** [fail ?from ?upto ?notes ?help msg] aborts parsing with a [Failed msg]
    error.

    [~from] and [~upto] make the error cover the bytes from [from] to [upto]
    instead of a single position, and [~notes] attaches {!type:note}s. Each of
    them defaults to the current position, and each can be before or after it.
    If [from] is after [upto], the two are swapped, so the range is never
    reversed. The error is located at the start of the range. {!render_error}
    underlines the range and prints the notes. [~help] tells the user how to
    correct the input. {!render_error} prints it as a ["* help:"] line after the
    notes. This is intended for user-initiated validation failures (e.g. a
    parsed value is out of range).

    In streaming, a range start older than the buffered input is kept as the
    position of the error. Unless [~source] is given, {!render_error} prints
    [byte N] and no excerpt when the line is not known. When the line is known,
    it prints the line and the byte column, shows the kept part of the line, and
    puts carets only under the kept part of the range, with no caret row when no
    part of the range is kept. A range end past the buffered input is kept as
    given. {!render_error} underlines up to the end of the line it can show.

    {@ocaml[
    let is_digit c = c >= '0' && c <= '9'

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

val error :
  ?from:int ->
  ?upto:int ->
  ?notes:note list ->
  ?help:string ->
  custom_error ->
  'a
(** [error ?from ?upto ?notes ?help e] aborts parsing with a user-defined error
    value. [~from], [~upto], [~notes] and [~help] work as in {!fail}.

    The error is returned as [Error { kind = Custom e; _ }]. Declare the cases
    of {!type:custom_error} for rich error reporting:
    {@ocaml[
    type custom_error += Out_of_range of int | Negative of int

    let octet () =
      let n = integer () in
      if n > 255 then
        error (Out_of_range n)
      else if n < 0 then
        error (Negative n)
      else
        n

    let () =
      match parse "300" octet with
      | Ok n ->
          Printf.printf "Got %d\n" n
      | Error { kind = Custom (Out_of_range n); _ } ->
          Printf.printf "%d is too large\n" n
      | Error { kind = Custom (Negative n); _ } ->
          Printf.printf "%d is negative\n" n
      | Error _ ->
          Printf.printf "Parse error\n"
    ]}

    {b Note:} Custom errors from [error] pass through {!val:expect} and
    {!val:one_of_labeled} without being caught or relabeled. However,
    backtracking combinators ({!val:or_}, {!val:many}, {!val:one_of},
    {!val:optional}, {!val:look_ahead}) will catch and absorb custom errors just
    like any other parse failure. An absorbed custom error is still reported
    when the parse fails and the error is at the furthest failure position. At
    the same position, a custom error wins over [Expected]. Between two custom
    errors at the same position, the first one wins.

    For a simpler string-based alternative that also escapes backtracking, see
    {!val:fail}. *)

val warn : ?pos:int -> 'd Diagnostics.t -> 'd -> unit
(** [warn diagnostics d] records [d] at the current position and continues
    parsing. [warn ~pos diagnostics d] records it at the byte offset [pos].
    [diagnostics] must be the collector given to the runner as [~diagnostics];
    for any other collector [warn] records nothing.

    A diagnostic lives as long as the input position that it was recorded at.
    When a parser fails and the position goes back, the diagnostics recorded
    since that position are removed. This holds for a branch of {!or_} or
    {!one_of}, an iteration of {!many}, {!optional}, and a {!capture} or
    {!recover} whose parser fails. It also holds for a successful {!look_ahead}
    or {!peek_with}, and for a repetition iteration that succeeds without
    consuming input. A {!Ref.set} is undone in the same cases.

    Thus a diagnostic recorded inside a construct is lost when an enclosing
    {!capture} or {!recover} fails later: only the error of the enclosing
    construct stays. This rule is final. To keep the inner diagnostics, give
    [~record] to {!recover} or {!sep_by_recover}. *)

val position : unit -> int
(** [position ()] returns the current parser offset in bytes from the start of
    the input. *)

val with_range : (unit -> 'a) -> 'a * range
(** [with_range parser] runs [parser] and returns its result with the
    {!type:range} of bytes it consumed. If [parser] fails, [with_range] fails
    with the same error.

    Example:
    {@ocaml[
    let checked_number () =
      let n, range = with_range integer in
      if n > 255 then
        fail ~notes:[ { at = Some range; text = "this value" } ] "out of range";
      n
    ]} *)

val recognize : (unit -> 'a) -> string
(** [recognize parser] runs [parser], drops its result and returns the input
    text that [parser] consumed. If [parser] fails, [recognize] fails with the
    same error. In {!parse_source}, the text must fit in [~backtrack_window],
    also after a {!commit}.

    Example:
    {@ocaml[
    let identifier () =
      recognize (fun () ->
          ignore (letter ());
          ignore (many (fun () -> or_ alphanum (fun () -> char '_') ()) ())
      )
    (* "foo_1 = 2" -> "foo_1" *)
    ]} *)

val recognize_span : (unit -> 'a) -> span
(** [recognize_span parser] is {!recognize} but returns a {!type:span}. With
    {!parse_source} the span is an owned copy. *)

val location : unit -> location
(** [location ()] returns the current parser location with line and column. Zero
    cost if never called — the line index is built lazily on first use and
    extended incrementally on subsequent calls. *)

val location_of_position : string -> int -> location
(** [location_of_position input pos] computes line and column for a byte
    position in [input]. Useful for converting error positions after parsing. *)

val end_of_input : unit -> unit
(** [end_of_input ()] succeeds only if no input remains. Use this to ensure the
    input has been consumed at a given point, for example in a sub-parser.
    {!parse} already does this at the end of the top-level parser unless you
    pass [~consume:`Partial].

    Example:
    {@ocaml[
    let some_parser () = consume "hello"

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
    let parse_array () = `Array (consume "[]")
    let parse_string () = `String (consume "\"\"")
    let parse_number () = `Number (number ())

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
          let _ = consume "true" in
          true
        )
        (fun () ->
          let _ = consume "false" in
          false
        )
        ()
    ]} *)

val look_ahead : (unit -> 'a) -> 'a
(** [look_ahead parser] runs [parser] without consuming any input. If [parser]
    succeeds, the position stays where it was before — useful for peeking at
    what comes next. Fails if [parser] fails. Diagnostics recorded by [parser]
    with {!warn} are removed when it succeeds, as the position is.

    Example:
    {@ocaml[
    let check_next_is_digit () = look_ahead digit
    (* position hasn't moved *)
    ]} *)

val peek_with : (unit -> 'a) -> 'a * int
(** [peek_with parser] runs [parser] and returns its value with the position
    where it stopped. It consumes no input: the position stays where it was
    before. Fails if [parser] fails.

    It follows the rule of {!look_ahead}: diagnostics recorded by [parser] with
    {!warn} and {!Ref} writes made by [parser] are removed, a {!commit} or
    {!cut} inside [parser] has no effect outside it, and the failures of its
    probes are forgotten when it succeeds.

    Use it with {!skip_to} to read a token once: look at the token, decide, then
    move after it without a second scan. A parser with one token of lookahead
    keeps the last result with the position where it was read. The position is
    all that a rewind must put back, so {!or_}, {!many}, {!capture} and
    {!recover} work with it.

    {@ocaml[
    type token = Name of string | Symbol of char | End

    let token () =
      skip_whitespace ();
      match peek_char () with
      | None ->
          End
      | Some 'a' .. 'z' ->
          Name (take_while (fun c -> c >= 'a' && c <= 'z'))
      | Some c ->
          ignore (any_char ());
          Symbol c

    let read_at = ref (-1)
    let current = ref End
    let stop = ref 0

    let peek () =
      let here = position () in
      if !read_at <> here then begin
        let t, after = peek_with token in
        read_at := here;
        current := t;
        stop := after
      end;
      !current

    let advance () =
      ignore (peek ());
      skip_to !stop
    ]}

    Set [read_at] to [-1] before each parse.

    To report an error of a token, return it in the value of [token] and record
    it with {!warn} in [advance].

    Each call pushes and restores a rewind frame. In a loop that reads each
    token this way, the pair needs about twice the instructions of a token kept
    in a [ref] by hand. A token kept by hand is not put back by a rewind: the
    parser must save and restore it.

    In {!parse_source}, the input that [parser] reads must fit in
    [~backtrack_window]. This includes the whitespace and comments that it skips
    before the token. *)

val skip_to : int -> unit
(** [skip_to target] moves the position forward to [target], an absolute offset
    as {!position} returns it. It reads nothing and never fails. Use it with a
    position that {!peek_with} returned.

    Raises [Invalid_argument] if [target] is before the position, or after the
    input that the parse has read. Each offset from the position to one that
    {!peek_with} returned is in the input read. *)

val capture : ?release:bool -> ?outer:bool -> (unit -> 'a) -> ('a, error) result
(** [capture parser] runs [parser] and returns its failure as a value, so that
    the parser can recover from an error and continue.

    If [parser] succeeds, [capture] returns [Ok v]. If [parser] fails, also with
    {!fail} or {!val-error}, [capture] returns [Error e], where [e] is the
    furthest failure inside [parser], built as {!parse} builds an error. The
    position then goes back to where [capture] started, and the diagnostics that
    [parser] recorded are removed, also those of a {!capture} or {!recover}
    inside [parser] that recovered. This rule is final; the [~record] of
    {!recover} gets the removed diagnostics. A {!cut} or {!commit} in [parser]
    does not reach past [capture]. [Depth_limit_exceeded] and
    [Backtrack_window_exceeded] are not caught. In {!parse_source}, the input
    that [parser] reads must fit in [~backtrack_window]. A traced parse gets a
    [Captured] event for [Error e].

    [capture] also returns a failure at the end of the input. A parser that
    records it and continues usually fails again at the end, so the input gets
    two errors. {!recover} and {!sep_by_recover} do not recover such a failure.

    With [~release:true], a {!commit} or {!cut} in [parser] that reaches
    [capture] moves the rewind point to the position of that call. A later
    failure of [parser] then goes back to that position, not to the start. It
    removes only the diagnostics and the {!Ref} changes made after that call;
    those made before it stay, as after a success. Do not parse the same input
    again with another parser after [Error e]: skip from the current position,
    and from [e.range.start] when it is later. A {!cut} also continues to the
    frames outside [capture]. In {!parse_source}, only the input that [parser]
    reads after the rewind point must fit in [~backtrack_window], so a parser
    that calls {!cut} after each decided part can be longer than the window. The
    default is [false].

    With [~outer:false], [e] keeps only the contexts that [parser] opened, as if
    the parse started at [capture]: the contexts open around [capture] are not
    in [e.contexts] and {!render_error} does not show them. Use it for an error
    that is recorded and parsed past, whose construct is [parser]. The default
    is [true].

    Example:
    {@ocaml[
    let statement_or_hole errors () =
      match capture statement with
      | Ok s ->
          s
      | Error e ->
          warn ~pos:e.range.start errors e;
          skip_while (fun c -> c <> ';');
          let _ = char ';' in
          `Hole
    ]} *)

val recover :
  ?release:bool ->
  ?outer:bool ->
  ?record:(error -> error Diagnostics.entry list -> unit) ->
  error Diagnostics.t ->
  sync:(unit -> unit) ->
  (unit -> 'a) ->
  (error -> 'a) ->
  'a
(** [recover errors ~sync parser hole] runs [parser] with {!capture}. If
    [parser] fails with the error [e], [recover] records [e] in [errors], calls
    [sync ()] to skip to a point where parsing can continue, and returns
    [hole e].

    The errors that [parser] recorded in [errors] before it failed are removed,
    as with {!capture}: only [e] stays. To keep them, give [~record].

    [~record] replaces the recording of [e]: [recover] calls [record e inner]
    where [inner] holds the errors that [parser] recorded in [errors] and that
    the failure removed, in the order they were recorded. To keep one, record it
    again: [warn ~pos:d.pos errors d.value] gives the same entry. With
    [~release:true], [inner] holds only the errors recorded after the last
    {!commit} or {!cut} that reached [recover], since the errors before it stay.
    For example, to keep the errors of [parser] and record [e] only when there
    are none:
    {@ocaml[
    let record errors e inner =
      List.iter
        (fun (d : _ Diagnostics.entry) -> warn ~pos:d.pos errors d.value)
        inner;
      if inner = [] then warn ~pos:e.range.start errors e
    ]}

    [~release] and [~outer] are given to {!capture}. With [~release:true],
    [sync] starts at the last {!commit} or {!cut} that reached [recover], and
    the errors recorded before that call stay.

    [errors] must be the collector given to the runner as [~diagnostics];
    otherwise [recover] calls [parser] directly, so the parse fails as without
    [recover]. If [sync] consumes nothing, [recover] still consumes input, so a
    loop around it always moves forward: it moves to the start of the error
    range and one character past it, but never past the end of the input. To
    continue without consuming input, use {!capture}. A failure at the end of
    the input is not recovered: the parse fails there, as without [recover].
    Thus an element that is cut short by the end of the input gives one error,
    not a second one where the enclosing construct expects more input.

    Example:
    {@ocaml[
    let statements errors () =
      many
        (fun () ->
          skip_whitespace ();
          recover errors
            ~sync:(fun () ->
              skip_while (fun c -> c <> ';');
              ignore (optional (fun () -> char ';') ())
            )
            statement
            (fun _ -> `Hole)
        )
        ()
    ]} *)

val sep_by_recover :
  ?release:bool ->
  ?outer:bool ->
  ?record:(error -> error Diagnostics.entry list -> unit) ->
  error Diagnostics.t ->
  sync:(unit -> unit) ->
  (unit -> 'a) ->
  (unit -> 'b) ->
  (error -> 'a) ->
  unit ->
  'a list
(** [sep_by_recover errors ~sync element separator hole] parses one or more
    occurrences of [element] with [separator] between each pair, as {!sep_by}
    does, and recovers from each [element] that fails. For a failure [e], it
    records [e] in [errors], goes back to the start of the element, calls
    [sync ()] to skip the broken element, and puts [hole e] in the list. The
    errors that the element recorded in [errors] before it failed are removed;
    [~release], [~outer] and [~record] are as in {!recover}. With
    [~release:true], a failure goes back to the last {!commit} or {!cut} that
    reached [sep_by_recover], not to the start of the element.

    [sync] must stop before the next separator and before the token that closes
    the list: {!skip_nested} does this for lists in brackets. Unlike {!recover},
    [sep_by_recover] does not consume input when [sync] consumes nothing, so a
    hole can be empty, as in [f(1, , 3)]. The list ends where [separator] fails.
    If [sync], [~record] or [hole] fails, the element fails as in {!sep_by}:
    after the first element, the list ends before it, and the failure is
    reported only if the parse then fails.

    A separator before the closing token, as in [{1, 2,}], is read as a
    separator and an empty element, which is recorded as an error. When the
    grammar allows it, make [separator] fail when the closing token follows, and
    parse an optional separator before the closing token. The failure must be
    one that {!sep_by} goes back from, such as a {!look_ahead} of {!satisfy}: a
    {!fail} is not undone, so the parse fails.
    {@ocaml[
    let separator () =
      skip_whitespace ();
      let _ = char ',' in
      skip_whitespace ();
      ignore (look_ahead (fun () -> satisfy ~label:"field" (fun c -> c <> '}')))

    let table errors field () =
      let _ = char '{' in
      skip_whitespace ();
      let fields =
        sep_by_recover errors
          ~sync:(skip_nested [ ('{', '}') ] (fun c -> c = ','))
          field separator
          (fun _ -> `Hole)
          ()
      in
      skip_whitespace ();
      let _ = optional (fun () -> char ',') () in
      skip_whitespace ();
      let _ = char '}' in
      fields
    (* "{1, 2, }" -> [1; 2], with no error recorded *)
    ]}

    A failure at the end of the input is not recovered: the parse fails there,
    as with {!sep_by}. A failing first element gives a list with one hole, so
    for a list that can be empty, test for the closing token first. [errors]
    must be the collector given to the runner as [~diagnostics]; otherwise
    [sep_by_recover] is [sep_by ~at_least:1 element separator ()].

    Example:
    {@ocaml[
    let expr () =
      skip_whitespace ();
      `Int (integer ())

    let arguments errors () =
      let _ = char '(' in
      let args =
        if peek_char () = Some ')' then
          []
        else
          sep_by_recover errors
            ~sync:(skip_nested [ ('(', ')'); ('[', ']') ] (fun c -> c = ','))
            expr
            (fun () -> char ',')
            (fun _ -> `Hole)
            ()
      in
      let _ = char ')' in
      args
    (* "(1, [2 3], 4)" -> [`Int 1; `Hole; `Int 4], with one error recorded *)
    ]} *)

val require : error Diagnostics.t -> (unit -> 'a) -> (error -> 'a) -> 'a
(** [require errors parser missing] runs [parser]. If the furthest failure of
    [parser] is at the position where [require] started, [require] records the
    error [e] in [errors], goes back to that position and returns [missing e]:
    it inserts the missing part, consumes nothing and the parse goes on. Use it
    for a token that the grammar needs at this point, such as [then], [;], or
    the [)] that closes a list, and for a missing expression.

    The furthest failure decides, not the position where [parser] stopped. A
    branch of [parser] that read further and failed means that the construct is
    there and broken, not missing: the failure goes on, as without [require],
    and an enclosing {!recover} or {!list} handles it. On ["ax"],
    [require errors (fun () -> ignore (optional ab ()); ignore (char ')'))
     ignore] with [ab] reading ['a'] then ['b'] does not insert: [ab] failed at
    1. With [capture ab] in place of [optional ab ()], the failure of [ab] is
    not counted, and [require] inserts the [)] at 0.

    A failure at the end of the input is inserted too, so a missing closing
    token at the end gives one error. [e] keeps the contexts open around
    [require], so {!render_error} shows the construct that the token closes. [e]
    also lists what other parsers expected at the same position, such as the
    separator that a {!list} tried before it stopped, as {!parse} would report
    it.

    With the collector, a {!commit} or {!cut} in [parser] does not reach the
    frames outside [require], as with {!recover}; without the collector it does.
    So an {!or_} around [require] can take its other branch with the collector
    and not without it. The diagnostics that [parser] recorded before a
    {!commit} stay after an insertion; the others are removed.

    In byte mode, a token parser usually skips the whitespace after it. Then [e]
    points after that whitespace, not at the end of the previous token.

    [errors] must be the collector given to the runner as [~diagnostics];
    otherwise [require] calls [parser] directly. A traced parse gets a
    [Captured] event for each insertion. Unstable: this function can change in a
    1.x release.

    Example:
    {@ocaml[
    type call = Call of string * int list

    let call errors () =
      let name =
        take_while ~at_least:1 ~label:"name" (fun c -> c >= 'a' && c <= 'z')
      in
      let _ = char '(' in
      let args =
        list
          ~sep:(fun () ->
            let _ = char ',' in
            skip_whitespace ()
          )
          errors
          ~close:(fun () -> peek_char () = Some ')')
          ~sync:(skip_nested [ ('(', ')') ] (fun c -> c = ','))
          (fun () ->
            let n = integer () in
            skip_whitespace ();
            n
          )
          (fun _ -> 0)
          ()
      in
      require errors (fun () -> ignore (char ')')) ignore;
      Call (name, args)

    let () =
      let errors = Diagnostics.create () in
      let r = parse ~diagnostics:errors "f(1, 2" (call errors) in
      assert (r = Ok (Call ("f", [ 1; 2 ])));
      match Diagnostics.to_list errors with
      | [ { Diagnostics.value = e; _ } ] ->
          assert (error_message e = "expected ')' or ',', found end of input")
      | _ ->
          assert false
    ]} *)

val list :
  ?release:bool ->
  ?outer:bool ->
  ?record:(error -> error Diagnostics.entry list -> unit) ->
  ?sep:(unit -> unit) ->
  ?trailing:bool ->
  error Diagnostics.t ->
  close:(unit -> bool) ->
  sync:(unit -> unit) ->
  (unit -> 'a) ->
  (error -> 'a) ->
  unit ->
  'a list
(** [list errors ~close ~sync element hole ()] parses [element] until [close ()]
    is [true] or the input ends, and recovers from each [element] that fails. It
    is one loop for a list of statements and, with [~sep], for a list with
    separators. It does not read the closing token: follow it with {!require}.

    [close] tells where the list ends. Its changes are undone, as with
    {!look_ahead}, so it can read the closing token. A failing [close] makes
    [list] fail. At the end of the input [list] does not call it.

    When [element] fails with the error [e], [list] records [e], calls [sync ()]
    and puts [hole e] in the list, as {!recover} does. [~release], [~outer] and
    [~record] are as in {!recover}.

    Without [~sep], [close] runs before each element. When [sync] consumes
    nothing, [list] skips one character past the error, as {!recover} does. An
    element that consumes nothing ends the list, as in {!many}, unless it
    recorded a diagnostic, such as an insertion of {!require}: then [list] keeps
    its value, calls [sync ()], and when [sync] consumes nothing, skips one
    character. Thus each element moves the position, or ends the list.

    With [~sep], [list] reads as {!sep_by}: an element, then a separator and an
    element, again and again. An element that succeeds on no input is a value,
    as in {!sep_by}. [close] runs before the first element, and after an element
    when the separator fails. The list ends where the separator fails at [close]
    or at the end of the input, or where a separator and an element together
    consume nothing.
    - When [close] is [true] before the first element, [element] runs without
      recovery: its value is the only element if it succeeds, and the list is
      empty if it fails.
    - [list] does not skip, as {!sep_by_recover}: [sync] must stop before the
      next separator and before the closing token, and {!skip_nested} does this.
      Thus a hole can be empty, as in [f(1, , 3)].
    - After a separator before the closing token, as in [{1, 2,}], [element]
      runs: a failure is recorded and gives [hole e], as with {!sep_by}, which
      rejects this input. At the end of the input the parse fails there. With
      [~trailing:true], [close] runs after each separator, and a separator after
      an element can come before the closing token, with no error and no element
      after it. A separator with no element before it, as in [{,}], is still an
      error.
    - When the separator is missing and an element follows, as in [f(1 2)],
      [list] records the error of the separator and goes on with the element.
      When no element follows, as in [f(1 ?)], [list] calls [sync ()]. If [sync]
      consumes input, [list] records the error of the separator and goes on.
      Otherwise the list ends there, and the {!require} of the closing token
      reports the error. The error of the separator goes through [~record], with
      no inner diagnostics, and it is kept only when its step succeeds.

    To find the next element when a separator is missing, [element] and [sep]
    must skip the whitespace after them, not before. [list] then runs [element]
    once to try it, so in {!parse_source} that element must fit in
    [~backtrack_window].

    A failure at the end of the input is not recovered: the parse fails there,
    as with {!recover}. [errors] must be the collector given to the runner as
    [~diagnostics]; otherwise [list] does not recover, and the first failure of
    [element] or [sep] makes the parse fail or ends the list. For valid input,
    [list] gives the same list as {!many} or {!sep_by}, and records nothing.
    Unstable: this function can change in a 1.x release.

    Example:
    {@ocaml[
    type stmt = Assign of string * int | Bad

    let statement () =
      let name =
        take_while ~at_least:1 ~label:"name" (fun c -> c >= 'a' && c <= 'z')
      in
      skip_whitespace ();
      let _ = char '=' in
      skip_whitespace ();
      let value = integer () in
      skip_whitespace ();
      let _ = char ';' in
      skip_whitespace ();
      Assign (name, value)

    let skip_statement () =
      skip_while (fun c -> c <> ';' && c <> '}');
      ignore (optional (fun () -> char ';') ());
      skip_whitespace ()

    let block errors () =
      context ~delimited:true "block" (fun () ->
          let _ = char '{' in
          skip_whitespace ();
          let stmts =
            list errors
              ~close:(fun () -> peek_char () = Some '}')
              ~sync:skip_statement statement
              (fun _ -> Bad)
              ()
          in
          require errors (fun () -> ignore (char '}')) ignore;
          stmts
      )

    let () =
      let errors = Diagnostics.create () in
      let r = parse ~diagnostics:errors "{ x = 1; y = ; " (block errors) in
      assert (r = Ok [ Assign ("x", 1); Bad ]);
      assert (
        List.map
          (fun { Diagnostics.value = e; _ } -> error_message e)
          (Diagnostics.to_list errors)
        = [ "expected integer, found ';'"; "expected '}', found end of input" ]
      )
    ]} *)

val skip_nested :
  ?atom:(unit -> unit) -> (char * char) list -> (char -> bool) -> unit -> unit
(** [skip_nested pairs stop ()] skips input up to the end of a broken list
    element. It is made for the [~sync] of {!sep_by_recover} and {!recover}:
    [~sync:(skip_nested pairs stop)].

    [pairs] gives the brackets of the grammar as [(opening, closing)]
    characters. The skip stops, without consuming, before the first of:

    - a character for which [stop] is true, outside all brackets that the skip
      opened;
    - a closing character whose opening character the skip did not consume: it
      closes a construct around the element;
    - the end of the input.

    Each kind of bracket is matched with its own kind. A closing character that
    does not match the innermost open bracket closes the nearest open bracket of
    its kind, and the brackets inside that one are taken as not closed. In
    [\[1 2), 3] the closing parenthesis has no opening parenthesis, so the skip
    stops before it.

    [~atom] is a parser for a piece of input that the skip must take as a whole,
    such as a string literal or a comment, so that a bracket or a separator
    inside it is not counted. The skip tries [atom] at each position; where
    [atom] fails or consumes nothing, the skip reads the next character. The
    failures of [atom] do not appear in later errors.

    Example:
    {@ocaml[
    let string_literal () =
      let _ = char '"' in
      skip_while (fun c -> c <> '"');
      ignore (char '"')

    let skip_argument =
      skip_nested ~atom:string_literal
        [ ('(', ')'); ('[', ']'); ('{', '}') ]
        (fun c -> c = ',')
    (* on "g(1, 2) \"a,b\" x, 3)" it stops before ", 3)" *)
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
    {!look_ahead} or {!peek_with}, and it does not reach the frames outside the
    look-ahead.

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

val context : ?delimited:bool -> ?start:int -> string -> (unit -> 'a) -> 'a
(** [context ?delimited ?start name parser] runs [parser] inside a named
    construct. The name is kept for the error report: a parse that fails inside
    [parser] can say which constructs were open at the furthest failure,
    outermost first.

    [~delimited:true] marks a construct that has an opening token, such as an
    array or a block. An error renderer can then point to where it was opened.

    [~start] is the position where the construct starts. The default is the
    current position. Give it when the opening token is already consumed, for
    example by a lexer that reads one token ahead. [context] raises
    [Invalid_argument] when [start] is negative or after the current position.

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
    opened since the innermost enclosing {!val:context}, so none of them
    backtracks after the cut. It stops at a {!look_ahead}, {!peek_with} or
    {!capture}. With [capture ~release:true], it moves the rewind point of the
    [capture] to the position of the cut and continues.

    When the enclosing {!val:context} then fails, the frames outside it are
    sealed too, so an enclosing {!or_}, {!many} or {!sep_by} cannot swallow the
    failure. If the context ends without failing, the outer frames stay as they
    were. Without an enclosing {!val:context}, [cut ()] seals the frames down to
    the start of the parse, and stops at the same places.

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
          (name, expr ())
      )
    ]} *)

val rec_ : (unit -> 'a) -> 'a
(** [rec_ parser] marks a recursive entry point for depth tracking. Wrap the
    body of recursive parsers with [rec_] so that {!parse} can enforce
    [max_depth] and fail cleanly instead of overflowing the stack.

    Example:
    {@ocaml[
    let rec json () =
      Parseff.rec_ (fun () -> Parseff.one_of [ array_parser; null_parser ] ())

    and array_parser () =
      let _ = Parseff.consume "[" in
      let elements = Parseff.sep_by json (fun () -> Parseff.char ',') () in
      let _ = Parseff.consume "]" in
      `Array elements

    and null_parser () =
      let _ = Parseff.consume "null" in
      `Null
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
    them). Custom errors raised via {!val-error} propagate unchanged — this lets
    you use [expect] around parsers that perform validation without losing the
    structured error:

    {@ocaml[
    type custom_error += Out_of_range of int

    let octet () =
      expect "an octet (0-255)" (fun () ->
          let n = integer () in
          if n > 255 then
            error (Out_of_range n)
          else
            n
      )
    (* A non-digit input gives: "expected an octet (0-255)" *)
    (* Input "300" gives: Custom (Out_of_range 300), not swallowed *)
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
    let reserved_word () =
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
    deeper keeps its own message. Custom errors raised via {!val-error} inside
    any branch propagate unchanged.

    Example:
    {@ocaml[
    let number_parser () = `Number (number ())
    let string_parser () = `String (consume "\"\"")
    let bool_parser () = `Bool (bool_parser ())

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
    let digits () = many digit () (* parses "123" -> ['1'; '2'; '3'] *)
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
        (fun () -> match_regex (regex (Re.Posix.re "[^,]+")))
        (fun () -> char ',')
        ()
    let csv_line_nonempty () =
      sep_by ~at_least:1
        (fun () -> match_regex (regex (Re.Posix.re "[^,]+")))
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

    Each operand takes a level of the stack. When the runtime reports a stack
    limit, a chain longer than [(Gc.get ()).stack_limit / 32] operands, with
    [stack_limit] read when Parseff is loaded ([4194304] with the default
    [OCAMLRUNPARAM=l]), fails with [Depth_limit_exceeded] instead of a stack
    overflow. Each combinator that keeps a rewind point open around the chain,
    such as [or_], [optional], [look_ahead], [capture], [recover] or an outer
    [fold_right], counts as one more operand.

    When [~otherwise] is specified, returns [otherwise] if there are zero
    [element] values instead of failing.

    The loop stops after an [op] and [element] that together consume no input,
    and that pair is not applied. *)

type ('a, +'kind) operator
(** An operator for {!expression} and {!precedence}. Each constructor takes a
    parser for the operator token. The parser returns the build function, as the
    [op] argument of {!fold_left} does.

    ['kind] tells where the operator goes: [`Infix], [`Prefix] or [`Postfix].
    {!expression} takes all three. {!precedence} reads prefix operators in its
    [operand], so a {!prefix} operator in its [operator] is a type error.

    When you write the type, keep ['kind] open, or the table does not
    type-check: [(int, [ `Infix ]) operator] and [(int, [ `Prefix ]) operator]
    have no common type. In an implementation, write [(int, _) operator]. In a
    signature, write [(int, [> `Infix ]) operator]. A record field or a table
    that holds operators of every kind takes the full set
    [(int, [ `Infix | `Prefix | `Postfix ]) operator].

    Example:
    {@ocaml[
    module Ops : sig
      val add : (int, [> `Infix ]) operator
      val neg : (int, [> `Prefix ]) operator
    end = struct
      let add : (int, _) operator =
        infix_left (fun () ->
            ignore (char '+');
            ( + )
        )

      let neg : (int, _) operator =
        prefix (fun () ->
            ignore (char '-');
            ( ~- )
        )
    end

    type grammar = {
      table : (int, [ `Infix | `Prefix | `Postfix ]) operator list list;
    }

    let grammar = { table = [ [ Ops.add ]; [ Ops.neg ] ] }

    let expr () =
      expression
        ~operand:(fun () -> Char.code (digit ()) - Char.code '0')
        grammar.table ()
    ]} *)

val infix_left : (unit -> 'a -> 'a -> 'a) -> ('a, [> `Infix ]) operator
(** [infix_left op] is a left-associative binary operator: [1-2-3] is [(1-2)-3].
*)

val infix_right : (unit -> 'a -> 'a -> 'a) -> ('a, [> `Infix ]) operator
(** [infix_right op] is a right-associative binary operator: [2^3^2] is
    [2^(3^2)]. *)

val prefix : (unit -> 'a -> 'a) -> ('a, [> `Prefix ]) operator
(** [prefix op] is a unary operator before its operand. It can repeat: [- - 1]
    is [-(-1)]. Its operand holds only operators of higher precedence, so with
    [-] below [^], [-2^2] is [-(2^2)]. Only {!expression} takes it. *)

val postfix : (unit -> 'a -> 'a) -> ('a, [> `Postfix ]) operator
(** [postfix op] is a unary operator after its operand. It can repeat: [3!!] is
    [(3!)!]. *)

val expression :
  operand:(unit -> 'a) ->
  ('a, [< `Infix | `Prefix | `Postfix ]) operator list list ->
  unit ->
  'a
(** [expression ~operand table] parses operands with [operand], joined by the
    operators of [table]. It is {!precedence} with a lookup made from [table]:
    the operators of level [i] have the precedence [i].

    [table] lists the precedence levels from the lowest to the highest
    precedence. [operand] parses an operand; for parentheses, it calls the
    expression parser again inside {!rec_}. [expression] skips no whitespace:
    skip it in the token parsers and in [operand].

    After each operand, [expression] runs token parsers until one succeeds. It
    starts with the operators that bind to that operand: the levels above the
    pending operator before it, and the level of that operator when it is an
    {!infix_right}. When none of them matches, it runs the build function of
    that pending operator and moves out to the operators that bind to its
    result, and so on down to the lowest level. Each group is tried in table
    order. Each token parser runs at most once for one operand, and the token
    parser that succeeds is not run again: its diagnostics, {!Ref} changes and
    {!commit} stay. The token parsers that fail add to the expected set of an
    error.

    When the token parser or the operand after it fails, [expression] continues
    with the next operator in this order at the same position, and stops before
    the operator when no other operator matches. Thus [<] can come before [<<]
    in the table: in [1<<2], the operand after [<] fails, and [<<] is next. A
    postfix operator has no operand, so when one token starts another, such as
    [!] and [!=], list the longer token first. A {!commit} in a token parser
    stops this search: when that operator or a later step fails, [expression]
    fails.

    A step whose operator and right operand together consume no input ends its
    group: [expression] goes back before that operator and moves out to the
    lower groups. The right operand includes the operators that bind to it: with
    an operand that can be empty, an infix operator [""] below a postfix [^]
    reads [2^] as [""] applied to [2] and to the empty operand with [^].

    Where an operand starts, [expression] tries the {!prefix} operators in table
    order, then [operand]. The operand of a prefix operator uses one depth level
    of {!rec_}. A chain of infix operators uses no depth level. When a build
    function fails, the expression fails, as in {!precedence}.

    Example:
    {@ocaml[
    let token c () =
      skip_whitespace ();
      ignore (char c);
      skip_whitespace ()

    let op c f () =
      token c ();
      f
    let add, sub, mul, div =
      (op '+' ( +. ), op '-' ( -. ), op '*' ( *. ), op '/' ( /. ))
    let neg, pow = (op '-' Float.neg, op '^' Float.pow)

    let table =
      [
        [ infix_left add; infix_left sub ];
        [ infix_left mul; infix_left div ];
        [ prefix neg ];
        [ infix_right pow ];
      ]

    let rec expr () = expression ~operand table ()

    and operand () =
      or_
        (fun () ->
          token '(' ();
          let e = rec_ expr in
          token ')' ();
          e
        )
        number ()
    ]} *)

val precedence :
  ?min:int ->
  operand:(unit -> 'a) ->
  operator:(unit -> (int * ('a, [< `Infix | `Postfix ]) operator) option) ->
  unit ->
  'a
(** [precedence ~operand ~operator] parses operands with [operand], joined by
    the operators that [operator] finds. {!expression} uses the same loop. Where
    {!expression} runs the token parsers of its table, [precedence] calls
    [operator] once after each operand. For a grammar with many precedence
    levels, this is faster.

    [operator] looks at the input and returns [Some (prec, op)], or [None] when
    no operator is next. A higher [prec] binds tighter, and each [int] is a
    valid [prec]. [op] is {!infix_left}, {!infix_right} or {!postfix}. For a
    prefix operator, read the token in [operand], then parse its operand with
    [precedence ~min] inside {!rec_}.

    There is one exception to the one call. When [operator] records a diagnostic
    or sets a {!Ref}, and the operator that it returns binds less tightly than
    the operator before it, [precedence] undoes those changes, runs the build
    functions that are ready, and calls [operator] again at the same position.
    Thus the changes of the build functions come first. An [operator] that only
    looks at the input is called once.

    When the operator belongs to the expression, [precedence] runs the token
    parser of [op] from the position where [operator] stopped. Thus [operator]
    can read the token, and the token parser of [op] then reads nothing. Or
    [operator] can only look at the token, and the token parser of [op] reads
    it. In all other cases, [precedence] goes back to the position before the
    call of [operator]: when [operator] returns [None], when it fails, and when
    [prec] is below [min].

    An [operator] that returns [None] adds nothing to the expected set of an
    error. Use {!peek_char}, or read a token and compare it, in place of one
    {!consume} for each operator.

    [operator] is a lookup: a {!commit} or {!cut} in it stops there, as at a
    {!look_ahead}, and has no effect. Thus [precedence] can always go back to
    the position before the lookup, also in a streaming source. To commit to an
    operator, call {!commit} in the token parser of [op].

    [min] is the lowest [prec] that [precedence] accepts. The default accepts
    all operators.

    A chain of operators uses no depth level of {!rec_}. When the token parser
    of [op] or the operand after it fails, the expression stops before that
    operator, unless the token parser called {!commit}; then [precedence] fails.
    The loop stops after an operator and operand that together consume no input.

    A build function runs when the operator after its right operand is known,
    with {!position} at the end of that operand. A postfix build function, and
    the build function of an {!infix_left} operator with [prec] of [max_int],
    run immediately: no operator binds tighter. When a build function fails, the
    expression fails at that position; it does not stop before the operator. A
    {!commit} in a build function has no effect.

    Example:
    {@ocaml[
    let read build () =
      ignore (any_char ());
      build

    let add = infix_left (read ( + ))
    let mul = infix_left (read ( * ))
    let pow = infix_right (read (fun a b -> int_of_float (float a ** float b)))

    let operator () =
      skip_whitespace ();
      match peek_char () with
      | Some '+' ->
          Some (1, add)
      | Some '*' ->
          Some (2, mul)
      | Some '^' ->
          Some (4, pow)
      | _ ->
          None

    let rec expr () = precedence ~operand ~operator ()

    and operand () =
      skip_whitespace ();
      match peek_char () with
      | Some '-' ->
          ignore (any_char ());
          -rec_ (precedence ~min:3 ~operand ~operator)
      | Some '(' ->
          ignore (any_char ());
          let e = rec_ expr in
          skip_whitespace ();
          ignore (char ')');
          e
      | _ ->
          integer ()
    ]} *)

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

val digit : unit -> char
(** [digit ()] parses an ASCII decimal digit, ['0'] to ['9'], and returns it.
    Use {!integer} for the value of a number, or [Char.code c - Char.code '0']
    for the value of one digit.

    Example:
    {@ocaml[
    let () = assert (parse "7" digit = Ok '7')
    ]} *)

val integer : unit -> int
(** [integer ()] parses a decimal integer: an optional [-], then digits. It
    fails without a digit, and when the value is outside the [int] range.

    Example:
    {@ocaml[
    let () = assert (parse "-42" integer = Ok (-42))
    ]} *)

val number : unit -> float
(** [number ()] parses a number with the JSON grammar: an optional [-], digits,
    an optional fraction and an optional exponent. ["-0.5"] and ["2.5e-3"]
    match.

    An [e] or [E] after the number always starts an exponent, so ["2em"] fails.
    For other formats, such as a unit after the number, a leading [+], [.5],
    digit separators or [inf], write your own parser with {!take_while},
    {!recognize} and [float_of_string].

    Example:
    {@ocaml[
    let () = assert (parse "6.02e23" number = Ok 6.02e23)
    ]} *)

val hex_integer : unit -> int
(** [hex_integer ()] parses hexadecimal digits and returns their value. It
    accepts no sign and no [0x] prefix. It fails without a digit, and when the
    value is above [max_int].

    Example:
    {@ocaml[
    let () = assert (parse "ff" hex_integer = Ok 255)
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

val lexeme : ?skip:(unit -> unit) -> (unit -> 'a) -> 'a
(** [lexeme p] runs [p], then [skip], and returns the result of [p]. [skip]
    defaults to {!skip_whitespace}; pass a parser that also skips comments when
    the grammar has them.

    Example:
    {@ocaml[
    let skip () =
      skip_whitespace ();
      while peek_char () = Some '#' do
        skip_while (fun c -> c <> '\n');
        skip_whitespace ()
      done

    let token p = lexeme ~skip p
    let comma () = token (fun () -> char ',')
    ]} *)

val keyword : ?is_ident:(char -> bool) -> string -> unit
(** [keyword s] matches the literal [s] only when the next byte cannot continue
    an identifier, so [keyword "let"] fails on ["letter"]. [is_ident] receives
    that one byte; the default accepts ASCII letters, digits and [_]. A failure
    is reported at the start of [s]. In streaming, [keyword] needs a backtrack
    window of [String.length s + 1] bytes, one more than {!consume}. Raises
    [Invalid_argument] if [s] is empty.

    Example:
    {@ocaml[
    let binding () =
      keyword "let";
      skip_whitespace ();
      take_while ~at_least:1 (fun c -> c >= 'a' && c <= 'z') ~label:"name"
    ]} *)

val alphanum : unit -> char
(** [alphanum ()] parses an alphanumeric character (letter or digit). *)

val any_char : unit -> char
(** [any_char ()] parses any character. *)

val take : int -> string
(** [take n] reads exactly [n] bytes and returns them as a string. Fails if
    fewer than [n] bytes remain, with
    [expected 5 bytes, found only 3 (end of input)], or [found end of input]
    when none remain. A negative [n] fails the parse with
    [expected non-negative length, found -1], so [or_] and [optional] recover
    from it and a length read from untrusted input cannot raise.

    Useful for length-prefixed binary fields and fixed-width text fields alike.

    Example:
    {@ocaml[
    let payload () =
      let len = BE.any_uint16 () in
      take len
    ]} *)

val take_span : int -> span
(** [take_span n] reads exactly [n] bytes like {!take}, and returns them as a
    {!type:span}. With {!parse_source} the span is an owned copy.

    Example:
    {@ocaml[
    let payload () =
      let len = BE.any_uint16 () in
      take_span len
    ]} *)

(** {1 Streaming} *)

(** Input sources for incremental parsing. A source wraps a readable byte stream
    — a channel, file descriptor, or custom reader — behind a uniform interface.
    The parser pulls data on demand by calling the source's read function;
    existing parser code works unchanged.

    A source is for one call of {!parse_source} or {!parse_source_each}.
    [of_string] starts again from the beginning, but the other sources continue
    where the last read stopped, and bytes that the first call read ahead are
    lost. To parse many values from one source, use {!parse_source_each}. *)
module Source : sig
  type t

  val of_string : string -> t
  (** [of_string s] creates a source from a complete string. Useful for testing
      streaming code paths with known input. *)

  val of_channel : ?buf_size:int -> in_channel -> t
  (** [of_channel ?buf_size ic] creates a source that reads from [ic].
      [buf_size] controls the internal read buffer (default 4096). Raises
      [Invalid_argument] if [buf_size] is less than [1]. *)

  val of_function :
    ?initial_buffer_size:int -> (bytes -> int -> int -> int) -> t
  (** [of_function ?initial_buffer_size read] creates a source that calls
      [read buf off len] to obtain up to [len] bytes starting at offset [off] in
      [buf]. Must return the number of bytes actually read; return [0] to signal
      EOF. A result outside [0..len] raises [Invalid_argument] from the parse.

      [initial_buffer_size] is the first size of the read buffer: the first
      [read] gets a [len] of at most [initial_buffer_size] (default 2040, small
      enough for the minor heap). After a read that fills the buffer, the next
      read gets a buffer of the larger of [initial_buffer_size] and 4096, and
      the buffer keeps that size. The larger buffer is allocated only when that
      read happens. An [initial_buffer_size] near the size of a short message,
      such as a request head, allocates less per source. Raises
      [Invalid_argument] if [initial_buffer_size] is less than [1].

      This is the low-level escape hatch for byte sources that fill a
      pre-allocated buffer, such as [Unix.read] or [Unix.recv]. For most use
      cases, prefer {!of_chunks} or {!of_seq}. *)

  val of_chunks : (unit -> string option) -> t
  (** [of_chunks read] creates a source that calls [read ()] to obtain the next
      chunk of input. Return [Some s] with a non-empty string for data, or
      [None] to signal EOF. Empty strings ([Some ""]) are silently skipped.

      Parseff can keep a chunk as its input with no copy, so a chunk must not
      change after [read] returns it. This matters only for a string made with
      [Bytes.unsafe_to_string] from bytes that are written again.

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

  type bigstring =
    (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t
  (** The buffer type of [Bigstringaf.t], [Cstruct.buffer] and
      [Unix.read_bigarray]. *)

  val of_bigstring_chunks : (unit -> (bigstring * int * int) option) -> t
  (** [of_bigstring_chunks read] creates a source that calls [read ()] to obtain
      the next chunk of input as a slice of a bigstring. Return
      [Some (buf, off, len)] for the [len] bytes of [buf] that start at [off],
      or [None] to signal EOF. Slices with [len = 0] are skipped, whatever their
      [off]. Any other slice outside [buf] raises [Invalid_argument] from the
      parse.

      Parseff parses bytes, not bigstrings: it copies a slice into its input
      buffer, at most 2040 bytes at a time, as the parser needs them. The bytes
      of a slice must not change until the next call to [read], but [read] can
      write the next chunk into the same buffer.

      This fits I/O that reads into a bigstring, such as [Eio.Flow.single_read]
      with a [Cstruct.t], or a network library that hands its read buffer to the
      parser:

      {@ocaml skip[
      let buf = Cstruct.create 4096 in
      let source =
        Source.of_bigstring_chunks (fun () ->
            match Eio.Flow.single_read flow buf with
            | n -> Some (buf.buffer, buf.off, n)
            | exception End_of_file -> None)
      ]}

      To parse a whole bigstring, give it as a single slice. *)

  val of_seq : string Seq.t -> t
  (** [of_seq seq] creates a source from a lazy sequence of string chunks. Each
      element is a chunk of input; the sequence ending ([Seq.Nil]) signals EOF.
      As with {!of_chunks}, a chunk must not change after the sequence returns
      it.

      {@ocaml[
      let source = Source.of_seq (List.to_seq [ "chunk1"; "chunk2"; "chunk3" ])
      ]} *)
end

val parse_source :
  ?max_depth:int ->
  ?backtrack_window:int ->
  ?consume:[ `All | `Partial ] ->
  ?diagnostics:'d Diagnostics.t ->
  ?trace:(trace_event -> unit) ->
  Source.t ->
  (unit -> 'a) ->
  ('a, error) Stdlib.result
(** [parse_source source parser] runs [parser] pulling input from [source] on
    demand. Takes the same [consume], [diagnostics] and [trace] arguments as
    {!parse} and behaves identically, but the input does not need to be fully
    available up front.

    With the default [`All], the check for remaining input pulls one more byte
    from [source] after the parser returns. It succeeds when [source] is at its
    end, and fails with [Expected End_of_input] as soon as one more byte exists.
    It does not read the rest of [source]. With [`Partial], [source] is read
    only as far as the parser needs.

    The same parsers work with both [parse] and [parse_source] — no changes
    required. Streaming backtracking is bounded by [backtrack_window] (default:
    [65536] bytes). If the parser needs to keep more rewind history than that,
    parsing fails with [Backtrack_window_exceeded]. Use {!commit} to seal
    branches once the grammar has clearly chosen them.

    [backtrack_window] must be at least [2], because {!match_regex} reads ahead
    half of the window. Smaller values raise [Invalid_argument].

    Only the source's read function may suspend, for example to wait on an Eio
    promise or yield to a scheduler. Two parses on one domain can then run
    concurrently. A parser callback that suspends, such as a [satisfy] predicate
    or a [many] body, is not supported.

    Example:
    {@ocaml[
    let read_json () =
      let ic = open_in "data.json" in
      let source = Source.of_channel ic in
      let result = parse_source source json in
      close_in ic;
      result
    ]} *)

val parse_source_each :
  ?max_depth:int ->
  ?backtrack_window:int ->
  ?diagnostics:'d Diagnostics.t ->
  ?trace:(trace_event -> unit) ->
  ?pending:(unit -> bool) ->
  Source.t ->
  (unit -> 'a) ->
  ('a, error) Stdlib.result Seq.t
(** [parse_source_each source parser] parses a stream of values from [source],
    such as NDJSON lines. Each element of the sequence runs [parser] once on the
    rest of [source], as {!parse_source} with [~consume:`Partial] does. The
    sequence ends when no input remains, or after the first [Error]. Memory is
    bounded by [backtrack_window], not by the number of values. Between two
    elements the consumer may suspend, for example in an Eio fiber.

    [parser] must also consume the separator after each value; a run that
    consumes no input is an [Error]. Positions count from the start of [source].
    [diagnostics] is emptied each time an element is forced, so read it before
    you force the next one. The sequence is ephemeral: forcing an element twice
    raises [Invalid_argument].

    [parser] must not read past its value, unless you give [pending]. A parser
    that keeps a lookahead, such as a lexer that reads the next token early, can
    hold the start of the last value when no input remains. [pending] is called
    before a run when no input remains, in the same run as [parser]. If it
    returns [true], [parser] runs one more time, and that run may consume no
    input. [pending] must return [false] after the held values are returned,
    otherwise the sequence does not end. An [Error] ends the sequence also when
    values are held.

    Example:
    {@ocaml[
    let record () =
      let id = take_while ~at_least:1 (fun c -> c >= '0' && c <= '9') in
      let _ = char '\n' in
      int_of_string id

    let ids text =
      parse_source_each (Source.of_string text) record
      |> Seq.filter_map Result.to_option
      |> List.of_seq
    ]}
    Here [ids "1\n22\n333\n"] is [[1; 22; 333]]. *)

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

val any_varint : unit -> int
(** Read an unsigned LEB128 varint: each byte gives 7 bits, least significant
    first, and a byte below [0x80] ends the number. [\xAC\x02] is [300]. A
    varint longer than 9 bytes or above [max_int] fails, so a negative Protocol
    Buffers [int32] or [int64] is rejected. Where [int] has fewer than 63 bits,
    such as on js_of_ocaml, a large value can give a wrong result instead of an
    error.

    Example:
    {@ocaml[
    let field () =
      let len = any_varint () in
      take len
    ]} *)

(** Big-endian binary parsers. *)
module BE : sig
  val any_int16 : unit -> int
  (** Read 2 bytes as a big-endian signed 16-bit integer. *)

  val any_uint16 : unit -> int
  (** Read 2 bytes as a big-endian unsigned 16-bit integer. *)

  val any_int32 : unit -> int32
  (** Read 4 bytes as a big-endian signed 32-bit integer. *)

  val any_uint32 : unit -> int
  (** Read 4 bytes as a big-endian unsigned 32-bit integer, from [0] to
      [4294967295]. Where [int] has fewer than 33 bits, such as on js_of_ocaml,
      a value above [max_int] gives a wrong result with no error. *)

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

  val any_uint32 : unit -> int
  (** Read 4 bytes as a little-endian unsigned 32-bit integer, from [0] to
      [4294967295]. Where [int] has fewer than 33 bits, such as on js_of_ocaml,
      a value above [max_int] gives a wrong result with no error. *)

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
  val satisfy : ?label:string -> (Uchar.t -> bool) -> Uchar.t
  (** [satisfy ?label pred] decodes the next UTF-8 code point and checks it
      against [pred]. Advances by the number of bytes in the UTF-8 encoding
      (1--4). Fails with [expected <label>] if the predicate returns [false] or
      the input ends. Without [~label] the label is ["at least 1 character"], as
      for [take_while ~at_least:1]. Invalid UTF-8 fails with
      [expected valid UTF-8], whatever [label] is.

      Example:
      {@ocaml[
      let cjk () =
        Parseff.Utf8.satisfy ~label:"CJK character" (fun u ->
            Uchar.to_int u >= 0x4E00 && Uchar.to_int u <= 0x9FFF
        )
      ]} *)

  val char : Uchar.t -> Uchar.t
  (** [char u] matches the exact Unicode code point [u].

      Example:
      {@ocaml[
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
      {@ocaml[
      let unicode_word () =
        Parseff.Utf8.take_while ~at_least:1 Uucp.Alpha.is_alphabetic
          ~label:"letter"
      ]} *)

  val skip_while : (Uchar.t -> bool) -> unit
  (** [skip_while pred] advances past code points while [pred] returns [true].
      Like {!take_while} but returns [unit] — no string allocation. *)

  val take_while_span : (Uchar.t -> bool) -> span
  (** [take_while_span pred] like {!take_while} but returns a zero-copy
      {!type:span}. *)

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
      {@ocaml[
      (* Matches 'a', 'é', 'λ', '中', 'д', etc. *)
      let l () = Parseff.Utf8.letter ()
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
