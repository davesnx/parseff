(** Parser combinators with OCaml 5 algebraic effects 

    Parseff is a parser combinator library that uses algebraic effects as the 
    communication channel between parsers and their runner, similar to how 
    JavaScript generators use yield.

    {1 Quick Start}

    {[
      open Parseff

      (* Parse an IP address *)
      let digit_parser () =
        let c = satisfy (fun c -> c >= '0' && c <= '9') "digit" in
        Char.code c - Char.code '0'

      let ip_address () =
        let a = digit_parser () in consume ".";
        let b = digit_parser () in consume ".";
        let c = digit_parser () in consume ".";
        let d = digit_parser () in
        end_of_input ();
        (a, b, c, d)

      (* Run the parser *)
      match run "192.168.1.1" ip_address with
      | Ok (result, _) -> Printf.printf "Parsed: %d.%d.%d.%d\n" (fst (fst (fst result))) ...
      | Error { pos; expected } -> Printf.printf "Error at %d: %s\n" pos expected
    ]}
*)

(** {1 Span Type} *)

(** A zero-copy slice of the input string.
    Use [span_to_string] to materialize when needed. *)
type span = {
  buf : string;
  off : int;
  len : int;
}

(** [span_to_string s] extracts the string from a span.
    Only call this when you actually need the string value. *)
val span_to_string : span -> string

(** {1 Effect Types} *)

(** Parser effects - the protocol between parsers and handlers *)
type _ Effect.t +=
  | Consume : string -> string Effect.t
      (** [Consume s] matches the exact literal string [s].
          Advances the input cursor on success. *)
  | Satisfy : (char -> bool) * string -> char Effect.t
      (** [Satisfy (pred, label)] matches a character satisfying [pred].
          The [label] is used in error messages. *)
  | Match_re : Re.re -> string Effect.t
      (** [Match_re re] matches a regular expression.
          The regex must be compiled with [Re.compile]. *)
  | Choose : (unit -> 'a) * (unit -> 'a) -> 'a Effect.t
      (** [Choose (left, right)] tries [left], backtracks and tries [right] on failure.
          This is the backtracking combinator. *)
  | Fail : string -> 'a Effect.t
      (** [Fail msg] aborts parsing with an error message. *)
  | Look_ahead : (unit -> 'a) -> 'a Effect.t
      (** [Look_ahead p] runs parser [p] without consuming input.
          Fails if [p] fails, but doesn't advance the cursor on success. *)
  | End_of_input : unit Effect.t
      (** [End_of_input] succeeds only if no input remains.
          Use this to ensure the entire input has been consumed. *)
  | Take_while : (char -> bool) -> string Effect.t
      (** [Take_while pred] consumes characters while [pred] holds.
          Returns the matched string (may be empty). *)
  | Skip_while : (char -> bool) -> unit Effect.t
      (** [Skip_while pred] skips characters while [pred] holds.
          Always succeeds (skips zero characters if predicate doesn't match). *)
  | Greedy_many : (unit -> 'a) -> 'a list Effect.t
      (** [Greedy_many p] applies [p] zero or more times greedily.
          More efficient than building many from Choose. *)
  | Skip_while_then_char : (char -> bool) * char -> unit Effect.t
      (** [Skip_while_then_char (pred, c)] skips chars matching [pred], then matches [c].
          Fused operation for efficiency. *)
  | Fused_sep_take : (char -> bool) * char * (char -> bool) -> string Effect.t
      (** [Fused_sep_take (ws_pred, sep_char, take_pred)]
          skip ws, match sep, skip ws, take_while1 — all in one effect. *)
  | Sep_by_take : (char -> bool) * char * (char -> bool) -> string list Effect.t
      (** [Sep_by_take (ws_pred, sep_char, take_pred)]
          Parses zero or more separated values entirely in the handler. *)
  | Take_while_span : (char -> bool) -> span Effect.t
      (** [Take_while_span pred] like Take_while but returns a zero-copy span. *)
  | Sep_by_take_span : (char -> bool) * char * (char -> bool) -> span list Effect.t
      (** [Sep_by_take_span] like Sep_by_take but returns zero-copy spans. *)

(** {1 Result Types} *)

(** Parse result *)
type 'a result =
  | Ok of 'a * int  (** Success: parsed value and final position *)
  | Error of {
      pos : int;  (** Position where parsing failed *)
      expected : string;  (** Description of what was expected *)
    }  (** Failure: position and expected token *)

(** Parse error exception *)
exception Parse_error of int * string

(** {1 Runner} *)

(** [run input parser] runs [parser] on [input] string.
    
    Returns [Ok (result, final_pos)] on success, or [Error { pos; expected }] on failure.
    
    Example:
    {[
      match run "hello" (fun () -> consume "hello") with
      | Ok (s, pos) -> Printf.printf "Matched %S at position %d\n" s pos
      | Error { pos; expected } -> Printf.printf "Failed at %d: %s\n" pos expected
    ]}
*)
val run : string -> (unit -> 'a) -> 'a result

(** [run_shallow input parser] runs [parser] on [input] using shallow effect handlers.
    
    Shallow handlers avoid re-installing the full handler record on every effect resume,
    which can reduce overhead for parsers that perform many simple effects. The handler
    is re-applied one effect at a time rather than wrapping the entire continuation.
    
    Semantically identical to [run] - same parsers work with both runners.
    
    Example:
    {[
      match run_shallow "hello world" my_parser with
      | Ok (result, pos) -> ...
      | Error { pos; expected } -> ...
    ]}
*)
val run_shallow : string -> (unit -> 'a) -> 'a result

(** {1 Primitive Combinators} *)

(** [consume s] matches the exact literal string [s].
    
    Example:
    {[
      let parser () = 
        consume "hello";
        consume " ";
        consume "world"
    ]}
*)
val consume : string -> string

(** [satisfy pred label] matches a character satisfying predicate [pred].
    The [label] is used in error messages.
    
    Example:
    {[
      let vowel () = satisfy (fun c -> String.contains "aeiou" c) "vowel"
    ]}
*)
val satisfy : (char -> bool) -> string -> char

(** [char c] matches the exact character [c].
    
    Example:
    {[
      let comma () = char ','
    ]}
*)
val char : char -> char

(** [match_re re] matches a regular expression.
    The regex must be compiled with [Re.compile].
    
    Example:
    {[
      let identifier () = 
        let re = Re.compile (Re.Posix.re "[a-zA-Z_][a-zA-Z0-9_]*") in
        match_re re
    ]}
*)
val match_re : Re.re -> string

(** [take_while pred] consumes characters while [pred] holds.
    Returns the matched string (may be empty).
    This is much faster than regex for simple character class scanning.

    Example:
    {[
      let digits () = take_while (fun c -> c >= '0' && c <= '9')
    ]}
*)
val take_while : (char -> bool) -> string

(** [take_while1 pred label] consumes one or more characters while [pred] holds.
    Fails with [label] if no characters match.

    Example:
    {[
      let digits1 () = take_while1 (fun c -> c >= '0' && c <= '9') "digit"
    ]}
*)
val take_while1 : (char -> bool) -> string -> string

(** [skip_while pred] skips characters while [pred] holds (returns unit).
    Always succeeds (skips zero characters if predicate doesn't match).
    More efficient than [take_while] when you don't need the matched string.

    Example:
    {[
      let skip_spaces () = skip_while (fun c -> c = ' ')
    ]}
*)
val skip_while : (char -> bool) -> unit

(** [skip_while_then_char pred c] skips characters matching [pred] then matches [c].
    Fused operation - more efficient than [skip_while pred; char c]. *)
val skip_while_then_char : (char -> bool) -> char -> unit

(** [sep_by_take ws_pred sep_char take_pred] parses a separated list entirely in the handler.
    Returns list of matched strings. Zero intermediate effect dispatches. *)
val sep_by_take : (char -> bool) -> char -> (char -> bool) -> string list

(** [take_while_span pred] like [take_while] but returns a zero-copy span.
    No string allocation until you call [span_to_string]. *)
val take_while_span : (char -> bool) -> span

(** [sep_by_take_span ws_pred sep_char take_pred] like [sep_by_take] but returns
    zero-copy spans. No String.sub allocations per element. *)
val sep_by_take_span : (char -> bool) -> char -> (char -> bool) -> span list

(** [fused_sep_take ws_pred sep_char take_pred] performs:
    skip whitespace, match separator, skip whitespace, take_while1
    all in a single effect dispatch. Returns the taken string.
    Much more efficient than separate calls for parsing separated values. *)
val fused_sep_take : (char -> bool) -> char -> (char -> bool) -> string

(** [fail msg] aborts parsing with an error message.
    
    Example:
    {[
      let validate_range n =
        if n >= 0 && n <= 255 then n
        else fail "number out of range"
    ]}
*)
val fail : string -> 'a

(** [end_of_input ()] succeeds only if no input remains.
    Use this to ensure the entire input has been consumed.
    
    Example:
    {[
      let complete_parser () =
        let result = some_parser () in
        end_of_input ();
        result
    ]}
*)
val end_of_input : unit -> unit

(** [(<|>)] is the alternation combinator. 
    Tries the left parser; if it fails, backtracks and tries the right parser.
    
    Example:
    {[
      let bool_parser () = 
        ((fun () -> consume "true"; true) <|> 
         (fun () -> consume "false"; false)) ()
    ]}
*)
val ( <|> ) : (unit -> 'a) -> (unit -> 'a) -> unit -> 'a

(** [look_ahead p] runs parser [p] without consuming input.
    Fails if [p] fails, but doesn't advance the cursor on success.
    
    Example:
    {[
      let check_next_is_digit () =
        look_ahead digit;
        (* cursor hasn't moved *)
    ]}
*)
val look_ahead : (unit -> 'a) -> 'a

(** [string s] is an alias for [consume s]. *)
val string : string -> string

(** {1 Repetition Combinators} *)

(** [many p] applies parser [p] zero or more times.
    Returns a list of results.
    
    Example:
    {[
      let digits () = many digit ()  (* parses "123" -> [1; 2; 3] *)
    ]}
*)
val many : (unit -> 'a) -> unit -> 'a list

(** [many1 p] applies parser [p] one or more times.
    Fails if [p] doesn't match at least once.
    
    Example:
    {[
      let non_empty_digits () = many1 digit ()
    ]}
*)
val many1 : (unit -> 'a) -> unit -> 'a list

(** [sep_by p sep] parses zero or more occurrences of [p] separated by [sep].
    
    Example:
    {[
      let csv_line () = 
        sep_by 
          (fun () -> match_re (Re.compile (Re.Posix.re "[^,]+")))
          (fun () -> char ',')
          ()
    ]}
*)
val sep_by : (unit -> 'a) -> (unit -> 'b) -> unit -> 'a list

(** [sep_by1 p sep] parses one or more occurrences of [p] separated by [sep].
    Fails if [p] doesn't match at least once.
*)
val sep_by1 : (unit -> 'a) -> (unit -> 'b) -> unit -> 'a list

(** [optional p] optionally applies parser [p].
    Returns [Some result] if [p] succeeds, [None] if it fails.
    
    Example:
    {[
      let optional_sign () = 
        optional (fun () -> char '-' <|> char '+') ()
    ]}
*)
val optional : (unit -> 'a) -> unit -> 'a option

(** [count n p] applies parser [p] exactly [n] times.
    Fails if [p] doesn't match [n] times.
    
    Example:
    {[
      let three_digits () = count 3 digit ()
    ]}
*)
val count : int -> (unit -> 'a) -> unit -> 'a list

(** {1 Convenience Combinators} *)

(** [digit ()] parses a decimal digit (0-9) and returns its integer value.
    
    Example:
    {[
      let d = digit () in  (* parses "7" -> 7 *)
      ...
    ]}
*)
val digit : unit -> int

(** [letter ()] parses an ASCII letter (a-z or A-Z). *)
val letter : unit -> char

(** [is_whitespace c] returns true for whitespace characters (space, tab, newline, CR). *)
val is_whitespace : char -> bool

(** [whitespace ()] parses zero or more whitespace characters (space, tab, newline, carriage return).
    Uses fast character scanning (not regex). *)
val whitespace : unit -> string

(** [whitespace1 ()] parses one or more whitespace characters. *)
val whitespace1 : unit -> string

(** [skip_whitespace ()] skips zero or more whitespace characters (returns unit).
    More efficient than [whitespace] when you don't need the matched string. *)
val skip_whitespace : unit -> unit

(** [alphanum ()] parses an alphanumeric character (letter or digit). *)
val alphanum : unit -> char

(** [any_char ()] parses any character. *)
val any_char : unit -> char
