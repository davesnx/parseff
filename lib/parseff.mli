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

(** [whitespace ()] parses zero or more whitespace characters (space, tab, newline, carriage return). *)
val whitespace : unit -> string

(** [whitespace1 ()] parses one or more whitespace characters. *)
val whitespace1 : unit -> string

(** [alphanum ()] parses an alphanumeric character (letter or digit). *)
val alphanum : unit -> char

(** [any_char ()] parses any character. *)
val any_char : unit -> char
