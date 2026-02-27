let is_digit c = c >= '0' && c <= '9'
let is_digit_or_sign c = is_digit c || c = '-' || c = '.'
let is_ws c = c = ' ' || c = '\t' || c = '\n' || c = '\r'
let is_not_comma c = c <> ','

let[@inline always] float_of_span (s : Parseff.span) =
  if s.len = 1 then
    Float.of_int (Char.code (String.unsafe_get s.buf s.off) - Char.code '0')
  else if s.len = 2 && String.unsafe_get s.buf s.off >= '1' then
    Float.of_int
      (((Char.code (String.unsafe_get s.buf s.off) - Char.code '0') * 10)
      + (Char.code (String.unsafe_get s.buf (s.off + 1)) - Char.code '0'))
  else float_of_string (Parseff.span_to_string s)

(** {1 JSON array} *)

let json_array () =
  Parseff.skip_while_then_char is_ws '[';
  Parseff.skip_while is_ws;
  let spans = Parseff.sep_by_take_span is_ws ',' is_digit_or_sign in
  let elements = List.map float_of_span spans in
  Parseff.skip_while_then_char is_ws ']';
  elements

let parse_json_array input =
  match Parseff.parse input json_array with
  | Ok result -> Some result
  | Error _ -> None

(** {1 CSV} *)

let csv () =
  let spans = Parseff.sep_by_take_span (fun _ -> false) ',' is_not_comma in
  List.map Parseff.span_to_string spans

let parse_csv input =
  match Parseff.parse input csv with
  | Ok result -> Some result
  | Error _ -> None

(** {1 Arithmetic} *)

let[@inline] int_of_span (s : Parseff.span) =
  let rec go acc i =
    if i >= s.off + s.len then acc
    else
      let d = Char.code (String.unsafe_get s.buf i) - Char.code '0' in
      go ((acc * 10) + d) (i + 1)
  in
  go 0 s.off

let is_arith_op c = c = '+' || c = '-' || c = '*' || c = '/'

let expr () =
  (* Parse the full expression as: number (op number)* using bulk reads *)
  let first_span = Parseff.take_while_span is_digit in
  let first = int_of_span first_span in
  (* Collect all (op, number) pairs *)
  let rec collect_ops nums ops =
    let op_span = Parseff.take_while_span is_arith_op in
    if op_span.len = 0 then (nums, ops)
    else
      let op_char = String.unsafe_get op_span.buf op_span.off in
      let num_span = Parseff.take_while_span is_digit in
      let num = int_of_span num_span in
      collect_ops (num :: nums) (op_char :: ops)
  in
  let nums_rev, ops_rev = collect_ops [] [] in
  let nums = List.rev nums_rev in
  let ops = List.rev ops_rev in
  (* Two-pass evaluation respecting precedence *)
  (* Pass 1: evaluate * and / left-to-right, collecting + and - *)
  let rec pass1 acc nums ops =
    match (nums, ops) with
    | n :: rest_nums, '*' :: rest_ops -> pass1 (acc * n) rest_nums rest_ops
    | n :: rest_nums, '/' :: rest_ops -> pass1 (acc / n) rest_nums rest_ops
    | _, ('+' | '-') :: _ -> (acc, nums, ops)
    | [], [] -> (acc, [], [])
    | _ -> (acc, nums, ops)
  in
  let rec pass2 result nums ops =
    match (nums, ops) with
    | [], [] -> result
    | n :: rest_nums, '+' :: rest_ops ->
        let term, rest_nums', rest_ops' = pass1 n rest_nums rest_ops in
        pass2 (result + term) rest_nums' rest_ops'
    | n :: rest_nums, '-' :: rest_ops ->
        let term, rest_nums', rest_ops' = pass1 n rest_nums rest_ops in
        pass2 (result - term) rest_nums' rest_ops'
    | _ -> result
  in
  let first_term, rest_nums, rest_ops = pass1 first nums ops in
  pass2 first_term rest_nums rest_ops

let parse_arithmetic input =
  match Parseff.parse input expr with
  | Ok result -> Some result
  | Error _ -> None
