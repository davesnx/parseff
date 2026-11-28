type headers = (string * string) list

type request = {
  meth : string;
  target : string;
  version : int * int;
  headers : headers;
}

type response = {
  version : int * int;
  status : int;
  reason : string;
  headers : headers;
}

type body = No_body | Data of int | Form of (string * string) list

type Parseff.custom_error +=
  | Bad_framing of string
  | Unsupported_coding of string
  | Body_too_large of int

let describe = function
  | Bad_framing m ->
      m
  | Unsupported_coding c ->
      "unsupported transfer coding " ^ c
  | Body_too_large n ->
      Printf.sprintf "body larger than %d bytes" n
  | _ ->
      "unknown error"

let max_method = 32
let max_target = 8190
let max_name = 256
let max_value = 8190
let max_headers = 100
let max_chunk_digits = 15
let max_chunk_extensions = 16
let default_max_body = 1 lsl 30

let is_tchar = function
  | '!'
  | '#'
  | '$'
  | '%'
  | '&'
  | '\''
  | '*'
  | '+'
  | '-'
  | '.'
  | '^'
  | '_'
  | '`'
  | '|'
  | '~'
  | '0' .. '9'
  | 'a' .. 'z'
  | 'A' .. 'Z' ->
      true
  | _ ->
      false

let is_ows c = c = ' ' || c = '\t'
let is_field_char c = c = '\t' || (c >= ' ' && c <> '\127')
let is_target_char c = c > ' ' && c <> '\127'
let is_digit c = c >= '0' && c <= '9'

let is_hex = function
  | '0' .. '9' | 'a' .. 'f' | 'A' .. 'F' ->
      true
  | _ ->
      false

let is_zero c = c = '0'
let is_qdtext c = c <> '"' && c <> '\\' && is_field_char c
let is_form_key c = c <> '=' && c <> '&'
let is_form_value c = c <> '&'
let is_any _ = true
let tchar = Parseff.Charset.of_predicate is_tchar
let field_char = Parseff.Charset.of_predicate is_field_char
let target_char = Parseff.Charset.of_predicate is_target_char
let ows = Parseff.Charset.of_predicate is_ows
let digit = Parseff.Charset.of_predicate is_digit
let hex = Parseff.Charset.of_predicate is_hex
let zero = Parseff.Charset.of_predicate is_zero
let qdtext = Parseff.Charset.of_predicate is_qdtext
let form_key = Parseff.Charset.of_predicate is_form_key
let form_value = Parseff.Charset.of_predicate is_form_value
let any_byte = Parseff.Charset.of_predicate is_any
let crlf () = Parseff.literal "\r\n"

let trim_ows s =
  let rec stop i =
    if i > 0 && is_ows s.[i - 1] then
      stop (i - 1)
    else
      i
  in
  let n = stop (String.length s) in
  if n = String.length s then
    s
  else
    String.sub s 0 n

let version () =
  Parseff.literal "HTTP/";
  let major = Parseff.digit () in
  let _ = Parseff.char '.' in
  let minor = Parseff.digit () in
  (Char.code major - 48, Char.code minor - 48)

let field () =
  let name =
    Parseff.take_while ~at_least:1 ~at_most:max_name ~label:"field name"
      ~set:tchar is_tchar
  in
  let _ = Parseff.char ':' in
  Parseff.skip_while ~set:ows is_ows;
  let value =
    Parseff.take_while ~at_most:max_value ~set:field_char is_field_char
  in
  crlf ();
  (name, trim_ows value)

let fields name () =
  Parseff.context name (fun () ->
      let fields = Parseff.many ~at_most:max_headers field in
      crlf ();
      fields
  )

let request_head () =
  if Parseff.peek_char () = Some '\r' then crlf ();
  let meth =
    Parseff.context "method" (fun () ->
        Parseff.take_while ~at_least:1 ~at_most:max_method ~label:"method"
          ~set:tchar is_tchar
    )
  in
  let _ = Parseff.char ' ' in
  let target =
    Parseff.context "request target" (fun () ->
        Parseff.take_while ~at_least:1 ~at_most:max_target
          ~label:"request target" ~set:target_char is_target_char
    )
  in
  let _ = Parseff.char ' ' in
  let version = version () in
  crlf ();
  let headers = fields "header section" () in
  { meth; target; version; headers }

let status () =
  let code = Parseff.take_while ~set:digit is_digit in
  match String.length code with
  | 0 ->
      Parseff.fail "status code empty"
  | 3 when code.[0] <> '0' ->
      int_of_string code
  | 1 | 2 | 3 ->
      Parseff.fail (Printf.sprintf "invalid status code: %S" code)
  | _ ->
      Parseff.fail (Printf.sprintf "status code too long: %S" code)

let folded_fields () =
  Parseff.context "header section" (fun () ->
      let start = Parseff.position () in
      let line () =
        match Parseff.peek_char () with
        | Some (' ' | '\t') when Parseff.position () > start ->
            Parseff.skip_while ~set:ows is_ows;
            let more =
              Parseff.take_while ~at_most:max_value ~set:field_char
                is_field_char
            in
            crlf ();
            `Fold more
        | _ ->
            `Field (field ())
      in
      let lines = Parseff.many ~at_most:max_headers line in
      crlf ();
      List.rev
        (List.fold_left
           (fun acc line ->
             match (line, acc) with
             | `Field f, _ ->
                 f :: acc
             | `Fold more, (name, value) :: rest ->
                 let value =
                   if value = "" then
                     more
                   else
                     value ^ " " ^ more
                 in
                 (name, trim_ows value) :: rest
             | `Fold _, [] ->
                 acc
           )
           [] lines
        )
  )

let rec skip_empty_lines () =
  if Parseff.peek_char () = Some '\r' then begin
    crlf ();
    skip_empty_lines ()
  end

let response_head () =
  skip_empty_lines ();
  let version = version () in
  if fst version <> 1 then
    Parseff.fail
      (Printf.sprintf "unsupported HTTP version: %d.%d" (fst version)
         (snd version)
      );
  let _ = Parseff.char ' ' in
  let status = status () in
  let _ = Parseff.char ' ' in
  let reason =
    Parseff.take_while ~at_most:max_value ~set:field_char is_field_char
  in
  crlf ();
  let headers = folded_fields () in
  { version; status; reason; headers }

let find name headers =
  List.find_map
    (fun (k, v) ->
      if String.lowercase_ascii k = name then
        Some v
      else
        None
    )
    headers

let values name headers =
  List.filter_map
    (fun (k, v) ->
      if String.lowercase_ascii k = name then
        Some v
      else
        None
    )
    headers

let list_elements values =
  List.concat_map (String.split_on_char ',') values
  |> List.map String.trim
  |> List.filter (fun v -> v <> "")

let chunk_size () =
  Parseff.context "chunk size" (fun () ->
      let start = Parseff.position () in
      Parseff.skip_while ~set:zero is_zero;
      let digits =
        Parseff.take_while ~at_most:max_chunk_digits ~label:"chunk size"
          ~set:hex is_hex
      in
      if Parseff.position () = start then
        ignore
          (Parseff.take_while ~at_least:1 ~label:"chunk size" ~set:hex is_hex);
      if digits = "" then
        0
      else
        int_of_string ("0x" ^ digits)
  )

let token label =
  Parseff.take_while ~at_least:1 ~at_most:max_name ~label ~set:tchar is_tchar

let quoted_string () =
  let _ = Parseff.char '"' in
  let rec rest () =
    ignore (Parseff.take_while ~at_most:max_value ~set:qdtext is_qdtext);
    if Parseff.peek_char () = Some '\\' then begin
      let _ = Parseff.char '\\' in
      ignore (Parseff.satisfy ~label:"quoted character" is_field_char);
      rest ()
    end else
      ignore (Parseff.char '"')
  in
  rest ()

let chunk_extension () =
  Parseff.skip_while ~set:ows is_ows;
  let _ = Parseff.char ';' in
  Parseff.skip_while ~set:ows is_ows;
  ignore (token "extension name");
  ignore
    (Parseff.optional (fun () ->
         Parseff.skip_while ~set:ows is_ows;
         let _ = Parseff.char '=' in
         Parseff.skip_while ~set:ows is_ows;
         Parseff.or_ quoted_string (fun () -> ignore (token "extension value"))
     )
    )

let chunk_line () =
  let size = chunk_size () in
  ignore (Parseff.many ~at_most:max_chunk_extensions chunk_extension);
  crlf ();
  size

let chunked_body ?(max_body = default_max_body) on_data () =
  let rec chunks total =
    let size = chunk_line () in
    if size = 0 then
      total
    else begin
      if size > max_body - total then Parseff.error (Body_too_large max_body);
      Parseff.Iter.take size on_data;
      crlf ();
      chunks (total + size)
    end
  in
  let total = chunks 0 in
  let trailers = fields "trailer section" () in
  (total, trailers)

let form_char () =
  match Parseff.any_char () with
  | '+' ->
      ' '
  | '%' ->
      Parseff.or_
        (fun () -> Char.chr (Parseff.within 2 Parseff.hex_integer))
        (fun () -> '%')
  | c ->
      c

let form_decode s =
  String.of_seq
    (List.to_seq
       (Result.get_ok (Parseff.parse s (fun () -> Parseff.many form_char)))
    )

let form_field () =
  let key =
    Parseff.take_while ~at_least:1 ~label:"form key" ~set:form_key is_form_key
  in
  let value =
    Parseff.optional (fun () ->
        let _ = Parseff.char '=' in
        Parseff.take_while ~set:form_value is_form_value
    )
  in
  (form_decode key, form_decode (Option.value value ~default:""))

let form () = Parseff.sep_by form_field (fun () -> Parseff.char '&')

let content_length headers =
  match
    ( values "content-length" headers,
      list_elements (values "content-length" headers)
    )
  with
  | [], _ ->
      None
  | _, v :: rest
    when v <> "" && String.for_all is_digit v && String.length v < 16 ->
      if List.for_all (String.equal v) rest then
        Some (int_of_string v)
      else
        Parseff.error (Bad_framing "conflicting Content-Length values")
  | _ ->
      Parseff.error (Bad_framing "invalid Content-Length")

let chunked ~version headers =
  match values "transfer-encoding" headers with
  | [] ->
      false
  | te ->
      if version < (1, 1) then
        Parseff.error (Bad_framing "Transfer-Encoding in an HTTP/1.0 message");
      if values "content-length" headers <> [] then
        Parseff.error
          (Bad_framing "Transfer-Encoding and Content-Length together");
      let codings = List.map String.lowercase_ascii (list_elements te) in
      begin match List.rev codings with
      | "chunked" :: before when not (List.mem "chunked" before) -> (
          match before with
          | [] ->
              true
          | coding :: _ ->
              Parseff.error (Unsupported_coding coding)
        )
      | _ ->
          Parseff.error
            (Bad_framing "chunked must be the final transfer coding, once")
      end

let body ?(max_body = default_max_body) ~version headers on_data =
  if chunked ~version headers then
    Data (fst (chunked_body ~max_body on_data ()))
  else
    match content_length headers with
    | None | Some 0 ->
        No_body
    | Some n when n > max_body ->
        Parseff.error (Body_too_large max_body)
    | Some n -> (
        match find "content-type" headers with
        | Some "application/x-www-form-urlencoded" ->
            Form (Parseff.within n form)
        | _ ->
            Parseff.Iter.take n on_data;
            Data n
      )

let response_body ?(max_body = default_max_body) ~meth (head : response) on_data
    =
  if
    meth = "HEAD" || head.status < 200 || head.status = 204 || head.status = 304
    || (meth = "CONNECT" && head.status < 300)
  then
    No_body
  else if
    chunked ~version:head.version head.headers
    || content_length head.headers <> None
  then
    body ~max_body ~version:head.version head.headers on_data
  else
    let total = ref 0 in
    Parseff.context "body" (fun () ->
        Parseff.Iter.take_while ~at_most:max_body ~set:any_byte is_any
          (fun piece ->
            total := !total + Parseff.Slice.length piece;
            on_data piece
        )
    );
    Data !total

let request ?max_body on_data () =
  let head = Parseff.context "head" request_head in
  Parseff.commit ();
  let body = body ?max_body ~version:head.version head.headers on_data in
  (head, body)

let status_of_error (e : Parseff.error) =
  let innermost =
    match List.rev e.contexts with c :: _ -> c.name | [] -> ""
  in
  match e.kind with
  | Parseff.Limit_exceeded _ when innermost = "method" ->
      (501, "Not Implemented")
  | Custom (Unsupported_coding _) ->
      (501, "Not Implemented")
  | Custom (Body_too_large _) ->
      (413, "Content Too Large")
  | Parseff.Limit_exceeded _ when innermost = "body" ->
      (413, "Content Too Large")
  | Parseff.Limit_exceeded _ when innermost = "request target" ->
      (414, "URI Too Long")
  | Parseff.Limit_exceeded _
    when innermost = "header section" || innermost = "trailer section" ->
      (431, "Request Header Fields Too Large")
  | Backtrack_window_exceeded _
    when List.exists (fun (c : Parseff.context) -> c.name = "head") e.contexts
    ->
      (431, "Request Header Fields Too Large")
  | Backtrack_window_exceeded _ ->
      (413, "Content Too Large")
  | _ ->
      (400, "Bad Request")
