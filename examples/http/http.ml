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

type body = No_body | Raw of string | Form of (string * string) list

type Parseff.custom_error +=
  | Bad_framing of string
  | Unsupported_coding of string

let describe = function
  | Bad_framing m ->
      m
  | Unsupported_coding c ->
      "unsupported transfer coding " ^ c
  | _ ->
      "unknown error"

let max_method = 32
let max_target = 8190
let max_name = 256
let max_value = 8190
let max_headers = 100
let max_chunk_digits = 15
let max_chunk_extensions = 16

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

let crlf () = ignore (Parseff.consume "\r\n")

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
  let _ = Parseff.consume "HTTP/" in
  let major = Parseff.digit () in
  let _ = Parseff.char '.' in
  let minor = Parseff.digit () in
  (Char.code major - 48, Char.code minor - 48)

let field () =
  let name =
    Parseff.take_while ~at_least:1 ~at_most:max_name ~label:"field name"
      is_tchar
  in
  let _ = Parseff.char ':' in
  Parseff.cut ();
  Parseff.skip_while is_ows;
  let value = Parseff.take_while ~at_most:max_value is_field_char in
  crlf ();
  (name, trim_ows value)

let fields name () =
  Parseff.context name (fun () ->
      let fields = Parseff.many ~at_most:max_headers field () in
      crlf ();
      fields
  )

let request_head () =
  let meth =
    Parseff.context "method" (fun () ->
        Parseff.take_while ~at_least:1 ~at_most:max_method ~label:"method"
          is_tchar
    )
  in
  let _ = Parseff.char ' ' in
  let target =
    Parseff.context "request target" (fun () ->
        Parseff.take_while ~at_least:1 ~at_most:max_target
          ~label:"request target" is_target_char
    )
  in
  let _ = Parseff.char ' ' in
  let version = version () in
  crlf ();
  let headers = fields "header section" () in
  { meth; target; version; headers }

let response_head () =
  let version = version () in
  let _ = Parseff.char ' ' in
  let status = Parseff.count 3 Parseff.digit () in
  let status =
    List.fold_left (fun n d -> (n * 10) + Char.code d - 48) 0 status
  in
  let _ = Parseff.char ' ' in
  let reason = Parseff.take_while ~at_most:max_value is_field_char in
  crlf ();
  let headers = fields "header section" () in
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
      let digits =
        Parseff.take_while ~at_least:1 ~at_most:max_chunk_digits
          ~label:"chunk size" is_hex
      in
      int_of_string ("0x" ^ digits)
  )

let token label =
  Parseff.take_while ~at_least:1 ~at_most:max_name ~label is_tchar

let quoted_string () =
  let _ = Parseff.char '"' in
  let rec rest () =
    ignore
      (Parseff.take_while ~at_most:max_value (fun c ->
           c <> '"' && c <> '\\' && is_field_char c
       )
      );
    if Parseff.peek_char () = Some '\\' then begin
      let _ = Parseff.char '\\' in
      ignore (Parseff.satisfy ~label:"quoted character" is_field_char);
      rest ()
    end else
      ignore (Parseff.char '"')
  in
  rest ()

let chunk_extension () =
  Parseff.skip_while is_ows;
  let _ = Parseff.char ';' in
  Parseff.skip_while is_ows;
  ignore (token "extension name");
  ignore
    (Parseff.optional
       (fun () ->
         Parseff.skip_while is_ows;
         let _ = Parseff.char '=' in
         Parseff.skip_while is_ows;
         Parseff.or_ quoted_string
           (fun () -> ignore (token "extension value"))
           ()
       )
       ()
    )

let chunked_body () =
  let buf = Buffer.create 256 in
  let rec chunks () =
    let size = chunk_size () in
    ignore (Parseff.many ~at_most:max_chunk_extensions chunk_extension ());
    crlf ();
    if size > 0 then begin
      Buffer.add_string buf (Parseff.take size);
      crlf ();
      chunks ()
    end
  in
  chunks ();
  let trailers = fields "trailer section" () in
  (Buffer.contents buf, trailers)

let form_field () =
  let key =
    Parseff.take_while ~at_least:1 ~label:"form key" (fun c ->
        c <> '=' && c <> '&'
    )
  in
  let value =
    Parseff.optional
      (fun () ->
        let _ = Parseff.char '=' in
        Parseff.take_while (fun c -> c <> '&')
      )
      ()
  in
  (key, Option.value value ~default:"")

let form () = Parseff.sep_by form_field (fun () -> Parseff.char '&') ()

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

let chunked headers =
  match values "transfer-encoding" headers with
  | [] ->
      false
  | te ->
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

let body headers =
  if chunked headers then
    Raw (fst (chunked_body ()))
  else
    match content_length headers with
    | None | Some 0 ->
        No_body
    | Some n -> (
        match find "content-type" headers with
        | Some "application/x-www-form-urlencoded" ->
            Form (Parseff.within n form)
        | _ ->
            Raw (Parseff.take n)
      )

let request () =
  let head = request_head () in
  Parseff.commit ();
  let body = body head.headers in
  (head, body)

let status_of_error (e : Parseff.error) =
  let innermost =
    match List.rev e.contexts with c :: _ -> c.name | [] -> ""
  in
  match e.kind with
  | Custom (Parseff.Limit_exceeded _) when innermost = "method" ->
      (501, "Not Implemented")
  | Custom (Unsupported_coding _) ->
      (501, "Not Implemented")
  | Custom (Parseff.Limit_exceeded _) when innermost = "request target" ->
      (414, "URI Too Long")
  | Custom (Parseff.Limit_exceeded _)
    when innermost = "header section" || innermost = "trailer section" ->
      (431, "Request Header Fields Too Large")
  | Backtrack_window_exceeded _ ->
      (431, "Request Header Fields Too Large")
  | _ ->
      (400, "Bad Request")
