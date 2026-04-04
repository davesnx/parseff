type color_type =
  | Grayscale
  | RGB
  | Indexed
  | Grayscale_alpha
  | RGBA
  | Unknown of int

type png_header = {
  width : int32;
  height : int32;
  bit_depth : int;
  color_type : color_type;
}

let color_type_of_int = function
  | 0 ->
      Grayscale
  | 2 ->
      RGB
  | 3 ->
      Indexed
  | 4 ->
      Grayscale_alpha
  | 6 ->
      RGBA
  | n ->
      Unknown n

let color_type_to_string = function
  | Grayscale ->
      "Grayscale"
  | RGB ->
      "RGB"
  | Indexed ->
      "Indexed"
  | Grayscale_alpha ->
      "Grayscale+Alpha"
  | RGBA ->
      "RGBA"
  | Unknown n ->
      Printf.sprintf "Unknown(%d)" n

(* Each PNG chunk has: 4-byte length, 4-byte type, data, 4-byte CRC *)
let chunk_header () =
  let length = Parseff.BE.any_int32 () in
  let chunk_type = Parseff.take 4 in
  (length, chunk_type)

(* The IHDR chunk contains the image dimensions and format *)
let ihdr_data () =
  let width = Parseff.BE.any_int32 () in
  let height = Parseff.BE.any_int32 () in
  let bit_depth = Parseff.BE.any_uint8 () in
  let color_type = color_type_of_int (Parseff.BE.any_uint8 ()) in
  (* Skip compression method, filter method, interlace method, CRC *)
  let _compression = Parseff.BE.any_uint8 () in
  let _filter = Parseff.BE.any_uint8 () in
  let _interlace = Parseff.BE.any_uint8 () in
  let _crc = Parseff.BE.any_int32 () in
  { width; height; bit_depth; color_type }

let parse_png_header () =
  (* PNG files start with an 8-byte magic signature *)
  let expected = "\x89PNG\r\n\x1a\n" in
  let actual = Parseff.take 8 in
  if actual <> expected then Parseff.fail "PNG signature";
  let _length, chunk_type = chunk_header () in
  if chunk_type <> "IHDR" then Parseff.fail "IHDR chunk";
  ihdr_data ()

let () =
  let path = Sys.argv.(1) in
  let input = In_channel.with_open_bin path In_channel.input_all in
  match Parseff.parse input parse_png_header with
  | Ok hdr ->
      Printf.printf "%s:\n" path;
      Printf.printf "  %ldx%ld, %d-bit %s\n" hdr.width hdr.height hdr.bit_depth
        (color_type_to_string hdr.color_type)
  | Error { pos; error = `Expected msg } ->
      Printf.printf "%s: error at byte %d: expected %s\n" path pos msg
  | Error { pos; error = `Failure msg } ->
      Printf.printf "%s: error at byte %d: %s\n" path pos msg
  | Error { pos; error = `Unexpected_end_of_input } ->
      Printf.printf "%s: unexpected end of input at byte %d\n" path pos
  | Error _ ->
      Printf.printf "%s: unknown error\n" path
