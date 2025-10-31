(*---------------------------------------------------------------------------
   Large parts of this codec are taken from the jsont codec.

   Copyright (c) 2024 The jsont programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)
open Bytesrw

(* Parser *)
module Textloc = struct
  type line_pos = int
  type byte_pos = int

  type t = {
    filename : string;
    first_byte : byte_pos;
    first_line : line_pos * byte_pos;
    last_byte : byte_pos;
    last_line : line_pos * byte_pos;
  }

  let make ~filename ~first_byte ~last_byte ~first_line ~last_line =
    { filename; first_byte; last_byte; first_line; last_line }

  let pp_path = Fmt.string
  let pf = Fmt.pf

  let is_none l = l.first_byte < 0
  let pp ppf l = match is_none l with
  | true -> pf ppf "File \"%a\"" pp_path l.filename
  | false ->
      let pp_lines ppf l = match fst l.first_line = fst l.last_line with
      | true -> pf ppf "line %d" (fst l.first_line)
      | false -> pf ppf "lines %d-%d" (fst l.first_line) (fst l.last_line)
      in
      (* "characters" represent positions (insertion points) not columns *)
      let pos_s = l.first_byte - snd l.first_line in
      let pos_e = l.last_byte - snd l.last_line + 1 in
      if pos_s = 0 && pos_e = 0
      then pf ppf "File \"%a\", %a" pp_path l.filename pp_lines l
      else pf ppf "File \"%a\", %a, characters %d-%d"
          pp_path l.filename pp_lines l pos_s pos_e
end

type error = Textloc.t * string
exception Error of error

module Error = struct
  let msg meta msg = raise_notrace (Error (meta, msg))
  let msgf meta fmt = Format.kasprintf (fun m -> msg meta m) fmt

  let () =
    Printexc.register_printer (function 
      | Error (txtloc, msg) ->
        Some (Fmt.str "%a: %s" Textloc.pp txtloc msg)
      | _ -> None
    ) 
end

type decoder = {
    filename : string;
    reader : Bytes.Reader.t; (* The source of bytes. *)
    mutable i : Stdlib.Bytes.t; (* Current input slice. *)
    mutable i_max : int; (* Maximum byte index in [i]. *)
    mutable i_next : int; (* Next byte index to read in [i]. *)
    mutable byte_count : int; (* Global byte count. *)
    mutable line : int; (* Current line number. *)
    mutable line_start : int; (* Current line global byte position. *)
    mutable c : int; (* Next character *)
    delims : int Queue.t; (* Queue of delims *)
    token : Buffer.t;
}

let[@inline] is_digit u = 0x0030 (* 0 *) <= u && u <= 0x0039 (* 9 *)
let[@inline] is_text u = 
  (0x0061 (* a *) <= u && u <= 0x007A (* z *)) ||
  (0x0041 (* A *) <= u && u <= 0x005A (* Z *))
let[@inline] is_underscore u = Int.equal u 95
let[@inline] is_lparen u = Int.equal u 0x28
let[@inline] is_rparen u = Int.equal u 0x29
let[@inline] is_lsquare u = Int.equal u 0x5B
let[@inline] is_minus u = Int.equal u 0x2D
let[@inline] is_period u = Int.equal u 0x2E
let[@inline] is_quote u = Int.equal u 0x22
let[@inline] is_space u = Int.equal u 0x20
let[@inline] _is_special u =
  is_rparen u 
  || is_lparen u
  || is_minus u
  || is_underscore u
  || is_period u
  || is_quote u
  || is_space u
  
let make_decoder ?(filename = "-") reader =
  let token = Buffer.create 255 in
  let i = Stdlib.Bytes.create 1 in
  let delims = Queue.create () in
  { filename; reader; delims;
    c = 0;
    i_max = 0; i_next = 1 (* triggers an initial refill *); i;
    byte_count = 0; line = 1; line_start = 0; token }

let sot = 0x1A0000  (* start of text U+10FFFF + 1 *)
let eot = 0x1A0001  (*   end of text U+10FFFF + 2 *)

(* Decoder positions *)
let[@inline] get_line_pos d = d.line, d.line_start
let get_last_byte d =
  if d.c <= 0x7F then d.byte_count - 1 else
  if d.c = sot || d.c = eot then d.byte_count else failwith "Impossible last byte" 

(* Errors *)

let err_to_here ~first_byte ~first_line d fmt =
  let last_byte = get_last_byte d and last_line = get_line_pos d in
  Error.msgf (Textloc.make ~first_byte ~first_line ~last_byte ~last_line ~filename:d.filename) fmt

let err_delimiter ~first_byte ~first_line d =
  err_to_here ~first_byte ~first_line d "Expected delimiter but got '%#x'" d.c

let err_malformed_number ~first_byte ~first_line d =
  err_to_here ~first_byte ~first_line d "Malformed number, expected it to start with -, + or [0-9] got '%c'" (Stdlib.Char.unsafe_chr d.c) 

let err_unexpected_character ~first_byte ~first_line e d =
  err_to_here ~first_byte ~first_line d 
    "Unexpected character, expected '%c' but got '%c'" (Stdlib.Char.unsafe_chr e) (Stdlib.Char.unsafe_chr d.c) 

(* Decode next character in d.u *)
let[@inline] is_eod d = d.i_max = - 1 (* Only happens on Slice.eod *)
let[@inline] available d = d.i_max - d.i_next + 1
let[@inline] set_slice d slice =
  d.i <- Bytes.Slice.bytes slice;
  d.i_next <- Bytes.Slice.first slice;
  d.i_max <- d.i_next + Bytes.Slice.length slice - 1

let rec nextc d =
  let first_byte = get_last_byte d and first_line = get_line_pos d in
  let a = available d in
  if a <= 0 then
    (if is_eod d
     then d.c <- eot
     else (set_slice d (Bytes.Reader.read d.reader); nextc d))
  else
  let b = Bytes.get d.i d.i_next in
  d.c <- match b with
  | '\x00' .. '\x09' | '\x0B' | '\x0E' .. '\x7F' as u -> (* ASCII fast path *)
      d.i_next <- d.i_next + 1; d.byte_count <- d.byte_count + 1;
      Stdlib.Char.code u
  | '\x0D' (* CR *) ->
      d.i_next <- d.i_next + 1; d.byte_count <- d.byte_count + 1;
      d.line_start <- d.byte_count; d.line <- d.line + 1;
      0x000D
  | '\x0A' (* LF *) ->
      d.i_next <- d.i_next + 1; d.byte_count <- d.byte_count + 1;
      d.line_start <- d.byte_count;
      if d.c <> 0x000D then d.line <- d.line + 1;
      0x000A
  | c ->
    err_to_here ~first_line ~first_byte d "Unsupported encoding, please use ASCII with LaTeX. Got %i." (Stdlib.Char.code c)

let[@inline] token_clear d = Buffer.clear d.token
let[@inline] token_pop d = let t = Buffer.contents d.token in (token_clear d; t)
let[@inline] token_add d u = Buffer.add_char d.token (Stdlib.Char.unsafe_chr u)
let[@inline] accept d = token_add d d.c; nextc d

(* Whitespace *)
let[@inline] is_ws u =
  if u > 0x20 then false else match Stdlib.Char.unsafe_chr u with
  | ' ' | '\t' | '\r' | '\n' -> true
  | _ -> false

let[@inline] read_ws d =
  while is_ws d.c do
    nextc d
  done

let readc c d =
  let first_byte = get_last_byte d and first_line = get_line_pos d in
  if d.c = c then nextc d
  else err_unexpected_character ~first_byte ~first_line c d

(* let unsigned_integer d = *)
(*   while is_digit d.c do *)
(*     accept d *)
(*   done; *)
(*   token_pop d *)
  
let is_negative d = 
  let first_byte = get_last_byte d and first_line = get_line_pos d in
  match d.c with 
    | 0x2B (* + *) -> nextc d; false
    | 0x2D (* - *) -> nextc d; true
    | _ when is_digit d.c -> false
    | _ -> err_malformed_number ~first_byte ~first_line d

let is_unl d =
  is_period d.c || is_digit d.c || Int.equal d.c 0x45

let consume_unl d =
  while is_unl d do
    accept d
  done;
  Float.of_string (token_pop d)

let consume_snl d =
  let neg = is_negative d in
  let v = consume_unl d in
  if neg then Float.neg v else v

let fill_n_numbers ?(depth=0) n d =
  let shape = match depth with
    | 0 -> [| n |]
    | 1 -> [| 1; n |]
    | 2 -> [| 1; 1; n |]
    | 3 -> [| 1; 1; 1; n |]
    | _ -> Fmt.invalid_arg "Fill N numbers depth must be less than 4"
  in
  let idx i = match depth with
   | 0 -> [ i ]
   | 1 -> [ 0; i ]
   | 2 -> [ 0; 0; i ]
   | 3 -> [ 0; 0; 0; i ]
   | _ -> Fmt.invalid_arg "Fill N numbers idx must have a depth less than 4"
  in
  let arr = Nx.zeros Nx.float64 shape in
  let rec loop = function
    | 0 -> arr
    | i ->
      let f = consume_snl d in
      read_ws d;
      Nx.set_item (idx (n - i)) f arr;
      loop (i - 1)
  in
  loop n

module Geometry = struct
  type kind =
    | TwoD
    | TwoDM
    | ThreeD
    | ThreeDM

  let dims = function
    | TwoD -> 2
    | TwoDM -> 3
    | ThreeD -> 3
    | ThreeDM -> 4

  let pp_kind fmt = function
    | TwoD -> Fmt.string fmt "2D"
    | ThreeD -> Fmt.string fmt "3D"
    | TwoDM -> Fmt.string fmt "2D-m"
    | ThreeDM-> Fmt.string fmt "3D-m"

  type 'a with_kind = {
    value : 'a;
    kind : kind;
  }

  let kind t = t.kind

  let pp_with_kind pp ppf m =
    Fmt.pf ppf "%a (%a)" pp m.value pp_kind m.kind

  type array = (float, Bigarray.float64_elt) Nx.t 

  type point = array 
  type linestring = array
  type polygon = linestring list
  type polyhedralsurface = polygon list

  type t = 
    | Point of point with_kind
    | LineString of linestring with_kind
    | Polygon of polygon with_kind
    | PolyhedralSurface of polyhedralsurface with_kind
    | MultiPoint of point list with_kind
    | MultiLineString of linestring list with_kind
    | MultiPolygon of polygon list with_kind
    | Collection of t list with_kind

  let make_point ?(kind=TwoD) arr =
    Point { value = arr; kind }

  let point = Fun.id
  let points = Fun.id

  let linestring ?(kind=TwoD) arr =
    LineString { value = arr; kind }

  let polygon ?(kind=TwoD) arr =
    Polygon { value = arr; kind }

  let pp ppf = function
    | Point arr -> Fmt.pf ppf "point (%a)" (pp_with_kind Nx.pp) arr
    | LineString arr -> Fmt.pf ppf "linestring (%a)" (pp_with_kind Nx.pp) arr
    | Polygon arr -> Fmt.pf ppf "polygon (%a)" (pp_with_kind (Fmt.list Nx.pp)) arr
    | _ -> Fmt.pf ppf "pp todo"

let consume_point ?(kind=TwoD) d =
  match kind with
  | TwoD -> fill_n_numbers 2 d
  | ThreeD | TwoDM -> fill_n_numbers 3 d
  | ThreeDM -> fill_n_numbers 4 d

let csv ?(delimiter=0x29) p d =
  let first_byte = get_last_byte d and first_line = get_line_pos d in
  let rec loop d = match d.c with
  | 0x2C (* , *) -> 
      nextc d; read_ws d; p d; read_ws d; loop d
  | c when Int.equal delimiter c -> ()
  | _ -> err_unexpected_character ~first_byte ~first_line 0x2C d
  in
  loop d

let read_lparen = readc 0x28
let read_rparen = readc 0x29

let consume_linestring ?(kind=TwoD) d =
  read_lparen d; 
  let dims = dims kind in
  let p1 = fill_n_numbers ~depth:1 dims d in
  let points = ref [ p1 ] in
  csv (fun d -> points := fill_n_numbers ~depth:1 dims d :: !points) d;
  read_rparen d;
  Nx.concatenate ~axis:0 (List.rev !points)

let consume_polygon ?(kind=TwoD) d =
  read_lparen d;
  let l1 = consume_linestring ~kind d in
  let lines = ref [ l1 ] in
  csv (fun d -> lines := consume_linestring ~kind d :: !lines) d;
  read_rparen d;
  List.rev !lines

(* Leaves the cursor on '(' *)
let consume_kind d = 
  let first_byte = get_last_byte d and first_line = get_line_pos d in
  match d.c with
  | 0x28 (* ( *) -> TwoD
  | 0x6D (* m *) -> nextc d; read_ws d; TwoDM
  | 0x7A (* z *) -> (
    nextc d;
    match d.c with
    | 0x28 -> ThreeD
    | 0x6D -> nextc d; read_ws d; ThreeDM
    | _ -> err_malformed_number ~first_byte ~first_line d 
  )
  | _ -> err_malformed_number ~first_byte ~first_line d

let consume_geometry d =
  let geo =
    while is_text d.c do
      accept d
    done;
    token_pop d
  in
  read_ws d;
  let with_kind d fn =
    let kind = consume_kind d in
    read_ws d;
    let v = fn ~kind d in
    read_ws d;
    v
  in
  match Stdlib.String.lowercase_ascii geo with
  | "point" -> 
    with_kind d @@ fun ~kind d ->
    read_lparen d;
    let point = consume_point ~kind d in
    read_rparen d;
    make_point ~kind point
  | "linestring" ->
    with_kind d @@ fun ~kind d ->
    let ls = consume_linestring ~kind d in
    linestring ~kind ls
  | "polygon" ->
    with_kind d @@ fun ~kind d ->
    let p = consume_polygon d in
    polygon ~kind p
  | n -> Fmt.failwith "Unknown geometry %s" n

let decode ?filename r =
  let d = make_decoder ?filename r in
  nextc d;
  consume_geometry d
end
module Crs = struct
  type kind =
    | Projected
    | Geographic
    | Geocentric

  let pp_kind fmt = function
    | Projected -> Fmt.string fmt "Projected" 
    | Geographic -> Fmt.string fmt "Geographic" 
    | Geocentric -> Fmt.string fmt "Geocentric" 

  type t = { name : string; kind : kind }
  let name t = t.name
  let kind t = t.kind

  let pp fmt t =
    Fmt.pf fmt "%a %s" pp_kind t.kind t.name

  let push_delimiter d =
    let first_byte = get_last_byte d and first_line = get_line_pos d in
    if is_lparen d.c || is_lsquare d.c then begin
      Queue.push d.c d.delims;
      nextc d
    end
    else
      err_delimiter ~first_byte ~first_line d 

  let _pop_delimiter d =
    let first_byte = get_last_byte d and first_line = get_line_pos d in
    let c = Queue.pop d.delims in
    if (is_lparen c && is_rparen d.c) || (is_lsquare c && is_rparen d.c) then
      nextc d
    else
      err_delimiter ~first_byte ~first_line d

  let consume_word d =
    while is_text d.c do
      accept d
    done;
    token_pop d

  let finish d =
    while not (Int.equal d.c eot) do
      nextc d
    done

  let quoted_name d =
    readc 0x22 d;
    while not (is_quote d.c) do
      accept d
    done;
    token_pop d

  let consume_projcs = finish
  let consume_geogcs = finish
  let consume_geoccs = finish

  let consume_crs d =
    let kind = consume_word d in
    push_delimiter d;
    let name = quoted_name d in
    match kind with
    | "PROJCRS" -> 
      consume_projcs d;
      { name; kind = Projected }
    | "GEOGCS" -> 
      consume_geogcs d;
      { name; kind = Geographic }
    | "GEOCCS" -> 
      consume_geoccs d;
      { name; kind = Geocentric }
    | s -> Fmt.failwith "Unknown spatial reference system: %s" s

  let decode ?filename r =
    let d = make_decoder ?filename r in
    nextc d;
    consume_crs d
end
