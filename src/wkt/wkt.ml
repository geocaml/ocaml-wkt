(*---------------------------------------------------------------------------
   Copyright (c) 2012 The jsonm programmers. All rights reserved.
   Copyright (c) 2022 Patrick Ferris <patrick@sirref.org>

   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

let io_buffer_size = 65535

type src = unit -> (bytes * int * int) option

(* The well-known text format consists of geometry kinds represented as a
   string (e.g. MULTIPOINT), opening parenthesis, closing parenthesis, commas
   and numbers. *)
type lexeme = [ `Comma | `Number of float | `Po | `Pc | `Geometry of string ]

(* Character checkers *)
let ux_eoi = max_int (* End of input, outside unicode range. *)

let ux_soi = max_int - 1 (* Start of input, outside unicode range. *)

let u_lpar = 0x28 (* '(' character. *)

let u_rpar = 0x29 (* ')' character. *)

let _u_dot = 0x2E (* . *)

let _u_plus = 0x2B (* + *)

let u_comma = 0x2C (* , *)

let _u_minus = 0x2D (* - *)

let is_white = function 0x20 | 0x09 | 0x0D | 0x0A -> true | _ -> false
let is_atom_char c = (0x41 <= c && c <= 0x5A) || (0x61 <= c && c <= 0x7A)
let is_digit u = 0x30 <= u && u <= 0x39

let is_val_sep = function
  (* N.B. Uutf normalizes U+000D to U+000A. *)
  | 0x20 | 0x09 | 0x0A | 0x2C | 0x5D | 0x7D -> true
  | _ -> false

type decoder = {
  src : src; (* Input source. *)
  mutable i : bytes; (* Current input chunk. *)
  mutable i_pos : int; (* Next input position to read. *)
  mutable i_max : int; (* Maximal input position to read. *)
  buff : Buffer.t; (* Buffer for chars. *)
  mutable c : int; (* Character lookahead. *)
  mutable nest : int; (* Parenthesis nesting. *)
}

let decoder src =
  {
    src;
    i = Bytes.empty;
    i_pos = max_int;
    i_max = 0;
    buff = Buffer.create 256;
    c = ux_soi;
    nest = 0;
  }

let eoi d =
  d.i <- Bytes.empty;
  d.i_pos <- max_int;
  d.i_max <- 0;
  d.c <- ux_eoi

let refill d =
  match d.src () with
  | None -> eoi d
  | Some (s, pos, len) ->
      d.i <- s;
      d.i_pos <- pos;
      d.i_max <- pos + len - 1

let rec readc d =
  if d.i_pos > d.i_max then
    if d.c = ux_eoi then ()
    else (
      refill d;
      readc d)
  else (
    d.c <- Char.code (Bytes.unsafe_get d.i d.i_pos);
    d.i_pos <- d.i_pos + 1)

let badd d = Buffer.add_char d.buff (Char.chr d.c)

let buff d =
  let a = Buffer.contents d.buff in
  Buffer.clear d.buff;
  a

let error d =
  d.nest <- 0 (* reset *);
  `Error "Nesting"

(* Reading numbers -- TODO: check + and - *)
let rec r_float d =
  if (not (is_val_sep d.c || d.c = u_rpar)) && d.c <> ux_eoi then (
    badd d;
    readc d;
    r_float d)
  else
    let s = buff d in
    try `Lexeme (`Number (float_of_string s))
    with Failure _ -> `Error ("number parsing failed: " ^ s)

let r_white d =
  while is_white d.c do
    readc d
  done

let r_end d = if d.nest = 0 then `End else error d

let r_po d =
  d.nest <- d.nest + 1;
  readc d;
  `Lexeme `Po

let r_pc d =
  d.nest <- d.nest - 1;
  readc d;
  if d.nest < 0 then error d else `Lexeme `Pc

let r_comma d =
  readc d;
  `Lexeme `Comma

let r_geometry d =
  while is_atom_char d.c do
    badd d;
    readc d
  done;
  `Lexeme (`Geometry (buff d))

let rec r_lexeme d =
  if is_white d.c then (
    r_white d;
    r_lexeme d)
  else if is_atom_char d.c then r_geometry d
  else if is_digit d.c then r_float d
  else if d.c = u_lpar then r_po d
  else if d.c = u_rpar then r_pc d
  else if d.c = u_comma then r_comma d
  else if d.c = ux_eoi then r_end d
  else if d.c = ux_soi then (
    readc d;
    r_lexeme d)
  else (
    ignore @@ failwith (String.make 1 (Char.chr d.c));
    `Error "Lexeme error")

let decode = r_lexeme

(* Encoding *)

type dst = (bytes * int * int) option -> unit

type encoder = {
  dst : dst; (* Output destination. *)
  buff : Buffer.t; (* Scratch buffer. *)
  mutable o : bytes; (* Current output chunk. *)
  mutable o_pos : int; (* Next output position to write. *)
  mutable o_max : int; (* Maximal output position to write. *)
  mutable nest : int; (* Parenthesis nesting. *)
}

let encoder ?(buf = Bytes.create io_buffer_size) dst =
  let o_max = Bytes.length buf - 1 in
  if o_max = 0 then invalid_arg "buf's length is empty"
  else { dst; o = buf; buff = Buffer.create 128; o_pos = 0; o_max; nest = 0 }

let flush e ~stop =
  if stop then (
    if e.o_pos <> 0 then e.dst (Some (e.o, 0, e.o_pos));
    e.dst None)
  else e.dst (Some (e.o, 0, e.o_pos));
  e.o_pos <- 0

let rec writec e c =
  if e.o_pos > e.o_max then (
    flush e ~stop:false;
    writec e c)
  else (
    Bytes.unsafe_set e.o e.o_pos c;
    e.o_pos <- e.o_pos + 1)

let rec writes e s j l =
  let rem = e.o_max - e.o_pos + 1 in
  let len = if l > rem then rem else l in
  String.unsafe_blit s j e.o e.o_pos len;
  e.o_pos <- e.o_pos + len;
  if len < l then (
    flush e ~stop:false;
    writes e s (j + len) (l - len))

let invalid_seq () = invalid_arg "non well-formed sequence"

let encode e v =
  match v with
  | `End -> if e.nest > 0 then invalid_seq () else flush e ~stop:true
  | `Lexeme l -> (
      match l with
      | `Pc when e.nest = 0 -> invalid_seq ()
      | `Comma -> writec e ','
      | `Pc ->
          e.nest <- e.nest - 1;
          writec e ')'
      | `Po ->
          e.nest <- e.nest + 1;
          writec e '('
      | `Number f ->
          Buffer.clear e.buff;
          Printf.bprintf e.buff "%.16g " f;
          writes e (Buffer.contents e.buff) 0 (Buffer.length e.buff)
      | `Geometry g ->
          let gl = String.length g in
          if gl = 0 then invalid_arg "empty geometry name" else writes e g 0 gl)

(*---------------------------------------------------------------------------
   Copyright (c) 2012 The jsonm programmers
   Copyright (c) 2022 Patrick Ferris <patrick@sirref.org>

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
  ---------------------------------------------------------------------------*)