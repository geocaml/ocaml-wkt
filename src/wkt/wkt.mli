(*---------------------------------------------------------------------------
   Copyright (c) 2012 The jsonm programmers. All rights reserved.
   Copyright (c) 2022 Patrick Ferris <patrick@sirref.org>

   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

type lexeme = [ `Comma | `Number of float | `Po | `Pc | `Geometry of string ]
(** The type for well-known text lexemes. *)

(* {1 Decoding} *)

type src = unit -> (bytes * int * int) option
(** A source is a function which provides input data to the decoder. 
    It returns [None] at the end of the input. For this to be non-blocking,
    you should use an asynchronous IO library like Eio (see the tests). *)

type decoder
(** A decoder *)

val decoder : src -> decoder
(** [decoder src] creates a fresh decoder using the [src] function to provide
    input data to the decoder. *)

val decode : decoder -> [ `Lexeme of lexeme | `Error of string | `End ]
(** [decode decoder] provides the next {! lexeme}, an error or marks the end
    of the data. *)

type dst = (bytes * int * int) option -> unit
(** A destination is a function which provides a means for the encoder 
    to output data to a destination. Giving the function [None] represents 
    the end of the output data. *)

(** {1 Encoding} *)

type encoder
(** An encoder *)

val encoder : ?buf:bytes -> dst -> encoder
(** [decoder src] creates a fresh decoder using the [src] function to provide
    input data to the decoder. *)

val encode : encoder -> [ `Lexeme of lexeme | `End ] -> unit
(** [encode encoder] outputs a {! lexeme} or the end of the data to the destination
    associated with the [encoder]. *)

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