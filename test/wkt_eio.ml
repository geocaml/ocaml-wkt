(*---------------------------------------------------------------------------
   Copyright (c) 2022 Patrick Ferris <patrick@sirref.org>

   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open Eio.Std

let read_file ~sw dir path =
  let flow = Eio.Dir.open_in ~sw dir path in
  let buff = Cstruct.create 2048 in
  fun () ->
    try
      let got = Eio.Flow.read flow buff in
      let t = Some (Cstruct.to_bytes buff, 0, got) in
      t
    with End_of_file ->
      Eio.Flow.close flow;
      None

let write_file ~sw ~create ?append dir path =
  let flow = Eio.Dir.open_out ~sw ~create ?append dir path in
  fun t ->
    match t with
    | None -> Eio.Flow.close flow
    | Some (buff, off, len) ->
        Eio.Flow.(
          copy (cstruct_source [ Cstruct.of_bytes ~off ~len buff ]) flow)

let copy src dst =
  let rec copy' () =
    match Wkt.decode src with
    | `Lexeme (`Geometry _) as v ->
        Wkt.encode dst v;
        copy' ()
    | `Lexeme `Po as v ->
        Wkt.encode dst v;
        copy' ()
    | `Lexeme `Pc as v ->
        Wkt.encode dst v;
        copy' ()
    | `Lexeme (`Number _) as v ->
        Wkt.encode dst v;
        copy' ()
    | `Lexeme `Comma as v ->
        Wkt.encode dst v;
        copy' ()
    | `End as v -> Wkt.encode dst v
    | `Error err -> failwith err
  in
  copy' ()

let () =
  Eio_main.run @@ fun env ->
  Switch.run @@ fun sw ->
  let src = read_file ~sw (Eio.Stdenv.cwd env) Sys.argv.(1) in
  let dst =
    write_file ~sw ~create:(`Exclusive 0o666) (Eio.Stdenv.cwd env) Sys.argv.(2)
  in
  let decoder = Wkt.decoder src in
  let encoder = Wkt.encoder dst in
  copy decoder encoder

(*---------------------------------------------------------------------------
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