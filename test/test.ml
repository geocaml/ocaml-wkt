open Bytesrw

let print filename =
  In_channel.with_open_bin filename @@ fun ic ->
  In_channel.fold_lines (fun () l ->
    let reader = Bytes.Reader.of_string l in 
    let wkt = Wkt.Geometry.decode reader in
    Fmt.pr "%a\n%!" Wkt.Geometry.pp wkt
  ) () ic

let print_crs filename =
  In_channel.with_open_bin filename @@ fun ic ->
  let reader = Bytes.Reader.of_in_channel ic in
  let crs = Wkt.Crs.decode ~filename reader in
  Fmt.pr "%a\n%!" Wkt.Crs.pp crs

let () =
  print "test.wkt";
  print_crs "./EPSG-CRS-3300.wkt"
