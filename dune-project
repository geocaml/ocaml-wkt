(lang dune 2.9)

(name wkt)

(generate_opam_files true)

(source
 (github geocaml/ocaml-wkt))

(license MIT)

(authors "Patrick Ferris")

(maintainers "pf341@patricoferris.com")

(package
 (name wkt)
 (synopsis "Pure OCaml, non-blocking well-known text (WKT) parser")
 (description "TODO")
 (depends
   (eio_main (and (>= "0.2") :with-test))))
