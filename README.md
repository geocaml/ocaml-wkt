ocaml-wkt
---------

*Status: Experimental and WIP*

A pure OCaml, non-blocking codec for the [Well-known Text](https://en.wikipedia.org/wiki/Well-known_text_representation_of_geometry) representation of Geometry. The library is heavily inspired by [jsonm](https://erratique.ch/software/jsonm) and [nbcodec](https://erratique.ch/repos/nbcodec).

Note that the current interface only provides a means to have a non-blocking codec via effects in Multicore OCaml (and keep it direct-style). See the `test/wkt_eio.ml` for an example using [Eio](https://github.com/ocaml-multicore/eio).