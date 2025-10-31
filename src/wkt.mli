(*---------------------------------------------------------------------------
   Copyright (c) 2025 The wkt programmers.

   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)
open Bytesrw

(** {1 Well-known Text}

   A codec for the well-known text format (WKT) in OCaml. WKT can encode
   geometry objects like points and polygons (see {! Geometry}) and 
   coordinate reference systems (see {! Crs}).

   For decoding geometry objects you will want {! Geometry.decode}. 

   {@ocaml[
     open Bytesrw

     let read_and_print s =
       let reader = Bytes.Reader.of_string s in
       Fmt.pr "%a%!" Wkt.Geometry.pp (Wkt.Geometry.decode reader)
   ]}

   [read_and_print] is a simple function that reads a WKT object from a string
   and prints some information about it. If you wish to re-encode the WKT object
   then you should use {! encode}.

   {@ocaml[
     # read_and_print "point (20 20)";;
     point (Nx Info:
              Shape: [2]
              Dtype: float64
              Strides: [1]
              Offset: 0
              Size: 2
              Data: [20, 20]
             (2D))
     - : unit = ()
   ]}

   The shape of the data will differ depending on what kind of WKT objects you read.

   {@ocaml[
     # read_and_print "linestring zm (10 10 10 3.5, 20 20 20 1.2)";;
     linestring (Nx Info:
                   Shape: [2x4]
                   Dtype: float64
                   Strides: [4; 1]
                   Offset: 0
                   Size: 8
                   Data: [[10, 10, 10, 3.5],
                          [20, 20, 20, 1.2]]
                  (3D-m))
     - : unit = ()
   ]}
*)

module Geometry : sig

  type kind = TwoD | TwoDM | ThreeD | ThreeDM
  (** The kind of geometry e.g. [TwoD] is two-dimensional
      whereas [TwoDM] is two-dimensional with a measurement *)

  type 'geo with_kind

  type array = (float, Bigarray.float64_elt) Nx.t

  val kind : _ with_kind -> kind
  (** Extract the kind from a geometry *)

  type point
  (** A single point *)

  type linestring
  (** A list of points forming a line *)

  type polygon = linestring list
  (** A list of {! linestrings} forming a polygon *)

  type polyhedralsurface = polygon list
  (** A list of polygons forming a polyhedral surface *)

  type t = 
    | Point of point with_kind
    | LineString of linestring with_kind
    | Polygon of polygon with_kind
    | PolyhedralSurface of polyhedralsurface with_kind
    | MultiPoint of point list with_kind
    | MultiLineString of linestring list with_kind
    | MultiPolygon of polygon list with_kind
    | Collection of t list with_kind

  val make_point : ?kind:kind -> array -> t

  val point : point -> (float, Bigarray.float64_elt) Nx.t
  (** Underlying data for the point *)

  val points : linestring -> (float, Bigarray.float64_elt) Nx.t 
  (** The points of a given linestring *)

  val pp : t Fmt.t
  (** A pretty printer for wkt objects *)

  (** {2 Decoding} *)

  val decode : ?filename:string -> Bytes.Reader.t -> t
  (** [decode ?filename r] parses a WKT object from [r] *)
end

module Crs : sig
  type kind = Projected | Geographic | Geocentric

  type t
  (** A CRS representation *)

  val pp : t Fmt.t
  (** A pretty printer for the CRS *)

  val name : t -> string
  (** Name of the CRS *)

  val kind : t -> kind
  (** The kind of CRS it is *)

  val decode : ?filename:string -> Bytes.Reader.t -> t
  (** [decode ?filename r] parses a CRS object from [r] *)
end