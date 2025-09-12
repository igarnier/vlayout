(** The type of images. *)
type t

(** [create ~xpixels ~ypixels] is an image with prescribed pixel size. *)
val create : xpixels:int -> ypixels:int -> t

val xpixels : t -> int

val ypixels : t -> int

val pixels :
  t -> (int32, Bigarray.int32_elt, Bigarray.c_layout) Bigarray.Array1.t

val bbox : t -> Bbox.t

val get : t -> int -> int -> int32

val set : t -> int -> int -> int32 -> unit
