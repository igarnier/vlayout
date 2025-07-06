(** The type of images. *)
type t

(** [create ~xsize ~ysize] is an image with prescribed size. *)
val create : xsize:int -> ysize:int -> t

val xsize : t -> int

val ysize : t -> int

val pixels :
  t -> (int32, Bigarray.int32_elt, Bigarray.c_layout) Bigarray.Array1.t

val bbox : t -> Bbox.t

val get : t -> int -> int -> int32

val set : t -> int -> int -> int32 -> unit
