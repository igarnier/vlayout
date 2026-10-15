type t = Gg.Color.t

(** Make an rgb t. *)
val rgb : float -> float -> float -> t

(** Some predefined colors. *)
val red : t

val green : t

val blue : t

val black : t

val white : t

val pink : t

val cyan : t

val gray : float -> t

(** Enumerating colors. The first 6 colors of the enumeration are [red],
    [green], [blue], [black], [pink] and [cyan]; the following ones are obtained
    by randomly perturbing those (using the global [Random] state). *)
val enum_colors : unit -> t Seq.t

val r : t -> float

val g : t -> float

val b : t -> float

val unpack : t -> float * float * float

(** Pack an rgb t into the 24 first bits of an int32. *)
val to_int : t -> int32
