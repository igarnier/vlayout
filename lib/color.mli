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

(** Enumerating colors. The 8 first colors of the enumeration are those above
    (except white), the one after that are obtained by sampling around the
    previous ones. *)
val enum_colors : t Seq.t

val unpack : t -> float * float * float

(** Pack an rgb t into the 24 first bits of an int32. *)
val to_int : t -> int32
