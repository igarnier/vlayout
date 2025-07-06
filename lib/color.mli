(** rgb t; the values are meant to be in [0,1]. As an example, the t red
    corresponds to { r = 1.0; g = 0.0; b = 0.0 } and the t blue to
    { r = 0.0; g = 0.0; b = 1.0 } *)
type t = { r : float; g : float; b : float }

(** Make an rgb t. *)
val rgb : float -> float -> float -> t

(** Some predefined colors. *)
val red : t

val green : t

val blue : t

val black : t

val white : t

val gray : float -> t

val pink : t

val cyan : t

(** Enumerating colors. The 8 first colors of the enumeration are those above
    (except white), the one after that are obtained by sampling around the
    previous ones. *)
val enum_colors : unit -> t

(** Pack an rgb t into the 24 first bits of an int32. *)
val to_int : t -> int32

(** Pretty-printer. *)
val pp : Format.formatter -> t -> unit
