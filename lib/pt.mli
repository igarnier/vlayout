type t = Gg.v2

val zero : t

val pt : float -> float -> t

val x : t -> float

val y : t -> float

val add : t -> t -> t

val sub : t -> t -> t

val mul : t -> t -> t

val scale : t -> float -> t

val neg : t -> t

val norm : t -> float

val barycenter : t -> t -> t

val dot : t -> t -> float

val normalize : t -> t

val angle_of_vec : t * t -> float

val rotate_vector : float -> t -> t

val rotate_point_about : t -> float -> t -> t

val rotate_90_cw : t -> t

val rotate_90_ccw : t -> t

val cross : t -> t -> float

val pmin : t -> t -> t

val pmax : t -> t -> t

val ( + ) : t -> t -> t

val ( - ) : t -> t -> t

val ( *| ) : t -> float -> t

val ( |* ) : float -> t -> t

val ( ~- ) : t -> t

val pp : Format.formatter -> t -> unit
