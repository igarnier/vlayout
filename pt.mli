type t = { x : float; y : float; }
val zero : t
val pt : float -> float -> t
val plus : t -> t -> t
val minus : t -> t -> t
val scale : t -> float -> t
val neg : t -> t
val norm : t -> float
val barycenter : t -> t -> t
val dot : t -> t -> float
val normalize : t -> t
val print : t -> string
val angle_of_vec : t * t -> float
val rotate_vector : float -> t -> t
val rotate_point_about : t -> float -> t -> t
val rotate_90_cw : t -> t
val rotate_90_ccw : t -> t
val cross : t -> t -> float
val pmin : t -> t -> t
val pmax : t -> t -> t
