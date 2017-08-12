type t = { mins : Pt.t; maxs : Pt.t; }
val box : Pt.t -> Pt.t -> t
val empty : t
val center : t -> Pt.t
val width : t -> float
val height : t -> float
val join : t -> t -> t
val translate : Pt.t -> t -> t
val of_points : Pt.t list -> t
val se : t -> Pt.t
val sw : t -> Pt.t
val ne : t -> Pt.t
val nw : t -> Pt.t
val print : t -> string
