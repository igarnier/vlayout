(** Axis-aligned bounding boxes. *)

module type S = sig
  (** Type of 2d bounding boxes. *)
  type t = Gg.Box2.t

  (** [box p1 p2] creates a bounding box wih [p1] being the lower left corner
      and [p2] being the upper right corner. *)
  val box : Pt.t -> Pt.t -> t

  (** The empty bounding box. *)
  val empty : t

  (** Returns the center of a bounding box. *)
  val center : t -> Pt.t

  (** Returns the width of a bounding box. *)
  val width : t -> float

  (** Returns the height of a bounding box. *)
  val height : t -> float

  (** [join b1 b2] returns the smallest bounding box enclosing both [b1] and
      [b2]. *)
  val join : t -> t -> t

  (** [translate v b] translates the box [b] by the vector [v] *)
  val translate : Pt.t -> t -> t

  (** [scale x y b] scales the box [b] by the factors [x,y] *)
  val scale : float -> float -> t -> t

  (** Rotate bbox by given angle in radians and recompute enclosing bbox. *)
  val rotate : float -> t -> t

  (** Enlarges (or shrinks) a box by given scale factors on the x and y axes. *)
  val enlarge : float -> float -> t -> t

  (** of_points l returns the smallest bounding box enclosing the list of points
      l *)
  val of_points : Pt.t list -> t

  (** Same as [of_points], taking an array instead of a list *)
  val of_points_arr : Pt.t array -> t

  (** The corners of a box b = { mins; maxs } can be accessed through
      the functions that follow, with the following convention:
      nw -n- ne
      |       |
      w       e
      |       |
      sw -s- se
  *)

  (** North point of a box. *)
  val n : t -> Pt.t

  (** North-west corner of a box. *)
  val nw : t -> Pt.t

  (** West point of a box. *)
  val w : t -> Pt.t

  (** South-west corner of a box. *)
  val sw : t -> Pt.t

  (** South point of a box. *)
  val s : t -> Pt.t

  (** South-east corner of a box. *)
  val se : t -> Pt.t

  (** East point of a box. *)
  val e : t -> Pt.t

  (** North-east corner of a box. *)
  val ne : t -> Pt.t

  (** Prints a bounding box. *)
  val pp : Format.formatter -> t -> unit
end

include S
