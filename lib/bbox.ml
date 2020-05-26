module type S = sig
  (** Type of 2d bounding boxes. *)
  type t = Gg.Box2.t

  (** [box p1 p2] creates a bounding box wih [p1] being the lower left corner and [p2] being the upper right corner. *)
  val box : Pt.t -> Pt.t -> t

  (** The empty bounding box. *)
  val empty : t

  (** Returns the center of a bounding box. *)
  val center : t -> Pt.t

  (** Returns the width of a bounding box. *)
  val width : t -> float

  (** Returns the height of a bounding box. *)
  val height : t -> float

  (** [join b1 b2] returns the smallest bounding box enclosing both [b1] and [b2]. *)
  val join : t -> t -> t

  (** [translate v b] translates the box [b] by the vector [v] *)
  val translate : Pt.t -> t -> t

  (** [scale x y b] scales the box [b] by the factors [x,y] *)
  val scale : float -> float -> t -> t

  (** Rotate bbox by given angle in radians and recompute enclosing bbox. *)
  val rotate : float -> t -> t

  (** Enlarges a box by given scale factors on the x and y axes. *)
  val enlarge : float -> float -> t -> t

  (** of_points l returns the smallest bounding box enclosing the list
      of points l *)
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
  val print : t -> string
end

open Gg

type t = Box2.t

let box = Box2.of_pts

let empty = Box2.empty

let center = Box2.mid

let width = Box2.w

let height = Box2.h

let join = Box2.union

let translate = Box2.move

let rotate radians bbox = Box2.ltr (M2.rot2 radians) bbox

let scale x y bbox = Box2.ltr (M2.scale2 (V2.v x y)) bbox

let enlarge sx sy bbox =
  let w = Box2.w bbox in
  let h = Box2.h bbox in
  let dx = 0.5 *. ((sx *. w) -. w) in
  let dy = 0.5 *. ((sy *. h) -. h) in
  let mins = Pt.(Box2.bl_pt bbox - pt dx dy) in
  let maxs = Pt.(Box2.tr_pt bbox + pt dx dy) in
  Box2.of_pts mins maxs

let of_points pt_list = List.fold_left Box2.add_pt Box2.empty pt_list

let of_points_arr pt_arr = Array.fold_left Box2.add_pt Box2.empty pt_arr

let n = Box2.tm_pt

let w = Box2.ml_pt

let s = Box2.bm_pt

let e = Box2.mr_pt

let se = Box2.br_pt

let sw = Box2.bl_pt

let ne = Box2.tr_pt

let nw = Box2.tl_pt

let print box =
  let mins = sw box and maxs = ne box in
  Printf.sprintf "{ mins = %s;  maxs = %s }" (Pt.print mins) (Pt.print maxs)
