(** The module type of drawing commands. *)

type name = string

(** A [position] consistsof an assignement of the coordinates [pos] to the
    symbolic position [relpos] of some bounding box. *)
type position = { pos : Pt.t; relpos : relpos }

and relpos =
  | Absolute
  | North
  | West
  | South
  | East
  | SouthWest
  | SouthEast
  | NorthWest
  | NorthEast

(** The type of commands. Best not used directly. *)
type t = { uid : int; desc : desc }

and desc =
  | Circle of { center : Pt.t; radius : float }
  | Box of { mins : Pt.t; maxs : Pt.t }
  | Text of { pos : position; text : Ctext.t }
  | Style of { style : Style.t; cmd : t }
  | Segment of { p1 : Pt.t; p2 : Pt.t }
  | Bezier of { p1 : Pt.t; c1 : Pt.t; p2 : Pt.t; c2 : Pt.t }
  | Image of { pos : Pt.t; image : Image.t }
  | DeclPt of { pt : Pt.t; name : name }
  | Rotate of { radians : float; cmd : t }
  | Translate of { v : Pt.t; cmd : t }
  | Scale of { xs : float; ys : float; cmd : t }
  | Wrap of t list

type alias = t

val text_position : position -> float -> float -> Pt.t

module Bbox : sig
  include Bbox.S

  val of_command : alias -> t

  val of_commands : alias list -> t
end

module Arrow : sig
  type style =
    { startp : float;  (** [0,1] *)
      endp : float;  (** [0,1] *)
      arrowp : float;  (** [0,1] *)
      legs : float;  (** legs length, >= 0 *)
      angle : float  (** legs angle *)
    }

  val default_style : style

  val mid_style : style

  val mkarrow : style:style -> start:Pt.t -> finish:Pt.t -> t list

  val mkarrow_curvy :
    style:style -> start:Pt.t -> finish:Pt.t -> angle:float -> t list

  val mk_multisegment_arrow : style:style -> points:Pt.t list -> t list
end

(** Draws a circle with [center] and [radius] parameters. *)
val circle : center:Pt.t -> radius:float -> t

(** Draws a box with [mins] as bottom-left corner and [maxs] as the top-right
    corner. *)
val box : mins:Pt.t -> maxs:Pt.t -> t

(** Draws text at position [pos], with size [float] and [text]. *)
val text : pos:position -> size:float -> text:string -> t

(** Applies a [style] to the given subcommand. *)
val style : style:Style.t -> t -> t

(** Draws a segment from [p1] to [p2] *)
val segment : p1:Pt.t -> p2:Pt.t -> t

(** Draws a Bezier curve from [p1] to [p2] with control points respectively [c1]
    and [c2]. *)
val bezier : p1:Pt.t -> c1:Pt.t -> p2:Pt.t -> c2:Pt.t -> t

(** Draws a Bezier curve with ingoing and outgoing angles specified by [angle].
*)
val ubezier : p1:Pt.t -> p2:Pt.t -> angle:float -> t

(** Draws an image. *)
val image : pos:Pt.t -> image:Image.t -> t

(** Declares an anchor named [name] at position [pt]. *)
val declpt : pt:Pt.t -> name:name -> t

(** Rotates the subcommand by [radians]. *)
val rotate : radians:float -> t -> t

(** Translates the subcommand by [v]. *)
val translate : v:Pt.t -> t -> t

(** Scales the x and y components of the subcommand by resp. [xs] and [ys]. *)
val scale : xs:float -> ys:float -> t -> t

(** Wraps a list of subcommands into one. *)
val wrap : t list -> t

(** Manual positioning of a the subcommand at the given position.. *)
val place : pos:position -> t -> t

(** Pretty-printing of commands *)
val pp : Format.formatter -> t -> unit

val center_to_page : float * float -> t -> t

module NameMap : Map.S with type key = name

val collect_declared_points : t -> Pt.t NameMap.t

(** Relative positioning of horizontal layout boxes. *)
type hposition = [ `Hcentered | `Bottom | `Top ]

(** Relative positioning of vertical layout boxes. *)
type vposition = [ `Vcentered | `Left | `Right ]

(** A [framing] rule specifies how a sublayout should be rescaled to fit in a
    given [frame]. *)
type framing =
  | Scale_to_frame of { frame : Bbox.t }
  | Preserve_aspect of { frame : Bbox.t }

type layout

(** Embeds a command as part of a layout. *)
val cmd : t -> layout

(** [hbox ~pos ~deltax layouts] automatically moves the sub-layout so that they
    are horizontally aligned, with [deltax] units between each layout bounding
    box, and using [pos] for alignement. *)
val hbox : ?pos:hposition -> ?deltax:float -> layout list -> layout

(** See [hbox]. *)
val vbox : ?pos:vposition -> ?deltay:float -> layout list -> layout

val arrow : start:name -> finish:name -> sty:Arrow.style -> layout -> layout

val frame : framing -> layout -> layout

val emit_commands_with_bbox : layout -> t * Bbox.t

val emit_commands : layout -> t
