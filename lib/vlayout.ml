type command = Commands.t

(** The module type of the backends. *)

module Bbox = Bbox
module Pt = Pt
module Image = Image
module Color = Color
module Ctext = Ctext
module Style = Style

module Commands : sig
  (** The module type of drawing commands. *)

  type name = string

  type relative_position

  val absolute : relative_position

  val n : relative_position

  val w : relative_position

  val s : relative_position

  val e : relative_position

  val sw : relative_position

  val se : relative_position

  val nw : relative_position

  val ne : relative_position

  (** A [position] consists of an assignement of the coordinates [pos] to the
      symbolic position [relpos] of some bounding box. *)
  type position

  type hposition = [ `Hcentered | `Bottom | `Top ]

  type vposition = [ `Vcentered | `Left | `Right ]

  type framing =
    | Scale_to_frame of { frame : Bbox.t }
    | Preserve_aspect of { frame : Bbox.t }

  module Arrow : sig
    type t

    type style

    val default_style : style
  end

  (** The type of commands. *)
  type t = command

  (** [bbox cmd] is the bounding box of the command [cmd]. *)
  val bbox : t -> Bbox.t

  (** [circle center radius] is a circle with [center] and [radius]. *)
  val circle : Gg.p2 -> float -> t

  (** [box ~mins ~maxs] is a box with [mins] as bottom-left corner and [maxs] as
      the top-right corner. *)
  val box : mins:Gg.p2 -> maxs:Gg.p2 -> t

  (** [text ~size pos str] is some text at position [pos], with size [size] and
      string [str]. *)
  val text : size:float -> position -> string -> t

  (** [style sty cmd] is [cmd] on which the style [sty] is applied. *)
  val style : Style.t -> t -> t

  (** [segment p0 p1] is a segment from [p0] to [p1]. *)
  val segment : Gg.p2 -> Gg.p2 -> t

  (** [bezier ~p1 ~c1 ~p2 ~c2] is a bezier curve from [p1] to [p2] with control
      points [c1] and [c2]. *)
  val bezier : p1:Gg.p2 -> c1:Gg.p2 -> p2:Gg.p2 -> c2:Gg.p2 -> t

  (** [bezier ~p1 ~p2 ~radians] is a bezier curve from [p1] to [p2] with control
      angle [radians]. *)
  val ubezier : p1:Gg.p2 -> p2:Gg.p2 -> radians:float -> t

  (** [image ~pos img] is an image at position [pos]. *)
  val image : pos:Gg.p2 -> Image.t -> t

  (** [rotate ~radians cmd] is [cmd] rotated by angle [radians]. *)
  val rotate : radians:float -> t -> t

  (** [translate v cmd] is [cmd] translated by [v] *)
  val translate : Gg.v2 -> t -> t

  (** [scale ~xs ~ys cmd] is [cmd] scaled by the vector [(xs, ys)] *)
  val scale : xs:float -> ys:float -> t -> t

  (** [group cmds] is a group of commands. *)
  val group : t list -> t

  (** [hbox ~pos ~deltax layouts] automatically arranges the commands so that
      they are horizontally aligned, with [deltax] units between each layout
      bounding box, and using [pos] for alignement (defaults to [`Hcentered]).
  *)
  val hbox : dx:float -> ?pos:hposition -> t list -> t

  (** See [hbox]. *)
  val vbox : dy:float -> ?pos:vposition -> t list -> t

  (** [place pos cmd] places [cmd] at [pos]. *)
  val place : relative_position -> Gg.p2 -> t -> t

  (** [frame framing cmd] frames [cmd] in [framing]. *)
  val frame : framing -> t -> t

  (** [arrow ~style ~start ~finish] creates an arrow with [style] going from
      [start] to [finish]. *)
  val arrow : style:Arrow.style -> start:Gg.p2 -> finish:Gg.p2 -> t

  (** [arrow_curvy ~style ~start ~finish] creates an arrow with [style] going
      from [start] to [finish] whose initial tangent vector has angle [radians]
      with the line from [start] to [finish]. *)
  val arrow_curvy :
    Arrow.style -> start:Gg.p2 -> finish:Gg.p2 -> radians:float -> t

  val segmented_arrow : Arrow.style -> Gg.p2 list -> t

  val center_to_page : w:float -> h:float -> t -> t

  val anchor : string -> Gg.p2 -> t -> t

  val bind : name -> command -> (Gg.p2 list -> command) -> command

  val bind_each : name -> command -> (Gg.p2 -> command) -> command
end =
  Commands

module Backends = Backends
