(** r/g/b/ color; the values are meant to be in [0,1]. As an example, the color red
    corresponds to { r = 1.0; g = 0.0; b = 0.0 } and the color blue to
    { r = 0.0; g = 0.0; b = 1.0 }
*)
type color = { r : float; g : float; b : float }

val r : color -> float

val g : color -> float

val b : color -> float

(** Make an rgb color. *)
val rgb : float -> float -> float -> color

(** Some predefined colors. *)
val red : color

val green : color

val blue : color

val black : color

val white : color

val gray : float -> color

val pink : color

val cyan : color

(** Enumerating colors. The 8 first colors of the enumeration are those above (except white), the one after
    that are obtained by sampling around the previous ones. *)
val enum_colors : unit -> color

(** Pack an rgb color into the 24 first bits of an int32. *)
val to_int : color -> int32

(** A path in color space of the form [(c_1,f_1); ...; (c_k,f_k)], where
    the f_i are required to be in [0,1] and be monotonically increasing. *)
type color_stops = (color * float) list

(** A pattern describes how to stroke or fill a surface. *)
type pattern =
  | Solid of { c : color }
  | Linear of { p0 : Pt.t; p1 : Pt.t; stops : color_stops }
  | Radial of
      { c0 : Pt.t; r0 : float; c1 : Pt.t; r1 : float; stops : color_stops }

type dash_pattern = float array

(** A style describes how to paint a surface enclosed by a path.
    The [stroke] field describes the enclosing path and is mandatory.
    [width] and [dash] optionally describe the width and the dash pattern
    of the stroke,
    while the optional [fill] field describes how the enclosed surface
    must be drawn.
 *)
type t =
  { stroke : pattern;
    width : float option;
    dash : dash_pattern option;
    fill : pattern option
  }

val make :
  stroke:pattern ->
  width:float option ->
  dash:dash_pattern option ->
  fill:pattern option ->
  t

val solid_stroke : clr:color -> pattern

val solid_fill : clr:color -> pattern

val with_dash : style:t -> dash:dash_pattern -> t

val dot_dash : dash_pattern

val small_dash : dash_pattern

val medium_dash : dash_pattern

val large_dash : dash_pattern

val vertical_gradient : path:color_stops -> pattern

val horizontal_gradient : path:color_stops -> pattern

val simple_vertical_gradient : clr1:color -> clr2:color -> pattern

val simple_horizontal_gradient : clr1:color -> clr2:color -> pattern

val pp_color : Format.formatter -> color -> unit

val pp_pattern : Format.formatter -> pattern -> unit

val pp : Format.formatter -> t -> unit

(* Predefined styles, for convenience *)

module Solid : sig
  val red : t

  val green : t

  val blue : t

  val gray : float -> t

  val black : t

  val pink : t

  val cyan : t
end

module DotDash : sig
  val red : t

  val green : t

  val blue : t

  val gray : float -> t

  val black : t

  val pink : t

  val cyan : t
end

module MediumDash : sig
  val red : t

  val green : t

  val blue : t

  val gray : float -> t

  val black : t

  val pink : t

  val cyan : t
end

module LargeDash : sig
  val red : t

  val green : t

  val blue : t

  val gray : float -> t

  val black : t

  val pink : t

  val cyan : t
end
