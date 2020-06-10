(** A pattern describes how to stroke or fill a surface. *)
type pattern =
  | Solid of { c : Color.t }
  | Linear of { p0 : Pt.t; p1 : Pt.t; stops : color_stops }
  | Radial of
      { c0 : Pt.t; r0 : float; c1 : Pt.t; r1 : float; stops : color_stops }

(** A path in Color.t space where each Color.t is weighted by a value in [0,1].
    As an example, [ (red, 0.0); (blue, 0.5); (red, 1.0) ] corresponds to
    a path that starts and finishes by red, going halfway through by blue.
    The intermediate values taken by the path depend on the kind of gradient
    algorithm used. *)
and color_stops = (Color.t * float) list

(** A [dash_pattern] specifies a dash (see Cairo doc). *)
type dash_pattern = float array

(** A style describes how to paint a surface enclosed by a path.
    The [stroke] field describes the enclosing path and is mandatory.
    [width] and [dash] optionally describe the width and the dash pattern
    of the stroke, while the optional [fill] field describes how the enclosed
    surface must be drawn. *)
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

(** Some useful patterns *)

val solid_stroke : clr:Color.t -> pattern

val solid_fill : clr:Color.t -> pattern

val with_dash : style:t -> dash:dash_pattern -> t

val dot_dash : dash_pattern

val small_dash : dash_pattern

val medium_dash : dash_pattern

val large_dash : dash_pattern

val vertical_gradient : path:color_stops -> pattern

val horizontal_gradient : path:color_stops -> pattern

val simple_vertical_gradient : clr1:Color.t -> clr2:Color.t -> pattern

val simple_horizontal_gradient : clr1:Color.t -> clr2:Color.t -> pattern

val pp_pattern : Format.formatter -> pattern -> unit

val pp : Format.formatter -> t -> unit

(** Predefined styles, for convenience. *)

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
