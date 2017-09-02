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

(** Pack an rgb color into the 24 first bits of an int32. *)
val to_int : color -> int32

(** A path in color space of the form [(c_1,f_1); ...; (c_k,f_k)], where
    the f_i are required to be in [0,1] and be monotonically increasing. *)
type color_stops = (color * float) list

(** A pattern describes how to stroke or fill a surface. *)                                  
type pattern =
  | Solid of  { c : color }
  | Linear of { p0 : Pt.t; p1 : Pt.t; stops : color_stops }
  | Radial of { c0 : Pt.t; r0 : float; c1 : Pt.t; r1 : float; stops : color_stops }

(* A style describes the stroke and fill pattern that can be used
   to draw all commands. *)
type t =
  {
    stroke : pattern;
    fill   : pattern option
  }

val make : stroke:pattern -> fill:(pattern option) -> t

val solid_stroke : clr:color -> pattern

val solid_fill : clr:color -> pattern

val vertical_gradient : path:color_stops -> pattern

val horizontal_gradient : path:color_stops -> pattern

val simple_vertical_gradient : clr1:color -> clr2:color -> pattern

val simple_horizontal_gradient : clr1:color -> clr2:color -> pattern


val print_color : color -> string

val print_pattern : pattern -> string

val print : t -> string
                                                               
