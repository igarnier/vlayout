(** r/g/b/ color; the values are meant to be in [0,1]. As an example, the color red
    corresponds to { r = 1.0; g = 0.0; b = 0.0 } and the color blue to
    { r = 0.0; g = 0.0; b = 1.0 }
*)
type color = { r : float; g : float; b : float }

let r { r; _ } = r

let g { g; _ } = g

let b { b; _ } = b

(** Make an rgb color. *)
let rgb r g b = { r; g; b }

(** Some predefined colors. *)
let red = { r = 1.0; g = 0.0; b = 0.0 }

let green = { r = 0.0; g = 1.0; b = 0.0 }

let blue = { r = 0.0; g = 0.0; b = 1.0 }

let black = { r = 0.0; g = 0.0; b = 0.0 }

let white = { r = 1.0; g = 1.0; b = 1.0 }

let pink = rgb (255. /. 255.) (105. /. 255.) (180. /. 255.)

let cyan = rgb 0.0 (255. /. 255.) (255. /. 255.)

let signed_float f =
  let f = Random.float f in
  if Random.bool () then f else ~-.f

let clamp f = if f < 0.0 then ~-.f else if f > 1.0 then 1.0 -. f else f

let sample_around { r; g; b } =
  let rdelta = signed_float 0.5 in
  let gdelta = signed_float 0.5 in
  let bdelta = signed_float 0.5 in
  let r = clamp (r +. rdelta) in
  let g = clamp (g +. gdelta) in
  let b = clamp (b +. bdelta) in
  { r; g; b }

let enum_colors =
  let colors = [| red; green; blue; black; pink; cyan |] in
  let len = Array.length colors in
  let i = ref 0 in
  fun () ->
    if !i < len then (
      let res = colors.(!i) in
      incr i ;
      res )
    else
      let res = sample_around colors.(!i) in
      incr i ;
      res

let gray p =
  if p < 0.0 || p > 1.0 then invalid_arg "gray percentage must be in [0,1]"
  else { r = p; g = p; b = p }

let to_int { r; g; b } =
  let open Int32 in
  let r = of_float (r *. 255.0) in
  let g = of_float (g *. 255.0) in
  let b = of_float (b *. 255.0) in
  logor (logor (shift_left r 16) (shift_left g 8)) b

(** A path in color space where each color is weighted by a value in [0,1].
    As an example, [ (red, 0.0); (blue, 0.5); (red, 1.0) ] corresponds to
    a path that starts and finishes by red, going halfway through by blue.
    The intermediate values taken by the path depend on the kind of gradient
    algorithm used.
*)
type color_stops = (color * float) list

(** A [pattern] describes how to paint a (potentially 1d) surface. *)
type pattern =
  | Solid of { c : color }
  | Linear of { p0 : Pt.t; p1 : Pt.t; stops : color_stops }
  | Radial of
      { c0 : Pt.t; r0 : float; c1 : Pt.t; r1 : float; stops : color_stops }

(** A [dash_pattern] specifies a dash (see Cairo doc). *)
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

let pp_color fmtr { r; g; b } =
  Format.fprintf fmtr "{ r=%f; g = %f; b = %f }" r g b

let pp_comma fmtr () = Format.pp_print_string fmtr ","

let pp_pattern fmtr = function
  | Solid { c } -> Format.fprintf fmtr "Solid %a" pp_color c
  | Linear { p0; p1; stops } ->
      Format.fprintf
        fmtr
        "Linear { p0 = %a; p1 = %a; stops = %a }"
        Pt.pp
        p0
        Pt.pp
        p1
        (Format.pp_print_list ~pp_sep:pp_comma (fun fmtr (c, ofs) ->
             Format.fprintf fmtr "%a at %f" pp_color c ofs))
        stops
  | Radial { c0; r0; c1; r1; stops } ->
      Format.fprintf
        fmtr
        "Radial { c0 = %a; r0 = %f; c1 = %a; r1 = %f; stops = %a }"
        Pt.pp
        c0
        r0
        Pt.pp
        c1
        r1
        (Format.pp_print_list ~pp_sep:pp_comma (fun fmtr (c, ofs) ->
             Format.fprintf fmtr "%a at %f" pp_color c ofs))
        stops

let pp_dash_pattern fmtr dash_pattern =
  let patt = Array.to_list dash_pattern in
  Format.fprintf
    fmtr
    "Dash %a"
    (Format.pp_print_list
       ~pp_sep:(fun fmtr () -> Format.pp_print_string fmtr ",")
       Format.pp_print_float)
    patt

let pp fmtr { stroke; dash; fill; _ } =
  Format.fprintf
    fmtr
    "{ stroke = %a; dash = %a; fill = %a }"
    pp_pattern
    stroke
    (Format.pp_print_option
       ~none:(fun fmtr () -> Format.pp_print_string fmtr "None")
       pp_dash_pattern)
    dash
    (Format.pp_print_option
       ~none:(fun fmtr () -> Format.pp_print_string fmtr "None")
       pp_pattern)
    fill

(** Make a style from a stroke and a fill. *)
let make ~stroke ~width ~dash ~fill = { stroke; width; dash; fill }

(** Some predefined stokes and fills. *)
let solid_stroke ~clr = Solid { c = clr }

let solid_fill ~clr = Solid { c = clr }

let with_dash ~style ~dash = { style with dash = Some dash }

let dot_dash = [| 1.0; 5.0 |]

let small_dash = [| 5.0; 5.0 |]

let medium_dash = [| 10.0; 10.0 |]

let large_dash = [| 20.0; 20.0 |]

let vertical_gradient ~path =
  let p0 = Pt.pt 0.0 0.0 in
  let p1 = Pt.pt 0.0 1.0 in
  Linear { p0; p1; stops = path }

let horizontal_gradient ~path =
  let p0 = Pt.pt 0.0 0.0 in
  let p1 = Pt.pt 1.0 0.0 in
  Linear { p0; p1; stops = path }

let simple_vertical_gradient ~clr1 ~clr2 =
  let p0 = Pt.pt 0.0 0.0 in
  let p1 = Pt.pt 0.0 1.0 in
  let stops = [(clr1, 0.0); (clr2, 1.0)] in
  Linear { p0; p1; stops }

let simple_horizontal_gradient ~clr1 ~clr2 =
  let p0 = Pt.pt 0.0 0.0 in
  let p1 = Pt.pt 1.0 0.0 in
  let stops = [(clr1, 0.0); (clr2, 1.0)] in
  Linear { p0; p1; stops }

let solid clr =
  make ~stroke:(Solid { c = clr }) ~width:None ~fill:None ~dash:None

module Solid = struct
  let red = solid red

  let green = solid green

  let blue = solid blue

  let gray p = solid (gray p)

  let black = solid black

  let pink = solid pink

  let cyan = solid cyan
end

module DotDash = struct
  let red = with_dash ~style:Solid.red ~dash:dot_dash

  let green = with_dash ~style:Solid.green ~dash:dot_dash

  let blue = with_dash ~style:Solid.blue ~dash:dot_dash

  let gray p = with_dash ~style:(Solid.gray p) ~dash:dot_dash

  let black = with_dash ~style:Solid.black ~dash:dot_dash

  let pink = with_dash ~style:Solid.pink ~dash:dot_dash

  let cyan = with_dash ~style:Solid.cyan ~dash:dot_dash
end

module MediumDash = struct
  let red = with_dash ~style:Solid.red ~dash:medium_dash

  let green = with_dash ~style:Solid.green ~dash:medium_dash

  let blue = with_dash ~style:Solid.blue ~dash:medium_dash

  let gray p = with_dash ~style:(Solid.gray p) ~dash:medium_dash

  let black = with_dash ~style:Solid.black ~dash:medium_dash

  let pink = with_dash ~style:Solid.pink ~dash:medium_dash

  let cyan = with_dash ~style:Solid.cyan ~dash:medium_dash
end

module LargeDash = struct
  let red = with_dash ~style:Solid.red ~dash:large_dash

  let green = with_dash ~style:Solid.green ~dash:large_dash

  let blue = with_dash ~style:Solid.blue ~dash:large_dash

  let gray p = with_dash ~style:(Solid.gray p) ~dash:large_dash

  let black = with_dash ~style:Solid.black ~dash:large_dash

  let pink = with_dash ~style:Solid.pink ~dash:large_dash

  let cyan = with_dash ~style:Solid.cyan ~dash:large_dash
end
