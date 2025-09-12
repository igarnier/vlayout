type pattern =
  | Solid of { c : Color.t }
  | Linear of { p0 : Pt.t; p1 : Pt.t; stops : color_stops }
  | Radial of { c0 : Pt.t; c1 : Pt.t; r1 : float; stops : color_stops }

and color_stops = Gg.Color.stops

type dash_pattern = float array

type t =
  { stroke : pattern;
    width : float option;
    dash : dash_pattern option;
    fill : pattern option
  }

let pp_comma fmtr () = Format.pp_print_string fmtr ","

let pp_pattern fmtr = function
  | Solid { c } -> Format.fprintf fmtr "Solid %a" Gg.V4.pp c
  | Linear { p0; p1; stops } ->
      Format.fprintf
        fmtr
        "@[Linear { p0 = %a; p1 = %a; stops = %a }@]"
        Pt.pp
        p0
        Pt.pp
        p1
        (Format.pp_print_list ~pp_sep:pp_comma (fun fmtr (ofs, c) ->
             Format.fprintf fmtr "%a at %f" Gg.V4.pp c ofs))
        stops
  | Radial { c0; c1; r1; stops } ->
      Format.fprintf
        fmtr
        "@[Radial { c0 = %a; c1 = %a; r1 = %f; stops = %a }@]"
        Pt.pp
        c0
        Pt.pp
        c1
        r1
        (Format.pp_print_list ~pp_sep:pp_comma (fun fmtr (ofs, c) ->
             Format.fprintf fmtr "%a at %f" Gg.V4.pp c ofs))
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
let solid c = Solid { c }

let linear ~p0 ~p1 stops = Linear { p0; p1; stops }

let radial ~c0 ~c1 r1 stops = Radial { c0; c1; r1; stops }

let with_dash dash style = { style with dash = Some dash }

let with_stroke stroke style = { style with stroke }

let with_fill fill style = { style with fill = Some fill }

let with_width width style = { style with width = Some width }

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
  let stops = [(0.0, clr1); (1.0, clr2)] in
  Linear { p0; p1; stops }

let simple_horizontal_gradient ~clr1 ~clr2 =
  let p0 = Pt.pt 0.0 0.0 in
  let p1 = Pt.pt 1.0 0.0 in
  let stops = [(0.0, clr1); (1.0, clr2)] in
  Linear { p0; p1; stops }

module Solid = struct
  open Color

  let solid clr =
    make ~stroke:(Solid { c = clr }) ~width:None ~fill:None ~dash:None

  let red = solid red

  let green = solid green

  let blue = solid blue

  let gray p = solid (Gg.Color.gray p)

  let black = solid black

  let pink = solid pink

  let cyan = solid cyan
end

module Make_dash (X : sig
  val dash : dash_pattern
end) =
struct
  open X

  let red = with_dash dash Solid.red

  let green = with_dash dash Solid.green

  let blue = with_dash dash Solid.blue

  let gray p = with_dash dash (Solid.gray p)

  let black = with_dash dash Solid.black

  let pink = with_dash dash Solid.pink

  let cyan = with_dash dash Solid.cyan
end

module DotDash = Make_dash (struct
  let dash = dot_dash
end)

module MediumDash = Make_dash (struct
  let dash = medium_dash
end)

module LargeDash = Make_dash (struct
  let dash = large_dash
end)
