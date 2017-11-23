module Log = Log.Make(struct let section = "Style" end)

open Batteries

(** r/g/b/ color; the values are meant to be in [0,1]. As an example, the color red
    corresponds to { r = 1.0; g = 0.0; b = 0.0 } and the color blue to
    { r = 0.0; g = 0.0; b = 1.0 }
*)
type color = { r : float; g : float; b : float }

let r { r } = r
let g { g } = g
let b { b } = b

(** Make an rgb color. *)
let rgb r g b = { r; g; b }

(** Some predefined colors. *)
let red   = { r = 1.0; g = 0.0; b = 0.0 }
let green = { r = 0.0; g = 1.0; b = 0.0 }
let blue  = { r = 0.0; g = 0.0; b = 1.0 }
let black = { r = 0.0; g = 0.0; b = 0.0 }
let white = { r = 1.0; g = 1.0; b = 1.0 }
              
let gray p =
  if p < 0.0 || p > 1.0 then
    (Log.error "gray percentage must be in [0,1]";
     exit 0)
  else
    { r = p; g = p; b = p }

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
  | Solid of  { c : color }
  | Linear of { p0 : Pt.t; p1 : Pt.t; stops : color_stops }
  | Radial of { c0 : Pt.t; r0 : float; c1 : Pt.t; r1 : float; stops : color_stops }

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
  {
    stroke : pattern;
    width  : float option;
    dash   : dash_pattern option;
    fill   : pattern option
  }

(** String description of a color. *)
let print_color { r; g; b } =
  Printf.sprintf "{ r=%f; g = %f; b = %f }" r g b

(** String description of a pattern. *)
let print_pattern = function
  | Solid { c } ->
     Printf.sprintf "Solid %s" (print_color c)
  | Linear { p0; p1; stops } ->
     let p0_s = Pt.print p0
     and p1_s = Pt.print p1
     and st_s = Tools.to_sseq (fun (c,ofs) -> Printf.sprintf "%s at %f" (print_color c) ofs) "," stops
     in
     Printf.sprintf "Linear { p0 = %s; p1 = %s; stops = %s }" p0_s p1_s st_s
  | Radial { c0; r0; c1; r1; stops } ->
     let c0_s = Pt.print c0
     and c1_s = Pt.print c1
     and st_s = Tools.to_sseq (fun (c,ofs) -> Printf.sprintf "%s at %f" (print_color c) ofs) "," stops
     in
     Printf.sprintf "Radial { c0 = %s; r0 = %f; c1 = %s; r1 = %f; stops = %s }" c0_s r0 c1_s r1 st_s

let print_dash_pattern dash_pattern =
  let patt = Array.to_list dash_pattern in
  let s = Tools.to_sseq string_of_float "," patt in
  Printf.sprintf "Dash %s" s


(** String description of a style. *)                    
let print { stroke; dash; fill } =
  let stroke_s = print_pattern stroke in
  let dash_s =
    match dash with
    | None -> "None"
    | Some patt ->
       print_dash_pattern patt
  in   
  let fill_s =
    match fill with
    | None -> "None"
    | Some patt ->
       print_pattern patt
  in
  Printf.sprintf "{ stroke = %s; dash = %s; fill = %s }" stroke_s dash_s fill_s


              
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
  let stops = [ (clr1, 0.0); (clr2, 1.0) ] in
  Linear { p0; p1; stops }

let simple_horizontal_gradient ~clr1 ~clr2 =
  let p0 = Pt.pt 0.0 0.0 in
  let p1 = Pt.pt 1.0 0.0 in
  let stops = [ (clr1, 0.0); (clr2, 1.0) ] in
  Linear { p0; p1; stops }
