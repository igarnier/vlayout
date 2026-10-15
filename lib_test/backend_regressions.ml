open Vlayout
module C = Commands

let failures = ref 0

let check name cond detail =
  if cond then Format.printf "PASS: %s@." name
  else (
    incr failures ;
    Format.printf "FAIL: %s (%s)@." name detail)

(* Render [cmd] on a white 100x100 RGB surface. *)
let render cmd =
  let surface = Cairo.Image.create Cairo.Image.RGB24 ~w:100 ~h:100 in
  let ctx = Cairo.create surface in
  Cairo.set_source_rgb ctx 1. 1. 1. ;
  Cairo.paint ctx ;
  Backends.Cairo.render ctx cmd ;
  Cairo.Surface.flush surface ;
  surface

let pixel surface x y =
  let v = (Cairo.Image.get_data32 surface).{y, x} in
  let c shift = Int32.(to_int (logand (shift_right v shift) 0xffl)) in
  (c 16, c 8, c 0)

let pp_rgb (r, g, b) = Printf.sprintf "(%d,%d,%d)" r g b

let style ?(stroke = Color.black) ?width ?fill () =
  Style.make ~stroke:(Style.solid stroke) ~width ~dash:None ~fill

(* Non-square images must render, with pixel (x, y) at (x, y). *)
let () =
  let image = Image.create ~xpixels:20 ~ypixels:10 in
  Bigarray.Array1.fill (Image.pixels image) (Color.to_int Color.white) ;
  Image.set image 19 0 (Color.to_int Color.red) ;
  match render (C.image ~pos:Pt.zero image) with
  | s ->
      check
        "non-square image pixel placement"
        (pixel s 19 0 = (255, 0, 0) && pixel s 0 19 = (255, 255, 255))
        (Printf.sprintf
           "pixel (19,0) = %s, pixel (0,19) = %s"
           (pp_rgb (pixel s 19 0))
           (pp_rgb (pixel s 0 19)))
  | exception exn ->
      check "non-square image renders" false (Printexc.to_string exn)

(* A gradient fill must follow the filled shape, whether the style is applied
   inside or outside a translation. *)
let () =
  let gradient =
    Style.simple_horizontal_gradient ~clr1:Color.red ~clr2:Color.blue
  in
  let sty = style ~width:0.0 ~fill:gradient () in
  let box = C.box ~mins:Pt.zero ~maxs:(Pt.pt 40.0 40.0) in
  let v = Pt.pt 50.0 50.0 in
  let test name cmd =
    let s = render cmd in
    let left = pixel s 52 70 and right = pixel s 88 70 in
    let (lr, _, lb) = left and (rr, _, rb) = right in
    check
      ("gradient: " ^ name)
      (lr > 200 && lb < 50 && rr < 50 && rb > 200)
      (Printf.sprintf
         "left %s, right %s; expected red then blue"
         (pp_rgb left)
         (pp_rgb right))
  in
  test "style outside translate" (C.translate v (C.style sty box)) ;
  test "style inside translate" (C.style sty (C.translate v box))

(* Stroke width must not depend on rotation. *)
let () =
  let sty = style ~width:4.0 () in
  let thickness_on_row y s =
    let n = ref 0 in
    for x = 0 to 99 do
      let (r, _, _) = pixel s x y in
      if r < 128 then incr n
    done ;
    !n
  in
  let vertical = C.style sty (C.segment Pt.zero (Pt.pt 0.0 60.0)) in
  let horizontal = C.style sty (C.segment Pt.zero (Pt.pt 60.0 0.0)) in
  let t_ref =
    thickness_on_row 50 (render (C.translate (Pt.pt 50.0 20.0) vertical))
  in
  let t_rot =
    thickness_on_row
      50
      (render
         (C.translate
            (Pt.pt 50.0 20.0)
            (C.rotate ~radians:(Float.pi /. 2.0) horizontal)))
  in
  check
    "stroke width invariant under rotation"
    (t_ref = t_rot)
    (Printf.sprintf
       "vertical segment: %dpx, horizontal rotated by 90deg: %dpx"
       t_ref
       t_rot)

(* With both stroke and fill, the full stroke width must be visible: a width-10
   stroke centered on the box edge at x=20 covers x in [15, 25). *)
let () =
  let sty = style ~stroke:Color.red ~width:10.0 ~fill:(Style.solid Color.blue) () in
  let s = render (C.style sty (C.box ~mins:(Pt.pt 20. 20.) ~maxs:(Pt.pt 80. 80.))) in
  let red = ref 0 in
  for x = 10 to 30 do
    if pixel s x 50 = (255, 0, 0) then incr red
  done ;
  check
    "filled box shows full stroke width"
    (!red = 10)
    (Printf.sprintf "%d red pixels across the border, expected 10" !red)

(* Fonts with identical specs must be shared. *)
let () =
  let a = Ctext.create "x" and b = Ctext.create "y" in
  check
    "font cache reuses scaled fonts"
    (a.Ctext.font == b.Ctext.font)
    "two Ctext.create calls produced distinct fonts"

let () = if !failures > 0 then exit 1
