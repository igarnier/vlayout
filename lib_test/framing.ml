open Vlayout
module C = Commands
module B = Backends.Cairo

let process_layout xmargin ymargin layout =
  let bbox = C.bbox layout in
  let w = xmargin +. Bbox.width bbox and h = ymargin +. Bbox.height bbox in
  (C.center_to_page ~w ~h layout, w, h)

let display_pdf filename layout =
  let (layout, w, h) = process_layout 10.0 10.0 layout in
  (* TODO: make margins a parameter *)
  let cairo_surface = Cairo.PDF.create filename ~w ~h in
  let ctx = Cairo.create cairo_surface in
  let _ =
    Cairo.set_matrix
      ctx
      Cairo.{ xx = 1.0; yx = 0.0; xy = 0.0; yy = ~-.1.0; x0 = 0.0; y0 = h } ;
    Cairo.set_line_width ctx 1.0 ;
    Cairo.select_font_face
      ctx
      "DejaVuSansMono"
      ~slant:Cairo.Upright
      ~weight:Cairo.Normal
  in
  (* 7. Render layout *)
  B.render ctx layout ;
  Cairo.Surface.finish cairo_surface

let eyes =
  let open C in
  hbox ~pos:`Hcentered ~dx:10.0 [circle Pt.zero 50.0; circle Pt.zero 50.0]

let put_in_box frame commands = C.frame (C.Preserve_aspect { frame }) commands

let commands =
  let box = Bbox.of_points [Pt.zero; Pt.pt 200.0 150.0] in
  C.group [put_in_box box eyes; C.box ~mins:(Bbox.sw box) ~maxs:(Bbox.ne box)]

let _ = display_pdf "framing.pdf" commands
