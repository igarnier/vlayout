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

let test =
  let open C in
  let cmd =
    hbox
      ~dx:10.0
      [ group [anchor "a" Pt.zero @@ circle Pt.zero 50.0];
        circle Pt.zero 50.0;
        group [anchor "b" Pt.zero @@ circle Pt.zero 50.0] ]
  in
  group
    [ cmd;
      ( bind_each "a" cmd @@ fun a ->
        bind_each "b" cmd @@ fun b ->
        arrow ~style:Arrow.default_style ~start:a ~finish:b ) ]

let _ = display_pdf "arrow.pdf" test
