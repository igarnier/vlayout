open Vlayout

(* We won't be using names for this example. *)
module Name =
struct

  type t = unit

  let compare _ _ = 0

  let pp fmtr () = Format.pp_print_string fmtr "()"

end

module C = Commands.Make(Name)

module B = Backends.Cairo(C)

let empty_box clr width height =
  let style = Style.make ~stroke:(Style.solid_stroke ~clr) ~width:None ~fill:None ~dash:None in
  C.style
    ~style
    (C.box ~mins:Pt.zero ~maxs:(Pt.pt width height))

let filled_box stroke_clr filled_clr width height =
  let style = Style.make
      ~stroke:(Style.solid_stroke ~clr:stroke_clr)
      ~width:None
      ~fill:(Some (Style.solid_stroke ~clr:filled_clr)) ~dash:None in
  C.style
    ~style
    (C.box ~mins:Pt.zero ~maxs:(Pt.pt width height))

let hgradient_box clr1 clr2 width height =
  let style = Style.make
      ~stroke:(Style.(solid_stroke ~clr:black))
      ~width:None
      ~fill:(Some (Style.simple_horizontal_gradient ~clr1 ~clr2))
      ~dash:None in
  C.style
    ~style
    (C.box ~mins:Pt.zero ~maxs:(Pt.pt width height))

let vgradient_box clr1 clr2 width height =
  let style = Style.make
      ~stroke:(Style.(solid_stroke ~clr:black))
      ~width:None
      ~fill:(Some (Style.simple_vertical_gradient ~clr1 ~clr2))
      ~dash:None in
  C.style
    ~style
    (C.box ~mins:Pt.zero ~maxs:(Pt.pt width height))

let vgradient_circle clr1 clr2 radius =
  let style = Style.make
      ~stroke:(Style.(solid_stroke ~clr:black))
      ~width:None
      ~fill:(Some (Style.simple_vertical_gradient ~clr1 ~clr2))
      ~dash:None in
  C.style
    ~style
    (C.circle ~center:Pt.zero ~radius)

let hgradient_circle clr1 clr2 radius =
  let style = Style.make
      ~stroke:(Style.(solid_stroke ~clr:black))
      ~width:None
      ~fill:(Some (Style.simple_horizontal_gradient ~clr1 ~clr2))
      ~dash:None in
  C.style
    ~style
    (C.circle ~center:Pt.zero ~radius)

let random_color () =
  let r = Random.float 1.0 in
  let g = Random.float 1.0 in
  let b = Random.float 1.0 in
  Style.rgb r g b

let random_float () =
  let f = Random.float 10.0 in
  if Random.bool () then
    f
  else
    -. f

let random_box_size () =
  let w = 30.0 +. (random_float ()) in
  let h = 30.0 +. (random_float ()) in
  (w, h)

let random_box () =
  let i = Random.int 6 in
  match i with
  | 0 ->
    let w, h = random_box_size () in
    empty_box (random_color ()) w h
  | 1 ->
    let w, h = random_box_size () in
    filled_box (random_color ()) (random_color ()) w h
  | 2 ->
    let w, h = random_box_size () in
    hgradient_box (random_color ()) (random_color ()) w h
  | 3 ->
    let w, h = random_box_size () in
    vgradient_box (random_color ()) (random_color ()) w h
  | 4 ->
    let radius = 2.0 *. (random_float ()) in
    vgradient_circle (random_color ()) (random_color ()) radius
  | 5 ->
    let radius = 2.0 *. (random_float ()) in
    hgradient_circle (random_color ()) (random_color ()) radius
  | _ ->
    failwith ""

let random_rotation cmd =
  let radians = Random.float (2.0 *. (acos (~-. 1.0))) in
  C.rotate ~radians cmd

let random_scaling cmd =
  let xs = Random.float 2.0 in
  let ys = xs +. (Random.float 0.5) in
  C.scale ~xs ~ys cmd


let rec random_layout depth =
  if depth = 0 then
    C.cmd (random_rotation (random_scaling (random_box ())))
  else
    let l1 = random_layout (depth - 1) in
    let l2 = random_layout (depth - 1) in
    if Random.bool () then
      C.hbox
        ~deltax:10.0
        [ l1; l2 ]
    else
      C.vbox
        ~deltay:10.0
        [ l1; l2 ]

let _ = Random.init 19

let commands = random_layout 7

let _commands =
  let t = C.text ~pos:{ C.pos = Pt.pt 0.0 0.0; relpos = West } ~size:5.0 ~text:"test" in
  let b = C.box ~mins:(Pt.pt 0.0 0.0) ~maxs:(Pt.pt 10.0 10.0) in
  C.(cmd (wrap [t;b]))

let _commands =
  let pos  = { C.pos = Pt.pt 0.0 0.0; relpos = NorthEast } in
  let text = C.text ~pos ~size:35.0 ~text:"test" in
  let bbox = C.Bbox.of_command text in
  let pos  = C.text_position pos (Bbox.width bbox) (Bbox.height bbox) in
  C.cmd (C.wrap [
    text;
    C.box ~mins:(C.Bbox.sw bbox) ~maxs:(C.Bbox.ne bbox);
    C.circle ~center:Pt.zero ~radius:10.0;
    C.circle ~center:pos ~radius:5.0;
    (* C.text  ~pos:{ C.pos = Pt.pt 0.0 50.0; relpos = South } ~width:40.0 ~height:10.0 ~text:"test"; *)
    C.box ~mins:(Pt.pt (~-. 90.0) (~-. 90.0)) ~maxs:(Pt.pt 90. 90.)
  ])


let process_layout xmargin ymargin layout =
  let (cmds, bbox) = C.emit_commands_with_bbox layout in
  let w    = xmargin +. (Bbox.width bbox)
  and h    = ymargin +. (Bbox.height bbox) in
  (C.center_to_page (w, h) cmds, w, h)

let display_pdf filename layout =
  let layout, w, h  = process_layout 10.0 10.0 layout in (* TODO: make margins a parameter *)
  let cairo_surface = Cairo.PDF.create filename ~w ~h in
  let ctx           = Cairo.create cairo_surface in
  let _ =
    Cairo.set_matrix
      ctx
      Cairo.({
          xx = 1.0 ; yx = 0.0 ;
          xy = 0.0 ; yy = ~-. 1.0 ;
          x0 = 0.0 ; y0 = h
        });
    Cairo.set_line_width ctx 1.0;
    Cairo.select_font_face
      ctx
      "DejaVuSansMono"
      ~slant:Cairo.Upright
      ~weight:Cairo.Normal
  in
  (* 7. Render layout *)
  B.render ctx layout;
  Cairo.Surface.finish cairo_surface

let _ =
  display_pdf "mondrian.pdf" commands
