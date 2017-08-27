open Vlayout

(* We won't be using names for this example. *)
module Name =
  struct

    type t = ()

    let compare _ _ = 0

    let print _ = "()"
    
  end

module C = Commands.Make(Name)

module B = Backends.Cairo(C)

let empty_box clr width height =
  let style = Style.make ~stroke:(Style.solid_stroke clr) ~fill:None in
  C.style
    ~style
    ~subcommands:[ C.box ~mins:Pt.zero ~maxs:(Pt.pt width height) ]

let filled_box stroke_clr filled_clr width height =
  let style = Style.make ~stroke:(Style.solid_stroke stroke_clr) ~fill:(Some (Style.solid_stroke filled_clr)) in
  C.style
    ~style
    ~subcommands:[ C.box ~mins:Pt.zero ~maxs:(Pt.pt width height) ]

let hgradient_box clr1 clr2 width height =
  let style = Style.make ~stroke:(Style.(solid_stroke black)) ~fill:(Some (Style.simple_horizontal_gradient clr1 clr2)) in
  C.style
    ~style
    ~subcommands:[ C.box ~mins:Pt.zero ~maxs:(Pt.pt width height) ]
    
let vgradient_box clr1 clr2 width height =
  let style = Style.make ~stroke:(Style.(solid_stroke black)) ~fill:(Some (Style.simple_vertical_gradient clr1 clr2)) in
  C.style
    ~style
    ~subcommands:[ C.box ~mins:Pt.zero ~maxs:(Pt.pt width height) ]

let vgradient_circle clr1 clr2 radius =
  let style = Style.make ~stroke:(Style.(solid_stroke black)) ~fill:(Some (Style.simple_vertical_gradient clr1 clr2)) in
  C.style
    ~style
    ~subcommands:[ C.circle ~center:Pt.zero ~radius ]

let hgradient_circle clr1 clr2 radius =
  let style = Style.make ~stroke:(Style.(solid_stroke black)) ~fill:(Some (Style.simple_horizontal_gradient clr1 clr2)) in
  C.style
    ~style
    ~subcommands:[ C.circle ~center:Pt.zero ~radius ]

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

let rec random_layout depth =
  if depth = 0 then
    C.cmd ~name:None [random_box ()]
  else
    let l1 = random_layout (depth - 1) in
    let l2 = random_layout (depth - 1) in
    if Random.bool () then
      C.hbox
        ~deltax:10.0
        ~layout_list:[ l1; l2 ]
    else
      C.vbox
        ~deltay:10.0
        ~layout_list:[ l1; l2 ]

let _ = Random.init 19
let commands = random_layout 7

                             
let process_layout xmargin ymargin layout =
  let (cmds, bbox) = C.emit_commands_with_bbox layout in
  let w    = xmargin +. (Bbox.width bbox)
  and h    = ymargin +. (Bbox.height bbox) in
  (C.center_to_page (w, h) cmds, w, h)

let display_pdf filename layout =
  let layout, w, h  = process_layout 10.0 10.0 layout in (* TODO: make margins a parameter *)
  let cairo_surface = Cairo.PDF.create ~fname:filename ~width:w ~height:h in  
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
  List.iter (B.render ctx) layout;
  Cairo.Surface.finish cairo_surface

let _ =
  display_pdf "mondrian.pdf" commands
                        
