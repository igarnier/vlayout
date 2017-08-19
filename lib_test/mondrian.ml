open Vlayout

(* We won't be using names for this example. *)
module Name =
  struct

    type t = ()

    let compare _ _ = 0

    let print _ = "()"
    
  end

module C = Commands.Make(Name)

module U = C.Untagged

module B = Backends.Cairo(C)

let empty_box clr width height =
  let style = Style.make ~stroke:(Style.solid_stroke clr) ~fill:None in
  U.style
    ~style
    ~subcommands:[ U.box ~mins:Pt.zero ~maxs:(Pt.pt width height) ]


let filled_box stroke_clr filled_clr width height =
  let style = Style.make ~stroke:(Style.solid_stroke stroke_clr) ~fill:(Some (Style.solid_stroke filled_clr)) in
  U.style
    ~style
    ~subcommands:[ U.box ~mins:Pt.zero ~maxs:(Pt.pt width height) ]
 
let vgradient_box clr1 clr2 width height =
  let style = Style.make ~stroke:(Style.(solid_stroke black)) ~fill:(Some (Style.vertical_gradient clr1 clr2)) in
  U.style
    ~style
    ~subcommands:[ U.box ~mins:Pt.zero ~maxs:(Pt.pt width height) ]

let vgradient_circle clr1 clr2 radius =
  let style = Style.make ~stroke:(Style.(solid_stroke black)) ~fill:(Some (Style.vertical_gradient clr1 clr2)) in
  U.style
    ~style
    ~subcommands:[ U.circle ~center:Pt.zero ~radius ]

let hgradient_circle clr1 clr2 radius =
  let style = Style.make ~stroke:(Style.(solid_stroke black)) ~fill:(Some (Style.horizontal_gradient clr1 clr2)) in
  U.style
    ~style
    ~subcommands:[ U.circle ~center:Pt.zero ~radius ]
    
    
let commands =
  C.(
    let h =
      hbox
        ~deltax:10.0
        ~layout_list:[
          cmd ~name:None [empty_box Style.black 50.0 50.0];
          cmd ~name:None [filled_box Style.black Style.red 50.0 50.0];
          cmd ~name:None [vgradient_box Style.black Style.red 50.0 50.0];        
        ]
    in
    vbox
      ~deltay:10.0
      ~layout_list:[
        cmd ~name:None [hgradient_circle Style.red Style.blue 30.0];
        h
      ]

  )

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
                        
