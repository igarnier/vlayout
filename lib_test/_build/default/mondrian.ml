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

let commands = C.cmd ~name:None [empty_box Style.red 50.0 50.0]

let cairo_init width height outfile =
  let srf  = Cairo.PDF.create ~fname:outfile ~width:(float width) ~height:(float height) in
  let ctx  = Cairo.create srf  in
  let _    =
    Cairo.set_matrix
      ctx
      Cairo.({
                xx = 1.0 ; yx = 0.0 ;
                xy = 0.0 ; yy = ~-. 1.0 ;
                x0 = 0.0 ; y0 = (float height)
              })
  in
  let _   = Cairo.set_line_width ctx 1.0 in
  (* let _   = Cairo.select_font_face ctx "Purisa" Cairo.FONT_SLANT_NORMAL Cairo.FONT_WEIGHT_NORMAL in *)
  (ctx, srf)

let render_layout_centered srf ctx xmargin ymargin layout =
  let (cmds, bbox) = C.emit_commands_with_bbox layout in
  let bbox = C.Bbox.of_commands cmds in
  let w    = xmargin +. (Bbox.width bbox)
  and h    = ymargin +. (Bbox.height bbox) in
  let cmds = C.center_to_page (w, h) cmds in
  Cairo.PDF.set_size srf w h;
  let _    =
    Cairo.set_matrix
      ctx
      Cairo.({
                xx = 1.0 ; yx = 0.0 ;
                xy = 0.0 ; yy = ~-. 1.0 ;
                x0 = 0.0 ; y0 = h
              })
  in
  List.iter (B.render ctx) cmds;
  Cairo.show_page ctx  

let _ =
  let ctx, srf = cairo_init 800 600 "mondrian.pdf" in
  render_layout_centered srf ctx 10.0 10.0 commands
                        
