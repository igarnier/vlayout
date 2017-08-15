external create_for_data32_unsafe : data:Cairo.Image.data32 ->
                                    Cairo.Image.format -> width:int -> height:int -> stride:int -> Cairo.Surface.t
    = "caml_cairo_image_surface_create_for_data32"


module Cairo(C : Commands.CommandsSig) =
  struct

    let render ctx cmd =
      match cmd.C.desc with
      | C.Circle { center; radius } ->
         let { Pt.x; y } = center in
         Cairo.translate ctx x y;
         Cairo.arc ctx 0.0 0.0 radius 0.0 (2.0 *. Tools.pi);
         Cairo.translate ctx (-. x) (-.y);
         Cairo.stroke ctx
      | C.Box { mins; maxs } ->
         Pt.(
          Cairo.move_to ctx mins.x mins.y;
          Cairo.line_to ctx mins.x maxs.y;
          Cairo.line_to ctx maxs.x maxs.y;
          Cairo.line_to ctx maxs.x mins.y;
          Cairo.Path.close ctx;
          Cairo.stroke ctx
         )
      | C.Text { pos; size; text } ->
         Pt.(
          let p = C.text_position pos size text in
          Cairo.set_font_size ctx size;
          Cairo.move_to ctx p.x p.y;
          Cairo.save ctx;
          Cairo.identity_matrix ctx;
          Cairo.show_text ctx text;
          Cairo.restore ctx;
          Cairo.stroke ctx
         )
      | C.Color { r; g; b } ->
         Cairo.set_source_rgb ctx r g b
      | C.Segment { p1; p2 } ->
         let { Pt.x = x1; y = y1 } = p1
         and { Pt.x = x2; y = y2 } = p2 in
         Cairo.move_to ctx x1 y1;
         Cairo.line_to ctx x2 y2;
         Cairo.stroke ctx
      | C.Bezier { p1; c1; p2; c2 } ->
         Pt.(
          Cairo.move_to ctx p1.x p1.y;
          Cairo.curve_to ctx c1.x c1.y c2.x c2.y p2.x p2.y;
          Cairo.stroke ctx
         )
      | C.Image { pos; image } ->
         Cairo.save ctx;
         let bbox = C.Bbox.of_command cmd in
         let ()   = Cairo.rectangle ctx ~x:bbox.mins.x ~y:bbox.mins.y ~w:(Bbox.width bbox) ~h:(Bbox.height bbox) in
         let ()   = Cairo.clip ctx in
         let xsize  = Image.xsize image in
         let ysize  = Image.ysize image in
         let pixels = Bigarray.reshape_2 (Bigarray.genarray_of_array1 (Image.pixels image)) xsize ysize in
         let surf   = Cairo.Image.create_for_data32 ~alpha:false pixels in
         let patt = Cairo.Pattern.create_for_surface surf in
         let matrix = Cairo.Matrix.init_translate ~x:(-. bbox.mins.x) ~y:(-. bbox.mins.y) in 
         Cairo.Pattern.set_matrix patt matrix;
         Cairo.set_source ctx patt;
         Cairo.paint ctx;
         Cairo.restore ctx
      | C.DeclPt { pt; name } -> ()
                            
  end
