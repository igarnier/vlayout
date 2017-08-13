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
         let surf = Cairo.Image.create_for_data32 image in
         let patt = Cairo.Pattern.create_for_surface surf in
         Cairo.set_source ctx patt;
         Cairo.paint ctx
      | C.DeclPt { pt; name } -> ()
                            
  end
