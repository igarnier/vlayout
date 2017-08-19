module Cairo(C : Commands.CommandsSig) =
  struct

    let set_pattern ctx patt =
      match patt with
      | Style.Solid { c }      ->
         let { Style.r; g; b } = c in
         let patt = Cairo.Pattern.create_rgb ~r ~g ~b in
         Cairo.set_source ctx patt
      | Style.Linear { p0; p1; stops } ->
         let { Pt.x = x0; y = y0 } = p0 in
         let { Pt.x = x1; y = y1 } = p1 in
         let patt = Cairo.Pattern.create_linear ~x0 ~y0 ~x1 ~y1 in
         List.iter (fun (c, ofs) ->
             let { Style.r; g; b } = c in
             Cairo.Pattern.add_color_stop_rgb patt ~ofs r g b
           ) stops;
         Cairo.set_source ctx patt
      | Style.Radial { c0; r0; c1; r1; stops } ->
         let { Pt.x = x0; y = y0 } = c0 in
         let { Pt.x = x1; y = y1 } = c1 in
         let patt = Cairo.Pattern.create_radial ~x0 ~y0 ~r0 ~x1 ~y1 ~r1 in
         List.iter (fun (c, ofs) ->
             let { Style.r; g; b } = c in
             Cairo.Pattern.add_color_stop_rgb patt ~ofs r g b
           ) stops;
         Cairo.set_source ctx patt

    let perform_stroke_and_fill_opt ctx fill_opt =
      match fill_opt with
      | None ->
         Cairo.stroke ctx
      | Some fill_pattern ->
         Cairo.stroke_preserve ctx;
         Cairo.save ctx;
         set_pattern ctx fill_pattern;
         Cairo.fill ctx;
         Cairo.restore ctx

    let adjust_fill_to_bbox bbox fill_opt =
      match fill_opt with
      | None ->
         fill_opt
      | Some patt ->
         match patt with
         | Style.Solid _ ->
            fill_opt
         | Style.Linear { p0; p1; stops } ->
            let open Pt in
            let w  = Bbox.width bbox in
            let h  = Bbox.height bbox in
            let x0 = p0.x *. w +. bbox.mins.x in
            let y0 = p0.y *. h +. bbox.mins.y in
            let x1 = p1.x *. w +. bbox.mins.x in
            let y1 = p1.y *. h +. bbox.mins.y in
            let p0 = { x = x0; y = y0 }
            and p1 = { x = x1; y = y1 } in
            Some (Style.Linear { p0; p1; stops })
         | Style.Radial { c0; r0; c1; r1; stops } ->
            let open Pt in
            let w  = Bbox.width bbox in
            let h  = Bbox.height bbox in
            let x0 = c0.x *. w +. bbox.mins.x in
            let y0 = c0.y *. h +. bbox.mins.y in
            let x1 = c1.x *. w +. bbox.mins.x in
            let y1 = c1.y *. h +. bbox.mins.y in
            let c0 = { x = x0; y = y0 }
            and c1 = { x = x1; y = y1 } in
            let r0 = r0 *. w in
            let r1 = r1 *. w in
            Some (Style.Radial { c0; r0; c1; r1; stops })
            
                       
    let rec render ctx fill_opt cmd =
      match cmd.C.desc with
      | C.Circle { center; radius } ->
         let { Pt.x; y } = center in
         Cairo.arc ctx ~x ~y ~r:radius ~a1:0.0 ~a2:(2.0 *. Tools.pi);
         (* Cairo.save ctx; *)
         (* Cairo.translate ctx x y; *)
         (* Cairo.scale ctx ~x:radius ~y:radius; *)
         (* Cairo.arc ctx ~x:0.0 ~y:0.0 ~r:1.0 ~a1:0.0 ~a2:(2.0 *. Tools.pi); *)
         (* translate cr (x +. width /. 2.) (y +. height /. 2.); *)
         (* scale cr (width /. 2.) (height /. 2.); *)
         (* arc cr 0. 0. 1. 0. (2 * pi); *)
         
         (* Cairo.arc ctx 0.0 0.0 1.0 0.0 (2.0 *. Tools.pi); *)
         (* Cairo.restore ctx; *)
         (* Cairo.translate ctx x y; *)
         (* Cairo.arc ctx 0.0 0.0 radius 0.0 (2.0 *. Tools.pi); *)
         (* Cairo.translate ctx (-. x) (-.y); *)
         perform_stroke_and_fill_opt ctx fill_opt
         
      | C.Box { mins; maxs } ->
         Pt.(
          Cairo.move_to ctx mins.x mins.y;
          Cairo.line_to ctx mins.x maxs.y;
          Cairo.line_to ctx maxs.x maxs.y;
          Cairo.line_to ctx maxs.x mins.y;
          Cairo.Path.close ctx;
          perform_stroke_and_fill_opt ctx fill_opt          
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
          perform_stroke_and_fill_opt ctx fill_opt
         )
      | C.Style { style; subcommands } ->
         let bbox          = C.Bbox.of_commands subcommands in
         let adjusted_fill = adjust_fill_to_bbox bbox style.Style.fill in 
         Cairo.save ctx;
         set_pattern ctx style.Style.stroke;
         List.iter (render ctx adjusted_fill) subcommands;
         Cairo.restore ctx
      | C.Segment { p1; p2 } ->
         let { Pt.x = x1; y = y1 } = p1
         and { Pt.x = x2; y = y2 } = p2 in
         Cairo.move_to ctx x1 y1;
         Cairo.line_to ctx x2 y2;
         perform_stroke_and_fill_opt ctx fill_opt         
      | C.Bezier { p1; c1; p2; c2 } ->
         Pt.(
          Cairo.move_to ctx p1.x p1.y;
          Cairo.curve_to ctx c1.x c1.y c2.x c2.y p2.x p2.y;
          perform_stroke_and_fill_opt ctx fill_opt
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
         let patt   = Cairo.Pattern.create_for_surface surf in
         let matrix = Cairo.Matrix.init_translate ~x:(-. bbox.mins.x) ~y:(-. bbox.mins.y) in 
         Cairo.Pattern.set_matrix patt matrix;
         Cairo.set_source ctx patt;
         Cairo.paint ctx;
         Cairo.restore ctx
      | C.DeclPt { pt; name } -> ()

    let render ctx cmd = render ctx None cmd
                            
  end
