open Batteries

module Cairo(C : Commands.CommandsSig) =
struct

  let set_pattern ctx patt =
    match patt with
    | Style.Solid { c }      ->
      let { Style.r; g; b } = c in
      let patt = Cairo.Pattern.create_rgb ~r ~g ~b in
      Cairo.set_source ctx patt
    | Style.Linear { p0; p1; stops } ->
      let x0 = Pt.x p0 and y0 = Pt.y p0 in
      let x1 = Pt.x p1 and y1 = Pt.y p1 in
      let patt = Cairo.Pattern.create_linear ~x0 ~y0 ~x1 ~y1 in
      List.iter (fun (c, ofs) ->
          let { Style.r; g; b } = c in
          Cairo.Pattern.add_color_stop_rgb patt ~ofs r g b
        ) stops;
      Cairo.set_source ctx patt
    | Style.Radial { c0; r0; c1; r1; stops } ->
      let x0 = Pt.x c0 and y0 = Pt.y c0 in
      let x1 = Pt.x c1 and y1 = Pt.y c1 in
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
        let x0 = (Pt.x p0) *. w +. (Pt.x (Bbox.sw bbox)) in
        let y0 = (Pt.y p0) *. h +. (Pt.y (Bbox.sw bbox)) in
        let x1 = (Pt.x p1) *. w +. (Pt.x (Bbox.sw bbox)) in
        let y1 = (Pt.y p1) *. h +. (Pt.y (Bbox.sw bbox)) in
        let p0 = Pt.pt x0 y0 in
        let p1 = Pt.pt x1 y1 in
        Some (Style.Linear { p0; p1; stops })
      | Style.Radial { c0; r0; c1; r1; stops } ->
        let open Pt in
        let w  = Bbox.width bbox in
        let h  = Bbox.height bbox in
        let x0 = (Pt.x c0) *. w +. (Pt.x (Bbox.sw bbox)) in
        let y0 = (Pt.y c0) *. h +. (Pt.y (Bbox.sw bbox)) in
        let x1 = (Pt.x c1) *. w +. (Pt.x (Bbox.sw bbox)) in
        let y1 = (Pt.y c1) *. h +. (Pt.y (Bbox.sw bbox)) in
        let c0 = Pt.pt x0 y0 in
        let c1 = Pt.pt x1 y1 in
        let r0 = r0 *. w in
        let r1 = r1 *. w in
        Some (Style.Radial { c0; r0; c1; r1; stops })


  let rec render ctx fill_opt cmd =
    match cmd.C.desc with
    | C.Circle { center; radius } ->
      let x = Pt.x center and y = Pt.y center in
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
        Cairo.move_to ctx (x mins) (y mins);
        Cairo.line_to ctx (x mins) (y maxs);
        Cairo.line_to ctx (x maxs) (y maxs);
        Cairo.line_to ctx (x maxs) (y mins);
        Cairo.Path.close ctx;
        perform_stroke_and_fill_opt ctx fill_opt          
      )
    | C.Text { pos; size; text } ->
      Pt.(
        let p = C.text_position pos size text in
        Cairo.set_font_size ctx size;
        Cairo.move_to ctx (x p) (y p);
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
      Option.may (Cairo.set_dash ctx) style.dash;
      List.iter (render ctx adjusted_fill) subcommands;
      Cairo.restore ctx
    | C.Segment { p1; p2 } ->
      let x1 = Pt.x p1 and y1 = Pt.y p1 in
      let x2 = Pt.x p2 and y2 = Pt.y p2 in
      Cairo.move_to ctx x1 y1;
      Cairo.line_to ctx x2 y2;
      perform_stroke_and_fill_opt ctx fill_opt         
    | C.Bezier { p1; c1; p2; c2 } ->
      Pt.(
        Cairo.move_to ctx (x p1) (y p1);
        Cairo.curve_to ctx (x c1) (y c1) (x c2) (y c2) (x p2) (y p2);
        perform_stroke_and_fill_opt ctx fill_opt
      )
    | C.Image { pos; image } ->
      Cairo.save ctx;
      let bbox = C.Bbox.of_command cmd in
      let ()   = Cairo.rectangle ctx ~x:(Pt.x (Bbox.sw bbox)) ~y:(Pt.y (Bbox.sw bbox)) ~w:(Bbox.width bbox) ~h:(Bbox.height bbox) in
      let ()   = Cairo.clip ctx in
      let xsize  = Image.xsize image in
      let ysize  = Image.ysize image in
      let pixels = Bigarray.reshape_2 (Bigarray.genarray_of_array1 (Image.pixels image)) xsize ysize in
      let surf   = Cairo.Image.create_for_data32 ~alpha:false pixels in
      let patt   = Cairo.Pattern.create_for_surface surf in
      let matrix = Cairo.Matrix.init_translate ~x:(-. (Pt.x (Bbox.sw bbox))) ~y:(-. (Pt.y (Bbox.sw bbox))) in 
      Cairo.Pattern.set_matrix patt matrix;
      Cairo.set_source ctx patt;
      Cairo.paint ctx;
      Cairo.restore ctx
    | C.DeclPt { pt; name } -> ()

  let render ctx cmd = render ctx None cmd

end
