module Cairo (C : Commands.S) = struct
  let set_pattern ctx patt =
    match patt with
    | Style.Solid { c } ->
        let { Style.r; g; b } = c in
        let patt = Cairo.Pattern.create_rgb r g b in
        Cairo.set_source ctx patt
    | Style.Linear { p0; p1; stops } ->
        let x0 = Pt.x p0 and y0 = Pt.y p0 in
        let x1 = Pt.x p1 and y1 = Pt.y p1 in
        let patt = Cairo.Pattern.create_linear ~x0 ~y0 ~x1 ~y1 in
        List.iter
          (fun (c, ofs) ->
            let { Style.r; g; b } = c in
            Cairo.Pattern.add_color_stop_rgb patt ~ofs r g b)
          stops ;
        Cairo.set_source ctx patt
    | Style.Radial { c0; r0; c1; r1; stops } ->
        let x0 = Pt.x c0 and y0 = Pt.y c0 in
        let x1 = Pt.x c1 and y1 = Pt.y c1 in
        let patt = Cairo.Pattern.create_radial ~x0 ~y0 ~r0 ~x1 ~y1 ~r1 in
        List.iter
          (fun (c, ofs) ->
            let { Style.r; g; b } = c in
            Cairo.Pattern.add_color_stop_rgb patt ~ofs r g b)
          stops ;
        Cairo.set_source ctx patt

  (* Perform the stroke at uniform scale, so that *)
  let stroke_uniform_scale ctx =
    Cairo.save ctx ;
    let m = Cairo.get_matrix ctx in
    let m = { m with xx = 1.0; yy = 1.0 } in
    Cairo.set_matrix ctx m ;
    (* let ux, uy = Cairo.device_to_user_distance ctx 1. 1. in *)
    (* Cairo.set_line_width ctx (max ux uy); *)
    Cairo.stroke ctx ;
    Cairo.restore ctx

  let stroke_preserve_uniform_scale ctx =
    Cairo.save ctx ;
    let m = Cairo.get_matrix ctx in
    let m = { m with xx = 1.0; yy = 1.0 } in
    Cairo.set_matrix ctx m ;
    let (ux, uy) = Cairo.device_to_user_distance ctx 1. 1. in
    Cairo.set_line_width ctx (max ux uy) ;
    Cairo.stroke_preserve ctx ;
    Cairo.restore ctx

  let perform_stroke_and_fill_opt ctx fill_opt =
    match fill_opt with
    | None -> stroke_uniform_scale ctx
    | Some fill_pattern ->
        stroke_preserve_uniform_scale ctx ;
        Cairo.stroke_preserve ctx ;
        Cairo.save ctx ;
        set_pattern ctx fill_pattern ;
        Cairo.fill ctx ;
        Cairo.restore ctx

  let adjust_fill_to_bbox bbox fill_opt =
    match fill_opt with
    | None -> fill_opt
    | Some patt -> (
        match patt with
        | Style.Solid _ -> fill_opt
        | Style.Linear { p0; p1; stops } ->
            let w = Bbox.width bbox in
            let h = Bbox.height bbox in
            let x0 = (Pt.x p0 *. w) +. Pt.x (Bbox.sw bbox) in
            let y0 = (Pt.y p0 *. h) +. Pt.y (Bbox.sw bbox) in
            let x1 = (Pt.x p1 *. w) +. Pt.x (Bbox.sw bbox) in
            let y1 = (Pt.y p1 *. h) +. Pt.y (Bbox.sw bbox) in
            let p0 = Pt.pt x0 y0 in
            let p1 = Pt.pt x1 y1 in
            Some (Style.Linear { p0; p1; stops })
        | Style.Radial { c0; r0; c1; r1; stops } ->
            let w = Bbox.width bbox in
            let h = Bbox.height bbox in
            let x0 = (Pt.x c0 *. w) +. Pt.x (Bbox.sw bbox) in
            let y0 = (Pt.y c0 *. h) +. Pt.y (Bbox.sw bbox) in
            let x1 = (Pt.x c1 *. w) +. Pt.x (Bbox.sw bbox) in
            let y1 = (Pt.y c1 *. h) +. Pt.y (Bbox.sw bbox) in
            let c0 = Pt.pt x0 y0 in
            let c1 = Pt.pt x1 y1 in
            let r0 = r0 *. w in
            let r1 = r1 *. w in
            Some (Style.Radial { c0; r0; c1; r1; stops }) )

  let render_box ctx mins maxs =
    let open Pt in
    Cairo.move_to ctx (x mins) (y mins) ;
    Cairo.line_to ctx (x mins) (y maxs) ;
    Cairo.line_to ctx (x maxs) (y maxs) ;
    Cairo.line_to ctx (x maxs) (y mins) ;
    Cairo.Path.close ctx

  (* for debug *)
  let _print_scale ctx =
    let m = Cairo.get_matrix ctx in
    let xscale = m.Cairo.xx in
    let yscale = m.Cairo.yy in
    Printf.printf "scale: %f %f\n" xscale yscale

  let _text_bbox ctx str =
    (* Using Christophe Troestler's notes at
       http://archimedes.forge.ocamlcore.org/cairo/textextents.ml *)
    let fe = Cairo.font_extents ctx in
    let te = Cairo.text_extents ctx str in
    let x = 0.5 -. te.x_bearing -. (te.width /. 2.)
    and y = 0.5 -. fe.descent +. (fe.baseline /. 2.) in
    let pos = Pt.pt x y in
    let mins = Pt.pt (x +. te.x_bearing) (y +. te.y_bearing) in
    let maxs = Pt.(mins + pt te.width te.height) in
    (pos, Bbox.box mins maxs)

  let rec render ctx fill_opt cmd =
    match cmd.C.desc with
    | C.Circle { center; radius } ->
        let x = Pt.x center and y = Pt.y center in
        Cairo.arc ctx x y ~r:radius ~a1:0.0 ~a2:(2.0 *. Tools.pi) ;
        perform_stroke_and_fill_opt ctx fill_opt
    | C.Box { mins; maxs } ->
        render_box ctx mins maxs ;
        perform_stroke_and_fill_opt ctx fill_opt
    | C.Text { pos; text } ->
        if text.Ctext.str = "" then ()
        else
          let base = text.Ctext.base in
          let box = text.Ctext.box in
          let delta = Pt.(base - Bbox.sw box) in
          let w = Bbox.width box in
          let h = Bbox.height box in
          let llc = C.text_position pos w h in
          let llc = Pt.(llc + delta) in
          Cairo.save ctx ;
          Cairo.move_to ctx (Pt.x llc) (Pt.y llc) ;
          Cairo.Scaled_font.set ctx text.Ctext.font ;
          Cairo.show_text ctx text.str ;
          Cairo.restore ctx ;
          perform_stroke_and_fill_opt ctx fill_opt
    | C.Style { style; subcommands } ->
        let bbox = C.Bbox.of_commands subcommands in
        let adjusted_fill = adjust_fill_to_bbox bbox style.Style.fill in
        Cairo.save ctx ;
        set_pattern ctx style.Style.stroke ;
        ( match style.dash with
        | None -> ()
        | Some patt -> Cairo.set_dash ctx patt ) ;
        ( match style.width with
        | None -> ()
        | Some width -> Cairo.set_line_width ctx width ) ;
        List.iter (render ctx adjusted_fill) subcommands ;
        Cairo.restore ctx
    | C.Segment { p1; p2 } ->
        let x1 = Pt.x p1 and y1 = Pt.y p1 in
        let x2 = Pt.x p2 and y2 = Pt.y p2 in
        Cairo.move_to ctx x1 y1 ;
        Cairo.line_to ctx x2 y2 ;
        perform_stroke_and_fill_opt ctx fill_opt
    | C.Bezier { p1; c1; p2; c2 } ->
        Pt.(
          Cairo.move_to ctx (x p1) (y p1) ;
          Cairo.curve_to ctx (x c1) (y c1) (x c2) (y c2) (x p2) (y p2) ;
          perform_stroke_and_fill_opt ctx fill_opt)
    | C.Image { pos = _; image } ->
        Cairo.save ctx ;
        let bbox = C.Bbox.of_command cmd in
        let () =
          Cairo.rectangle
            ctx
            (Pt.x (Bbox.sw bbox))
            (Pt.y (Bbox.sw bbox))
            ~w:(Bbox.width bbox)
            ~h:(Bbox.height bbox)
        in
        let () = Cairo.clip ctx in
        let xsize = Image.xsize image in
        let ysize = Image.ysize image in
        let pixels =
          Bigarray.reshape_2
            (Bigarray.genarray_of_array1 (Image.pixels image))
            xsize
            ysize
        in
        let surf =
          Cairo.Image.create_for_data32 ~w:xsize ~h:ysize ~alpha:false pixels
        in
        let patt = Cairo.Pattern.create_for_surface surf in
        let matrix =
          Cairo.Matrix.init_translate
            (-.Pt.x (Bbox.sw bbox))
            (-.Pt.y (Bbox.sw bbox))
        in
        Cairo.Pattern.set_matrix patt matrix ;
        Cairo.set_source ctx patt ;
        Cairo.paint ctx ;
        Cairo.restore ctx
    | C.DeclPt { pt = _; name = _ } -> ()
    | C.Rotate { radians; subcommands } ->
        Cairo.save ctx ;
        Cairo.rotate ctx radians ;
        List.iter (render ctx fill_opt) subcommands ;
        Cairo.restore ctx
    | C.Translate { v; subcommands } ->
        Cairo.save ctx ;
        Cairo.translate ctx (Pt.x v) (Pt.y v) ;
        List.iter (render ctx fill_opt) subcommands ;
        Cairo.restore ctx
    | C.Scale { xs; ys; subcommands } ->
        Cairo.save ctx ;
        Cairo.scale ctx xs ys ;
        List.iter (render ctx fill_opt) subcommands ;
        Cairo.restore ctx

  let render ctx cmd = render ctx None cmd
end
