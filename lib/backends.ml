module Cairo = struct
  let set_pattern ctx patt =
    match patt with
    | Style.Solid { c } ->
        let (r, g, b) = Color.unpack c in
        let patt = Cairo.Pattern.create_rgb r g b in
        Cairo.set_source ctx patt
    | Style.Linear { p0; p1; stops } ->
        let x0 = Pt.x p0 and y0 = Pt.y p0 in
        let x1 = Pt.x p1 and y1 = Pt.y p1 in
        let patt = Cairo.Pattern.create_linear ~x0 ~y0 ~x1 ~y1 in
        List.iter
          (fun (ofs, c) ->
            let (r, g, b) = Color.unpack c in
            Cairo.Pattern.add_color_stop_rgb patt ~ofs r g b)
          stops ;
        Cairo.set_source ctx patt
    | Style.Radial { c0; c1; r1; stops } ->
        let x0 = Pt.x c0 and y0 = Pt.y c0 in
        let x1 = Pt.x c1 and y1 = Pt.y c1 in
        let patt = Cairo.Pattern.create_radial ~x0 ~y0 ~r0:0.0 ~x1 ~y1 ~r1 in
        List.iter
          (fun (ofs, c) ->
            let (r, g, b) = Color.unpack c in
            Cairo.Pattern.add_color_stop_rgb patt ~ofs r g b)
          stops ;
        Cairo.set_source ctx patt

  (* Perform the stroke at uniform scale *)
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
        | Style.Radial { c0; c1; r1; stops } ->
            let w = Bbox.width bbox in
            let h = Bbox.height bbox in
            let x0 = (Pt.x c0 *. w) +. Pt.x (Bbox.sw bbox) in
            let y0 = (Pt.y c0 *. h) +. Pt.y (Bbox.sw bbox) in
            let x1 = (Pt.x c1 *. w) +. Pt.x (Bbox.sw bbox) in
            let y1 = (Pt.y c1 *. h) +. Pt.y (Bbox.sw bbox) in
            let c0 = Pt.pt x0 y0 in
            let c1 = Pt.pt x1 y1 in
            let r1 = r1 *. w in
            Some (Style.Radial { c0; c1; r1; stops }))

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
    Format.eprintf "scale: %f %f@." xscale yscale

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

  let rec render ctx fill_opt (cmd : Commands.t) =
    match cmd.desc with
    | Circle { center; radius } ->
        let x = Pt.x center and y = Pt.y center in
        Cairo.arc ctx x y ~r:radius ~a1:0.0 ~a2:(2.0 *. Tools.pi) ;
        perform_stroke_and_fill_opt ctx fill_opt
    | Box { mins; maxs } ->
        render_box ctx mins maxs ;
        perform_stroke_and_fill_opt ctx fill_opt
    | Text { pos; text } ->
        if text.Ctext.str = "" then ()
        else
          let base = text.Ctext.base in
          let box = text.Ctext.box in
          let delta = Pt.(base - Bbox.sw box) in
          let w = Bbox.width box in
          let h = Bbox.height box in
          let llc = Commands.text_position pos w h in
          let llc = Pt.(llc + delta) in
          Cairo.save ctx ;
          Cairo.move_to ctx (Pt.x llc) (Pt.y llc) ;
          Cairo.Scaled_font.set ctx text.Ctext.font ;
          Cairo.show_text ctx text.str ;
          Cairo.restore ctx ;
          perform_stroke_and_fill_opt ctx fill_opt
    | Style { style; cmd } ->
        let bbox = cmd.bbox in
        let adjusted_fill = adjust_fill_to_bbox bbox style.Style.fill in
        Cairo.save ctx ;
        set_pattern ctx style.Style.stroke ;
        (match style.dash with
        | None -> ()
        | Some patt -> Cairo.set_dash ctx patt) ;
        (match style.width with
        | None -> ()
        | Some width -> Cairo.set_line_width ctx width) ;
        render ctx adjusted_fill cmd ;
        Cairo.restore ctx
    | Segment { p1; p2 } ->
        let x1 = Pt.x p1 and y1 = Pt.y p1 in
        let x2 = Pt.x p2 and y2 = Pt.y p2 in
        Cairo.move_to ctx x1 y1 ;
        Cairo.line_to ctx x2 y2 ;
        perform_stroke_and_fill_opt ctx fill_opt
    | Bezier { p1; c1; p2; c2 } ->
        Pt.(
          Cairo.move_to ctx (x p1) (y p1) ;
          Cairo.curve_to ctx (x c1) (y c1) (x c2) (y c2) (x p2) (y p2) ;
          perform_stroke_and_fill_opt ctx fill_opt)
    | Image { pos = _; image } ->
        Cairo.save ctx ;
        let bbox = cmd.bbox in
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
    | Rotate { radians; cmd } ->
        Cairo.save ctx ;
        Cairo.rotate ctx radians ;
        render ctx fill_opt cmd ;
        Cairo.restore ctx
    | Translate { v; cmd } ->
        Cairo.save ctx ;
        Cairo.translate ctx (Pt.x v) (Pt.y v) ;
        render ctx fill_opt cmd ;
        Cairo.restore ctx
    | Scale { xs; ys; cmd } ->
        Cairo.save ctx ;
        Cairo.scale ctx xs ys ;
        render ctx fill_opt cmd ;
        Cairo.restore ctx
    | Group subcommands -> List.iter (render ctx fill_opt) subcommands

  let render ctx cmd = render ctx None cmd
end

(* module Vg_image = struct *)
(*   let set_pattern patt = *)
(*     match patt with *)
(*     | Style.Solid { c } -> Vg.I.const c *)
(*     | Style.Linear { p0; p1; stops } -> Vg.I.axial stops p0 p1 *)
(*     | Style.Radial { c0; c1; r1; stops } -> Vg.I.radial stops ~f:c0 c1 r1 *)

(*   (\* (\\* Perform the stroke at uniform scale *\\) *\) *)
(*   (\* let stroke_uniform_scale ctx = *\) *)
(*   (\*   Cairo.save ctx ; *\) *)
(*   (\*   let m = Cairo.get_matrix ctx in *\) *)
(*   (\*   let m = { m with xx = 1.0; yy = 1.0 } in *\) *)
(*   (\*   Cairo.set_matrix ctx m ; *\) *)
(*   (\*   (\\* let ux, uy = Cairo.device_to_user_distance ctx 1. 1. in *\\) *\) *)
(*   (\*   (\\* Cairo.set_line_width ctx (max ux uy); *\\) *\) *)
(*   (\*   Cairo.stroke ctx ; *\) *)
(*   (\*   Cairo.restore ctx *\) *)

(*   (\* let stroke_preserve_uniform_scale ctx = *\) *)
(*   (\*   Cairo.save ctx ; *\) *)
(*   (\*   let m = Cairo.get_matrix ctx in *\) *)
(*   (\*   let m = { m with xx = 1.0; yy = 1.0 } in *\) *)
(*   (\*   Cairo.set_matrix ctx m ; *\) *)
(*   (\*   let (ux, uy) = Cairo.device_to_user_distance ctx 1. 1. in *\) *)
(*   (\*   Cairo.set_line_width ctx (max ux uy) ; *\) *)
(*   (\*   Cairo.stroke_preserve ctx ; *\) *)
(*   (\*   Cairo.restore ctx *\) *)

(*   (\* let perform_stroke_and_fill_opt store fill_opt = *\) *)
(*   (\*   match fill_opt with *\) *)
(*   (\*   | None -> stroke_uniform_scale ctx *\) *)
(*   (\*   | Some fill_pattern -> *\) *)
(*   (\*       stroke_preserve_uniform_scale ctx ; *\) *)
(*   (\*       Cairo.stroke_preserve ctx ; *\) *)
(*   (\*       Cairo.save ctx ; *\) *)
(*   (\*       set_pattern ctx fill_pattern ; *\) *)
(*   (\*       Cairo.fill ctx ; *\) *)
(*   (\*       Cairo.restore ctx *\) *)

(*   let adjust_fill_to_bbox bbox fill_opt = *)
(*     match fill_opt with *)
(*     | None -> fill_opt *)
(*     | Some patt -> ( *)
(*         match patt with *)
(*         | Style.Solid _ -> fill_opt *)
(*         | Style.Linear { p0; p1; stops } -> *)
(*             let w = Bbox.width bbox in *)
(*             let h = Bbox.height bbox in *)
(*             let x0 = (Pt.x p0 *. w) +. Pt.x (Bbox.sw bbox) in *)
(*             let y0 = (Pt.y p0 *. h) +. Pt.y (Bbox.sw bbox) in *)
(*             let x1 = (Pt.x p1 *. w) +. Pt.x (Bbox.sw bbox) in *)
(*             let y1 = (Pt.y p1 *. h) +. Pt.y (Bbox.sw bbox) in *)
(*             let p0 = Pt.pt x0 y0 in *)
(*             let p1 = Pt.pt x1 y1 in *)
(*             Some (Style.Linear { p0; p1; stops }) *)
(*         | Style.Radial { c0; c1; r1; stops } -> *)
(*             let w = Bbox.width bbox in *)
(*             let h = Bbox.height bbox in *)
(*             let x0 = (Pt.x c0 *. w) +. Pt.x (Bbox.sw bbox) in *)
(*             let y0 = (Pt.y c0 *. h) +. Pt.y (Bbox.sw bbox) in *)
(*             let x1 = (Pt.x c1 *. w) +. Pt.x (Bbox.sw bbox) in *)
(*             let y1 = (Pt.y c1 *. h) +. Pt.y (Bbox.sw bbox) in *)
(*             let c0 = Pt.pt x0 y0 in *)
(*             let c1 = Pt.pt x1 y1 in *)
(*             let r1 = r1 *. w in *)
(*             Some (Style.Radial { c0; c1; r1; stops })) *)

(*   let render_box mins maxs = *)
(*     let open Gg.P2 in *)
(*     let open Vg.P in *)
(*     empty *)
(*     |> sub (v (x mins) (y mins)) *)
(*     |> line (v (x mins) (y maxs)) *)
(*     |> line (v (x maxs) (y maxs)) *)
(*     |> line (v (x maxs) (y mins)) *)
(*     |> close *)

(*   (\* (\\* for debug *\\) *\) *)
(*   (\* let _print_scale ctx = *\) *)
(*   (\*   let m = Cairo.get_matrix ctx in *\) *)
(*   (\*   let xscale = m.Cairo.xx in *\) *)
(*   (\*   let yscale = m.Cairo.yy in *\) *)
(*   (\*   Format.eprintf "scale: %f %f@." xscale yscale *\) *)

(*   (\* let _text_bbox ctx str = *\) *)
(*   (\*   (\\* Using Christophe Troestler's notes at *\) *)
(*   (\*      http://archimedes.forge.ocamlcore.org/cairo/textextents.ml *\\) *\) *)
(*   (\*   let fe = Cairo.font_extents ctx in *\) *)
(*   (\*   let te = Cairo.text_extents ctx str in *\) *)
(*   (\*   let x = 0.5 -. te.x_bearing -. (te.width /. 2.) *\) *)
(*   (\*   and y = 0.5 -. fe.descent +. (fe.baseline /. 2.) in *\) *)
(*   (\*   let pos = Pt.pt x y in *\) *)
(*   (\*   let mins = Pt.pt (x +. te.x_bearing) (y +. te.y_bearing) in *\) *)
(*   (\*   let maxs = Pt.(mins + pt te.width te.height) in *\) *)
(*   (\*   (pos, Bbox.box mins maxs) *\) *)

(*   let rec render (ctx : Gg.M3.t) fill_opt (cmd : Commands.t) = *)
(*     let open Vg in *)
(*     match cmd.desc with *)
(*     | Circle { center; radius } -> *)
(*         let path = P.circle center radius P.empty |> P.close in *)
(*         I.cut path fill_opt *)
(*     | Box { mins; maxs } -> I.cut (render_box mins maxs) fill_opt *)
(*     | Text { pos; text } -> *)
(*         if text.Ctext.str = "" then Vg.I.void *)
(*         else *)
(*           let base = text.Ctext.base in *)
(*           let box = text.Ctext.box in *)
(*           let delta = Pt.(base - Bbox.sw box) in *)
(*           let w = Bbox.width box in *)
(*           let h = Bbox.height box in *)
(*           let llc = Commands.text_position pos w h in *)
(*           let llc = Pt.(llc + delta) in *)
(*           Cairo.save ctx ; *)
(*           Cairo.move_to ctx (Pt.x llc) (Pt.y llc) ; *)
(*           Cairo.Scaled_font.set ctx text.Ctext.font ; *)
(*           Cairo.show_text ctx text.str ; *)
(*           Cairo.restore ctx ; *)
(*           perform_stroke_and_fill_opt ctx fill_opt *)
(*     | Style { style; cmd } -> *)
(*         let bbox = Commands.Bbox.of_command cmd in *)
(*         let adjusted_fill = adjust_fill_to_bbox bbox style.Style.fill in *)
(*         Cairo.save ctx ; *)
(*         set_pattern ctx style.Style.stroke ; *)
(*         (match style.dash with *)
(*         | None -> () *)
(*         | Some patt -> Cairo.set_dash ctx patt) ; *)
(*         (match style.width with *)
(*         | None -> () *)
(*         | Some width -> Cairo.set_line_width ctx width) ; *)
(*         render ctx adjusted_fill cmd ; *)
(*         Cairo.restore ctx *)
(*     | Segment { p1; p2 } -> *)
(*         let path = P.empty |> P.sub p1 |> P.line p2 |> P.close in *)
(*         I.cut path fill_opt *)
(*     | Bezier { p1; c1; p2; c2 } -> *)
(*         let open Vg.P in *)
(*         let path = empty |> sub p1 |> ccurve c1 c2 p2 in *)
(*         I.cut path fill_opt *)
(*     | Image { pos = _; image } -> *)
(*         Cairo.save ctx ; *)
(*         let bbox = Commands.Bbox.of_command cmd in *)
(*         let () = *)
(*           Cairo.rectangle *)
(*             ctx *)
(*             (Pt.x (Bbox.sw bbox)) *)
(*             (Pt.y (Bbox.sw bbox)) *)
(*             ~w:(Bbox.width bbox) *)
(*             ~h:(Bbox.height bbox) *)
(*         in *)
(*         let () = Cairo.clip ctx in *)
(*         let xsize = Image.xsize image in *)
(*         let ysize = Image.ysize image in *)
(*         let pixels = *)
(*           Bigarray.reshape_2 *)
(*             (Bigarray.genarray_of_array1 (Image.pixels image)) *)
(*             xsize *)
(*             ysize *)
(*         in *)
(*         let surf = *)
(*           Cairo.Image.create_for_data32 ~w:xsize ~h:ysize ~alpha:false pixels *)
(*         in *)
(*         let patt = Cairo.Pattern.create_for_surface surf in *)
(*         let matrix = *)
(*           Cairo.Matrix.init_translate *)
(*             (-.Pt.x (Bbox.sw bbox)) *)
(*             (-.Pt.y (Bbox.sw bbox)) *)
(*         in *)
(*         Cairo.Pattern.set_matrix patt matrix ; *)
(*         Cairo.set_source ctx patt ; *)
(*         Cairo.paint ctx ; *)
(*         Cairo.restore ctx *)
(*     | Anchor { pt = _; name = _ } -> Vg.I.void *)
(*     | Rotate { radians; cmd } -> *)
(*         render (Gg.M3.mul (Gg.M3.rot2 radians) ctx) fill_opt cmd *)
(*     | Translate { v; cmd } -> *)
(*         render (Gg.M3.mul (Gg.M3.move2 v) ctx) fill_opt cmd *)
(*     | Scale { xs; ys; cmd } -> *)
(*         render (Gg.M3.mul (Gg.M3.scale2 (Gg.V2.v xs ys)) ctx) fill_opt cmd *)
(*     | Wrap subcommands -> *)
(*         List.fold_left *)
(*           (fun acc cmd -> I.blend (render ctx fill_opt cmd) acc) *)
(*           I.void *)
(*           subcommands *)

(*   (\* let render ctx cmd = render ctx None cmd *\) *)
(* end *)
