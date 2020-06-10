(* Handling text. *)

type font_spec =
  { family : string; slant : Cairo.slant; weight : Cairo.weight; size : float }

type t =
  { str : string;
    spec : font_spec;
    font : [ `Toy ] Cairo.Scaled_font.t;
    base : Pt.t;
    box : Bbox.t
  }

let face_table = Hashtbl.create 10

let font_table = Hashtbl.create 10

let font_spec ?(slant = Cairo.Upright) ?(weight = Cairo.Normal) family =
  match Hashtbl.find_opt face_table (slant, weight, family) with
  | None ->
      let face = Cairo.Font_face.create ~family slant weight in
      Hashtbl.add face_table (slant, weight, family) face ;
      face
  | Some face -> face

let font family slant weight size =
  match Hashtbl.find_opt font_table (family, slant, weight, size) with
  | None ->
      let face = font_spec ~slant ~weight family in
      let mat1 = Cairo.Matrix.init_scale size ~-.size in
      let mat2 = Cairo.Matrix.init_identity () in
      let opts = Cairo.Font_options.create () in
      Cairo.Scaled_font.create face mat1 mat2 opts
  | Some font -> font

let create ?(size = 10.0) ?(family = "fixed") ?(slant = Cairo.Upright)
    ?(weight = Cairo.Normal) str =
  let font = font family slant weight size in
  let fe = Cairo.Scaled_font.extents font in
  let te = Cairo.Scaled_font.text_extents font str in
  let x = 0.5 -. te.x_bearing -. (te.width /. 2.)
  and y = 0.5 -. fe.descent +. (fe.baseline /. 2.) in
  let base = Pt.pt x y in
  let mins = Pt.pt (x +. te.x_bearing) (y +. te.y_bearing) in
  let maxs = Pt.(mins + pt te.width te.height) in
  let box = Bbox.box mins maxs in
  let spec = { family; slant; weight; size } in
  { str; spec; font; base; box }
