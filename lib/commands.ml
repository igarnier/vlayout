module Name = struct
  type t = string

  let compare = String.compare

  let pp = Format.pp_print_string
end

module Name_map = Map.Make (Name)

type name = Name.t

(** A [position] is an absolute coordinate together with a position relative to
    that coordinate. *)
type position = { pos : Pt.t; relative_position : relative_position }

and relative_position =
  | Absolute
  | North
  | West
  | South
  | East
  | SouthWest
  | SouthEast
  | NorthWest
  | NorthEast

let absolute = Absolute

let n = North

let w = West

let s = South

let e = South

let sw = SouthWest

let se = SouthEast

let nw = NorthWest

let ne = NorthEast

let point_of_position { pos; _ } = pos

let pp_position fmtr { pos; relative_position } =
  match relative_position with
  | Absolute -> Format.fprintf fmtr "Absolute(%a)" Pt.pp pos
  | North -> Format.fprintf fmtr "North(%a)" Pt.pp pos
  | West -> Format.fprintf fmtr "West(%a)" Pt.pp pos
  | South -> Format.fprintf fmtr "South(%a)" Pt.pp pos
  | East -> Format.fprintf fmtr "East(%a)" Pt.pp pos
  | SouthWest -> Format.fprintf fmtr "SouthWest(%a)" Pt.pp pos
  | SouthEast -> Format.fprintf fmtr "SouthEast(%a)" Pt.pp pos
  | NorthWest -> Format.fprintf fmtr "NorthWest(%a)" Pt.pp pos
  | NorthEast -> Format.fprintf fmtr "NorthEast(%a)" Pt.pp pos

(* Arrows *)
module Arrow = struct
  type style =
    { startp : float;
      (* [0,1] *)
      endp : float;
      (* [0,1] *)
      arrowp : float;
      (* [0,1] *)
      legs : float;
      (* legs length, >= 0 *)
      angle : float (* legs angle *)
    }

  type t =
    { start : name;
      (* Name_sig of starting point (see Anchor)*)
      finish : name;
      (* Name_sig of finishing point *)
      style : style;
      smart : bool
          (* A smart arrow will (try to) avoid command layout boxes (Cmd)
           * except those from which it starts and to which it ends.
           * TODO: in the future we might want to allow more options. *)
    }

  let style ~startp ~endp ~arrowp ~legs ~angle =
    { startp; endp; arrowp; legs; angle }

  let pp_style fmtr { startp; endp; arrowp; legs; angle } =
    Format.fprintf
      fmtr
      "{startp=%f; endp=%f; arrowp=%f; legs=%f; angle=%f}"
      startp
      endp
      arrowp
      legs
      angle

  let pp fmtr { start; finish; style; smart } =
    Format.fprintf
      fmtr
      "{name=%s; finish=%s; style=%a; smart=%b}"
      start
      finish
      pp_style
      style
      smart

  let make_arrow_head ~legs_length ~legs_angle =
    let hangle = 0.5 *. legs_angle in
    let x = cos hangle *. legs_length in
    let y = sin hangle *. legs_length in
    (Pt.pt ~-.x y, Pt.pt ~-.x ~-.y)

  let default_style =
    { startp = 0.0;
      endp = 1.0;
      arrowp = 1.0;
      legs = 10.0;
      angle = Tools.pi *. 0.2
    }

  let mid_style =
    { startp = 0.05;
      endp = 0.95;
      arrowp = 0.5;
      legs = 10.0;
      angle = Tools.pi *. 0.2
    }
end

type arrow = Arrow.t

type hposition = [ `Hcentered | `Bottom | `Top ]

type vposition = [ `Vcentered | `Left | `Right ]

type framing =
  | Scale_to_frame of { frame : Bbox.t }
  | Preserve_aspect of { frame : Bbox.t }

let pp_hposition fmtr (hpos : hposition) =
  let open Format in
  match hpos with
  | `Hcentered -> pp_print_string fmtr "Hcentered"
  | `Bottom -> pp_print_string fmtr "Bottom"
  | `Top -> pp_print_string fmtr "Top"

let pp_vposition fmtr (vpos : vposition) =
  let open Format in
  match vpos with
  | `Vcentered -> pp_print_string fmtr "Vcentered"
  | `Left -> pp_print_string fmtr "Left"
  | `Right -> pp_print_string fmtr "Right"

let pp_framing fmtr (framing : framing) =
  let open Format in
  match framing with
  | Scale_to_frame { frame } -> fprintf fmtr "Scale_to_frame(%a)" Bbox.pp frame
  | Preserve_aspect { frame } ->
      fprintf fmtr "Preserve_aspect(%a)" Bbox.pp frame

type points = Pt.t list Name_map.t

type command = { uid : int; desc : desc; bbox : Bbox.t; points : points }

and desc =
  | Circle of { center : Pt.t; radius : float }
  | Box of { mins : Pt.t; maxs : Pt.t }
  | Text of { pos : position; text : Ctext.t }
  | Style of { style : Style.t; cmd : command }
  | Segment of { p1 : Pt.t; p2 : Pt.t }
  | Bezier of { p1 : Pt.t; c1 : Pt.t; p2 : Pt.t; c2 : Pt.t }
  | Image of { pos : Pt.t; image : Image.t }
  | Rotate of { radians : float; cmd : command }
  | Translate of { v : Pt.t; cmd : command }
  | Scale of { xs : float; ys : float; cmd : command }
  | Group of command list

type t = command

let bbox { bbox; _ } = bbox

let fresh =
  let acc = ref (-1) in
  fun () ->
    incr acc ;
    !acc

module Helpers = struct
  (* Compute the lower left corner of a box of dimensions (w, h) so that
     [pos] is at [relative_position] of that box. *)
  let base_of_positioned_box h w { pos; relative_position } =
    let x = Pt.x pos and y = Pt.y pos in
    match relative_position with
    | Absolute -> pos
    (* x = base_x + w / 2, y = base_y + h *)
    | North -> Pt.pt (x -. (w *. 0.5)) (y -. h)
    (* x = base_x, y = base_y + h / 2 *)
    | West -> Pt.pt x (y -. (h *. 0.5))
    (* x = base_x + w /2, y = base_y *)
    | South -> Pt.pt (x -. (w *. 0.5)) y
    (* x = base_x + w, y = base_y + h / 2 *)
    | East -> Pt.pt (x -. w) (y -. (h *. 0.5))
    (* x = base_x, y = base_y  *)
    | SouthWest -> Pt.pt x y
    (* x = base_x + w, y = base_y *)
    | SouthEast -> Pt.pt (x -. w) y
    (* x = base_x, y = base_y + h *)
    | NorthWest -> Pt.pt x (y -. h)
    (* x = base_x + w, y = base_y + h*)
    | NorthEast -> Pt.pt (x -. w) (y -. h)

  let text_position pos width height = base_of_positioned_box height width pos

  let ubezier_control_points ~p1 ~p2 ~radians =
    let delta = Pt.sub p2 p1 and n_angle = Pt.angle_of_vec (p1, p2) in
    let hdist = 0.3 *. Pt.norm delta in
    let out_angle = n_angle +. radians in
    let in_angle = n_angle -. radians in
    let vec1 = Pt.scale (Pt.pt (cos out_angle) (sin out_angle)) hdist in
    let vec2 = Pt.scale (Pt.pt (cos in_angle) (sin in_angle)) ~-.hdist in
    let c1 = Pt.add p1 vec1 in
    let c2 = Pt.add p2 vec2 in
    (c1, c2)

  let mid a b = Pt.barycenter a b

  (* midpoint of a Bezier curve *)
  let bezier_midpoint p1 c1 p2 c2 =
    let mid1 = mid p1 c1 and mid2 = mid c1 c2 and mid3 = mid c2 p2 in
    let mid12 = mid mid1 mid2 and mid23 = mid mid2 mid3 in
    mid mid12 mid23
end

module Alignment = struct
  (* given box1, compute displacement vector for box2 to
   * be aligned the right of box1, in a centered way. *)
  let align_horiz_centered_vector deltax box1 box2 =
    let h1 = Bbox.height box1 and h2 = Bbox.height box2 in
    let deltay = (h1 -. h2) *. 0.5 in
    Pt.(Bbox.se box1 - Bbox.sw box2 + Pt.pt deltax deltay)

  (* given box1, compute displacement vector for box2 to
   * be aligned the right of box1, so that their bottoms sit on the same line. *)
  let align_horiz_bottom_vector deltax box1 box2 =
    Pt.(Bbox.se box1 - Bbox.sw box2 + Pt.pt deltax 0.0)

  (* given box1, compute displacement vector for box2 to
   * be aligned the right of box1, so that their top sit on the same line. *)
  let align_horiz_top_vector deltax box1 box2 =
    Pt.(Bbox.ne box1 - Bbox.nw box2 + Pt.pt deltax 0.0)

  (* given box1, compute displacement vector for box2 to  *)
  (* be aligned at the bottom of box1, in a centered way. *)
  let align_vert_centered_vector deltay box1 box2 =
    let w1 = Bbox.width box1 and w2 = Bbox.width box2 in
    let deltax = (w1 -. w2) *. 0.5 in
    Pt.(Bbox.sw box1 - Bbox.nw box2 + Pt.pt deltax ~-.deltay)

  let align_vert_left_vector deltay box1 box2 =
    Pt.(Bbox.sw box1 - Bbox.nw box2 + Pt.pt 0.0 ~-.deltay)

  let align_vert_right_vector deltay box1 box2 =
    Pt.(Bbox.se box1 - Bbox.ne box2 + Pt.pt 0.0 ~-.deltay)

  let horiz_align (pos : hposition) =
    match pos with
    | `Hcentered -> align_horiz_centered_vector
    | `Bottom -> align_horiz_bottom_vector
    | `Top -> align_horiz_top_vector

  let vert_align (pos : vposition) =
    match pos with
    | `Vcentered -> align_vert_centered_vector
    | `Left -> align_vert_left_vector
    | `Right -> align_vert_right_vector
end

module Compute_bbox = struct
  let of_circle center radius =
    let x = Pt.x center and y = Pt.y center in
    Bbox.box
      (Pt.pt (x -. radius) (y -. radius))
      (Pt.pt (x +. radius) (y +. radius))

  let of_text pos text =
    let width = Bbox.width text.Ctext.box in
    let height = Bbox.height text.Ctext.box in
    let base = Helpers.text_position pos width height in
    Bbox.box base Pt.(base + pt width height)

  let of_segment p1 p2 =
    let x1 = Pt.x p1 and y1 = Pt.y p1 in
    let x2 = Pt.x p2 and y2 = Pt.y p2 in
    Bbox.box (Pt.pt (min x1 x2) (min y1 y2)) (Pt.pt (max x1 x2) (max y1 y2))

  let of_bezier p1 c1 p2 c2 =
    let open Bbox in
    join (box p1 c1) (box p2 c2)
end

let mktag desc bbox points = { uid = fresh (); desc; bbox; points }

let circle center radius : command =
  mktag
    (Circle { center; radius })
    (Compute_bbox.of_circle center radius)
    Name_map.empty

let box ~mins ~maxs =
  mktag (Box { mins; maxs }) (Bbox.box mins maxs) Name_map.empty

let text ~size relpos pt text =
  let text = Ctext.create ~size text in
  let pos = { pos = pt; relative_position = relpos } in
  mktag (Text { pos; text }) (Compute_bbox.of_text pos text) Name_map.empty

let style style cmd = mktag (Style { style; cmd }) cmd.bbox cmd.points

let segment p1 p2 =
  mktag (Segment { p1; p2 }) (Compute_bbox.of_segment p1 p2) Name_map.empty

let bezier ~p1 ~c1 ~p2 ~c2 =
  mktag
    (Bezier { p1; c1; p2; c2 })
    (Compute_bbox.of_bezier p1 c1 p2 c2)
    Name_map.empty

let ubezier ~p1 ~p2 ~radians =
  let (c1, c2) = Helpers.ubezier_control_points ~p1 ~p2 ~radians in
  bezier ~p1 ~c1 ~p2 ~c2

let image ~pos image =
  mktag
    (Image { pos; image })
    (Bbox.translate pos (Image.bbox image))
    Name_map.empty

let anchor name pt cmd =
  let points =
    Name_map.update
      name
      (function None -> Some [pt] | Some list -> Some (pt :: list))
      cmd.points
  in
  { cmd with points }

let apply_matrix mat points =
  Name_map.map (fun ps -> List.map (fun p -> Gg.P2.tr mat p) ps) points

let rotate ~radians cmd =
  let points = apply_matrix (Gg.M3.rot2 radians) cmd.points in
  mktag (Rotate { radians; cmd }) (Bbox.rotate radians cmd.bbox) points

let translate v cmd =
  let points = apply_matrix (Gg.M3.move2 v) cmd.points in
  mktag (Translate { v; cmd }) (Bbox.translate v cmd.bbox) points

let scale ~xs ~ys cmd =
  let points = apply_matrix (Gg.M3.scale2 (Gg.V2.v xs ys)) cmd.points in
  mktag (Scale { xs; ys; cmd }) (Bbox.scale xs ys cmd.bbox) points

let merge map1 map2 =
  Name_map.union (fun _name p1 p2 -> Some (List.rev_append p1 p2)) map1 map2

let group = function
  | [] -> failwith "Commands.group: empty list"
  | cmds ->
      let (box, points) =
        List.fold_left
          (fun (box, points) cmd ->
            (Bbox.join box cmd.bbox, merge points cmd.points))
          (Bbox.empty, Name_map.empty)
          cmds
      in
      mktag (Group cmds) box points

(* invariant: must preserve order of commands *)
let halign l align_function =
  let rec halign_aux l acc =
    match l with
    | [] -> List.rev acc
    | [elt] -> List.rev (elt :: acc)
    | cmd1 :: cmd2 :: l ->
        let v = align_function cmd1.bbox cmd2.bbox in
        let command = translate v cmd2 in
        halign_aux (command :: l) (cmd1 :: acc)
  in
  halign_aux l []

let hbox ~dx ?(pos = `Hcentered) cmds =
  let aligned = halign cmds (Alignment.horiz_align pos dx) in
  group aligned

(* invariant: must preserve order of commands *)
let valign l align_function =
  let rec valign_aux l acc =
    match l with
    | [] -> List.rev acc
    | [elt] -> List.rev (elt :: acc)
    | cmd1 :: cmd2 :: l ->
        let v = align_function cmd1.bbox cmd2.bbox in
        let command = translate v cmd2 in
        valign_aux (command :: l) (cmd1 :: acc)
  in
  valign_aux l []

let vbox ~dy ?(pos = `Vcentered) cmds =
  let aligned = valign cmds (Alignment.vert_align pos dy) in
  group aligned

let place relative_position point cmd =
  let bbox = cmd.bbox in
  let width = Bbox.width bbox in
  let height = Bbox.height bbox in
  let base =
    Helpers.base_of_positioned_box
      height
      width
      { pos = point; relative_position }
  in
  let v = Pt.(base - Bbox.sw bbox) in
  translate v cmd

let frame framing cmd =
  let bbox = cmd.bbox in
  match framing with
  | Scale_to_frame { frame } ->
      let frame_width = Bbox.width frame in
      let frame_height = Bbox.height frame in
      let pw = Bbox.width bbox in
      let ph = Bbox.height bbox in
      let xx = frame_width /. pw in
      let yy = frame_height /. ph in
      let (x0, y0) = Gg.V2.to_tuple (Bbox.sw bbox) in
      let x0 = x0 *. xx in
      let y0 = y0 *. yy in
      translate Pt.(sub (Bbox.sw frame) (pt x0 y0)) (scale ~xs:xx ~ys:yy cmd)
  | Preserve_aspect { frame } ->
      let frame_width = Bbox.width frame in
      let frame_height = Bbox.height frame in
      let pw = Bbox.width bbox in
      let ph = Bbox.height bbox in
      let xx = frame_width /. pw in
      let yy = frame_height /. ph in
      (* compute smallest scaling such that the figure fits in the
               frame *)
      let (scaling, direction) =
        if xx >= 1.0 && yy >= 1.0 then
          if xx < yy then (1. /. xx, `Horizontal) else (1. /. yy, `Vertical)
        else if xx < yy then (xx, `Horizontal)
        else (yy, `Vertical)
      in
      let (point, relative_position) =
        match direction with
        | `Horizontal -> (Bbox.w frame, West)
        | `Vertical -> (Bbox.s frame, South)
      in
      place relative_position point (scale ~xs:scaling ~ys:scaling cmd)

let arrow ~style ~start ~finish =
  let (left_leg, right_leg) =
    Arrow.make_arrow_head
      ~legs_length:style.Arrow.legs
      ~legs_angle:style.Arrow.angle
  in
  let vec = Pt.sub finish start in
  let vec_angle = Pt.angle_of_vec (start, finish) in
  let shaft_start = Pt.add start (Pt.scale vec style.startp) in
  let shaft_finish = Pt.add start (Pt.scale vec style.endp) in
  let arrow_pos = Pt.add start (Pt.scale vec style.arrowp) in
  let left_leg = Pt.add arrow_pos (Pt.rotate_vector vec_angle left_leg) in
  let right_leg = Pt.add arrow_pos (Pt.rotate_vector vec_angle right_leg) in
  group
    [ segment shaft_start shaft_finish;
      segment arrow_pos left_leg;
      segment arrow_pos right_leg ]

(* let make_arrow legs_length legs_angle start finish = *)
let arrow_curvy style ~start ~finish ~radians =
  if not (List.mem style.Arrow.arrowp [0.0; 0.5; 1.0]) then
    failwith "Commands.make_arrow_curvy: arrowp must be either 0, 0.5 or 1"
  else
    let (left_leg, right_leg) =
      Arrow.make_arrow_head ~legs_length:style.legs ~legs_angle:style.angle
    in
    let vec = Pt.sub finish start in
    let vec_angle = Pt.angle_of_vec (start, finish) in
    let shaft_start = Pt.add start (Pt.scale vec style.startp) in
    let shaft_finish = Pt.add start (Pt.scale vec style.endp) in
    let (c1, c2) =
      Helpers.ubezier_control_points ~p1:shaft_start ~p2:shaft_finish ~radians
    in
    let (arrow_pos, rot_angle) =
      if style.arrowp = 0.0 then (shaft_start, vec_angle +. radians)
      else if style.arrowp = 0.5 then
        let m = Helpers.bezier_midpoint shaft_start c1 shaft_finish c2 in
        (m, vec_angle)
      else
        (* style.arrowp = 1.0 *)
        (shaft_finish, Tools.pi -. (vec_angle +. radians))
    in
    let left_leg = Pt.add arrow_pos (Pt.rotate_vector rot_angle left_leg) in
    let right_leg = Pt.add arrow_pos (Pt.rotate_vector rot_angle right_leg) in
    group
      [ bezier ~p1:shaft_start ~c1 ~p2:shaft_finish ~c2;
        segment arrow_pos left_leg;
        segment arrow_pos right_leg ]

(* multi-segments arrows *)

let rec to_segments points =
  match points with
  | [] -> failwith "Commands.Arrow.to_segments: not enough points"
  | [_] -> []
  | p1 :: p2 :: tl -> (p1, p2) :: to_segments (p2 :: tl)

let segmented_arrow style points =
  let segments = to_segments points in
  let segments =
    match segments with
    | [] -> failwith "Commands.Arrow.segmented_arrow: not enough points"
    | (p1, p2) :: tl ->
        let vec = Pt.(p2 - p1) in
        let effective_start = Pt.add p1 (Pt.scale vec style.Arrow.startp) in
        (effective_start, p2) :: tl
  in
  match List.rev segments with
  | [] -> failwith "Commands.Arrow.segmented_arrow: not enough points"
  | (pn, pn') :: lt ->
      let start = pn and finish = pn' in
      let (left_leg, right_leg) =
        Arrow.make_arrow_head ~legs_length:style.legs ~legs_angle:style.angle
      in
      let vec = Pt.sub finish start in
      let vec_angle = Pt.angle_of_vec (start, finish) in
      let shaft_finish = Pt.add start (Pt.scale vec style.endp) in
      let arrow_pos = Pt.add start (Pt.scale vec style.arrowp) in
      let left_leg = Pt.add arrow_pos (Pt.rotate_vector vec_angle left_leg) in
      let right_leg = Pt.add arrow_pos (Pt.rotate_vector vec_angle right_leg) in
      let arrow =
        [ segment pn shaft_finish;
          segment arrow_pos left_leg;
          segment arrow_pos right_leg ]
      in
      let segments = List.map (fun (p1, p2) -> segment p1 p2) lt in
      group (segments @ arrow)

(* ------------------------------------------------------------ *)

let rec pp fmtr c =
  let open Format in
  match c.desc with
  | Circle { center; radius } ->
      fprintf fmtr "Circle(%a, %f)" Pt.pp center radius
  | Box { mins; maxs } -> fprintf fmtr "Box(%a, %a)" Pt.pp mins Pt.pp maxs
  | Text { pos; text } ->
      fprintf fmtr "Text(%a, %s)" pp_position pos text.Ctext.str
  | Style { style; cmd } -> fprintf fmtr "Style(%a, %a)" Style.pp style pp cmd
  | Segment { p1; p2 } -> fprintf fmtr "Segment(%a, %a)" Pt.pp p1 Pt.pp p2
  | Bezier { p1; c1; p2; c2 } ->
      fprintf fmtr "Bezier(%a, %a, %a, %a)" Pt.pp p1 Pt.pp c1 Pt.pp p2 Pt.pp c2
  | Image { pos; image } ->
      fprintf
        fmtr
        "Image(%a, %d x %d)"
        Pt.pp
        pos
        (Image.xsize image)
        (Image.ysize image)
  | Rotate { radians; cmd } -> fprintf fmtr "Rotate(%f, %a)" radians pp cmd
  | Translate { v; cmd } -> fprintf fmtr "Translate(%a, %a)" Pt.pp v pp cmd
  | Scale { xs; ys; cmd } -> fprintf fmtr "Scale(%f, %f, %a)" xs ys pp cmd
  | Group subcommands -> fprintf fmtr "Group(%a)" pp_list subcommands

and pp_list fmtr cl =
  Format.pp_print_list
    ~pp_sep:(fun fmtr () -> Format.fprintf fmtr "; ")
    pp
    fmtr
    cl

let text_position pos width height =
  Helpers.base_of_positioned_box height width pos

let center_to_page ~w ~h cmd =
  let b = cmd.bbox in
  let boxw = Bbox.width b in
  let boxh = Bbox.height b in
  let deltah = 0.5 *. (h -. boxh) in
  let deltaw = 0.5 *. (w -. boxw) in
  translate (Pt.sub (Pt.pt deltaw deltah) (Bbox.sw b)) cmd

let bind name cmd f =
  let pts = Option.value (Name_map.find_opt name cmd.points) ~default:[] in
  f pts

let bind_each name cmd f =
  let pts = Option.value (Name_map.find_opt name cmd.points) ~default:[] in
  group (List.map f pts)

(* let anchor_of c = *)
(*   match c.desc with *)
(*   | Circle { center; _ } -> center *)
(*   | Box { mins; maxs } -> mid mins maxs *)
(*   | Text { pos; _ } -> point_of_position pos *)
(*   | Segment { p1; p2 } -> mid p1 p2 *)
(*   | Bezier { p1; c1; p2; c2 } -> bezier_midpoint p1 c1 p2 c2 *)
(*   | Image { pos; _ } -> pos *)
(*   | Rotate _ | Translate _ | Scale _ | Group _ -> *)
(*       failwith *)
(*         "Commands.anchor_of: unable to compute anchor for linear \ *)
(*          transformations" *)

(* let _bouquet_of_segments start finish names = *)
(*   match names with *)
(*   | [] -> failwith "Commands.bouquet_of_segments: empty list of names" *)
(*   | [name] -> *)
(*       let edge = segment ~p1:start ~p2:finish in *)
(*       [edge; declpt ~pt:(anchor_of edge) ~name] *)
(*   | _ -> *)
(*       let len = List.length names in *)
(*       let pi4 = Tools.pi *. 0.25 in *)
(*       let angles = Tools.interpolate ~-.pi4 pi4 (len - 1) in *)
(*       let l = List.combine names angles in *)
(*       List.fold_left *)
(*         (fun acc (name, angle) -> *)
(*           let edge = ubezier ~p1:start ~p2:finish ~radians in *)
(*           declpt ~pt:(anchor_of edge) ~name :: edge :: acc) *)
(*         [] *)
(*         l *)

(* let collect_declared_points cmd = *)
(*   let rec traverse cmd mat map = *)
(*     match cmd.desc with *)
(*     | Circle _ | Box _ | Text _ | Style _ | Segment _ | Bezier _ | Image _ -> *)
(*         map *)
(*     | Anchor { pt; name } -> *)
(*         let global_pt = Gg.P2.tr mat pt in *)
(*         Name_map.add name global_pt map *)
(*     | Rotate { radians; cmd } -> *)
(*         let rot = Gg.M3.rot2 radians in *)
(*         let mat = Gg.M3.mul mat rot in *)
(*         traverse cmd mat map *)
(*     | Translate { v; cmd } -> *)
(*         let tra = Gg.M3.move2 v in *)
(*         let mat = Gg.M3.mul mat tra in *)
(*         traverse cmd mat map *)
(*     | Scale { xs; ys; cmd } -> *)
(*         let sca = Gg.M3.scale2 (Pt.pt xs ys) in *)
(*         let mat = Gg.M3.mul mat sca in *)
(*         traverse cmd mat map *)
(*     | Group subcommands -> traverse_list subcommands mat map *)
(*   and traverse_list cmds mat map = *)
(*     List.fold_left (fun map c -> traverse c mat map) map cmds *)
(*   in *)
(*   traverse cmd Gg.M3.id Name_map.empty *)

(* Box autolayout *)
(* let place ~pos cmd = *)
(*   let bbox = Bbox.of_command cmd in *)
(*   let width = Bbox.width bbox in *)
(*   let height = Bbox.height bbox in *)
(*   let base = base_of_positioned_box height width pos in *)
(*   let v = Pt.(base - Bbox.sw bbox) in *)
(*   translate v cmd *)

(* let pp_framing fmtr = function
 *   | Scale_to_frame { frame } ->
 *       Format.fprintf fmtr "Scale_to_frame(%a)" Bbox.pp frame
 *   | Preserve_aspect { frame } ->
 *       Format.fprintf fmtr "Preserve_aspect(%a)" Bbox.pp frame *)

(*     | Frame { framing; _ } -> (
 *         match framing with
 *         | Scale_to_frame { frame } | Preserve_aspect { frame } -> frame )
 *
 *     | Frame { framing; cmd } ->
 *         fprintf fmtr "Frame(%a, %a)" pp_framing framing pp cmd)
 *
 *   | Frame { framing; _ } -> (
 *       match framing with
 *       | Scale_to_frame { frame } | Preserve_aspect { frame } ->
 *           Bbox.center frame )
 *
 * let frame framing cmd = mktag (Frame { framing; cmd }) *)

(* an arrow makes only sense wrt point declared in a sublayout *)

(* A public interface to build stuff in [layout] *)

(* let smart_arrow *)
(*     ~start:(s : name) *)
(*     ~finish:(f : name) *)
(*     ~sty:(sty : Arrow.style) cmd = *)
(*   Arrow(Arrow.({ start = s; finish = f; style = sty; smart = true }), cmd) *)

(* Layout algorithm *)

(* horizontal alignement functions *)

(* This function performs the layout. It produces:
     . a list of named commands ((t * int option) list) where the name
      corresponds to the named_command they belong to
     . a bounding box for the list of commands (Bbox.t)
  *)
(* let rec emit_commands_with_bbox (l : layout) : t * Bbox.t = *)
(*   match l with *)
(*   | Cmd cmd -> *)
(*       (\* let cmd = crop cmd in *\) *)
(*       (\* let named_cmds = tag cmds named_command.cmd_name in *\) *)
(*       (cmd, Bbox.of_command cmd) *)
(*   | Hbox { pos; deltax; layouts } -> *)
(*       let ls = List.map emit_commands_with_bbox layouts in *)
(*       let aligned = halign ls (horiz_align pos deltax) in *)
(*       let (commands, boxes) = List.split aligned in *)
(*       let bbox = List.fold_left Bbox.join Bbox.empty boxes in *)
(*       let cmds = group commands in *)
(*       (cmds, bbox) *)
(*   | Vbox { pos; deltay; layouts } -> *)
(*       let ls = List.map emit_commands_with_bbox layouts in *)
(*       let aligned = valign ls (vert_align pos deltay) in *)
(*       let (commands, boxes) = List.split aligned in *)
(*       let bbox = List.fold_left Bbox.join Bbox.empty boxes in *)
(*       let cmds = group commands in *)
(*       (cmds, bbox) *)
(*   | Arrow { arrow; layout } -> *)
(*       let { Arrow.start; finish; style; _ } = arrow in *)
(*       let (cmd, bbox) = emit_commands_with_bbox layout in *)
(*       let map = collect_declared_points cmd in *)
(*       let s = *)
(*         try Name_map.find start map *)
(*         with Not_found -> *)
(*           Format.eprintf *)
(*             "Commands.emit_commands_with_bbox: arrow start point %a was not \ *)
(*              declared\n" *)
(*             Name.pp *)
(*             start ; *)
(*           Format.eprintf "command:@.%a@." pp cmd ; *)
(*           raise Emit_error *)
(*       in *)
(*       let f = *)
(*         try Name_map.find finish map *)
(*         with Not_found -> *)
(*           Format.eprintf *)
(*             "Commands.emit_commands_with_bbox: arrow end point %a was not \ *)
(*              declared\n" *)
(*             Name.pp *)
(*             finish ; *)
(*           Format.eprintf "commands:@.%a@." pp cmd ; *)
(*           raise Emit_error *)
(*       in *)
(*       (\* if arr.Arrow.smart then *\) *)
(*       (\*   (match s_cmdbox, f_cmdbox with *\) *)
(*       (\*    | None, _ *\) *)
(*       (\*    | _, None -> *\) *)
(*       (\*      failwith "Commands.emit_commands_with_bbox: some endpoints of a smart arrow belong to an unnamed boxe" *\) *)
(*       (\*    | Some si, Some fi -> *\) *)
(*       (\*      (\\* partition commands by their tag *\\) *\) *)
(*       (\*      let fibers = fibers_from_list sublayout_cmds in *\) *)
(*       (\*      (\\* extract all commands whose tag is different from s_cmdbox and f_cmdbox and None *\\) *\) *)
(*       (\*      let fibers = *\) *)
(*       (\*        List.filter *\) *)
(*       (\*          (fun (tag, _) -> *\) *)
(*       (\*             tag <> None && tag <> s_cmdbox && tag <> f_cmdbox *\) *)
(*       (\*          ) fibers *\) *)
(*       (\*      in *\) *)
(*       (\*      (\\* compute bounding boxes for each tag *\\) *\) *)
(*       (\*      let bboxes = List.map (fun (tag, cmds) -> Bbox.of_commands cmds) fibers in *\) *)
(*       (\*      let solution = SmartPath.produce_path (2.0 ** (float (depth sublayout))) s f bboxes in *\) *)
(*       (\*      (\\* produce_path  *\\) *\) *)
(*       (\*      let cmds = tag (Arrow.segmented_arrow style solution) None in *\) *)
(*       (\*      (cmds @ sublayout_cmds,  Bbox.join (Bbox.of_commands cmds) bbox) *\) *)
(*       (\*   ) *\) *)
(*       (\* else *\) *)
(*       let arrow = Arrow.make_arrow ~style ~start:s ~finish:f in *)
(*       (group (cmd :: arrow), Bbox.join (Bbox.of_commands arrow) bbox) *)
(*   | Frame { framing; layout } -> ( *)
(*       let (cmd, bbox) = emit_commands_with_bbox layout in *)
(*       match framing with *)
(*       | Scale_to_frame { frame } -> *)
(*           let frame_width = Bbox.width frame in *)
(*           let frame_height = Bbox.height frame in *)
(*           let pw = Bbox.width bbox in *)
(*           let ph = Bbox.height bbox in *)
(*           let xx = frame_width /. pw in *)
(*           let yy = frame_height /. ph in *)
(*           let (x0, y0) = Gg.V2.to_tuple (Bbox.sw bbox) in *)
(*           let x0 = x0 *. xx in *)
(*           let y0 = y0 *. yy in *)
(*           ( translate *)
(*               ~v:Pt.(sub (Bbox.sw frame) (pt x0 y0)) *)
(*               (scale ~xs:xx ~ys:yy cmd), *)
(*             frame ) *)
(*       | Preserve_aspect { frame } -> *)
(*           let frame_width = Bbox.width frame in *)
(*           let frame_height = Bbox.height frame in *)
(*           let pw = Bbox.width bbox in *)
(*           let ph = Bbox.height bbox in *)
(*           let xx = frame_width /. pw in *)
(*           let yy = frame_height /. ph in *)
(*           (\* compute smallest scaling such that the figure fits in the *)
(*                frame *\) *)
(*           let (scaling, direction) = *)
(*             if xx >= 1.0 && yy >= 1.0 then *)
(*               if xx < yy then (1. /. xx, `Horizontal) else (1. /. yy, `Vertical) *)
(*             else if xx < yy then (xx, `Horizontal) *)
(*             else (yy, `Vertical) *)
(*           in *)
(*           let position = *)
(*             match direction with *)
(*             | `Horizontal -> { pos = Bbox.w frame; relative_position = West } *)
(*             | `Vertical -> { pos = Bbox.s frame; relative_position = South } *)
(*           in *)
(*           (place ~pos:position (scale ~xs:scaling ~ys:scaling cmd), frame)) *)

(* let emit_commands l = *)
(*   let (cmds, _bbox) = emit_commands_with_bbox l in *)
(*   cmds *)
