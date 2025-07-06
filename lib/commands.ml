(** The module type of names, used to refer to points, anchors and so on. *)
module type Name_sig = sig
  type t

  val compare : t -> t -> int

  val pp : Format.formatter -> t -> unit
end

(** The module type of drawing commands. *)
module type S = sig
  type name

  (** A [position] consistsof an assignement of the coordinates [pos] to the
      symbolic position [relpos] of some bounding box. *)
  type position = { pos : Pt.t; relpos : relpos }

  and relpos =
    | Absolute
    | North
    | West
    | South
    | East
    | SouthWest
    | SouthEast
    | NorthWest
    | NorthEast

  (** The type of commands. Best not used directly. *)
  type t = { uid : int; desc : desc }

  and desc =
    | Circle of { center : Pt.t; radius : float }
    | Box of { mins : Pt.t; maxs : Pt.t }
    | Text of { pos : position; text : Ctext.t }
    | Style of { style : Style.t; cmd : t }
    | Segment of { p1 : Pt.t; p2 : Pt.t }
    | Bezier of { p1 : Pt.t; c1 : Pt.t; p2 : Pt.t; c2 : Pt.t }
    | Image of { pos : Pt.t; image : Image.t }
    | DeclPt of { pt : Pt.t; name : name }
    | Rotate of { radians : float; cmd : t }
    | Translate of { v : Pt.t; cmd : t }
    | Scale of { xs : float; ys : float; cmd : t }
    | Wrap of t list

  type alias = t

  val text_position : position -> float -> float -> Pt.t

  module Bbox : sig
    include Bbox.S

    val of_command : alias -> t

    val of_commands : alias list -> t
  end

  module Arrow : sig
    type style =
      { startp : float;  (** [0,1] *)
        endp : float;  (** [0,1] *)
        arrowp : float;  (** [0,1] *)
        legs : float;  (** legs length, >= 0 *)
        angle : float  (** legs angle *)
      }

    val default_style : style

    val mid_style : style

    val mkarrow : style:style -> start:Pt.t -> finish:Pt.t -> t list

    val mkarrow_curvy :
      style:style -> start:Pt.t -> finish:Pt.t -> angle:float -> t list

    val mk_multisegment_arrow : style:style -> points:Pt.t list -> t list
  end

  (** Draws a circle with [center] and [radius] parameters. *)
  val circle : center:Pt.t -> radius:float -> t

  (** Draws a box with [mins] as bottom-left corner and [maxs] as the top-right
      corner. *)
  val box : mins:Pt.t -> maxs:Pt.t -> t

  (** Draws text at position [pos], with size [float] and [text]. *)
  val text : pos:position -> size:float -> text:string -> t

  (** Applies a [style] to the given subcommand. *)
  val style : style:Style.t -> t -> t

  (** Draws a segment from [p1] to [p2] *)
  val segment : p1:Pt.t -> p2:Pt.t -> t

  (** Draws a Bezier curve from [p1] to [p2] with control points respectively
      [c1] and [c2]. *)
  val bezier : p1:Pt.t -> c1:Pt.t -> p2:Pt.t -> c2:Pt.t -> t

  (** Draws a Bezier curve with ingoing and outgoing angles specified by
      [angle]. *)
  val ubezier : p1:Pt.t -> p2:Pt.t -> angle:float -> t

  (** Draws an image. *)
  val image : pos:Pt.t -> image:Image.t -> t

  (** Declares an anchor named [name] at position [pt]. *)
  val declpt : pt:Pt.t -> name:name -> t

  (** Rotates the subcommand by [radians]. *)
  val rotate : radians:float -> t -> t

  (** Translates the subcommand by [v]. *)
  val translate : v:Pt.t -> t -> t

  (** Scales the x and y components of the subcommand by resp. [xs] and [ys]. *)
  val scale : xs:float -> ys:float -> t -> t

  (** Wraps a list of subcommands into one. *)
  val wrap : t list -> t

  (** Manual positioning of a the subcommand at the given position.. *)
  val place : pos:position -> t -> t

  (** Pretty-printing of commands *)
  val pp : Format.formatter -> t -> unit

  val center_to_page : float * float -> t -> t

  module NameMap : Map.S with type key = name

  val collect_declared_points : t -> Pt.t NameMap.t

  (** Relative positioning of horizontal layout boxes. *)
  type hposition = [ `Hcentered | `Bottom | `Top ]

  (** Relative positioning of vertical layout boxes. *)
  type vposition = [ `Vcentered | `Left | `Right ]

  (** A [framing] rule specifies how a sublayout should be rescaled to fit in a
      given [frame]. *)
  type framing =
    | Scale_to_frame of { frame : Bbox.t }
    | Preserve_aspect of { frame : Bbox.t }

  type layout

  (** Embeds a command as part of a layout. *)
  val cmd : t -> layout

  (** [hbox ~pos ~deltax layouts] automatically moves the sub-layout so that
      they are horizontally aligned, with [deltax] units between each layout
      bounding box, and using [pos] for alignement. *)
  val hbox : ?pos:hposition -> ?deltax:float -> layout list -> layout

  (** See [hbox]. *)
  val vbox : ?pos:vposition -> ?deltay:float -> layout list -> layout

  val arrow : start:name -> finish:name -> sty:Arrow.style -> layout -> layout

  val frame : framing -> layout -> layout

  val emit_commands_with_bbox : layout -> t * Bbox.t

  val emit_commands : layout -> t
end

module Make (N : Name_sig) : S with type name = N.t = struct
  type name = N.t

  type position = { pos : Pt.t; relpos : relpos }

  and relpos =
    | Absolute
    | North
    | West
    | South
    | East
    | SouthWest
    | SouthEast
    | NorthWest
    | NorthEast

  let point_of_position { pos; _ } = pos

  let pp_position fmtr { pos; relpos } =
    match relpos with
    | Absolute -> Format.fprintf fmtr "Absolute(%a)" Pt.pp pos
    | North -> Format.fprintf fmtr "North(%a)" Pt.pp pos
    | West -> Format.fprintf fmtr "West(%a)" Pt.pp pos
    | South -> Format.fprintf fmtr "South(%a)" Pt.pp pos
    | East -> Format.fprintf fmtr "East(%a)" Pt.pp pos
    | SouthWest -> Format.fprintf fmtr "SouthWest(%a)" Pt.pp pos
    | SouthEast -> Format.fprintf fmtr "SouthEast(%a)" Pt.pp pos
    | NorthWest -> Format.fprintf fmtr "NorthWest(%a)" Pt.pp pos
    | NorthEast -> Format.fprintf fmtr "NorthEast(%a)" Pt.pp pos

  type t = { uid : int; desc : desc }

  and desc =
    | Circle of { center : Pt.t; radius : float }
    | Box of { mins : Pt.t; maxs : Pt.t }
    | Text of { pos : position; text : Ctext.t }
    | Style of { style : Style.t; cmd : t }
    | Segment of { p1 : Pt.t; p2 : Pt.t }
    | Bezier of { p1 : Pt.t; c1 : Pt.t; p2 : Pt.t; c2 : Pt.t }
    | Image of { pos : Pt.t; image : Image.t }
    | DeclPt of { pt : Pt.t; name : name }
    | Rotate of { radians : float; cmd : t }
    | Translate of { v : Pt.t; cmd : t }
    | Scale of { xs : float; ys : float; cmd : t }
    | Wrap of t list

  type alias = t

  let ubezier_control_points ~p1 ~p2 ~angle =
    let delta = Pt.sub p2 p1 and n_angle = Pt.angle_of_vec (p1, p2) in
    let hdist = 0.3 *. Pt.norm delta in
    let out_angle =
      n_angle +. angle
      (* +. angle *)
    in
    let in_angle =
      n_angle -. angle
      (* +. Tools.pi *. 0.5 -. angle *)
    in
    let vec1 = Pt.scale (Pt.pt (cos out_angle) (sin out_angle)) hdist in
    let vec2 = Pt.scale (Pt.pt (cos in_angle) (sin in_angle)) ~-.hdist in
    let c1 = Pt.add p1 vec1 in
    let c2 = Pt.add p2 vec2 in
    (c1, c2)

  let fresh =
    let acc = ref (-1) in
    fun () ->
      incr acc ;
      !acc

  let mktag desc = { uid = fresh (); desc }

  let circle ~center ~radius = mktag (Circle { center; radius })

  let box ~mins ~maxs = mktag (Box { mins; maxs })

  let text ~pos ~size ~text =
    mktag (Text { pos; text = Ctext.create ~size text })

  let style ~style cmd = mktag (Style { style; cmd })

  let segment ~p1 ~p2 = mktag (Segment { p1; p2 })

  let bezier ~p1 ~c1 ~p2 ~c2 = mktag (Bezier { p1; c1; p2; c2 })

  let ubezier ~p1 ~p2 ~angle =
    let (c1, c2) = ubezier_control_points ~p1 ~p2 ~angle in
    bezier ~p1 ~c1 ~p2 ~c2

  let image ~pos ~image = mktag (Image { pos; image })

  let declpt ~pt ~name = mktag (DeclPt { pt; name })

  let rotate ~radians cmd = mktag (Rotate { radians; cmd })

  let translate ~v cmd = mktag (Translate { v; cmd })

  let scale ~xs ~ys cmd = mktag (Scale { xs; ys; cmd })

  let wrap = function
    | [] -> failwith "Commands.wrap: empty list"
    | [cmd] -> cmd
    | cmds -> mktag (Wrap cmds)

  (* Each basic command (type t) is associated to a designated point we call the "anchor"  *)
  let mid a b = Pt.barycenter a b

  (* midpoint of a Bezier curve *)
  let bezier_midpoint p1 c1 p2 c2 =
    let mid1 = mid p1 c1 and mid2 = mid c1 c2 and mid3 = mid c2 p2 in
    let mid12 = mid mid1 mid2 and mid23 = mid mid2 mid3 in
    mid mid12 mid23

  let anchor_of c =
    match c.desc with
    | Circle { center; _ } -> center
    | Box { mins; maxs } -> mid mins maxs
    (* | Text { pos } -> point_of_position pos *)
    | Text { pos; _ } -> point_of_position pos
    | Segment { p1; p2 } -> mid p1 p2
    | Bezier { p1; c1; p2; c2 } -> bezier_midpoint p1 c1 p2 c2
    | Image { pos; _ } -> pos
    | DeclPt _ | Style _ ->
        failwith "Commands.anchor_of: DeclPt and Style have no anchor"
    | Rotate _ | Translate _ | Scale _ | Wrap _ ->
        failwith
          "Commands.anchor_of: unable to compute anchor for linear \
           transformations"

  let _bouquet_of_segments start finish names =
    match names with
    | [] -> failwith "Commands.bouquet_of_segments: empty list of names"
    | [name] ->
        let edge = segment ~p1:start ~p2:finish in
        [edge; declpt ~pt:(anchor_of edge) ~name]
    | _ ->
        let len = List.length names in
        let pi4 = Tools.pi *. 0.25 in
        let angles = Tools.interpolate ~-.pi4 pi4 (len - 1) in
        let l = List.combine names angles in
        List.fold_left
          (fun acc (name, angle) ->
            let edge = ubezier ~p1:start ~p2:finish ~angle in
            declpt ~pt:(anchor_of edge) ~name :: edge :: acc)
          []
          l

  let rec pp fmtr c =
    Format.(
      match c.desc with
      | Circle { center; radius } ->
          fprintf fmtr "Circle(%a, %f)" Pt.pp center radius
      | Box { mins; maxs } -> fprintf fmtr "Box(%a, %a)" Pt.pp mins Pt.pp maxs
      | Text { pos; text } ->
          fprintf fmtr "Text(%a, %s)" pp_position pos text.Ctext.str
      | Style { style; cmd } ->
          fprintf fmtr "Style(%a, %a)" Style.pp style pp cmd
      | Segment { p1; p2 } -> fprintf fmtr "Segment(%a, %a)" Pt.pp p1 Pt.pp p2
      | Bezier { p1; c1; p2; c2 } ->
          fprintf
            fmtr
            "Bezier(%a, %a, %a, %a)"
            Pt.pp
            p1
            Pt.pp
            c1
            Pt.pp
            p2
            Pt.pp
            c2
      | Image { pos; image } ->
          fprintf
            fmtr
            "Image(%a, %d x %d)"
            Pt.pp
            pos
            (Image.xsize image)
            (Image.ysize image)
      | DeclPt { pt; name } -> fprintf fmtr "DeclPt(%a, %a)" Pt.pp pt N.pp name
      | Rotate { radians; cmd } -> fprintf fmtr "Rotate(%f, %a)" radians pp cmd
      | Translate { v; cmd } -> fprintf fmtr "Translate(%a, %a)" Pt.pp v pp cmd
      | Scale { xs; ys; cmd } -> fprintf fmtr "Scale(%f, %f, %a)" xs ys pp cmd
      | Wrap subcommands -> fprintf fmtr "Wrap(%a)" pp_list subcommands)

  and pp_list fmtr cl =
    Format.pp_print_list
      ~pp_sep:(fun fmtr () -> Format.fprintf fmtr "; ")
      pp
      fmtr
      cl

  let base_of_positioned_box h w { pos; relpos } =
    let x = Pt.x pos and y = Pt.y pos in
    match relpos with
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

  module Bbox = struct
    include Bbox

    let rec of_command c =
      match c.desc with
      | Circle { center; radius } ->
          let x = Pt.x center and y = Pt.y center in
          box
            (Pt.pt (x -. radius) (y -. radius))
            (Pt.pt (x +. radius) (y +. radius))
      | Box { mins; maxs } -> box mins maxs
      | Text { pos; text } ->
          let width = Bbox.width text.Ctext.box in
          let height = Bbox.height text.Ctext.box in
          let base = text_position pos width height in
          box base Pt.(base + pt width height)
      | Segment { p1; p2 } ->
          let x1 = Pt.x p1 and y1 = Pt.y p1 in
          let x2 = Pt.x p2 and y2 = Pt.y p2 in
          box (Pt.pt (min x1 x2) (min y1 y2)) (Pt.pt (max x1 x2) (max y1 y2))
      | Bezier { p1; c1; p2; c2 } -> join (box p1 c1) (box p2 c2)
      | Image { pos; image } -> Bbox.translate pos (Image.bbox image)
      | Style { cmd; _ } -> of_command cmd
      | DeclPt { pt; _ } -> box pt pt
      | Rotate { radians; cmd } ->
          let bbox = of_command cmd in
          rotate radians bbox
      | Translate { v; cmd } ->
          let bbox = of_command cmd in
          translate v bbox
      | Scale { xs; ys; cmd } ->
          let bbox = of_command cmd in
          scale xs ys bbox
      | Wrap subcommands -> of_commands subcommands

    and of_commands cl =
      List.fold_left
        (fun acc cmd ->
          let bbox = of_command cmd in
          join bbox acc)
        empty
        cl
  end

  let center_to_page (w, h) (cmd : t) =
    let b = Bbox.of_command cmd in
    let boxw = Bbox.width b in
    let boxh = Bbox.height b in
    let deltah = 0.5 *. (h -. boxh) in
    let deltaw = 0.5 *. (w -. boxw) in
    translate ~v:(Pt.sub (Pt.pt deltaw deltah) (Bbox.sw b)) cmd

  module NameMap = Map.Make (N)

  let collect_declared_points cmd =
    let rec traverse cmd mat map =
      match cmd.desc with
      | Circle _ | Box _ | Text _ | Style _ | Segment _ | Bezier _ | Image _ ->
          map
      | DeclPt { pt; name } ->
          let global_pt = Gg.P2.tr mat pt in
          NameMap.add name global_pt map
      | Rotate { radians; cmd } ->
          let rot = Gg.M3.rot2 radians in
          let mat = Gg.M3.mul mat rot in
          traverse cmd mat map
      | Translate { v; cmd } ->
          let tra = Gg.M3.move2 v in
          let mat = Gg.M3.mul mat tra in
          traverse cmd mat map
      | Scale { xs; ys; cmd } ->
          let sca = Gg.M3.scale2 (Pt.pt xs ys) in
          let mat = Gg.M3.mul mat sca in
          traverse cmd mat map
      | Wrap subcommands -> traverse_list subcommands mat map
    and traverse_list cmds mat map =
      List.fold_left (fun map c -> traverse c mat map) map cmds
    in
    traverse cmd Gg.M3.id NameMap.empty

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
        (* Name_sig of starting point (see DeclPt)*)
        finish : name;
        (* Name_sig of finishing point *)
        style : style;
        smart : bool
            (* A smart arrow will (try to) avoid command layout boxes (Cmd)
             * except those from which it starts and to which it ends.
             * TODO: in the future we might want to allow more options. *)
      }

    let mkarrowhead ~legs_length ~legs_angle =
      let hangle = 0.5 *. legs_angle in
      let x = cos hangle *. legs_length in
      let y = sin hangle *. legs_length in
      (Pt.pt ~-.x y, Pt.pt ~-.x ~-.y)

    (* let y = cos (legs_angle *. 0.5) *. legs_length in *)
    (* let x = sqrt (legs_length *. legs_length -. y *. y) in *)
    (* (Pt.pt (~-. x) (~-. y), Pt.pt x (~-. y)) *)

    (* let mkarrow legs_length legs_angle start finish = *)
    let mkarrow ~style ~start ~finish =
      let (left_leg, right_leg) =
        mkarrowhead ~legs_length:style.legs ~legs_angle:style.angle
      in
      let vec = Pt.sub finish start in
      let vec_angle = Pt.angle_of_vec (start, finish) in
      let shaft_start = Pt.add start (Pt.scale vec style.startp) in
      let shaft_finish = Pt.add start (Pt.scale vec style.endp) in
      let arrow_pos = Pt.add start (Pt.scale vec style.arrowp) in
      let left_leg = Pt.add arrow_pos (Pt.rotate_vector vec_angle left_leg) in
      let right_leg = Pt.add arrow_pos (Pt.rotate_vector vec_angle right_leg) in
      [ segment ~p1:shaft_start ~p2:shaft_finish;
        segment ~p1:arrow_pos ~p2:left_leg;
        segment ~p1:arrow_pos ~p2:right_leg ]

    (* let mkarrow legs_length legs_angle start finish = *)
    let mkarrow_curvy ~style ~start ~finish ~angle =
      if not (List.mem style.arrowp [0.0; 0.5; 1.0]) then
        failwith "Commands.mkarrow_curvy: arrowp must be either 0, 0.5 or 1"
      else
        let (left_leg, right_leg) =
          mkarrowhead ~legs_length:style.legs ~legs_angle:style.angle
        in
        let vec = Pt.sub finish start in
        let vec_angle = Pt.angle_of_vec (start, finish) in
        let shaft_start = Pt.add start (Pt.scale vec style.startp) in
        let shaft_finish = Pt.add start (Pt.scale vec style.endp) in
        let (c1, c2) =
          ubezier_control_points ~p1:shaft_start ~p2:shaft_finish ~angle
        in
        let (arrow_pos, rot_angle) =
          if style.arrowp = 0.0 then (shaft_start, vec_angle +. angle)
          else if style.arrowp = 0.5 then
            let m = bezier_midpoint shaft_start c1 shaft_finish c2 in
            (m, vec_angle)
          else
            (* style.arrowp = 1.0 *)
            (shaft_finish, Tools.pi -. (vec_angle +. angle))
        in
        let left_leg = Pt.add arrow_pos (Pt.rotate_vector rot_angle left_leg) in
        let right_leg =
          Pt.add arrow_pos (Pt.rotate_vector rot_angle right_leg)
        in
        [ bezier ~p1:shaft_start ~c1 ~p2:shaft_finish ~c2;
          segment ~p1:arrow_pos ~p2:left_leg;
          segment ~p1:arrow_pos ~p2:right_leg ]

    (* multi-segments arrows *)

    let rec to_segments points =
      match points with
      | [] -> failwith "Commands.Arrow.to_segments: not enough points"
      | [_] -> []
      | p1 :: p2 :: tl -> (p1, p2) :: to_segments (p2 :: tl)

    let mk_multisegment_arrow ~style ~points =
      let segments = to_segments points in
      let segments =
        match segments with
        | [] ->
            failwith "Commands.Arrow.mk_multisegment_arrow: not enough points"
        | (p1, p2) :: tl ->
            let vec = Pt.(p2 - p1) in
            let effective_start = Pt.add p1 (Pt.scale vec style.startp) in
            (effective_start, p2) :: tl
      in
      match List.rev segments with
      | [] -> failwith "Commands.Arrow.mk_multisegment_arrow: not enough points"
      | (pn, pn') :: lt ->
          let start = pn and finish = pn' in
          let (left_leg, right_leg) =
            mkarrowhead ~legs_length:style.legs ~legs_angle:style.angle
          in
          let vec = Pt.sub finish start in
          let vec_angle = Pt.angle_of_vec (start, finish) in
          let shaft_finish = Pt.add start (Pt.scale vec style.endp) in
          let arrow_pos = Pt.add start (Pt.scale vec style.arrowp) in
          let left_leg =
            Pt.add arrow_pos (Pt.rotate_vector vec_angle left_leg)
          in
          let right_leg =
            Pt.add arrow_pos (Pt.rotate_vector vec_angle right_leg)
          in
          let arrow =
            [ segment ~p1:pn ~p2:shaft_finish;
              segment ~p1:arrow_pos ~p2:left_leg;
              segment ~p1:arrow_pos ~p2:right_leg ]
          in
          let segments = List.map (fun (p1, p2) -> segment ~p1 ~p2) lt in
          segments @ arrow

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

  (* Box autolayout *)
  let place ~pos cmd =
    let bbox = Bbox.of_command cmd in
    let width = Bbox.width bbox in
    let height = Bbox.height bbox in
    let base = base_of_positioned_box height width pos in
    let v = Pt.(base - Bbox.sw bbox) in
    translate ~v cmd

  type hposition = [ `Hcentered | `Bottom | `Top ]

  type vposition = [ `Vcentered | `Left | `Right ]

  type framing =
    | Scale_to_frame of { frame : Bbox.t }
    | Preserve_aspect of { frame : Bbox.t }

  (* let pp_framing fmtr = function
   *   | Scale_to_frame { frame } ->
   *       Format.fprintf fmtr "Scale_to_frame(%a)" Bbox.pp frame
   *   | Preserve_aspect { frame } ->
   *       Format.fprintf fmtr "Preserve_aspect(%a)" Bbox.pp frame *)

  type layout =
    | Cmd of t
    | Hbox of { pos : hposition; deltax : float; layouts : layout list }
    | Vbox of { pos : vposition; deltay : float; layouts : layout list }
    | Arrow of { arrow : Arrow.t; layout : layout }
    | Frame of { framing : framing; layout : layout }

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

  let cmd c = Cmd c

  let hbox ?(pos = `Hcentered) ?(deltax = 0.0) layouts =
    Hbox { pos; deltax; layouts }

  let vbox ?(pos = `Vcentered) ?(deltay = 0.0) layouts =
    Vbox { pos; deltay; layouts }

  let arrow ~start:(s : name) ~finish:(f : name) ~(sty : Arrow.style) cmd =
    Arrow
      { arrow = Arrow.{ start = s; finish = f; style = sty; smart = false };
        layout = cmd
      }

  let frame framing layout = Frame { framing; layout }

  (* let smart_arrow *)
  (*     ~start:(s : name) *)
  (*     ~finish:(f : name) *)
  (*     ~sty:(sty : Arrow.style) cmd = *)
  (*   Arrow(Arrow.({ start = s; finish = f; style = sty; smart = true }), cmd) *)

  (* Layout algorithm *)

  (* horizontal alignement functions *)

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

  (* invariant: must preserve order of commands *)
  let halign l align_function =
    let rec halign_aux l acc =
      match l with
      | [] -> List.rev acc
      | [elt] -> List.rev (elt :: acc)
      | (cmd1, box1) :: (cmd2, box2) :: l ->
          let v = align_function box1 box2 in
          let command = translate ~v cmd2 in
          let box2 = Bbox.translate v box2 in
          halign_aux ((command, box2) :: l) ((cmd1, box1) :: acc)
    in
    halign_aux l []

  (* invariant: must preserve order of commands *)
  let valign l align_function =
    let rec valign_aux l acc =
      match l with
      | [] -> List.rev acc
      | [elt] -> List.rev (elt :: acc)
      | (cmd1, box1) :: (cmd2, box2) :: l ->
          let v = align_function box1 box2 in
          let command = translate ~v cmd2 in
          let box2 = Bbox.translate v box2 in
          valign_aux ((command, box2) :: l) ((cmd1, box1) :: acc)
    in
    valign_aux l []

  exception Emit_error

  (* This function performs the layout. It produces:
     . a list of named commands ((t * int option) list) where the name
      corresponds to the named_command they belong to
     . a bounding box for the list of commands (Bbox.t)
  *)
  let rec emit_commands_with_bbox (l : layout) : t * Bbox.t =
    match l with
    | Cmd cmd ->
        (* let cmd = crop cmd in *)
        (* let named_cmds = tag cmds named_command.cmd_name in *)
        (cmd, Bbox.of_command cmd)
    | Hbox { pos; deltax; layouts } ->
        let ls = List.map emit_commands_with_bbox layouts in
        let aligned = halign ls (horiz_align pos deltax) in
        let (commands, boxes) = List.split aligned in
        let bbox = List.fold_left Bbox.join Bbox.empty boxes in
        let cmds = wrap commands in
        (cmds, bbox)
    | Vbox { pos; deltay; layouts } ->
        let ls = List.map emit_commands_with_bbox layouts in
        let aligned = valign ls (vert_align pos deltay) in
        let (commands, boxes) = List.split aligned in
        let bbox = List.fold_left Bbox.join Bbox.empty boxes in
        let cmds = wrap commands in
        (cmds, bbox)
    | Arrow { arrow; layout } ->
        let { Arrow.start; finish; style; _ } = arrow in
        let (cmd, bbox) = emit_commands_with_bbox layout in
        let map = collect_declared_points cmd in
        let s =
          try NameMap.find start map
          with Not_found ->
            Format.eprintf
              "Commands.emit_commands_with_bbox: arrow start point %a was not \
               declared\n"
              N.pp
              start ;
            Format.eprintf "command:@.%a@." pp cmd ;
            raise Emit_error
        in
        let f =
          try NameMap.find finish map
          with Not_found ->
            Format.eprintf
              "Commands.emit_commands_with_bbox: arrow end point %a was not \
               declared\n"
              N.pp
              finish ;
            Format.eprintf "commands:@.%a@." pp cmd ;
            raise Emit_error
        in
        (* if arr.Arrow.smart then *)
        (*   (match s_cmdbox, f_cmdbox with *)
        (*    | None, _ *)
        (*    | _, None -> *)
        (*      failwith "Commands.emit_commands_with_bbox: some endpoints of a smart arrow belong to an unnamed boxe" *)
        (*    | Some si, Some fi -> *)
        (*      (\* partition commands by their tag *\) *)
        (*      let fibers = fibers_from_list sublayout_cmds in *)
        (*      (\* extract all commands whose tag is different from s_cmdbox and f_cmdbox and None *\) *)
        (*      let fibers = *)
        (*        List.filter *)
        (*          (fun (tag, _) -> *)
        (*             tag <> None && tag <> s_cmdbox && tag <> f_cmdbox *)
        (*          ) fibers *)
        (*      in *)
        (*      (\* compute bounding boxes for each tag *\) *)
        (*      let bboxes = List.map (fun (tag, cmds) -> Bbox.of_commands cmds) fibers in *)
        (*      let solution = SmartPath.produce_path (2.0 ** (float (depth sublayout))) s f bboxes in *)
        (*      (\* produce_path  *\) *)
        (*      let cmds = tag (Arrow.mk_multisegment_arrow style solution) None in *)
        (*      (cmds @ sublayout_cmds,  Bbox.join (Bbox.of_commands cmds) bbox) *)
        (*   ) *)
        (* else *)
        let arrow = Arrow.mkarrow ~style ~start:s ~finish:f in
        (wrap (cmd :: arrow), Bbox.join (Bbox.of_commands arrow) bbox)
    | Frame { framing; layout } -> (
        let (cmd, bbox) = emit_commands_with_bbox layout in
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
            ( translate
                ~v:Pt.(sub (Bbox.sw frame) (pt x0 y0))
                (scale ~xs:xx ~ys:yy cmd),
              frame )
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
                if xx < yy then (1. /. xx, `Horizontal)
                else (1. /. yy, `Vertical)
              else if xx < yy then (xx, `Horizontal)
              else (yy, `Vertical)
            in
            let position =
              match direction with
              | `Horizontal -> { pos = Bbox.w frame; relpos = West }
              | `Vertical -> { pos = Bbox.s frame; relpos = South }
            in
            (place ~pos:position (scale ~xs:scaling ~ys:scaling cmd), frame))

  let emit_commands l =
    let (cmds, _bbox) = emit_commands_with_bbox l in
    cmds
end
