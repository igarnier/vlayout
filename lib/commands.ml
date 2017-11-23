open Batteries

module type Name =
sig
  type t
  val compare : t -> t -> int
  val print : t -> string
end

module type CommandsSig =
sig

  type name

  type position  = { pos : Pt.t; relpos : relpos }
  and relpos =
    | North    
    | West     
    | South    
    | East     
    | SouthWest
    | SouthEast
    | NorthWest
    | NorthEast

  type t =
    {
      tag  : int option;
      desc : desc
    }
  and desc =
    | Circle  of { center : Pt.t; radius : float }
    | Box     of { mins : Pt.t; maxs : Pt.t }
    | Text    of { pos : position; width : float; height : float; text : string }
    | Style   of { style : Style.t; subcommands : t list }
    | Segment of { p1 : Pt.t; p2 : Pt.t }
    | Bezier  of { p1 : Pt.t; c1 : Pt.t; p2 : Pt.t; c2 : Pt.t }
    | Image   of { pos : Pt.t; image : Image.t }
    | DeclPt  of { pt : Pt.t; name : name }
    | Rotate  of { radians : float; subcommands : t list }
    | Translate of { v : Pt.t; subcommands : t list }
    | Scale of { xs : float; ys : float; subcommands : t list }

  type alias = t

  val text_position : position -> float -> float -> Pt.t

  module Bbox :
    sig
      include Bbox.S
      val of_command :  alias -> t
      val of_commands : alias list -> t
    end

  module Arrow :
  sig
    type style

    val default_style : style
    val mid_style     : style

    val mkarrow       : style:style -> start:Pt.t -> finish:Pt.t -> t list
    val mkarrow_curvy : style:style -> start:Pt.t -> finish:Pt.t -> angle:float -> t list
    val mk_multisegment_arrow : style:style -> points:Pt.t list -> t list

  end

  val circle  : center:Pt.t -> radius:float -> t
  val box     : mins:Pt.t -> maxs:Pt.t -> t
  val text    : pos:position -> width:float -> height:float -> text:string -> t
  val style   : style:Style.t -> subcommands:(t list) -> t
  val segment : p1:Pt.t -> p2:Pt.t -> t
  val bezier  : p1:Pt.t -> c1:Pt.t -> p2:Pt.t -> c2:Pt.t -> t
  val ubezier : p1:Pt.t -> p2:Pt.t -> angle:float -> t
  val image   : pos:Pt.t -> image:Image.t -> t
  val declpt  : pt:Pt.t -> name:name -> t
  val rotate    : radians:float -> subcommands:(t list) -> t
  val translate : v:Pt.t -> subcommands:(t list) -> t
  val scale     : xs:float -> ys:float -> subcommands:(t list) -> t

  (* val crop : t list -> t list *)
  val center_to_page : float*float -> t list -> t list
  (* val translate : t list -> Pt.t -> t list *)
  (* val scale : t list -> Pt.t -> t list *)
  (* val map_pt : t list -> (Pt.t -> Pt.t) -> t list *)

  type layout

  val cmd  : name:(int option) -> t list -> layout
  val hbox : deltax:float -> layout_list:(layout list) -> layout
  val vbox : deltay:float -> layout_list:(layout list) -> layout

  val arrow : start:name -> finish:name -> sty:Arrow.style -> layout -> layout
  val smart_arrow : start:name -> finish:name -> sty:Arrow.style -> layout -> layout                                  
  val emit_commands_with_bbox : layout -> t list * Bbox.t
  val emit_commands : layout -> t list
  (* val emit_commands_centered : float * float -> layout -> t list *)
end

module Make(N : Name) =
  (struct

    type name = N.t

    type position  = { pos : Pt.t; relpos : relpos }
    and relpos =
      | North    
      | West     
      | South    
      | East     
      | SouthWest
      | SouthEast
      | NorthWest
      | NorthEast

    let point_of_position { pos } = pos

    let print_position { pos; relpos } =
      match relpos with
      | North     -> Printf.sprintf "North(%s)" (Pt.print pos)
      | West      -> Printf.sprintf "West(%s)" (Pt.print pos)
      | South     -> Printf.sprintf "South(%s)" (Pt.print pos)
      | East      -> Printf.sprintf "East(%s)" (Pt.print pos)
      | SouthWest -> Printf.sprintf "SouthWest(%s)" (Pt.print pos)
      | SouthEast -> Printf.sprintf "SouthEast(%s)" (Pt.print pos)
      | NorthWest -> Printf.sprintf "NorthWest(%s)" (Pt.print pos)
      | NorthEast -> Printf.sprintf "NorthEast(%s)" (Pt.print pos)

    type t =
      {
        tag  : int option;
        desc : desc
      }
    and desc =
      | Circle  of { center : Pt.t; radius : float }
      | Box     of { mins : Pt.t; maxs : Pt.t }
      | Text    of { pos : position; width : float; height : float; text : string }
      | Style   of { style : Style.t; subcommands : t list }
      | Segment of { p1 : Pt.t; p2 : Pt.t }
      | Bezier  of { p1 : Pt.t; c1 : Pt.t; p2 : Pt.t; c2 : Pt.t }
      | Image   of { pos : Pt.t; image : Image.t }
      | DeclPt  of { pt : Pt.t; name : name }
      | Rotate  of { radians : float; subcommands : t list }
      | Translate of { v : Pt.t; subcommands : t list }
      | Scale of { xs : float; ys : float; subcommands : t list }

    type alias = t

    let ubezier_control_points ~p1:p1 ~p2:p2 ~angle:angle =
      let delta   = Pt.minus p2 p1
      and n_angle = Pt.angle_of_vec (p1, p2) in
      let hdist    = 0.3 *. (Pt.norm delta) in
      let out_angle = n_angle +. angle (* +. angle *) in
      let in_angle  = n_angle -. angle (* +. Tools.pi *. 0.5 -. angle *) in
      let vec1 = Pt.scale (Pt.pt (cos out_angle) (sin out_angle)) hdist in
      let vec2 = Pt.scale (Pt.pt (cos in_angle) (sin in_angle)) (~-. hdist) in
      let c1   = Pt.plus p1 vec1 in
      let c2   = Pt.plus p2 vec2 in
      (c1, c2)


    let mktag tag desc = { tag; desc }

    let circle ~center ~radius =
      mktag None (Circle { center; radius })

    let box ~mins ~maxs =
      mktag None (Box { mins; maxs })

    let text ~pos ~width ~height ~text =
      mktag None (Text { pos; width; height; text })

    let style ~style ~subcommands =
      mktag None (Style { style; subcommands })

    (* let color ~tag ~r ~g ~b = *)
    (*   pattern tag Pattern.(Solid { c = { r; g; b } }) *)

    let segment ~p1 ~p2 =
      mktag None (Segment { p1; p2 })

    let bezier ~p1 ~c1 ~p2 ~c2 =
      mktag None (Bezier { p1; c1; p2; c2 })

    let ubezier ~p1 ~p2 ~angle =
      let (c1, c2) = ubezier_control_points p1 p2 angle in
      bezier ~p1:p1 ~c1:c1 ~p2:p2 ~c2:c2

    let image ~pos ~image =
      mktag None (Image { pos; image })

    let declpt ~pt ~name =
      mktag None (DeclPt { pt; name })

    let rotate ~radians ~subcommands =
      mktag None (Rotate { radians; subcommands })

    let translate ~v ~subcommands =
      mktag None (Translate { v; subcommands })

    let scale ~xs ~ys ~subcommands =
      mktag None (Scale { xs; ys; subcommands })


    (* Each basic command (type t) is associated to a designated point we call the "anchor"  *)
    let mid a b =
      Pt.barycenter a b

    (* midpoint of a Bezier curve *)
    let bezier_midpoint p1 c1 p2 c2 =
      let mid1  = mid p1 c1
      and mid2  = mid c1 c2
      and mid3  = mid c2 p2 in
      let mid12 = mid mid1 mid2
      and mid23 = mid mid2 mid3 in
      mid mid12 mid23

    let anchor_of c =
      match c.desc with
      | Circle { center }  -> center
      | Box { mins; maxs } -> mid mins maxs
      | Text { pos } -> point_of_position pos
      | Segment { p1; p2 } -> mid p1 p2
      | Bezier { p1; c1; p2; c2 } ->
        bezier_midpoint p1 c1 p2 c2
      | Image { pos }      ->  pos
      | DeclPt _ | Style _ ->
        failwith "Commands.anchor_of: DeclPt and Style have no anchor"
      | Rotate _ | Translate _ | Scale _ ->
        failwith "Commands.anchor_of: unable to compute anchor for linear transformations"

    let bouquet_of_segments start finish names =
      match names with
      | [] -> 
        failwith "Commands.bouquet_of_segments: empty list of names"
      | name :: [] ->
        let edge = segment start finish in
        edge :: (declpt (anchor_of edge) name) :: []
      | _ -> 
        let len    = List.length names in
        let pi4    = Tools.pi *. 0.25 in
        let angles = Tools.interpolate (~-. pi4) pi4 (len - 1) in
        let l      = List.combine names angles in
        List.fold_left
          (fun acc (name, angle) ->
             let edge = ubezier start finish angle in
             (declpt (anchor_of edge) name) :: edge :: acc
          ) [] l

    let rec print_cmd c =
      Printf.(
        match c.desc with
        | Circle { center; radius } ->
          sprintf "Circle(%s, %f)" (Pt.print center) radius
        | Box { mins; maxs } ->
          sprintf "Box(%s, %s)" (Pt.print mins) (Pt.print maxs)
        | Text { pos; width; height; text } ->
          sprintf "Text(%s, %f, %f, %s)" (print_position pos) width height text
        | Style { style; subcommands } ->
          let s    = Tools.to_sseq print_cmd "; " subcommands in          
          sprintf "Style(%s, %s)" (Style.print style) s
        | Segment { p1; p2 } ->
          sprintf "Segment(%s, %s)" (Pt.print p1) (Pt.print p2)
        | Bezier { p1; c1; p2; c2 } ->
          sprintf "Bezier(%s, %s, %s, %s)"
            (Pt.print p1) (Pt.print c1)
            (Pt.print p2) (Pt.print c2)
        | Image { pos; image } ->
          sprintf "Image(%s, %d x %d)"
            (Pt.print pos) (Image.xsize image) (Image.ysize image)
        | DeclPt { pt; name } ->
          sprintf "DeclPt(%s, %s)" (Pt.print pt) (N.print name)
        | Rotate { radians; subcommands } ->
          let s    = Tools.to_sseq print_cmd "; " subcommands in
          sprintf "Rotate(%f, %s)" radians s
        | Translate { v; subcommands } ->
          let s    = Tools.to_sseq print_cmd "; " subcommands in
          sprintf "Translate(%s, %s)" (Pt.print v) s
        | Scale { xs; ys; subcommands } ->
          let s    = Tools.to_sseq print_cmd "; " subcommands in
          sprintf "Scale(%f, %f, %s)" xs ys s
      )         

    module NameMap = Map.Make(N)

    let base_of_positioned_box h w { pos; relpos } =
      let x = Pt.x pos and y = Pt.y pos in
      match relpos with
      (* x = base_x + w / 2, y = base_y + h *)              
      | North -> Pt.pt (x -. w *. 0.5) (y -. h)
      (* x = base_x, y = base_y + h / 2 *)                                      
      | West  -> Pt.pt x (y -. h *. 0.5)
      (* x = base_x + w /2, y = base_y *)
      | South -> Pt.pt (x -. w *. 0.5) y
      (* x = base_x + w, y = base_y + h / 2 *)
      | East  -> Pt.pt (x -. w) (y -. h *. 0.5)
      (* x = base_x, y = base_y  *)
      | SouthWest -> Pt.pt x y
      (* x = base_x + w, y = base_y *)
      | SouthEast -> Pt.pt (x -. w) y
      (* x = base_x, y = base_y + h *)
      | NorthWest -> Pt.pt x (y -. h)
      (* x = base_x + w, y = base_y + h*)
      | NorthEast -> Pt.pt (x -. w) (y -. h)

    let text_position pos width height =
      (* let max_h = size in *)
      (* let max_w = (\* max_h *. *\) 5. *. (float (String.length text)) in (\* TODO: this is the ugliest hack *\) *)
      base_of_positioned_box height width pos

    module Bbox =
    struct

      include Bbox

      let rec of_command c =
        match c.desc with
        | Circle { center; radius } ->
          let x = Pt.x center and y = Pt.y center in
          box (Pt.pt (x -. radius) (y -. radius)) (Pt.pt (x +. radius) (y +. radius))
        | Box { mins; maxs } ->
          box mins maxs
        | Text { pos; width; height; text } ->          
          (* let max_h = size in *)
          (* let max_w = max_h *. (float (String.length text)) in *)
          let base  = text_position pos width height in
          box base Pt.(base + (pt width height))
        | Segment { p1; p2 } ->
          let x1 = Pt.x p1 and y1 = Pt.y p1 in
          let x2 = Pt.x p2 and y2 = Pt.y p2 in
          box
            (Pt.pt (min x1 x2) (min y1 y2))
            (Pt.pt (max x1 x2) (max y1 y2))
        | Bezier { p1; c1; p2; c2 } ->
          join (box p1 c1) (box p2 c2)
        | Image { pos; image } ->
          Bbox.translate pos (Image.bbox image)
        | Style { subcommands } ->
          of_commands subcommands
        | DeclPt { pt } -> box pt pt
        | Rotate { radians; subcommands } ->
          let bbox = of_commands subcommands in
          rotate radians bbox
        | Translate { v; subcommands } ->
          let bbox = of_commands subcommands in
          translate v bbox
        | Scale { xs; ys; subcommands } ->
          let bbox = of_commands subcommands in
          scale xs ys bbox

      and of_commands cl =
        let bboxes = List.rev_map of_command cl in
        List.fold_left join empty bboxes

    end


    let tag (commands : t list) (tag : int option) : t list =
      List.map (fun c -> { c with tag }) commands

    (* let untag (commands : t list) : t list = *)
    (*   List.map (fun c -> { c with tag = None }) commands *)

    (* invariant: preserve order of commands *)
    (* let rec translate commands v = *)
    (*   List.map (fun x -> *)
    (*       let desc = *)
    (*         match x.desc with *)
    (*         | Circle { center; radius } -> *)
    (*           Circle { center = Pt.plus center v; radius } *)
    (*         | Box { mins; maxs } -> *)
    (*           Box { mins = Pt.plus mins v; maxs = Pt.plus maxs v } *)
    (*         | Text { pos; size; text } -> *)
    (*           Text { pos = { pos = Pt.plus pos.pos v; relpos = pos.relpos }; size; text } *)
    (*         | Segment { p1; p2 } -> *)
    (*           Segment { p1 = Pt.plus p1 v; p2 = Pt.plus p2 v } *)
    (*         | Bezier { p1; c1; p2; c2 } -> *)
    (*           Bezier { p1 = Pt.plus p1 v; *)
    (*                    c1 = Pt.plus c1 v; *)
    (*                    p2 = Pt.plus p2 v; *)
    (*                    c2 = Pt.plus c2 v } *)
    (*         | Style { style; subcommands } -> *)
    (*           Style { style; *)
    (*                   subcommands = translate subcommands v *)
    (*                 } *)
    (*         | Image { pos; image } -> *)
    (*           Image { pos = Pt.plus pos v; image } *)
    (*         | DeclPt { pt; name } -> *)
    (*           DeclPt { pt = Pt.plus pt v; name } *)
    (*       in *)
    (*       { x with desc } *)
    (*     ) commands *)

    (* let rec scale commands v = *)
    (*   List.map (fun x -> *)
    (*       let desc = *)
    (*         match x.desc with *)
    (*         | Circle { center; radius } -> *)
    (*           Circle { center = Pt.mul center v; radius } *)
    (*         | Box { mins; maxs } -> *)
    (*           Box { mins = Pt.mul mins v; maxs = Pt.mul maxs v } *)
    (*         | Text { pos; size; text } -> *)
    (*           Text { pos = { pos = Pt.mul pos.pos v; relpos = pos.relpos }; size; text } *)
    (*         | Segment { p1; p2 } -> *)
    (*           Segment { p1 = Pt.mul p1 v; p2 = Pt.mul p2 v } *)
    (*         | Bezier { p1; c1; p2; c2 } -> *)
    (*           Bezier { p1 = Pt.mul p1 v; *)
    (*                    c1 = Pt.mul c1 v; *)
    (*                    p2 = Pt.mul p2 v; *)
    (*                    c2 = Pt.mul c2 v } *)
    (*         | Style { style; subcommands } -> *)
    (*           Style { style; *)
    (*                   subcommands = scale subcommands v *)
    (*                 } *)
    (*         | Image { pos; image } -> *)
    (*           Image { pos = Pt.mul pos v; image } *)
    (*         | DeclPt { pt; name } -> *)
    (*           DeclPt { pt = Pt.mul pt v; name } *)
    (*       in *)
    (*       { x with desc } *)
    (*     ) commands *)

    (* let rec map_pt commands f = *)
    (*   List.map (fun x -> *)
    (*       let desc = *)
    (*         match x.desc with *)
    (*         | Circle { center; radius } -> *)
    (*           Circle { center = f center; radius } *)
    (*         | Box { mins; maxs } -> *)
    (*           Box { mins = f mins; maxs = f maxs } *)
    (*         | Text { pos; size; text } -> *)
    (*           Text { pos = { pos = f pos.pos; relpos = pos.relpos }; size; text } *)
    (*         | Segment { p1; p2 } -> *)
    (*           Segment { p1 = f p1; p2 = f p2 } *)
    (*         | Bezier { p1; c1; p2; c2 } -> *)
    (*           Bezier { p1 = f p1; *)
    (*                    c1 = f c1; *)
    (*                    p2 = f p2; *)
    (*                    c2 = f c2 } *)
    (*         | Style { style; subcommands } -> *)
    (*           Style { style; *)
    (*                   subcommands = map_pt subcommands f *)
    (*                 } *)
    (*         | Image { pos; image } -> *)
    (*           Image { pos = f pos; image } *)
    (*         | DeclPt { pt; name } -> *)
    (*           DeclPt { pt = f pt; name } *)
    (*       in *)
    (*       { x with desc } *)
    (*     ) commands *)

    let center_to_page (w, h) (commands : t list) =
      let b      = Bbox.of_commands commands in
      let boxw   = Bbox.width b in
      let boxh   = Bbox.height b in
      let deltah = 0.5 *. (h -. boxh) in
      let deltaw = 0.5 *. (w -. boxw) in
      [translate ~v:(Pt.plus (Pt.scale (Bbox.sw b) (~-. 1.0)) (Pt.pt deltaw deltah)) ~subcommands:commands]

    (* invariant: preserve order of commands *)
    let crop (commands : t list) =
      let b = Bbox.of_commands commands in
      translate ~v:(Pt.scale (Bbox.sw b) (~-. 1.0)) ~subcommands:commands

    (* let rec point_map_of_commands cmds = *)
    (*   match cmds with *)
    (*   | [] -> [] *)
    (*   | c :: l -> *)
    (*      (match c.desc with *)
    (*       | Circle(_,_) *)
    (*       | Box(_, _) *)
    (*       | Text(_, _, _) *)
    (*       | Segment(_, _) *)
    (*       | Bezier(_, _, _, _) *)
    (*       | Color(_, _,_ ) -> point_map_of_commands l *)
    (*       | DeclPt(p, n) -> *)
    (*          (n, p) :: (point_map_of_commands l) *)
    (*      ) *)

    let rec point_map_of_commands cmds acc =
      List.fold_left
        (fun acc c ->
           match c.desc with
           | Circle _
           | Box _
           | Text _
           | Segment _
           | Bezier _
           | Image _ -> acc
           | Style { subcommands }
           | Rotate { subcommands }
           | Translate { subcommands } 
           | Scale { subcommands } ->
             point_map_of_commands subcommands acc
           | DeclPt { pt; name } ->
             NameMap.add name (pt, c.tag) acc
        ) acc cmds           

    let point_map_of_commands cmds =
      point_map_of_commands cmds NameMap.empty

    (* Arrows *)
    module Arrow =
    struct

      type style =
        {
          startp : float; (* [0,1] *)
          endp   : float; (* [0,1] *)
          arrowp : float; (* [0,1] *)
          legs   : float; (* legs length, >= 0 *)
          angle  : float  (* legs angle *)
        }

      type t =
        {
          start  : name;   (* Name of starting point (see DeclPt)*)
          finish : name;   (* Name of finishing point *)
          style  : style;
          smart  : bool   (* A smart arrow will (try to) avoid command layout boxes (Cmd)
                           * except those from which it starts and to which it ends.
                           * TODO: in the future we might want to allow more options. *)
        }


      let mkarrowhead ~legs_length ~legs_angle =
        let hangle = 0.5 *. legs_angle in
        let x = (cos hangle) *. legs_length in
        let y = (sin hangle) *. legs_length in
        (Pt.pt (~-. x) y, Pt.pt (~-. x) (~-. y))
      (* let y = cos (legs_angle *. 0.5) *. legs_length in *)
      (* let x = sqrt (legs_length *. legs_length -. y *. y) in *)
      (* (Pt.pt (~-. x) (~-. y), Pt.pt x (~-. y)) *)


      (* let mkarrow legs_length legs_angle start finish = *)
      let mkarrow ~style ~start ~finish =
        let (left_leg, right_leg) = mkarrowhead style.legs style.angle in
        let vec                   = Pt.minus finish start in
        let vec_angle             = Pt.angle_of_vec (start, finish) in
        let shaft_start           = Pt.plus start (Pt.scale vec style.startp) in
        let shaft_finish          = Pt.plus start (Pt.scale vec style.endp) in
        let arrow_pos             = Pt.plus start (Pt.scale vec style.arrowp) in
        let left_leg              = Pt.plus arrow_pos (Pt.rotate_vector vec_angle left_leg) in
        let right_leg             = Pt.plus arrow_pos (Pt.rotate_vector vec_angle right_leg) in
        [
          segment shaft_start shaft_finish;
          segment arrow_pos left_leg;
          segment arrow_pos right_leg
        ]

      (* let mkarrow legs_length legs_angle start finish = *)
      let mkarrow_curvy ~style ~start ~finish ~angle =
        if not (List.mem style.arrowp [0.0; 0.5; 1.0]) then
          failwith "Commands.mkarrow_curvy: arrowp must be either 0, 0.5 or 1"
        else
          let (left_leg, right_leg) = mkarrowhead style.legs style.angle in
          let vec                   = Pt.minus finish start in
          let vec_angle             = Pt.angle_of_vec (start, finish) in
          let shaft_start           = Pt.plus start (Pt.scale vec style.startp) in
          let shaft_finish          = Pt.plus start (Pt.scale vec style.endp) in
          let (c1, c2)              = ubezier_control_points shaft_start shaft_finish angle in
          let arrow_pos, rot_angle  =
            if style.arrowp = 0.0 then
              (shaft_start, vec_angle +. angle)
            else if style.arrowp = 0.5 then
              let m = bezier_midpoint shaft_start c1 shaft_finish c2 in
              (m, vec_angle)
            else (* style.arrowp = 1.0 *)
              (shaft_finish, Tools.pi -. (vec_angle +. angle))
          in
          let left_leg  = Pt.plus arrow_pos (Pt.rotate_vector rot_angle left_leg) in
          let right_leg = Pt.plus arrow_pos (Pt.rotate_vector rot_angle right_leg) in
          [
            bezier shaft_start c1 shaft_finish c2;
            segment arrow_pos left_leg;
            segment arrow_pos right_leg
          ]

      (* multi-segments arrows *)

      let rec to_segments points =
        match points with
        | []  -> failwith "Commands.Arrow.to_segments: not enough points"
        | [_] -> []
        | p1 :: p2 :: tl ->
          (p1, p2) :: (to_segments (p2 :: tl))

      let mk_multisegment_arrow ~style ~points =
        let segments = to_segments points in
        let segments =
          match segments with
          | [] -> failwith "Commands.Arrow.mk_multisegment_arrow: not enough points"
          | (p1, p2) :: tl ->
            let vec = Pt.(p2 - p1) in
            let effective_start = Pt.plus p1 (Pt.scale vec style.startp) in
            (effective_start, p2) :: tl
        in
        match List.rev segments with
        | [] -> failwith "Commands.Arrow.mk_multisegment_arrow: not enough points"
        | (pn, pn') :: lt ->
          let start = pn and finish = pn' in
          let (left_leg, right_leg) = mkarrowhead style.legs style.angle in
          let vec                   = Pt.minus finish start in
          let vec_angle             = Pt.angle_of_vec (start, finish) in
          let shaft_finish          = Pt.plus start (Pt.scale vec style.endp) in
          let arrow_pos             = Pt.plus start (Pt.scale vec style.arrowp) in
          let left_leg              = Pt.plus arrow_pos (Pt.rotate_vector vec_angle left_leg) in
          let right_leg             = Pt.plus arrow_pos (Pt.rotate_vector vec_angle right_leg) in
          let arrow =
            [
              segment pn shaft_finish;
              segment arrow_pos left_leg;
              segment arrow_pos right_leg
            ]
          in
          let segments = List.map (fun (p1, p2) -> segment p1 p2) lt in
          segments @ arrow

      let default_style =
        {
          startp = 0.0;
          endp   = 1.0;
          arrowp = 1.0;
          legs   = 10.0;
          angle  = Tools.pi *. 0.2
        }

      let mid_style =
        {
          startp = 0.05;
          endp   = 0.95;
          arrowp = 0.5;
          legs   = 10.0;
          angle  = Tools.pi *. 0.2
        }

    end


    (* Box autolayout *)

    type layout =
      | Cmd of named_command
      | Hbox of float * layout list
      | Vbox of float * layout list
      | Arrow of Arrow.t * layout (* an arrow makes only sense wrt point declared in a sublayout *)

    and named_command = { cmd : t list;
                          cmd_name : int option } (* this [int option] might not be the best choice *)



    (* A public interface to build stuff in [layout] *)

    let cmd ~name:n cmds = Cmd { cmd = cmds; cmd_name = n }

    let hbox ~deltax:(dx : float) ~layout_list:llist =
      Hbox(dx, llist)

    let vbox ~deltay:(dy : float) ~layout_list:llist =
      Vbox(dy, llist)

    let arrow
        ~start:(s : name)
        ~finish:(f : name)
        ~sty:(sty : Arrow.style) cmd =
      Arrow(Arrow.({ start = s; finish = f; style = sty; smart = false }), cmd)

    let smart_arrow
        ~start:(s : name)
        ~finish:(f : name)
        ~sty:(sty : Arrow.style) cmd =
      Arrow(Arrow.({ start = s; finish = f; style = sty; smart = true }), cmd)



    (* Layout algorithm *)

    (* given box1, compute displacement vector for box2 to 
     * be aligned the right of box1, in a centered way. *)
    let align_right_centered_vector box1 box2 deltax =
      let h1 = Bbox.height box1
      and h2 = Bbox.height box2 in
      let deltay = (h1 -. h2) *. 0.5 in
      Pt.((Bbox.se box1) - (Bbox.sw box2) + (Pt.pt deltax deltay))

    (* given box1, compute displacement vector for box2 to  *)
    (* be aligned at the bottom of box1, in a centered way. *)
    let align_bottom_centered_vector box1 box2 deltay =
      let w1 = Bbox.width box1
      and w2 = Bbox.width box2 in
      let deltax = (w1 -. w2) *. 0.5 in
      Pt.((Bbox.sw box1) - (Bbox.nw box2) + (Pt.pt deltax (~-. deltay)))

    (* invariant: must preserve order of commands *)
    let halign l deltax =
      let rec halign_aux l acc =
        match l with
        | [] -> List.rev acc
        | [elt] -> List.rev (elt :: acc)
        | (cmds1, box1) :: (cmds2, box2) :: l ->
          let v = align_right_centered_vector box1 box2 deltax in
          let commands = [translate ~v ~subcommands:cmds2] in
          let box2     = Bbox.translate v box2 in
          halign_aux ((commands, box2) :: l) ((cmds1, box1) :: acc)
      in
      halign_aux l []

    (* invariant: must preserve order of commands *)
    let valign l deltay =
      let rec valign_aux l acc =
        match l with
        | [] -> List.rev acc
        | [elt] -> List.rev (elt :: acc)
        | (cmds1, box1) :: (cmds2, box2) :: l ->
          let v = align_bottom_centered_vector box1 box2 deltay in
          let commands = [translate ~v ~subcommands:cmds2] in
          let box2     = Bbox.translate v box2 in
          valign_aux ((commands, box2) :: l) ((cmds1, box1) :: acc)
      in
      valign_aux l []

    let rec depth =
      function
      | Cmd _ -> 1
      | Hbox(_, ll)
      | Vbox(_, ll) ->       
        1 + (List.fold_left min max_int (List.map depth ll))
      | Arrow(_, l) ->
        1 + (depth l)

    let fibers_from_list =
      let rec insert a b fibers =
        match fibers with
        | [] ->
          [(b, [a])]
        | ((b', l) as b'_fiber) :: tl ->
          if b = b' then
            (b', a :: l) :: tl
          else
            b'_fiber :: (insert a b tl)
      in
      let rec loop l acc =
        match l with
        | [] -> acc
        | c :: tl ->
          loop tl (insert c c.tag acc)
      in
      fun l -> loop l []


    exception Emit_error

    (* This function performs the layout. It produces:
     * . a list of named commands ((t * int option) list) where the name
     *   corresponds to the named_command they belong to
     * . a bounding box for the list of commands (Bbox.t)
    *)
    let rec emit_commands_with_bbox (l : layout) : ((t list) * Bbox.t) =
      match l with
      | Cmd named_command ->
        let cmds = [crop named_command.cmd] in
        let named_cmds = tag cmds named_command.cmd_name in
        (named_cmds, Bbox.of_commands cmds)
      | Hbox(deltax, ls) ->
        let ls = List.map emit_commands_with_bbox ls in
        let aligned = halign ls deltax in
        let (commands, boxes) = List.split aligned in
        let bbox = List.fold_left Bbox.join Bbox.empty boxes in
        let cmds = List.fold_left (@) [] commands in
        (cmds, bbox)
      | Vbox(deltay, ls) ->
        let ls = List.map emit_commands_with_bbox ls in
        let aligned = valign ls deltay in
        let (commands, boxes) = List.split aligned in
        let bbox = List.fold_left Bbox.join Bbox.empty boxes in
        let cmds = List.fold_left (@) [] commands in
        (cmds, bbox)
      | Arrow(arr, sublayout) ->
        let { Arrow.start; finish; style } = arr in
        let (sublayout_cmds, bbox) = emit_commands_with_bbox sublayout in
        let map   = point_map_of_commands sublayout_cmds in
        let (s, s_cmdbox) =
          try NameMap.find start map
          with Not_found ->
            (Printf.printf "Commands.emit_commands_with_bbox: arrow start point %s was not declared\n" (N.print start);
             let s = Tools.to_sseq print_cmd ";\n" sublayout_cmds in
             Printf.printf "commands:\n%s\n" s;
             raise Emit_error)
        in
        let (f, f_cmdbox) =
          try NameMap.find finish map
          with Not_found ->
            (Printf.printf "Commands.emit_commands_with_bbox: arrow end point %s was not declared\n" (N.print finish);
             let s = Tools.to_sseq print_cmd ";\n" sublayout_cmds in
             Printf.printf "commands:\n%s\n" s;
             raise Emit_error)
        in
        if arr.Arrow.smart then
          (match s_cmdbox, f_cmdbox with
           | None, _
           | _, None ->
             failwith "Commands.emit_commands_with_bbox: some endpoints of a smart arrow belong to an unnamed boxes"
           | Some si, Some fi ->
             (* partition commands by their tag *)
             let fibers = fibers_from_list sublayout_cmds in
             (* extract all commands whose tag is different from s_cmdbox and f_cmdbox and None *)
             let fibers =
               List.filter
                 (fun (tag, _) ->
                    tag <> None && tag <> s_cmdbox && tag <> f_cmdbox
                 ) fibers
             in
             (* compute bounding boxes for each tag *)
             let bboxes = List.map (fun (tag, cmds) -> Bbox.of_commands cmds) fibers in
             let solution = SmartPath.produce_path (2.0 ** (float (depth sublayout))) s f bboxes in
             (* produce_path  *)
             let cmds = tag (Arrow.mk_multisegment_arrow style solution) None in
             (cmds @ sublayout_cmds,  Bbox.join (Bbox.of_commands cmds) bbox)
          )
        else
          let cmds  = tag (Arrow.mkarrow style s f) None in
          (cmds @ sublayout_cmds,  Bbox.join (Bbox.of_commands cmds) bbox)
    (* let ls = List.map emit_commands_with_bbox ls in *)

    let emit_commands l =
      let (cmds, bbox) = emit_commands_with_bbox l in
      [crop cmds]

    (* let emit_commands_centered (w,h) l = *)
    (*   let (cmds, bbox) = emit_commands_with_bbox l in *)
    (*   center_to_page (w,h) cmds *)


  end : CommandsSig with type name = N.t)

