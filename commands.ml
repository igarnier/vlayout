
module type Name =
  sig
    type t

    val compare : t -> t -> int

    val print : t -> string
  end

module Make(N : Name) =
  struct
    
    type 'a s =
      {
        tag : 'a;
        desc : desc;
      }
     and desc =
       | Circle of Pt.t * float         (* x, y, radius *)
       | Box of Pt.t * Pt.t             (* mins; maxs *)
       | Text of Pt.t * int * string 
       | Color of float * float * float (* r, g, b *)
       | Segment of Pt.t * Pt.t
       | Bezier of Pt.t * Pt.t * Pt.t * Pt.t
       | DeclPt of Pt.t * N.t
                            
    type t        = (int option) s
    type untagged = unit s



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
            
    (* public interface for creating stuff of type [tagged] *)                         
    module Tagged =
      struct

        let mktag tag desc = { tag; desc }
        
        let circle ~tag:tag ~p:p ~radius:r = mktag tag (Circle(p, r))

        let box ~tag:tag ~mins:p1 ~maxs:p2 = mktag tag  (Box(p1, p2))

        let text ~tag:tag ~p:p ~sz:sz ~s:s = mktag tag  (Text(p, sz, s))

        let color ~tag:tag ~r:r ~g:g ~b:b = mktag tag  (Color(r, g, b))

        let segment ~tag:tag ~p1:p1 ~p2:p2 = mktag tag  (Segment(p1, p2))

        let bezier ~tag:tag ~p1:p1 ~c1:c1 ~p2:p2 ~c2:c2 = mktag tag  (Bezier(p1, c1, p2, c2))
            
        let ubezier ~tag:tag ~p1:p1 ~p2:p2 ~angle:angle =
          let (c1, c2) = ubezier_control_points p1 p2 angle in
          bezier ~tag:tag ~p1:p1 ~c1:c1 ~p2:p2 ~c2:c2
                 
        let declpt ~tag:tag ~p:p ~n:n = mktag tag  (DeclPt(p, n))

                                                    
      end
        
    (* public interface for creating stuff of type [untagged] *)
        
    let mkunit d = { tag = (); desc = d }

    let circle = Tagged.circle ~tag:()

    let box = Tagged.box ~tag:()

    let text = Tagged.text ~tag:()

    let color = Tagged.color ~tag:()

    let segment = Tagged.segment ~tag:()

    let bezier = Tagged.bezier ~tag:()

    let ubezier = Tagged.ubezier ~tag:()
             
    let declpt = Tagged.declpt ~tag:()

    (* Each basic command (type 'a s) is associated to a designated point we call the "anchor"  *)

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
      | Circle(center, _) ->
         center
      | Box(p1, p2) ->
         mid p1 p2
      | Text(pos, sz, _) ->
         pos
      | Segment(p1, p2) ->
         mid p1 p2
      | Bezier(p1, c1, p2, c2) ->
         bezier_midpoint p1 c1 p2 c2
      | DeclPt(_, _)
      | Color(_, _, _) ->
         failwith "Commands.anchor_of: DeclPt and Color have no anchor"

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
           
    let print_cmd c =
      Printf.(
        match c.desc with
        | Circle(p, radius) ->
           sprintf "Circle(%s, %f)" (Pt.print p) radius
        | Box(p1, p2) ->
           sprintf "Box(%s, %s)" (Pt.print p1) (Pt.print p2)
        | Text(p, sz, txt) ->
           sprintf "Text(%s, %d, %s)" (Pt.print p) sz txt
        | Color(r, g, b) ->
           sprintf "Color(%f, %f, %f)" r g b
        | Segment(p1, p2) ->
           sprintf "Segment(%s, %s)" (Pt.print p1) (Pt.print p2)
        | Bezier(p1, c1, p2, c2) ->
           sprintf "Bezier(%s, %s, %s, %s)"
                   (Pt.print p1) (Pt.print c1)
                   (Pt.print p2) (Pt.print c2)
        | DeclPt(p, n) ->
           sprintf "DeclPt(%s, %s)" (Pt.print p) (N.print n)                   
      )         
            
    module NameMap = Map.Make(N)
                             
    module Bbox =
      struct

        include Bbox

        open Pt
               
        let rec of_command c =
          match c.desc with
          | Circle( { x; y }, radius) ->
             box { x = x -. radius; y = y -. radius } { x = x +. radius; y = y +. radius }
          | Box(mins, maxs) ->
             box mins maxs
          | Text(p, sz, text) ->
             (* magical constants ...*)
             let max_h = (float sz) in
             let max_w = max_h *. (float (String.length text)) in
             let base  = p in
             box base (plus base (pt max_w max_h))
          | Segment(p1, p2) ->
             let { x = x1; y = y1 } = p1
             and { x = x2; y = y2 } = p2 in
             box (pt (min x1 x2) (min y1 y2)) (pt (max x1 x2) (max y1 y2))
          | Bezier(p1, c1, p2, c2) ->
             Bbox.join (Bbox.box p1 c1) (Bbox.box p2 c2)
          | Color _ ->
             empty
          | DeclPt(p, _) -> box p p

        let of_commands cl =
          let bboxes = List.map of_command cl in
          List.fold_left join empty bboxes

      end


    let tag (commands : ('a s) list) (t : 'b) : ('b s) list =
      List.map (fun c -> { c with tag = t }) commands

    let untag (commands : ('a s) list) : untagged list =
      List.map (fun c -> { c with tag = () }) commands
               
    (* invariant: preserve order of commands *)
    let translate commands v =
      List.map (fun x ->
                let desc =
                  match x.desc with
                  | Circle(pt, radius) ->
                     Circle(Pt.plus pt v, radius)
                  | Box(mins, maxs) ->
                     Box(Pt.plus mins v, Pt.plus maxs v)
                  | Text(p, sz, str) ->
                     Text(Pt.plus p v, sz, str)
                  | Segment(p1, p2) ->
                     Segment(Pt.plus p1 v, Pt.plus p2 v)
                  | Bezier(p1, c1, p2, c2) ->
                     Bezier(Pt.plus p1 v,
                            Pt.plus c1 v,
                            Pt.plus p2 v,
                            Pt.plus c2 v)
                  | Color(_,_,_) -> x.desc
                  | DeclPt(p, n) ->
                     DeclPt(Pt.plus p v, n)
                in
                { x with desc }
               ) commands

    let center_to_page (w, h) (commands : ('a s) list) =
      let b      = Bbox.of_commands commands in
      let boxw   = Bbox.width b in
      let boxh   = Bbox.height b in
      let deltah = 0.5 *. (h -. boxh) in
      let deltaw = 0.5 *. (w -. boxw) in
      translate commands (Pt.plus (Pt.scale b.Bbox.mins (~-. 1.0)) (Pt.pt deltaw deltah))
               
    (* invariant: preserve order of commands *)
    let crop (commands : ('a s) list) =
      let b = Bbox.of_commands commands in
      translate commands (Pt.scale b.Bbox.mins (~-. 1.0))

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

    let point_map_of_commands cmds =
      List.fold_left
        (fun acc c ->
         match c.desc with
         | Circle(_,_)
         | Box(_, _)
         | Text(_, _, _)
         | Segment(_, _)
         | Bezier(_, _, _, _)
         | Color(_, _,_ ) -> acc
         | DeclPt(p, n) ->
            NameMap.add n (p, c.tag) acc
        ) NameMap.empty cmds           

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
            start  : N.t;   (* Name of starting point (see DeclPt)*)
            finish : N.t;   (* Name of finishing point *)
            style  : style;
            smart  : bool   (* A smart arrow will (try to) avoid command layout boxes (Cmd)
                             * except those from which it starts and to which it ends.
                             * TODO: in the future we might want to allow more options. *)
          }

            
        let mkarrowhead legs_length legs_angle =
          let hangle = 0.5 *. legs_angle in
          let x = (cos hangle) *. legs_length in
          let y = (sin hangle) *. legs_length in
          (Pt.pt (~-. x) y, Pt.pt (~-. x) (~-. y))
        (* let y = cos (legs_angle *. 0.5) *. legs_length in *)
        (* let x = sqrt (legs_length *. legs_length -. y *. y) in *)
        (* (Pt.pt (~-. x) (~-. y), Pt.pt x (~-. y)) *)
            

        (* let mkarrow legs_length legs_angle start finish = *)
        let mkarrow style start finish =
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
        let mkarrow_curvy style start finish angle =
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
                           
        let mk_multisegment_arrow style points =
          let segments = to_segments points in
          let segments =
            match segments with
            | [] -> failwith "Commands.Arrow.mk_multisegment_arrow: not enough points"
            | (p1, p2) :: tl ->
               let ({ Pt.x; y } as vec) = Pt.minus p2 p1 in
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
                             
     and named_command = { cmd : untagged list;
                           cmd_name : int option } (* this [int option] might not be the best choice *)
                           



                           
    (* A public interface to build stuff in [layout] *)

    let cmd ~name:n cmds = Cmd { cmd = cmds; cmd_name = n }

    let hbox ~deltax:(dx : float) ~layout_list:llist =
      Hbox(dx, llist)

    let vbox ~deltay:(dy : float) ~layout_list:llist =
      Vbox(dy, llist)

    let arrow
          ~start:(s : N.t)
          ~finish:(f : N.t)
          ~sty:(sty : Arrow.style) cmd =
      Arrow(Arrow.({ start = s; finish = f; style = sty; smart = false }), cmd)
           
    let smart_arrow
          ~start:(s : N.t)
          ~finish:(f : N.t)
          ~sty:(sty : Arrow.style) cmd =
      Arrow(Arrow.({ start = s; finish = f; style = sty; smart = true }), cmd)


           
    (* Layout algorithm *)
           
           
           
    (* given box1, compute displacement vector for box2 to 
     * be aligned the right of box1, in a centered way. *)
    let align_right_centered_vector box1 box2 deltax =
      let h1 = Bbox.height box1
      and h2 = Bbox.height box2 in
      let deltay = (h1 -. h2) *. 0.5 in
      Pt.plus (Pt.plus (Bbox.se box1) (Pt.pt deltax deltay))
              (Pt.scale (Bbox.sw box2) (~-. 1.0))

    (* given box1, compute displacement vector for box2 to  *)
    (* be aligned at the bottom of box1, in a centered way. *)
    let align_bottom_centered_vector box1 box2 deltay =
      let w1 = Bbox.width box1
      and w2 = Bbox.width box2 in
      let deltax = (w1 -. w2) *. 0.5 in
      Pt.plus (Pt.plus (Bbox.sw box1) (Pt.pt deltax deltay))
              (Pt.plus (Pt.pt 0.0 (~-. (Bbox.height box2))) (Pt.scale (Bbox.sw box2) (~-. 1.0)))

    (* invariant: must preserve order of commands *)
    let halign l deltax =
      let rec halign_aux l =
        match l with
        | [] | [_] -> l
        | (cmds1, box1) :: (cmds2, box2) :: l ->
           let v = align_right_centered_vector box1 box2 deltax in
           let commands = translate cmds2 v in
           let box2     = Bbox.of_commands commands in (* could just translate it *)
           (cmds1, box1) :: (halign_aux ((commands, box2) :: l))
      in
      halign_aux l

    (* invariant: must preserve order of commands *)
    let valign l deltay =
      let rec valign_aux l =
        match l with
        | [] | [_] -> l
        | (cmds1, box1) :: (cmds2, box2) :: l ->
           let v = align_bottom_centered_vector box1 box2 deltay in
           let commands = translate cmds2 v in
           let box2     = Bbox.of_commands commands in (* could just translate it *)
           (cmds1, box1) :: (valign_aux ((commands, box2) :: l))
      in
      valign_aux l                            

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
         let cmds = crop named_command.cmd in
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
      crop cmds

    let emit_commands_centered (w,h) l =
      let (cmds, bbox) = emit_commands_with_bbox l in
      center_to_page (w,h) cmds
           

  end
    
