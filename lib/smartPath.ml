open Pt

let epsilon = 0.001

let near_zero x = abs_float x <= epsilon

let dist p1 p2 = norm (minus p2 p1)

let segment_intersection (p1, p2) (p3, p4) =
  let n12 = normalize (p2 - p1) in
  let n34 = normalize (p4 - p3) in
  let u1x = Pt.x n12 in
  let u1y = Pt.y n12 in
  let u2x = Pt.x n34 in
  let u2y = Pt.y n34 in
  let p1x = Pt.x p1 in
  let p1y = Pt.y p1 in
  let p3x = Pt.x p3 in
  let p3y = Pt.y p3 in
  (* let { x = u1x; y = u1y } = normalize (minus p2 p1) in *)
  (* let { x = u2x; y = u2y } = normalize (minus p4 p3) in *)
  (* let { x = p1x; y = p1y } = p1 in *)
  (* let { x = p3x; y = p3y } = p3 in *)
  let dot = (u1x *. u2x) +. (u1y *. u2y) in
  if near_zero (dot -. 1.0) then (* colinear vectors *)
    None
  else if near_zero u1x && near_zero u2x then None
  else if near_zero u1x then
    let k2 = (p1x -. p3x) /. u2x in
    let k1 = p3y -. p1y +. (k2 *. u2y) in
    if k1 <= epsilon || k1 >= dist p1 p2 || k2 <= epsilon || k2 >= dist p3 p4
    then None
    else Some (plus p3 (pt (k2 *. u2x) (k2 *. u2y)))
  else if near_zero u2x then
    let k1 = (p3x -. p1x) /. u1x in
    let k2 = p1y -. p3y +. (k1 *. u1y) in
    if k1 <= epsilon || k1 >= dist p1 p2 || k2 <= epsilon || k2 >= dist p3 p4
    then None
    else Some (plus p3 (pt (k2 *. u2x) (k2 *. u2y)))
  else
    (* regular case, at last *)
    let k2 =
      ((p3x *. (u1y /. u1x)) +. p1y -. p3y) /. (u2y -. (u2x *. u1y /. u1x))
    in
    if k2 <= epsilon || k2 >= dist p3 p4 then None
    else
      let k1 = ((k2 *. u2x) +. p3x -. p1x) /. u1x in
      if k1 <= epsilon || k1 >= dist p1 p2 then None
      else Some (plus p3 (pt (k2 *. u2x) (k2 *. u2y)))

(* let point_in_box = Gg.Box2.mem *)

(*  C -- N -- D
 *  |         |
 *  W         E
 *  |         |
 *  A -- S -- B
 *)

let corners bbox = Bbox.(sw bbox, ne bbox, nw bbox, se bbox)

let corners_list bbox = Bbox.[sw bbox; ne bbox; nw bbox; se bbox]

type orient = UpLeft | UpRight | DownLeft | DownRight

let orient_of_segment (p1, p2) =
  let p1x = Pt.x p1 and p1y = Pt.y p1 in
  let p2x = Pt.x p2 and p2y = Pt.y p2 in
  let dx = p2x -. p1x in
  let dy = p2y -. p1y in
  let xtest = dx > 0.0 in
  let ytest = dy > 0.0 in
  if xtest && ytest then UpRight
  else if xtest && not ytest then DownRight
  else if (not xtest) && not ytest then DownLeft
  else UpLeft

(* result of an intersection *)
type result = NW | NE | SW | SE | WE | NS

(* we assume that neither endopint of [seg] is in [box] *)

let intersect_box box seg =
  (* let (s1, s2) = seg in *)
  (* if point_in_box s1 box || point_in_box s2 box then *)
  (*   None *)
  (* else *)
  let (a, b, c, d) = corners box in
  let south = segment_intersection (a, b) seg in
  let west = segment_intersection (a, c) seg in
  let north = segment_intersection (c, d) seg in
  let east = segment_intersection (b, d) seg in
  match (south, west, north, east) with
  | (None, None, None, None) -> None
  | (Some s, Some w, None, None) -> Some (SW, s, w)
  | (Some s, None, Some n, None) -> Some (NS, n, s)
  | (Some s, None, None, Some e) -> Some (SE, s, e)
  | (None, Some w, Some n, None) -> Some (NW, n, w)
  | (None, Some w, None, Some e) -> Some (WE, w, e)
  | (None, None, Some n, Some e) -> Some (NE, n, e)
  | _ ->
      let _ =
        Printf.printf
          "SmartPath.intersect_box: warning, borderline case ignored\n%!"
      in
      None

(* failwith "SmartPath.intersect_box: error case; planar segment? lack of luck?" *)

let min_dist p lp =
  List.fold_left
    (fun min_dist point -> min min_dist (dist point p))
    max_float
    lp

let sort_boxes_by_dist origin boxes =
  List.sort
    (fun b1 b2 ->
      let d1 = min_dist origin (corners_list b1)
      and d2 = min_dist origin (corners_list b2) in
      compare d1 d2)
    boxes

let solution shift box pb orient =
  let west_sol w = minus w (pt shift 0.0) in
  let south_sol s = minus s (pt 0.0 shift) in
  let north_sol n = plus n (pt 0.0 shift) in
  let east_sol e = plus e (pt shift 0.0) in
  let (a, b, c, d) = corners box in
  let a_sol = minus a (pt shift shift) in
  let b_sol = plus b (pt shift ~-.shift) in
  let c_sol = plus c (pt ~-.shift shift) in
  let d_sol = plus d (pt shift shift) in
  match pb with
  | (SW, s, w) -> (
      let w' = west_sol w in
      let s' = south_sol s in
      match orient with
      | DownRight -> (w', a_sol, s')
      | UpLeft -> (s', a_sol, w')
      | _ -> failwith "orientation error : SW" )
  | (SE, s, e) -> (
      let s' = south_sol s in
      let e' = east_sol e in
      match orient with
      | UpRight -> (s', b_sol, e')
      | DownLeft -> (e', b_sol, s')
      | _ -> failwith "orientation error : SE" )
  | (NW, n, w) -> (
      let w' = west_sol w in
      let n' = north_sol n in
      match orient with
      | UpRight -> (w', c_sol, n')
      | DownLeft -> (n', c_sol, w')
      | _ -> failwith "orientation error : NW" )
  | (NE, n, e) -> (
      let n' = north_sol n in
      let e' = east_sol e in
      match orient with
      | DownRight -> (n', d_sol, e')
      | UpLeft -> (e', d_sol, n')
      | _ -> failwith "orientation error : NE" )
  | (NS, _n, _s) -> (
      (* let n' = north_sol n in
         let s' = south_sol s in*)
      (* This heuristic might not be very adapted in general *)
      match orient with
      | UpRight | UpLeft -> (b_sol, barycenter b_sol d_sol, d_sol)
      | _ -> (c_sol, barycenter c_sol a_sol, a_sol) )
  | (WE, _w, _e) -> (
      (* let w' = west_sol w in
         let e' = east_sol e in *)
      match orient with
      | UpRight | DownRight -> (c_sol, barycenter c_sol d_sol, d_sol)
      | _ -> (b_sol, barycenter b_sol a_sol, a_sol) )

let rec solve shift ((origin, target) as segment) boxes =
  let boxes = sort_boxes_by_dist origin boxes in
  match boxes with
  | [] -> [origin; target]
  | box :: tl -> (
      let problem = intersect_box box segment in
      match problem with
      | None -> solve shift segment tl
      | Some pb ->
          let (p1, p2, new_origin) =
            solution shift box pb (orient_of_segment segment)
          in
          origin :: p1 :: p2 :: solve shift (new_origin, target) boxes )

let produce_path shift start finish bboxes = solve shift (start, finish) bboxes
