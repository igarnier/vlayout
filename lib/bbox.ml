open Gg

type t = Box2.t

let box = Box2.of_pts

let empty = Box2.empty

let center = Box2.mid

let width = Box2.w

let height = Box2.h

let join = Box2.union

let translate = Box2.move

let of_points pt_list =
  List.fold_left Box2.add_pt Box2.empty pt_list

let of_points_arr pt_arr =
  Array.fold_left Box2.add_pt Box2.empty pt_arr

let se = Box2.br_pt

let sw = Box2.bl_pt

let ne = Box2.tr_pt

let nw = Box2.tl_pt

let print box =
  let mins = sw box
  and maxs = ne box in
  Printf.sprintf "{ mins = %s;  maxs = %s }" (Pt.print mins) (Pt.print maxs)
