type t =
  {
    mins : Pt.t;
    maxs : Pt.t
  }
       
let box p1 p2 = { mins = p1; maxs = p2 }

let empty =
  {
    mins = Pt.pt max_float max_float;
    maxs = Pt.pt (~-. max_float) (~-. max_float)
  }

let center { mins; maxs } =
  Pt.pt (0.5 *. (maxs.Pt.x -. mins.Pt.x)) (0.5 *. (maxs.Pt.y -. mins.Pt.y))

let width { mins; maxs } = maxs.Pt.x -. mins.Pt.x

let height { mins; maxs } = maxs.Pt.y -. mins.Pt.y
                                        
let join b1 b2 =
  {
    mins = Pt.pt (min b1.mins.Pt.x b2.mins.Pt.x) (min b1.mins.Pt.y b2.mins.Pt.y);
    maxs = Pt.pt (max b1.maxs.Pt.x b2.maxs.Pt.x) (max b1.maxs.Pt.y b2.maxs.Pt.y)
  }

let translate p bbox =
  {
    mins = Pt.plus p bbox.mins;
    maxs = Pt.plus p bbox.maxs
  }

let of_points pt_list =
  let (mins, maxs) =
    List.fold_left
      (fun (mins, maxs) p ->
       (Pt.pmin mins p, Pt.pmax maxs p)
      ) (empty.mins, empty.maxs) pt_list
  in
  { mins; maxs }

let se { mins; maxs } =
  Pt.pt maxs.Pt.x mins.Pt.y

let sw { mins; maxs } =
  mins

let ne { mins; maxs } =
  maxs

let nw { mins; maxs } =
  Pt.pt mins.Pt.x maxs.Pt.y

let print { mins; maxs } =
  Printf.sprintf "{ mins = %s;  maxs = %s }" (Pt.print mins) (Pt.print maxs)
