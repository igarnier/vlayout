type t = { x : float; y : float }

let zero = { x = 0.0; y = 0.0 }
           
let pt x y = { x; y }

let plus pt1 pt2  = pt (pt1.x +. pt2.x) (pt1.y +. pt2.y)

let minus pt1 pt2 = pt (pt1.x -. pt2.x) (pt1.y -. pt2.y)

let scale p s = pt (p.x *. s) (p.y *. s)

let neg p = scale p (~-. 1.0)

let norm { x; y } = sqrt (x *. x +. y *. y)

let barycenter p1 p2 = pt (0.5 *. (p1.x +. p2.x)) (0.5 *. (p1.y +. p2.y ))

let dot p1 p2 = p1.x *. p2.x +. p1.y *. p2.y
                           
let normalize p =
  let il = 1.0 /. (norm p) in
  pt (p.x *. il) (p.y *. il)

let print { x; y } = Printf.sprintf "(%f, %f)" x y
                                       
let angle_of_vec (p1, p2) =
  let n = normalize (minus p2 p1) in
  if n.y < 0.0 then
    ~-. (acos n.x)
  else
    acos n.x
         
let rotate_vector angle { x; y } =
  let c = cos angle
  and s = sin angle in
  {
    x = x *. c -. y *. s;
    y = x *. s +. y *. c
  }

let rotate_point_about center angle point =
  plus (rotate_vector angle (minus point center)) center

let rotate_90_cw { x; y } = pt y (~-. x)

let rotate_90_ccw { x; y } = pt (~-. y) x

let cross p1 p2 =
  p1.x *. p2.y -. p1.y *. p2.x

let pmin p1 p2 =
  pt (min p1.x p2.x) (min p1.y p2.y)

let pmax p1 p2 =
  pt (max p1.x p2.x) (max p1.y p2.y)

let (+)        = plus
let (-)        = minus
let ( *| )     = scale
let ( |* ) s p = scale p s
let (~-) p     = scale p (-. 1.0)

