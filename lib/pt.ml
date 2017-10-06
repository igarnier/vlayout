open Gg

type t = p2

let zero = P2.o
           
let pt = P2.v

let x = V2.x

let y = V2.y

let plus = V2.add

let minus = V2.sub

let scale p s = V2.smul s p

let neg p = scale p (~-. 1.0)

let norm = V2.norm

let barycenter p1 p2 = V2.mix p1 p2 0.5

let dot = V2.dot
                           
let normalize = V2.unit

let print v = Printf.sprintf "(%f, %f)" (V2.x v) (V2.y v)
                                       
let angle_of_vec (p1, p2) =
  let n = normalize (minus p2 p1) in
  if V2.y n < 0.0 then
    ~-. (acos (V2.x n))
  else
    acos (V2.x n)
         
let rotate_vector angle v =
  let x = V2.x v
  and y = V2.y v in
  let c = cos angle
  and s = sin angle in
  let x' = x *. c -. y *. s
  and y' = x *. s +. y *. c in
  V2.v x' y'

let rotate_point_about center angle point =
  plus (rotate_vector angle (minus point center)) center

let rotate_90_cw v =
  let x = V2.x v and y = V2.y v in
  pt y (~-. x)

let rotate_90_ccw v =
  let x = V2.x v and y = V2.y v in
  pt (~-. y) x

let cross p1 p2 =
  (V2.x p1) *. (V2.y p2) -. (V2.y p1) *. (V2.x p2)

let pmin p1 p2 =
  pt (min (V2.x p1) (V2.x p2)) (min (V2.y p1) (V2.y p2))

let pmax p1 p2 =
  pt (max (V2.x p1) (V2.x p2)) (max (V2.y p1) (V2.y p2))

let (+)        = plus
let (-)        = minus
let ( *| )     = scale
let ( |* ) s p = scale p s
let (~-) p     = scale p (-. 1.0)
