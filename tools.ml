let pi = acos (~-. 1.0)

(* interpolate a b 1 = [a; b] *)
let interpolate a b n =
  let d = (b -. a) /. (float n) in
  let rec loop x i =
    if i = (n+1) then
      []
    else
      x :: (loop (x +. d) (i+1))
  in
  loop a 0
              
let rec to_sseq f sep l =
  match l with
  | [] -> ""
  | [x] -> f x
  | x :: tl ->
    let res = List.fold_right (fun elt acc ->
      sep ^ (f elt) ^ acc
    ) tl "" in
    (f x) ^ res
