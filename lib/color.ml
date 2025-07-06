type t = { r : float; g : float; b : float }

let rgb r g b = { r; g; b }

let red = { r = 1.0; g = 0.0; b = 0.0 }

let green = { r = 0.0; g = 1.0; b = 0.0 }

let blue = { r = 0.0; g = 0.0; b = 1.0 }

let black = { r = 0.0; g = 0.0; b = 0.0 }

let white = { r = 1.0; g = 1.0; b = 1.0 }

let pink = rgb (255. /. 255.) (105. /. 255.) (180. /. 255.)

let cyan = rgb 0.0 (255. /. 255.) (255. /. 255.)

let signed_float f =
  let f = Random.float f in
  if Random.bool () then f else ~-.f

let clamp f = if f < 0.0 then ~-.f else if f > 1.0 then 1.0 -. f else f

let sample_around { r; g; b } =
  let rdelta = signed_float 0.5 in
  let gdelta = signed_float 0.5 in
  let bdelta = signed_float 0.5 in
  let r = clamp (r +. rdelta) in
  let g = clamp (g +. gdelta) in
  let b = clamp (b +. bdelta) in
  { r; g; b }

let enum_colors =
  let colors = [| red; green; blue; black; pink; cyan |] in
  let len = Array.length colors in
  let i = ref 0 in
  fun () ->
    if !i < len then (
      let res = colors.(!i) in
      incr i ;
      res)
    else
      let res = sample_around colors.(!i) in
      incr i ;
      res

let gray p =
  if p < 0.0 || p > 1.0 then invalid_arg "gray percentage must be in [0,1]"
  else { r = p; g = p; b = p }

let to_int { r; g; b } =
  let open Int32 in
  let r = of_float (r *. 255.0) in
  let g = of_float (g *. 255.0) in
  let b = of_float (b *. 255.0) in
  logor (logor (shift_left r 16) (shift_left g 8)) b

let pp fmtr { r; g; b } =
  Format.fprintf fmtr "@[{ r=%f;@, g = %f;@, b = %f@, }@]" r g b
