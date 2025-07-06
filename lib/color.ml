open Gg

type t = Color.t

let rgb r g b = Color.v_srgb r g b

let red = Color.red

let green = Color.green

let blue = Color.blue

let black = Color.black

let white = Color.white

let pink = rgb (255. /. 255.) (105. /. 255.) (180. /. 255.)

let cyan = rgb 0.0 (255. /. 255.) (255. /. 255.)

let signed_float f =
  let f = Random.float f in
  if Random.bool () then f else ~-.f

let clamp f = if f < 0.0 then ~-.f else if f > 1.0 then 1.0 -. f else f

let unpack color =
  let r = Color.r color in
  let g = Color.g color in
  let b = Color.b color in
  (r, g, b)

let sample_around color =
  let r = Color.r color in
  let g = Color.g color in
  let b = Color.b color in
  let rdelta = signed_float 0.5 in
  let gdelta = signed_float 0.5 in
  let bdelta = signed_float 0.5 in
  let r = clamp (r +. rdelta) in
  let g = clamp (g +. gdelta) in
  let b = clamp (b +. bdelta) in
  rgb r g b

let enum_colors =
  let colors = [| red; green; blue; black; pink; cyan |] in
  let len = Array.length colors in
  let i = ref 0 in
  Seq.of_dispenser @@ fun () ->
  if !i < len then (
    let res = colors.(!i) in
    incr i ;
    Some res)
  else
    let res = sample_around colors.(!i) in
    incr i ;
    Some res

let to_int color =
  let open Int32 in
  let (r, g, b, _a) = Color.to_srgbi color in
  logor
    (logor (shift_left (Int32.of_int r) 16) (shift_left (Int32.of_int g) 8))
    (Int32.of_int b)
