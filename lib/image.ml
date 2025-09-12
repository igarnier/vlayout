open Gg

type t =
  { pixels : (int32, Bigarray.int32_elt, Bigarray.c_layout) Bigarray.Array1.t;
    xpixels : int;
    ypixels : int
  }

let create ~xpixels ~ypixels =
  if xpixels < 1 || ypixels < 1 then
    invalid_arg "Image.create: xpixels and ypixels must be at least 1" ;
  let pixels =
    Bigarray.Array1.create Bigarray.int32 Bigarray.c_layout (xpixels * ypixels)
  in
  { pixels; xpixels; ypixels }

let xpixels { xpixels; _ } = xpixels

let ypixels { ypixels; _ } = ypixels

let pixels { pixels; _ } = pixels

let bbox { xpixels; ypixels; _ } =
  Box2.of_pts Pt.zero (Pt.pt (float xpixels) (float ypixels))

let get { pixels; xpixels; ypixels = _ } x y = pixels.{(y * xpixels) + x}

let set { pixels; xpixels; ypixels = _ } x y clr =
  pixels.{(y * xpixels) + x} <- clr
