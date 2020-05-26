type t =
  { pixels : (int32, Bigarray.int32_elt, Bigarray.c_layout) Bigarray.Array1.t;
    xsize : int;
    ysize : int
  }

let create xsize ysize =
  let pixels =
    Bigarray.Array1.create Bigarray.int32 Bigarray.c_layout (xsize * ysize)
  in
  { pixels; xsize; ysize }

let xsize { xsize; _ } = xsize

let ysize { ysize; _ } = ysize

let pixels { pixels; _ } = pixels

let bbox { xsize; ysize; _ } =
  Bbox.box Pt.zero (Pt.pt (float xsize) (float ysize))

let get { pixels; xsize; ysize = _ } x y = pixels.{(y * xsize) + x}

let set { pixels; xsize; ysize = _ } x y clr = pixels.{(y * xsize) + x} <- clr
