
type t = (int32, Bigarray.int32_elt, Bigarray.c_layout) Bigarray.Array2.t


let create xsize ysize =
  Bigarray.Array2.create Bigarray.int32 Bigarray.c_layout xsize ysize

let xsize data = Bigarray.Array2.dim1 data (* number of lines = x *)
                                      
let ysize data = Bigarray.Array2.dim2 data (* number of columns = y *)

let bbox data =
  Bbox.box Pt.zero (Pt.pt (float (xsize data)) (float (ysize data)))

let get im x y = im.{x, y}

let set im x y clr = im.{x, y} <- clr
