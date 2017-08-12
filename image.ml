
type t = (int32, Bigarray.int32_elt, Bigarray.c_layout) Bigarray.Array2.t

let xsize data = Bigarray.Array2.dim1 data (* number of lines = x *)
let ysize data = Bigarray.Array2.dim2 data (* number of columns = y *)

let bbox data =
  Bbox.box Pt.zero (Pt.pt (float (xsize data)) (float (ysize data)))
