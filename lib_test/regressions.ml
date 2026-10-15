open Vlayout
module C = Commands

let failures = ref 0

let check name cond =
  if not cond then (
    incr failures ;
    Format.eprintf "FAIL: %s@." name)

let eps = 1e-6

let close a b = Float.abs (a -. b) < eps

let unit_box = C.box ~mins:Pt.zero ~maxs:(Pt.pt 10.0 10.0)

(* [e] must place the east point of the box, not the south one. *)
let () =
  let b = C.bbox (C.place C.e Pt.zero unit_box) in
  let e = Bbox.e b in
  check
    "place e: east point at origin"
    (close (Pt.x e) 0.0 && close (Pt.y e) 0.0)

(* [Preserve_aspect] must enlarge a figure smaller than the frame, and shrink
   one larger than the frame. *)
let () =
  let frame = Bbox.box Pt.zero (Pt.pt 200.0 100.0) in
  let fit cmd = C.bbox (C.frame (C.Preserve_aspect { frame }) cmd) in
  let small = fit unit_box in
  check "preserve_aspect: small enlarged" (close (Bbox.height small) 100.0) ;
  check "preserve_aspect: small aspect" (close (Bbox.width small) 100.0) ;
  let large = fit (C.box ~mins:Pt.zero ~maxs:(Pt.pt 400.0 100.0)) in
  check "preserve_aspect: large shrunk" (close (Bbox.width large) 200.0) ;
  check "preserve_aspect: large aspect" (close (Bbox.height large) 50.0)

(* The head of a curvy arrow ending at [finish] must point along the incoming
   tangent: for a left-to-right arrow, its legs lie left of [finish]. *)
let () =
  let finish = Pt.pt 100.0 0.0 in
  let arrow = C.arrow_curvy Pt.zero finish ~radians:0.5 in
  check
    "arrow_curvy: head legs behind the tip"
    (Pt.x (Bbox.ne (C.bbox arrow)) <= Pt.x finish +. eps)

(* Empty groups are valid commands and are handled by layout functions. *)
let () =
  let empty = C.group [] in
  check "group []: empty bbox" (Bbox.is_empty (C.bbox empty)) ;
  let row = C.hbox ~dx:10.0 [unit_box; empty; unit_box] in
  check "hbox with empty: width" (close (Bbox.width (C.bbox row)) 30.0) ;
  let col = C.vbox ~dy:10.0 [empty; unit_box; unit_box] in
  check "vbox with empty: height" (close (Bbox.height (C.bbox col)) 30.0) ;
  let none = C.bind_each "missing" unit_box (fun p -> C.circle p 1.0) in
  check "bind_each missing name" (Bbox.is_empty (C.bbox none)) ;
  let frame = Bbox.box Pt.zero (Pt.pt 10.0 10.0) in
  ignore (C.place C.ne Pt.zero empty) ;
  ignore (C.frame (C.Preserve_aspect { frame }) empty) ;
  ignore (C.center_to_page ~w:10.0 ~h:10.0 empty) ;
  (* Rendering a gradient-filled empty group must not raise. *)
  let gradient =
    Style.simple_vertical_gradient ~clr1:Color.red ~clr2:Color.blue
  in
  let style =
    Style.make
      ~stroke:(Style.solid Color.black)
      ~width:None
      ~dash:None
      ~fill:(Some gradient)
  in
  let surface = Cairo.Image.create Cairo.Image.RGB24 ~w:10 ~h:10 in
  Backends.Cairo.render (Cairo.create surface) (C.style style empty)

(* Enumerated colors must stay within [0, 1]. *)
let () =
  Random.init 0 ;
  let in_range c = c >= 0.0 && c <= 1.0 in
  let ok =
    Color.enum_colors () |> Seq.take 1000
    |> Seq.for_all (fun c ->
           let (r, g, b) = Color.unpack c in
           in_range r && in_range g && in_range b)
  in
  check "enum_colors: components in [0, 1]" ok

(* Zero-length arrows have no direction: they draw nothing instead of
   producing NaN coordinates that poison enclosing layouts. *)
let () =
  let p = Pt.pt 5.0 5.0 in
  check "arrow p p: empty" (Bbox.is_empty (C.bbox (C.arrow p p))) ;
  check
    "arrow_curvy p p: empty"
    (Bbox.is_empty (C.bbox (C.arrow_curvy p p ~radians:0.5))) ;
  let row = C.hbox ~dx:10.0 [unit_box; C.arrow p p; unit_box] in
  check
    "hbox with zero-length arrow: width"
    (close (Bbox.width (C.bbox row)) 30.0) ;
  let a = Pt.zero and b = Pt.pt 100.0 0.0 in
  check
    "segmented_arrow: repeated last point ignored"
    (Gg.Box2.equal
       (C.bbox (C.segmented_arrow [a; b; b]))
       (C.bbox (C.segmented_arrow [a; b]))) ;
  check
    "segmented_arrow: repeated inner point ignored"
    (Gg.Box2.equal
       (C.bbox (C.segmented_arrow [a; a; b]))
       (C.bbox (C.segmented_arrow [a; b]))) ;
  check
    "segmented_arrow: single distinct point is empty"
    (Bbox.is_empty (C.bbox (C.segmented_arrow [a; a])))

let () = if !failures > 0 then exit 1
