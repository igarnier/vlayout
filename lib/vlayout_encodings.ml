open Vlayout

module Pt = struct
  let encoding : Pt.t Data_encoding.t =
    let open Data_encoding in
    conv
      (fun pt -> (Pt.x pt, Pt.y pt))
      (fun (x, y) -> Pt.pt x y)
      (tup2 float float)
end

module Bbox = struct
  let encoding : Bbox.t Data_encoding.t =
    let open Data_encoding in
    conv
      (fun box -> (Bbox.sw box, Bbox.ne box))
      (fun (sw, ne) -> Bbox.of_points [sw; ne])
      (tup2 Pt.encoding Pt.encoding)
end

module Color = struct
  let encoding : Vlayout.Color.t Data_encoding.t =
    let open Data_encoding in
    conv
      (fun { Color.r; g; b } -> (r, g, b))
      (fun (r, g, b) -> { Color.r; g; b })
      (tup3 float float float)
end

module Ctext = struct
  let slant_encoding : Cairo.slant Data_encoding.t =
    let open Data_encoding in
    conv
      (function Cairo.Upright -> 0 | Cairo.Italic -> 1 | Cairo.Oblique -> 2)
      (fun i ->
        match i with
        | 0 -> Cairo.Upright
        | 1 -> Cairo.Italic
        | 2 -> Cairo.Oblique
        | _ -> assert false)
      int31

  let weight_encoding : Cairo.weight Data_encoding.t =
    let open Data_encoding in
    conv
      (function Cairo.Normal -> 0 | Cairo.Bold -> 1)
      (fun i ->
        match i with 0 -> Cairo.Normal | 1 -> Cairo.Bold | _ -> assert false)
      int31

  let spec_encoding : Ctext.font_spec Data_encoding.t =
    let open Data_encoding in
    conv
      (fun { Ctext.family; slant; weight; size } ->
        (family, slant, weight, size))
      (fun (family, slant, weight, size) ->
        { Ctext.family; slant; weight; size })
      (tup4 string slant_encoding weight_encoding float)

  let encoding : Vlayout.Ctext.t Data_encoding.t =
    let open Data_encoding in
    conv
      (fun { Ctext.str; spec; _ } -> (str, spec))
      (fun (str, spec) ->
        let { Ctext.family; slant; weight; size } = spec in
        Ctext.create ~size ~family ~slant ~weight str)
      (tup2 string spec_encoding)
end

module Style = struct
  let color_stops_encoding : Style.color_stops Data_encoding.t =
    let open Data_encoding in
    list (tup2 Color.encoding float)

  let dash_pattern_encoding : Style.dash_pattern Data_encoding.t =
    let open Data_encoding in
    array float

  let pattern_encoding : Style.pattern Data_encoding.t =
    let open Data_encoding in
    union
      [ case
          ~title:"solid"
          (Tag 0)
          Color.encoding
          (function Style.Solid { c } -> Some c | _ -> None)
          (fun c -> Style.Solid { c });
        case
          ~title:"linear"
          (Tag 1)
          (tup3 Pt.encoding Pt.encoding color_stops_encoding)
          (function
            | Style.Linear { p0; p1; stops } -> Some (p0, p1, stops) | _ -> None)
          (fun (p0, p1, stops) -> Style.Linear { p0; p1; stops });
        case
          ~title:"radial"
          (Tag 2)
          (tup5 Pt.encoding float Pt.encoding float color_stops_encoding)
          (function
            | Style.Radial { c0; r0; c1; r1; stops } ->
                Some (c0, r0, c1, r1, stops)
            | _ -> None)
          (fun (c0, r0, c1, r1, stops) ->
            Style.Radial { c0; r0; c1; r1; stops }) ]

  let encoding : Style.t Data_encoding.t =
    let open Data_encoding in
    conv
      (fun { Style.stroke; width; dash; fill } -> (stroke, width, dash, fill))
      (fun (stroke, width, dash, fill) -> { stroke; width; dash; fill })
      (tup4
         pattern_encoding
         (option float)
         (option dash_pattern_encoding)
         (option pattern_encoding))
end

module Commands (N : sig
  include Commands.Name_sig

  val encoding : t Data_encoding.t
end) : sig
  val position_encoding : Vlayout.Commands.Make(N).position Data_encoding.t

  val relpos_encoding : Vlayout.Commands.Make(N).relpos Data_encoding.t

  val encoding : Vlayout.Commands.Make(N).t Data_encoding.t
end = struct
  module C = Commands.Make (N)

  let relpos_encoding : C.relpos Data_encoding.t =
    let open Data_encoding in
    let open C in
    union
      [ case
          ~title:"Absolute"
          (Tag 0)
          unit
          (function Absolute -> Some () | _ -> None)
          (fun () -> Absolute);
        case
          ~title:"North"
          (Tag 1)
          unit
          (function North -> Some () | _ -> None)
          (fun () -> North);
        case
          ~title:"West"
          (Tag 2)
          unit
          (function West -> Some () | _ -> None)
          (fun () -> West);
        case
          ~title:"South"
          (Tag 3)
          unit
          (function South -> Some () | _ -> None)
          (fun () -> South);
        case
          ~title:"East"
          (Tag 4)
          unit
          (function East -> Some () | _ -> None)
          (fun () -> East);
        case
          ~title:"SouthWest"
          (Tag 5)
          unit
          (function SouthWest -> Some () | _ -> None)
          (fun () -> SouthWest);
        case
          ~title:"SouthEast"
          (Tag 6)
          unit
          (function SouthEast -> Some () | _ -> None)
          (fun () -> SouthEast);
        case
          ~title:"NorthWest"
          (Tag 7)
          unit
          (function NorthWest -> Some () | _ -> None)
          (fun () -> NorthWest);
        case
          ~title:"NorthEast"
          (Tag 8)
          unit
          (function NorthEast -> Some () | _ -> None)
          (fun () -> NorthEast) ]

  let position_encoding : C.position Data_encoding.t =
    let open Data_encoding in
    conv
      (fun { C.pos; relpos } -> (pos, relpos))
      (fun (pos, relpos) -> { C.pos; relpos })
      (tup2 Pt.encoding relpos_encoding)

  let command_encoding desc_encoding : C.t Data_encoding.t =
    let open Data_encoding in
    conv
      (fun { C.uid; desc } -> (uid, desc))
      (fun (uid, desc) -> { C.uid; desc })
      (tup2 int31 desc_encoding)

  let desc_encoding command_encoding : C.desc Data_encoding.t =
    let open Data_encoding in
    let open C in
    union
      [ case
          ~title:"Circle"
          (Tag 0)
          (tup2 Pt.encoding float)
          (function
            | Circle { center; radius } -> Some (center, radius) | _ -> None)
          (fun (center, radius) -> Circle { center; radius });
        case
          ~title:"Box"
          (Tag 1)
          (tup2 Pt.encoding Pt.encoding)
          (function Box { mins; maxs } -> Some (mins, maxs) | _ -> None)
          (fun (mins, maxs) -> Box { mins; maxs });
        case
          ~title:"Text"
          (Tag 2)
          (tup2 position_encoding Ctext.encoding)
          (function Text { pos; text } -> Some (pos, text) | _ -> None)
          (fun (pos, text) -> Text { pos; text });
        case
          ~title:"Style"
          (Tag 3)
          (tup2 Style.encoding command_encoding)
          (function Style { style; cmd } -> Some (style, cmd) | _ -> None)
          (fun (style, cmd) -> Style { style; cmd });
        case
          ~title:"Segment"
          (Tag 4)
          (tup2 Pt.encoding Pt.encoding)
          (function Segment { p1; p2 } -> Some (p1, p2) | _ -> None)
          (fun (p1, p2) -> Segment { p1; p2 });
        case
          ~title:"Bezier"
          (Tag 5)
          (tup4 Pt.encoding Pt.encoding Pt.encoding Pt.encoding)
          (function
            | Bezier { p1; c1; p2; c2 } -> Some (p1, c1, p2, c2) | _ -> None)
          (fun (p1, c1, p2, c2) -> Bezier { p1; c1; p2; c2 });
        case
          ~title:"Image"
          (Tag 6)
          null
          (function Image _ -> assert false | _ -> None)
          (fun _ -> assert false);
        case
          ~title:"DeclPt"
          (Tag 7)
          (tup2 Pt.encoding N.encoding)
          (function DeclPt { pt; name } -> Some (pt, name) | _ -> None)
          (fun (pt, name) -> DeclPt { pt; name });
        case
          ~title:"Rotate"
          (Tag 8)
          (tup2 float command_encoding)
          (function
            | Rotate { radians; cmd } -> Some (radians, cmd) | _ -> None)
          (fun (radians, cmd) -> Rotate { radians; cmd });
        case
          ~title:"Translate"
          (Tag 9)
          (tup2 Pt.encoding command_encoding)
          (function Translate { v; cmd } -> Some (v, cmd) | _ -> None)
          (fun (v, cmd) -> Translate { v; cmd });
        case
          ~title:"Scale"
          (Tag 10)
          (tup3 float float command_encoding)
          (function Scale { xs; ys; cmd } -> Some (xs, ys, cmd) | _ -> None)
          (fun (xs, ys, cmd) -> Scale { xs; ys; cmd });
        case
          ~title:"Wrap"
          (Tag 11)
          (list command_encoding)
          (function Wrap l -> Some l | _ -> None)
          (fun l -> Wrap l) ]

  let encoding =
    let open Data_encoding in
    mu "vlayout" (fun fix -> command_encoding (desc_encoding fix))
end
