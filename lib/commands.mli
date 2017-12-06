module type Name =
  sig
    type t
    val compare : t -> t -> int
    val print : t -> string
  end

module type CommandsSig =
  sig

    type name

    (** Relative positioning. For instance, p = { pos; relpos = North } means
        that the object at relative position p has a bounding box with [pos]
        at its North point (North = mîdpoint of the top side of the box). *)
    type position  = { pos : Pt.t; relpos : relpos }
     and relpos =
       | North    
       | West     
       | South    
       | East     
       | SouthWest
       | SouthEast
       | NorthWest
       | NorthEast

    type t =
      {
        uid  : int;
        desc : desc
      }
     and desc =
       | Circle  of { center : Pt.t; radius : float }
       | Box     of { mins : Pt.t; maxs : Pt.t }
       (* | Text    of { pos : position; width : float; height : float; text : string } *)
       | Text    of { pos : position; text : Ctext.t }
       | Style   of { style : Style.t; subcommands : t list }
       | Segment of { p1 : Pt.t; p2 : Pt.t }
       | Bezier  of { p1 : Pt.t; c1 : Pt.t; p2 : Pt.t; c2 : Pt.t }
       | Image   of { pos : Pt.t; image : Image.t }
       | DeclPt  of { pt : Pt.t; name : name }
       | Rotate  of { radians : float; subcommands : t list }
       | Translate of { v : Pt.t; subcommands : t list }
       | Scale   of { xs : float; ys : float; subcommands : t list }
                    
    type alias = t

    val text_position : position -> float -> float -> Pt.t

    module Bbox :
    sig
      include Bbox.S

      val of_command :  alias -> t
      val of_commands : alias list -> t
    end

    module Arrow :
    sig
      type style =
        {
          startp : float; (* [0,1] *)
          endp   : float; (* [0,1] *)
          arrowp : float; (* [0,1] *)
          legs   : float; (* legs length, >= 0 *)
          angle  : float  (* legs angle *)
        }

      val default_style : style
      val mid_style     : style

      val mkarrow       : style:style -> start:Pt.t -> finish:Pt.t -> t list
      val mkarrow_curvy : style:style -> start:Pt.t -> finish:Pt.t -> angle:float -> t list
      val mk_multisegment_arrow : style:style -> points:Pt.t list -> t list
             
    end

    val circle    : center:Pt.t -> radius:float -> t
    val box       : mins:Pt.t -> maxs:Pt.t -> t
    (* val text      : pos:position -> width:float -> height:float -> text:string -> t *)
    val text      : pos:position -> size:float -> text:string -> t
    val style     : style:Style.t -> subcommands:(t list) -> t
    val segment   : p1:Pt.t -> p2:Pt.t -> t
    val bezier    : p1:Pt.t -> c1:Pt.t -> p2:Pt.t -> c2:Pt.t -> t
    val ubezier   : p1:Pt.t -> p2:Pt.t -> angle:float -> t
    val image     : pos:Pt.t -> image:Image.t -> t
    val declpt    : pt:Pt.t -> name:name -> t
    val rotate    : radians:float -> subcommands:(t list) -> t
    val translate : v:Pt.t -> subcommands:(t list) -> t
    val scale     : xs:float -> ys:float -> subcommands:(t list) -> t

    val print : t -> string

    val center_to_page : float*float -> t list -> t list

    module NameMap : Map.S with type key = name

    val collect_declared_points : t list -> Pt.t NameMap.t

    type hposition = 
      [ `Hcentered
      | `Bottom
      | `Top
      ]

    type vposition = 
      [ `Vcentered
      | `Left
      | `Right
      ]

    type layout

    val cmd  : t list -> layout
    val hbox : ?pos:hposition -> ?deltax:float -> layout list -> layout
    val vbox : ?pos:vposition -> ?deltay:float -> layout list -> layout

    val arrow : start:name -> finish:name -> sty:Arrow.style -> layout -> layout
    (* val smart_arrow : start:name -> finish:name -> sty:Arrow.style -> layout -> layout *)

    val emit_commands_with_bbox : layout -> t list * Bbox.t
    val emit_commands : layout -> t list

  end
    
module Make : functor (N : Name) -> (CommandsSig with type name = N.t)
