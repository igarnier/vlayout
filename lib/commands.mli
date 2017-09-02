module type Name =
  sig
    type t
    val compare : t -> t -> int
    val print : t -> string
  end

module type CommandsSig =
  sig

    type name

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
        tag  : int option;
        desc : desc
      }
     and desc =
       | Circle  of { center : Pt.t; radius : float }
       | Box     of { mins : Pt.t; maxs : Pt.t }
       | Text    of { pos : position; size : float; text : string }
       | Style   of { style : Style.t; subcommands : t list }
       | Segment of { p1 : Pt.t; p2 : Pt.t }
       | Bezier  of { p1 : Pt.t; c1 : Pt.t; p2 : Pt.t; c2 : Pt.t }
       | Image   of { pos : Pt.t; image : Image.t }
       | DeclPt  of { pt : Pt.t; name : name }

    type alias = t

    val text_position : position -> float -> string -> Pt.t

    module Bbox :
    sig
      type t = Bbox.t = { mins : Pt.t; maxs : Pt.t; }
      val box : Pt.t -> Pt.t -> t
      val empty : t
      val center : t -> Pt.t
      val width : t -> float
      val height : t -> float
      val join : t -> t -> t
      val of_points : Pt.t list -> t
      val se : t -> Pt.t
      val sw : t -> Pt.t
      val ne : t -> Pt.t
      val nw : t -> Pt.t
      val print : t -> string
      val of_command :  alias -> t
      val of_commands : alias list -> t
    end

    module Arrow :
    sig
      type style

      val default_style : style
      val mid_style     : style

      val mkarrow       : style:style -> start:Pt.t -> finish:Pt.t -> t list
      val mkarrow_curvy : style:style -> start:Pt.t -> finish:Pt.t -> angle:float -> t list
      val mk_multisegment_arrow : style:style -> points:Pt.t list -> t list
             
    end

    val circle  : center:Pt.t -> radius:float -> t
    val box     : mins:Pt.t -> maxs:Pt.t -> t
    val text    : pos:position -> size:float -> text:string -> t
    val style   : style:Style.t -> subcommands:(t list) -> t
    val segment : p1:Pt.t -> p2:Pt.t -> t
    val bezier  : p1:Pt.t -> c1:Pt.t -> p2:Pt.t -> c2:Pt.t -> t
    val ubezier : p1:Pt.t -> p2:Pt.t -> angle:float -> t
    val image   : pos:Pt.t -> image:Image.t -> t
    val declpt  : pt:Pt.t -> name:name -> t


    type layout

    val cmd  : name:(int option) -> t list -> layout
    val hbox : deltax:float -> layout_list:(layout list) -> layout
    val vbox : deltay:float -> layout_list:(layout list) -> layout

    val arrow : start:name -> finish:name -> sty:Arrow.style -> layout -> layout
    val smart_arrow : start:name -> finish:name -> sty:Arrow.style -> layout -> layout                                                                            
    val emit_commands_with_bbox : layout -> t list * Bbox.t
    val emit_commands : layout -> t list
    val emit_commands_centered : float * float -> layout -> t list
  end
    
module Make : functor (N : Name) -> (CommandsSig with type name = N.t)
