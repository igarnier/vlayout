module type Name =
  sig
    type t
    val compare : t -> t -> int
    val print : t -> string
  end

module type CommandsSig =
  sig

    type name
           
    type 'a s = { tag : 'a; desc : desc; }
     and desc =
         Circle of Pt.t * float
       | Box of Pt.t * Pt.t
       | Text of Pt.t * int * string
       | Color of float * float * float
       | Segment of Pt.t * Pt.t
       | Bezier of Pt.t * Pt.t * Pt.t * Pt.t
       | Image of Pt.t * Image.t                                          
       | DeclPt of Pt.t * name

    type t = int option s
    type untagged = unit s

    module Tagged :
    sig
      val mktag : 'a -> desc -> 'a s
      val circle : tag:'a -> p:Pt.t -> radius:float -> 'a s
      val box : tag:'a -> mins:Pt.t -> maxs:Pt.t -> 'a s
      val text : tag:'a -> p:Pt.t -> sz:int -> s:string -> 'a s
      val color : tag:'a -> r:float -> g:float -> b:float -> 'a s
      val segment : tag:'a -> p1:Pt.t -> p2:Pt.t -> 'a s
      val bezier :
        tag:'a -> p1:Pt.t -> c1:Pt.t -> p2:Pt.t -> c2:Pt.t -> 'a s
      val ubezier : tag:'a -> p1:Pt.t -> p2:Pt.t -> angle:float -> 'a s
      val image : tag:'a -> p:Pt.t -> im:Image.t -> 'a s
      val declpt : tag:'a -> p:Pt.t -> n:name -> 'a s
    end

    val mkunit : desc -> untagged
    val circle : p:Pt.t -> radius:float -> untagged
    val box : mins:Pt.t -> maxs:Pt.t -> untagged
    val text : p:Pt.t -> sz:int -> s:string -> untagged
    val color : r:float -> g:float -> b:float -> untagged
    val segment : p1:Pt.t -> p2:Pt.t -> untagged
    val bezier : p1:Pt.t -> c1:Pt.t -> p2:Pt.t -> c2:Pt.t -> untagged
    val ubezier : p1:Pt.t -> p2:Pt.t -> angle:float -> untagged
    val image : p:Pt.t -> im:Image.t -> untagged
    val declpt : p:Pt.t -> n:name -> untagged
    val mid : Pt.t -> Pt.t -> Pt.t
    val bezier_midpoint : Pt.t -> Pt.t -> Pt.t -> Pt.t -> Pt.t
    val anchor_of : 'a s -> Pt.t
    val bouquet_of_segments : Pt.t -> Pt.t -> name list -> untagged list
    val print_cmd : 'a s -> string
                              
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
      val of_command : 'a s -> t
      val of_commands : 'a s list -> t
    end

    val tag : 'a s list -> 'b -> 'b s list
    val untag : 'a s list -> untagged list
    val translate : 'a s list -> Pt.t -> 'a s list
    val center_to_page : float * float -> 'a s list -> 'a s list
    val crop : 'a s list -> 'a s list
                               
    module Arrow :
    sig
      type style = {
          startp : float;
          endp : float;
          arrowp : float;
          legs : float;
          angle : float;
        }
      type t = {
          start : name;
          finish : name;
          style : style;
          smart : bool;
        }
      val mkarrowhead : float -> float -> Pt.t * Pt.t
      val mkarrow : style -> Pt.t -> Pt.t -> untagged list
      val mkarrow_curvy : style -> Pt.t -> Pt.t -> float -> untagged list
      val to_segments : 'a list -> ('a * 'a) list
      val mk_multisegment_arrow : style -> Pt.t list -> untagged list
      val default_style : style
      val mid_style : style
    end
      
    type layout
           
    val cmd : name:int option -> untagged list -> layout
    val hbox : deltax:float -> layout_list:layout list -> layout
    val vbox : deltay:float -> layout_list:layout list -> layout
    val arrow :
      start:name -> finish:name -> sty:Arrow.style -> layout -> layout
    val smart_arrow :
      start:name -> finish:name -> sty:Arrow.style -> layout -> layout
                                                                
    val halign :
      ('a s list * Bbox.t) list -> float -> ('a s list * Bbox.t) list
    val valign :
      ('a s list * Bbox.t) list -> float -> ('a s list * Bbox.t) list
                                                                 
    exception Emit_error

    val emit_commands_with_bbox : layout -> t list * Bbox.t
                                                       
    val emit_commands : layout -> t list
                                    
    val emit_commands_centered : float * float -> layout -> t list

  end

    
module Make : functor (N : Name) -> (CommandsSig with type name = N.t)
                                                                   
