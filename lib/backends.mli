module Cairo :
functor (C : Commands.CommandsSig) ->
sig

  val render : Cairo.context -> C.t -> unit
  
end
