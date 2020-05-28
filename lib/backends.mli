module Cairo (C : Commands.S) : sig
  val render : Cairo.context -> C.t -> unit
end
