signature SOLVER =
sig
	type move
	val getAllLines : string list * TextIO.instream -> string list
	val parse : string -> move list
	val solve : string -> int
end
