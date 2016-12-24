signature SOLVER =
sig
	type move

	val debug : bool
	val debugPrint : string -> unit
	val debugPrintln : string -> unit

	val getAllLines : string list * TextIO.instream -> string list
	val parse : string -> move list
	val solve : string -> int
end
