signature TAXI =
sig
	type taxidir
	type orientation
	type taximove

	val parse : string -> taximove list

	val taxiMove : (int * int) * taximove -> (int * int)
	val taxiSolve : (int * int) * taximove list -> (int * int)
end;
