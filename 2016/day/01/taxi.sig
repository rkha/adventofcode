signature TAXI =
sig
	type taxidir
	type orientation
	type taximove

	val parse : string -> taximove list

	val rotate : orientation * taxidir -> orientation

	val taxiMove : (int * int) * orientation * taximove -> ((int * int) * orientation)
	val taxiSolve : (int * int) * orientation * taximove list -> (int * int)
end;
