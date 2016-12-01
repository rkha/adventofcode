signature TAXI =
sig
	type taxidir
	type orientation
	type taximove

	exception Taxi

	val rotate : orientation * taxidir -> orientation

	val taxiMove : ((int * int) * orientation * taximove) -> ((int * int) * orientation)
	val taxiSolve : ((int * int) * orientation * taximove list) -> (int * int)

	val parseMove : char list -> taximove
	val parse : string -> taximove list
	val solve : string -> int
end;
