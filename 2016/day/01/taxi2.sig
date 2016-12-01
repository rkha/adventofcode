signature TAXI2 =
sig
	type taxidir
	type orientation
	type taximove

	exception Taxi

	val rotate : orientation * taxidir -> orientation

	val taxiSolve : ((int * int) * orientation * taximove list) -> (int * int)

	val parseMove : char list -> taximove
	val parse : string -> taximove list
	val solve : string -> int
end;
