structure Taxi :> TAXI =
struct
	datatype taxidir = RIGHT | LEFT;
	datatype orientation = NORTH | EAST | SOUTH | WEST;
	type taximove = {dir : taxidir, blocks : int};

	(* val parse : string -> taximove list *)
	fun parse filename = [];

	(* val taxiMove : (int * int) * taximove -> (int * int) *)
	fun taxiMove ((posX, posY), {dir=RIGHT, blocks=blocks}) = (posX, posY);

	(* val taxiSolve : (int * int) * taximove list -> (int * int) *)
	fun taxiSolve (pos, moves) = List.foldr taxiMove pos moves;
end;
