structure Taxi :> TAXI =
struct
	datatype taxidir = RIGHT | LEFT;
	datatype orientation = NORTH | EAST | SOUTH | WEST;
	type taximove = {dir : taxidir, blocks : int};

	(* val parse : string -> taximove list *)
	fun parse filename = [];

	(* val rotate : orientation * taxidir -> orientation *)
	fun rotate (NORTH, RIGHT) = EAST
	|   rotate (EAST, RIGHT) = SOUTH
	|   rotate (SOUTH, RIGHT) = WEST
	|   rotate (WEST, RIGHT) = NORTH
	|   rotate (NORTH, LEFT) = WEST
	|   rotate (WEST, LEFT) = SOUTH
	|   rotate (SOUTH, LEFT) = EAST
	|   rotate (EAST, LEFT) = NORTH;

	(* val taxiMove : (int * int) * taximove -> (int * int) *)
	fun taxiMove ((posX, posY), {dir=RIGHT, blocks=blocks}) = (posX, posY)
	|   taxiMove ((posX, posY), {dir=LEFT, blocks=blocks}) = (posX, posY);

	(* val taxiSolve : (int * int) * taximove list -> (int * int) *)
	fun taxiSolve (pos, moves) = List.foldr (fn (x,z) => taxiMove (z,x)) pos moves;
end;
