structure Taxi :> TAXI =
struct
	datatype taxidir = RIGHT | LEFT;
	datatype orientation = NORTH | EAST | SOUTH | WEST;
	type taximove = {dir : taxidir, blocks : int};

	(* val parse : string -> taximove list *)
	fun parse filename =
	let
	in
	end;

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
	fun taxiMoveHelper ((posX, posY), NORTH, blocks) = ((posX, posY + blocks), NORTH)
	|   taxiMoveHelper ((posX, posY), EAST, blocks) = ((posX + blocks, posY), EAST)
	|   taxiMoveHelper ((posX, posY), SOUTH, blocks) = ((posX, posY - block), SOUTH)
	|   taxiMoveHelper ((posX, posY), WEST, blocks) = ((posX - blocks, posY), WEST);
	fun taxiMove (pos, ori, {dir=nextDir, blocks=nextBlocks}) =
	let
		val newOri = rotate(ori, nextDir)
	in
		taxiMoveHelper (pos, newOri, nextBlocks);
	end;

	(* val taxiSolve : (int * int) * taximove list -> (int * int) *)
	fun taxiSolve (pos, ori, moves) =
	let
		fun taxiSolveHelper (move, (pos, ori)) = taxiMove (pos, ori, move)
		val (pos, ori) = List.foldr taxiSolveHelper pos moves
	in
		pos
	end;
end
