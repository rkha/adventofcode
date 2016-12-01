structure Taxi1 :> TAXI =
struct
	datatype taxidir = RIGHT | LEFT;
	datatype orientation = NORTH | EAST | SOUTH | WEST;
	type taximove = {dir : taxidir, blocks : int};

	exception Taxi;

	val debug = false;

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
	|   taxiMoveHelper ((posX, posY), SOUTH, blocks) = ((posX, posY - blocks), SOUTH)
	|   taxiMoveHelper ((posX, posY), WEST, blocks) = ((posX - blocks, posY), WEST);
	fun taxiMove (pos, ori, {dir=nextDir, blocks=nextBlocks}) =
	let
		val newOri = rotate(ori, nextDir)
	in
		taxiMoveHelper (pos, newOri, nextBlocks)
	end;

	(* val taxiSolve : (int * int) * orientation * taximove list -> (int * int) *)
	fun taxiSolve (pos, ori, moves) =
	let
		fun taxiSolveHelper (move, (pos', ori')) = taxiMove (pos', ori', move)
		val (finalPos, finalOri) = List.foldl taxiSolveHelper (pos, ori) moves
	in
		finalPos
	end;

	(* val parse : string -> taximove list *)
	fun parseMove ((#"R") :: L) =
		{dir=RIGHT, blocks=valOf (Int.fromString (String.implode L))}
	|   parseMove ((#"L") :: L) =
		{dir=LEFT, blocks=valOf (Int.fromString (String.implode L))}
	|   parseMove _ = raise Taxi;

	fun parse filename =
	let
		val ins = TextIO.openIn filename
		val moveString = valOf (TextIO.inputLine ins)
		fun parseCSV c = (Char.isSpace c) orelse (c = #",")
		val moveTokens = String.tokens parseCSV moveString
		val moveList = List.map (parseMove o String.explode) moveTokens
	in
		moveList
	end;

	fun solve filename =
	let
		val (posX, posY) = taxiSolve ((0,0), NORTH, parse filename)
	in
		(if (debug) then (print ("(" ^ (Int.toString posX) ^ "," ^ (Int.toString posY) ^ ")\n")) else ();
		(Int.abs posX) + (Int.abs posY)
		)
	end;
end
