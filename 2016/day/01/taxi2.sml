structure Taxi2 :> TAXI2 =
struct
	datatype taxidir = RIGHT | LEFT;
	datatype orientation = NORTH | EAST | SOUTH | WEST;
	type taximove = {dir : taxidir, blocks : int};

	exception Taxi;

	val debug = false;
	fun debugPrint s = if (debug) then print s else ();

	fun debugPrintTuple (x,y) = debugPrint (
		"(" ^ (Int.toString x) ^ "," ^ (Int.toString y) ^ ")"
	);
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
	fun getDelta NORTH = (0, 1)
	|   getDelta EAST = (1, 0)
	|   getDelta SOUTH = (0, ~1)
	|   getDelta WEST = (~1, 0);
	fun taxiMoveHelper (posAcc, pos, ori, 0) = (
		List.map (debugPrintTuple) posAcc;
		debugPrint "\n";
		((pos, ori), posAcc)
	)
	|   taxiMoveHelper (posAcc, (posX, posY), ori, blocks) =
	let
		val (dx, dy) = getDelta ori
		val (newX, newY) = (posX + dx, posY + dy)
	in
		taxiMoveHelper ((newX, newY) :: posAcc, (newX, newY), ori, blocks-1)
	end;
	fun taxiMove (posList, pos, ori, {dir=nextDir, blocks=nextBlocks}) =
	let
		val newOri = rotate(ori, nextDir)
	in
		taxiMoveHelper (posList, pos, newOri, nextBlocks)
	end;

	(* val taxiSolve : (int * int) * orientation * taximove list -> (int * int) *)
	fun taxiDupeFinder acc [] = (0,0)
	|   taxiDupeFinder acc (x :: L) =
		if (List.exists (fn y => x=y) acc)
		then
			x
		else
			taxiDupeFinder (x :: acc) L;

	fun taxiSolve (pos, ori, moves) =
	let
		fun taxiSolveHelper (move, ((pos', ori'), posList)) =
			taxiMove (posList, pos', ori', move)
		val ((finalPos, finalOri), finalPosList) =
			List.foldl taxiSolveHelper ((pos, ori), [(0,0)]) moves
		val solution = taxiDupeFinder [] (List.rev finalPosList)
	in
		solution
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
		(debugPrintTuple (posX, posY);
		(Int.abs posX) + (Int.abs posY)
		)
	end;
end
