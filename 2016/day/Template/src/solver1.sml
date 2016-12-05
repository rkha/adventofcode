structure Solver1 :> SOLVER =
struct
	datatype move = UP | DOWN | LEFT | RIGHT;

	exception Solver;

	val debug = true;
	fun debugPrint s = if (debug) then (print s) else ();
	fun debugPrintln s = if (debug) then (debugPrint (s ^ "\n")) else ();

	fun parseLine L = raise Solver;

	fun getAllLines (acc, ins) = (case (TextIO.inputLine ins) of
		NONE => List.rev acc
	|	SOME(line) => getAllLines (line :: acc, ins)
	);

	fun parse filename =
	let
		val ins = TextIO.openIn filename
		val moveStrings = getAllLines ([], ins)
		val _ = debugPrint ("Parsed " ^ (Int.toString (List.length moveStrings)) ^ " lines of input.\n")
		val moveList = List.map (parseLine o String.explode) moveStrings
	in
		moveList
	end;

	fun solve filename =
	let
		val _ = parse filename
	in
		(debugPrint ("(" ^ (Int.toString 0) ^ ")\n");
		0
		)
	end;
end
