structure Solver1 :> SOLVER =
struct
	type move = string list;

	exception Solver;

	val debug = true;
	fun debugPrint s = if (debug) then (print s) else ();

	fun isValid (L as ([_, _, _])) =
	let
		val [a, b, c] = ListMergeSort.sort (Int.>) L
	in
		c < (a + b)
	end
	|   isValid _ = raise Solver;

	fun parseLine L = String.tokens (Char.isSpace) L;

	fun getAllLines (acc, ins) = (case (TextIO.inputLine ins) of
		NONE => List.rev acc
	|	SOME(line) => getAllLines (line :: acc, ins)
	);

	fun parse filename =
	let
		val ins = TextIO.openIn filename
		val moveStrings = getAllLines ([], ins)
		val moveList = List.map (parseLine) moveStrings
	in
		moveList
	end;

	fun solve filename =
	let
		val triangles = List.map (fn x => List.map (fn y => valOf (Int.fromString y)) x) (parse filename)
		val valids = List.filter (isValid) triangles
	in
		(debugPrint ("(" ^ (Int.toString (List.length triangles)) ^ ")\n");
		(List.length valids)
		)
	end;
end
