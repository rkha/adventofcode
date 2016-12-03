structure Solver2 :> SOLVER =
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

	fun convertTris [] = []
	(* This makes me cry *)
	|   convertTris ([a1, a2, a3] :: [b1, b2, b3] :: [c1, c2, c3] :: L) = [a1, b1, c1] :: [a2, b2, c2] :: [a3, b3, c3] :: (convertTris L)
	|   convertTris _ = raise Solver;

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
		val colTris = convertTris triangles
		val valids = List.filter (isValid) colTris
	in
		(debugPrint ("(" ^ (Int.toString (List.length triangles)) ^ ")\n");
		(List.length valids)
		)
	end;
end
