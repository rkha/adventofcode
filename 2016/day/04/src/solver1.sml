structure Solver1 :> SOLVER =
struct
	type move = string list * int * string
	(*type dict = {a : int, b : int, c : int, d : int, e : int, f : int, g : int, h : int, i : int, j : int, k : int, l : int, m : int,
		n : int, o : int, p : int, q : int, r : int, s : int, t : int, u : int, v : int, w : int, x : int, y : int, z : int
	};*)
	type bucket = char list * int;

	exception Solver;

	val debug = false;
	fun debugPrint s = if (debug) then (print s) else ();

	val charList = [#"a", #"b", #"c", #"d", #"e", #"f", #"g", #"h", #"i", #"j", #"k", #"l", #"m", 
		#"n", #"o", #"p", #"q", #"r", #"s", #"t", #"u", #"v", #"w", #"x", #"y", #"z"
	];
	fun countChars (c, L) = List.foldl (fn (x,z) => (if (c = x) then 1 else 0) + z) 0 L;

	fun getTopFive acc [] = List.rev acc
	|   getTopFive [] ((c, ccount) :: L) = getTopFive [([c], ccount)] L
	|   getTopFive acc ((_, 0) :: L) = List.rev acc
	|   getTopFive (acc as ((aL, acount) :: ass)) ((next as (c, ccount)) :: L) =
		if ((List.length acc) = 5)
		then
			if (acount = ccount)
			then
				getTopFive ((c :: aL, acount) :: ass) L
			else
				List.rev acc
		else if (acount = ccount)
		then
			getTopFive ((c :: aL, acount) :: ass) L
		else
			getTopFive (([c], ccount) :: acc) L;
	fun getUntiedFive acc [] = List.rev acc
	|   getUntiedFive acc ((cL, ccount) :: L) =
		if ((List.length acc) = 5)
		then
			List.rev acc
		else if ((List.length acc) + (List.length cL) > 5)
		then
			getUntiedFive acc [(List.take (cL, 5 - (List.length acc)), ccount)]
		else
			getUntiedFive ((List.map (fn c => (c,ccount)) cL) @ acc) L
	fun solveLine (nameTokens, ID, checksum) =
	let
		val mergedTokens = List.foldl (fn (x,z) => (String.explode x) @ z) [] nameTokens
		val charCounts = List.map (fn c => (c, List.length (List.filter (fn c' => c = c') mergedTokens))) charList
		val sortedCounts = ListMergeSort.sort (fn ((a, count1),(b, count2)) => count1 < count2) charCounts
		val topFiveCounts = getUntiedFive [] (getTopFive [] sortedCounts)
		val _ = List.app (fn (x, xcount) => debugPrint ("(" ^ (Char.toString x) ^ ", " ^ (Int.toString xcount) ^ ")")) topFiveCounts
		val _ = debugPrint "\n"
		val checksumCheck = List.map (fn c => List.exists (fn (c', _) => c=c') topFiveCounts) (String.explode checksum)
			(* List.map (fn c => List.exists (fn (xL, xcount) => List.exists (fn c' => c=c') xL) topFiveCounts) (String.explode checksum) *)
			(* List.map (fn (xL, xcount) => List.exists (fn c => List.exists (fn c' => c'=c) xL) (String.explode checksum)) topFiveCounts *)
	in
		List.foldl (fn (x,z) => x andalso z) true checksumCheck
	end;

	fun splitChecksumID s =
	let
		val [ID, checksumBracket] = String.tokens (fn c => c = #"[") s
		val checksum :: blanks = (debugPrint ("checksumBracket = " ^ checksumBracket); String.tokens (fn c => c = #"]") checksumBracket)
	in
		(valOf (Int.fromString ID), checksum)
	end;
	fun parseLine line =
	let
		val hyphenTokens = String.tokens (fn c => c = #"-") line
		val (checksumID :: nameRevTokens) = List.rev hyphenTokens
		val nameTokens = List.rev nameRevTokens
		val (ID, checksum) = splitChecksumID checksumID
	in
		(nameTokens, ID, checksum)
	end;

	fun getAllLines (acc, ins) = (case (TextIO.inputLine ins) of
		NONE => List.rev acc
	|	SOME(line) => getAllLines (line :: acc, ins)
	);

	fun parse filename =
	let
		val ins = TextIO.openIn filename
		val moveStrings = getAllLines ([], ins)
		val _ = debugPrint ("Parsed " ^ (Int.toString (List.length moveStrings)) ^ " lines of input.\n")
		val moveList = List.map parseLine moveStrings
	in
		moveList
	end;

	fun solve filename =
	let
		val realNames = List.filter solveLine (parse filename)
		val _ = List.app (fn (names, _, _) => List.app debugPrint names) realNames
		val idSum = List.foldl (Int.+) 0 (List.map (fn (_, id, _) => id) realNames)
	in
		(debugPrint ("(Number of real lines: " ^ (Int.toString (List.length realNames)) ^ ")\n");
		idSum
		)
	end;
end
