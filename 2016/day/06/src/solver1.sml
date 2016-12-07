structure Solver1 :> SOLVER =
struct
	type move = char list;

	exception Solver;

	val debug = true;
	fun debugPrint s = if (debug) then (print s) else ();
	fun debugPrintln s = if (debug) then (debugPrint (s ^ "\n")) else ();

	val charList = [#"a", #"b", #"c", #"d", #"e", #"f", #"g", #"h", #"i", #"j", #"k", #"l", #"m", 
		#"n", #"o", #"p", #"q", #"r", #"s", #"t", #"u", #"v", #"w", #"x", #"y", #"z"
	];

	fun upto i j = if (i <= j) then (i :: (upto (i+1) j)) else [];

	fun countChars pos c L = List.foldl (fn (x,z) => if ((List.nth(x,pos)) = c) then (z+1) else z) 0 L;

	fun solveLine i moveList =
	let
		val counts = List.map (fn c => (c, countChars i c moveList)) charList
		val sortedCounts = ListMergeSort.sort (fn ((c1,x), (c2,y)) => x<y) counts
		val (topChar, topCount) = hd sortedCounts
	in
		topChar
	end;

	fun parseLine L = List.filter (fn c => not (Char.isSpace c)) L;

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
		val moveList = parse filename
		val messageLength = List.length (hd moveList)
		val messageList = List.map (fn i => solveLine i moveList) (upto 0 (messageLength-1))
	in
		(debugPrint ("(" ^ (String.implode messageList) ^ ")\n");
		0
		)
	end;
end
