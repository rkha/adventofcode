structure Solver1 :> SOLVER =
struct
	datatype ipv7 = IP of string | HYPERNET of string | UNMATCHED of string;
	type move = ipv7 list;

	exception Solver;

	val debug = true;
	fun debugPrint s = if (debug) then (print s) else ();
	fun debugPrintln s = if (debug) then (debugPrint (s ^ "\n")) else ();

	fun toString (IP s) = "IP " ^ s ^ " "
	|   toString (HYPERNET s) = "HYPERNET " ^ s ^ " "
	|   toString (UNMATCHED s) = "UNMATCHED " ^ s ^ " ";

	fun matchBracketsLeft [] [] = []
	|   matchBracketsLeft acc [] = [IP (String.implode (List.rev acc))]
	|   matchBracketsLeft acc (#"[" :: L) = (IP (String.implode (List.rev acc))) :: (matchBracketsRight [] L)
	|   matchBracketsLeft acc (c :: L) = matchBracketsLeft (c :: acc) L

	and matchBracketsRight acc [] = [UNMATCHED (String.implode (List.rev acc))]
	|   matchBracketsRight acc (#"]" :: L) = (HYPERNET (String.implode (List.rev acc))) :: (matchBracketsLeft [] L)
	|   matchBracketsRight acc (c :: L) = matchBracketsRight (c :: acc) L;

	fun matchBrackets [] = []
	|   matchBrackets L = matchBracketsLeft [] L;

	fun isABBA [] = false
	|   isABBA (a :: b :: c :: d :: L) =
		if ((a = d) andalso (b = c) andalso (not(a = b)))
		then
			true
		else
			(isABBA (b :: c :: d :: L))
	|   isABBA _ = false;

	fun solveLine moveList =
	let 
		val ipList = List.filter (fn (IP s) => true | _ => false) moveList
		val hyperList = List.filter (fn (HYPERNET s) => true | _ => false) moveList
		val isAbbaList = List.map (fn (IP s) => isABBA (String.explode s) | _ => false) ipList
		val antiAbbaList = List.map (fn (HYPERNET s) => isABBA (String.explode s) | _ => false) hyperList
	in
		if (List.foldl (fn (x,z) => x orelse z) false antiAbbaList)
		then
			false
		else
			List.foldl (fn (x,z) => x orelse z) false isAbbaList
	end;

	fun parseLine s =
	let
		val ipVals = matchBrackets (String.explode s)
	in
		ipVals
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
		val moveList = parse filename
		val abbaList = List.filter (solveLine) moveList
		val unmatchedList = List.filter (fn L => List.foldl (fn (UNMATCHED s, z) => true | (x,z) => false orelse z) false L) moveList
		val notAbbaList = List.filter (fn m => not(solveLine m)) moveList
	in
		(debugPrint ("(" ^ (Int.toString (List.length notAbbaList)) ^ ")\n");
		(*List.map (fn m => (List.app (fn ip => debugPrint (toString ip)) m; debugPrintln ("Result: " ^ (Bool.toString (solveLine m))))) notAbbaList;*)
		List.length abbaList
		)
	end;
end
