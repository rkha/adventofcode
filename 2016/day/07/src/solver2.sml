structure Solver2 :> SOLVER =
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

	fun isABA [] = false
	|   isABA (a :: b :: c :: L) = ((a = c) andalso (not(a = b))) orelse (isABA (b :: c :: L))
	|   isABA _ = false;

	fun getABA [] = []
	|   getABA (L as (a :: b :: c :: L')) =
		if (isABA [a, b, c])
		then
			(a,b) :: (getABA (b :: c :: L'))
		else
			getABA (b :: c :: L')
	|   getABA _ = [];

	fun solveLine moveList =
	let 
		val ipList = List.filter (fn (IP s) => true | _ => false) moveList
		val hyperList = List.filter (fn (HYPERNET s) => true | _ => false) moveList

		val abaList = List.foldl (fn (x,z) => x@z) [] (List.map (fn (IP s) => getABA (String.explode s) | _ => []) ipList)
		val babList = List.foldl (fn (x,z) => x@z) [] (List.map (fn (HYPERNET s) => getABA (String.explode s) | _ => []) hyperList)

		val sslCheck = List.foldl (fn ((b,a),check) => check orelse (List.exists (fn (c1,c2) => (a=c1) andalso (b=c2)) abaList)) false babList
	in
		sslCheck
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
		val abaList = List.filter (solveLine) moveList
	in
		(debugPrint ("(" ^ (Int.toString (List.length abaList)) ^ ")\n");
		(*List.map (fn m => (List.app (fn ip => debugPrint (toString ip)) m; debugPrintln ("Result: " ^ (Bool.toString (solveLine m))))) notAbbaList;*)
		List.length abaList
		)
	end;
end
