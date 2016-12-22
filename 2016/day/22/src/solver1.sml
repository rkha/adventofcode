structure Solver1 :> SOLVER =
struct
	type move = {x : int, y : int, size : int, used : int, avail : int, use : int};

	exception Solver;

	val debug = true;
	fun debugPrint s = if (debug) then (print s) else ();
	fun debugPrintln s = if (debug) then (debugPrint (s ^ "\n")) else ();

	fun parseNode (_ :: x :: y :: _) =
	let
		val xVal = hd (String.tokens (fn c => c= #"x") x)
		val yVal = hd (String.tokens (fn c => c= #"y") y)
	in
		(valOf (Int.fromString xVal), valOf (Int.fromString yVal))
	end
	|   parseNode _ = raise Solver;

	fun parseLine s =
	let
		val (devgrid :: sizestr :: usedstr :: availstr :: usestr :: []) = String.tokens Char.isSpace s
		val size = valOf (Int.fromString sizestr)
		val used = valOf (Int.fromString usedstr)
		val avail = valOf (Int.fromString availstr)
		val use = valOf (Int.fromString usestr)
		val (x,y) = parseNode (String.tokens (fn c => c = #"-") devgrid)
		val _ = debugPrintln ("Parsed node at (" ^ (Int.toString x) ^ "," ^ (Int.toString y) ^ ") with fields: " ^ (Int.toString size) ^ " " ^ (Int.toString used) ^ " " ^ (Int.toString avail) ^ " " ^ (Int.toString use))
	in
		{x=x,y=y,size=size,used=used,avail=avail,use=use}
	end;

	fun getAllLines (acc, ins) = (case (TextIO.inputLine ins) of
		NONE => List.rev acc
	|	SOME(line) => getAllLines (line :: acc, ins)
	);

	fun parse filename =
	let
		val ins = TextIO.openIn filename
		val moveStrings = List.drop(getAllLines ([], ins), 2)
		val _ = debugPrint ("Parsed " ^ (Int.toString (List.length moveStrings)) ^ " lines of input.\n")
		val moveList = List.map parseLine moveStrings
	in
		moveList
	end;

	fun isViable ({x=x1,y=y1,size=size1,used=used1,avail=avail1,use=use1},{x=x2,y=y2,size=size2,used=used2,avail=avail2,use=use2}) =
	let
		val emptyCheck = not(used1 = 0)
		val equalCheck = (not(x1=x2)) orelse (not(y1=y2))
		val fitCheck = used1 <= avail2
		val _ = if (fitCheck) then (debugPrintln ("Nodes (" ^ (Int.toString x1) ^ "," ^ (Int.toString y1) ^ ") and (" ^ (Int.toString x2) ^ "," ^ (Int.toString y2) ^ ") fit.")) else ()
	in
		emptyCheck andalso equalCheck andalso fitCheck
	end;

	fun solve filename =
	let
		val nodeList = parse filename
		val allPairs = List.foldl (op@) [] (List.map (fn x => List.map (fn y => (x,y)) nodeList) nodeList)
		val _ = debugPrintln ("Got " ^ (Int.toString (List.length allPairs)) ^ " combinations of nodes.")
		val allValidPairs = List.filter isViable allPairs
	in
		(debugPrint ("(" ^ (Int.toString 0) ^ ")\n");
		List.length allValidPairs
		)
	end;
end
