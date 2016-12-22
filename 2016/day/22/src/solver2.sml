structure Solver2 :> SOLVER =
struct
	type move = {x : int, y : int, size : int, used : int, avail : int, use : int};

	exception Solver;

	val debug = true;
	fun debugPrint s = if (debug) then (print s) else ();
	fun debugPrintln s = if (debug) then (debugPrint (s ^ "\n")) else ();

	fun seq i j = if (i <= j) then i :: (seq (i+1) j) else [];

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
		(* val _ = debugPrintln ("Parsed node at (" ^ (Int.toString x) ^ "," ^ (Int.toString y) ^ ") with fields: " ^ (Int.toString size) ^ " " ^ (Int.toString used) ^ " " ^ (Int.toString avail) ^ " " ^ (Int.toString use)) *)
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
		(*val _ = if (fitCheck) then (debugPrintln ("Nodes (" ^ (Int.toString x1) ^ "," ^ (Int.toString y1) ^ ") and (" ^ (Int.toString x2) ^ "," ^ (Int.toString y2) ^ ") fit.")) else ()*)
	in
		emptyCheck andalso equalCheck andalso fitCheck
	end;

	fun makeSquareCoords L = List.foldl (op@) [] (List.map (fn x => List.map (fn y => (x,y)) L) L);
	fun makeRectCoords L1 L2 = List.map (fn x => List.map (fn y => (x,y)) L2) L1;

	fun matrixMap f M = List.map (List.map f) M;

	fun isNode (xTarget,yTarget) {x=x,y=y,size=size,used=used,avail=avail,use=use} = (x=xTarget) andalso (y=yTarget);

	fun nodeToChar nodeList (x,y) =
	let
		val filterList = List.filter (isNode (x,y)) nodeList
		val [targetNode] = filterList
		val {x=tempX,y=tempY,size=tempSize,used=tempUsed,avail=tempAvail,use=tempUse} = targetNode
	in
		if (tempSize > 100)
		then
			"# "
		else if (tempUsed = 0)
		then
			"_ "
		else if ((tempX,tempY) = (0,0))
		then
			"S "
		else if ((tempX,tempY) = (34,0))
		then
			"G "
		else
			". "
	end;

	fun findMaxCoords (xacc,yacc) (node :: nodeList) =
	let
		val {x=x,y=y,size=_,used=_,avail=_,use=_} = node
	in
		findMaxCoords (Int.max(xacc,x), Int.max(yacc,y)) nodeList
	end
	|   findMaxCoords (xacc,yacc) [] = (xacc,yacc);

	fun solve filename =
	let
		val nodeList = parse filename
		val allPairs = makeSquareCoords nodeList
		val _ = debugPrintln ("Got " ^ (Int.toString (List.length allPairs)) ^ " combinations of nodes.")
		val allValidPairs = List.filter isViable allPairs

		val (gridX, gridY) = findMaxCoords (0,0) nodeList

		val coords = (makeRectCoords (seq 0 gridX) (seq 0 gridY))
		val gridChars = matrixMap (nodeToChar nodeList) coords

		val gridLines = List.map (fn L => List.foldr (op^) "" L) gridChars
		val _ = List.app debugPrintln gridLines
	in
		(debugPrint ("(" ^ (Int.toString 0) ^ ")\n");
		List.length allValidPairs
		)
	end;
end
