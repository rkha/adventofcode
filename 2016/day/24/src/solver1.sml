structure Solver1 :> SOLVER =
struct
	type move = char list;
	datatype cube = OPEN | WALL | FILLED;

	exception Solver;

	val debug = true;
	fun debugPrint s = if (debug) then (print s) else ();
	fun debugPrintln s = if (debug) then (debugPrint (s ^ "\n")) else ();

	fun seq i j = if (i <= j) then i :: (seq (i+1) j) else [];

	fun getMatrixSize M = (List.length M, List.length (hd M));
	fun printTuple (row,col) = "(" ^ (Int.toString row) ^ "," ^ (Int.toString col) ^ ")";

	fun setList L index newVal =
	let
		val front = List.take(L, index)
		val back = List.drop(L, index+1)
	in
		front @ (newVal :: back)
	end;
	fun setMatrix M (x,y) newVal =
	let
		val newRow = setList (List.nth(M, y)) x newVal
	in
		setList M y newRow
	end;
	fun getMatrix M (x,y) =
	let
		(* val _ = debugPrint ("Matrix size: " ^ (printTuple (getMatrixSize M)) ^ "; Checking row = " ^ (Int.toString y) ^ " ")
		val _ = debugPrintln ("Checking y = " ^ (Int.toString y)) *)
	in
		List.nth(List.nth(M,y),x)
	end;

	fun matrixMap f M = List.map (fn row => List.map f row) M;

	fun coordFilterX (x,y) [] = []
	|   coordFilterX (x,y) (col :: row) = (case col of
		FILLED => (x,y) :: (coordFilterX (x+1,y) row)
	|	_ => coordFilterX (x+1,y) row
	);
	fun coordFilter _ [] = []
	|   coordFilter y (row :: M) = (coordFilterX (0,y) row) @ (coordFilter (y+1) M);

	fun isValidCoord M (x,y) =
	let
		val height = List.length M
		val width = List.length (hd M)
	in
		if ((x >= 0) andalso (x < width) andalso (y >= 0) andalso (y < height))
		then
			(case (getMatrix M (x,y)) of
				OPEN => true
			|	_ => false
			)
		else
			false
	end;

	fun printMatrix [] = "\n"
	|   printMatrix (row :: M) = (String.implode ((List.map (fn FILLED => #"O" | OPEN => #"." | WALL => #"#") row) @ [#"\n"])) ^ (printMatrix M);

	fun countBits 0w0 = 0
	|   countBits n = (if ((Word.andb(n,0w1)) = 0w1) then 1 else 0) + (countBits (Word.>>(n,0w1)));

	fun isWall G (x,y) = (getMatrix G (x,y)) = #"#";
	fun isOpen G (x,y) = not(isWall G (x,y))

	fun floodFill Mstate acc (x,y) =
	let
		val _ = debugPrintln ("Starting new floodfill step #" ^ (Int.toString acc))
		(* val _ = debugPrintln (printMatrix Mstate) *)

		(* Get list of all filled coords *)
		val filledCoords = coordFilter 0 Mstate
		val _ = debugPrintln ("Number of filled coords: " ^ (Int.toString (List.length filledCoords)))
		(* Get list of all possible expansion coords *)
		val possibleCoords' = List.map (fn (col,row) => [(col-1,row),(col+1,row),(col,row-1),(col,row+1)]) filledCoords
		val possibleCoords = List.foldl (fn (coords,moreCoords) => coords @ moreCoords) [] possibleCoords'
		(* Filter out negative or wall or filled coords *)
		val filteredCoords = List.filter (isValidCoord Mstate) possibleCoords
		val _ = debugPrintln ("Number of newly filled coords: " ^ (Int.toString (List.length filteredCoords)))
		val _ = if ((List.length filteredCoords) = 0) then raise Solver else ()
		(* Fill in new coords *)
		val newState = List.foldl (fn (coord,prevState) => setMatrix prevState coord FILLED) Mstate filteredCoords
		val _ = debugPrintln ("Grid size: " ^ (printTuple (getMatrixSize newState)) ^ "; Looking up: " ^ (printTuple (x,y)))
	in
		(case (getMatrix newState (x,y)) of
			FILLED => (debugPrintln "Done"; acc+1)
		|	_ => floodFill newState (acc+1) (x,y)
		)
	end;

	fun solveMaze G (startCol, startRow) (targetCol, targetRow) =
	let
		val (height,width) = getMatrixSize G
		val _ = debugPrintln ("Finding path from " ^ (printTuple (startCol, startRow)) ^ " to " ^ (printTuple (targetCol, targetRow)))
		val newState = List.map (fn row => List.map (fn col => (col,row)) (seq 0 (width-1))) (seq 0 (height-1));
		val newState' = matrixMap (fn (col,row) => if (isWall G (col,row)) then WALL else OPEN) newState
		val newState'' = setMatrix newState' (startCol, startRow) FILLED
	in
		floodFill newState'' 0 (targetCol, targetRow)
	end;

	fun parseLine s = List.filter (fn c => not (Char.isSpace c)) (String.explode s);

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

	(* Max n is length-1, so shouldn't subscript *)
	fun listRemove (L,n) = List.take(L,n) @ (List.drop(L,n+1));
	fun getAllCombinations [x] = [[x]]
	|   getAllCombinations L = List.foldl (op@) [] (List.map (fn x => List.map (fn l => (List.nth(L,x))::l) (getAllCombinations (listRemove(L,x)))) (seq 0 ((List.length L)-1)));
	fun getPathLength ({startNode=startNode,endNode=endNode,distance=distance} :: G) (node1,node2) = if ((node1,node2) = (startNode,endNode)) then distance else getPathLength G (node1,node2)
	|   getPathLength [] _ = raise Solver;
	fun sumCombination G (a :: (L as (b :: _))) = (getPathLength G (a,b)) + (sumCombination G L)
	|   sumCombination G _ = 0;
	fun printIntListHelper [] = ""
	|   printIntListHelper [x] = (Int.toString x)
	|   printIntListHelper (x :: L) = (Int.toString x) ^ "," ^ (printIntListHelper L);
	fun printIntList L = "[" ^ (printIntListHelper L) ^ "]";

	fun findLeftPath (G,rightPath) =
	let
		val rightEnd = List.last rightPath
		val allLeftPaths = List.map (fn L => rightEnd :: L) (getAllCombinations (seq 5 7))
		val (shortestLength, shortestPath) = List.foldl (fn (nextPath,(accLength,accPath)) => if ((sumCombination G nextPath) < accLength) then (sumCombination G nextPath, nextPath) else (accLength,accPath)) (999999,[]) (allLeftPaths)
	in
		rightPath @ (List.drop(shortestPath,1))
	end;

	fun findShortest acc G [] = acc
	|   findShortest (accLength, accPath) G (nextPath :: paths) =
	let
		val nextLength = sumCombination G nextPath
	in
		if (nextLength < accLength)
		then
			findShortest (nextLength, nextPath) G paths
		else
			findShortest (accLength, accPath) G paths
	end;

	fun matrixExists f M = List.foldl (fn (row,z) => z orelse (List.exists f row)) false M;
	fun matrixGetCoords acc f [] = raise Solver
	|   matrixGetCoords (colacc,rowacc) f ([] :: M) = matrixGetCoords (0,rowacc+1) f M
	|   matrixGetCoords (colacc,rowacc) f ((x :: row) :: M) =
		if (f x)
		then
			(colacc,rowacc)
		else
			matrixGetCoords (colacc+1,rowacc) f (row :: M);
	fun solve filename =
	let
		(*val maxWire = 7
		val wireCoords2 = [(153,9), (167,17), (139,41), (171,31), (131,1), (1,41), (23,9), (7,21)]*)

		val ductGrid = parse filename
		val _ = debugPrintln ("Input grid size: " ^ (printTuple (List.length ductGrid, List.length (hd ductGrid))))
		(* Find largest node label *)
		val maxWire = (List.length (List.filter (fn x => matrixExists (fn c => (Char.toString c) = (Int.toString x)) ductGrid) (seq 0 9))) - 1
		val _ = debugPrintln ("There are: " ^ (Int.toString (maxWire+1)) ^ " nodes to visit.")
		(* Find all wire coords *)
		val _ = debugPrintln ("Finding all wire coordinates.")
		val wireCoords = List.map (fn wire => matrixGetCoords (0,0) (fn c => (Char.toString c) = (Int.toString wire)) ductGrid) (seq 0 maxWire)
		val _ = debugPrintln ("Done finding all wire coordinates.")
		(* Generate all path pairs *)
		val _ = debugPrintln ("Generating all path length pairs.")
		val edgeList' = List.foldl (op@) [] (List.map (fn x => List.map (fn y => (x,y)) (seq 0 maxWire)) (seq 0 maxWire))
		val edgeList = List.filter (fn (x,y) => not(x=y)) edgeList'
		val _ = debugPrintln ("Done generating all path length pairs.")
		(* Solve for each pair *)
		val _ = debugPrintln ("Generating all path combinations.")
		val finalGraph = List.map (fn (index1,index2) => {startNode=index1,endNode=index2,distance=solveMaze ductGrid (List.nth(wireCoords, index1)) (List.nth(wireCoords, index2))}) edgeList
		(* Generate all permutations of solution paths. Note, all paths have to start at node 0 *)
		val _ = debugPrintln ("Summing all path combinations.")
		val allRightPaths = List.map (fn L => 0 :: L) (getAllCombinations (seq 1 maxWire))
		val _ = debugPrintln ("Done generating all path combinations.")
		val _ = List.app (fn L => debugPrintln ("Generated combination: " ^ (printIntList L))) allRightPaths
		(* Find the shortest *)
		val _ = debugPrintln ("Finding the shortest path.")
		(* val (shortestPath, pathList) = List.foldl (fn (currentP,(accShort,accP)) => if ((sumCombination finalGraph currentP) < accShort) then (sumCombination finalGraph currentP, currentP) else (accShort,accP)) (999999, []) allPaths *)
		val (shortestLength, shortestPath) = findShortest (999999,[]) finalGraph allRightPaths
		val _ = debugPrintln ("Shortest path length is: " ^ (Int.toString shortestLength) ^ " and the path is: " ^ (printIntList shortestPath))
		(* val moveCount = solveMaze favNumber target *)
	in
		(debugPrint ("(" ^ (Int.toString 0) ^ ")\n");
		shortestLength
		)
	end;
end
