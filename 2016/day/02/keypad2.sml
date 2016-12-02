structure Keypad2 =
struct
	datatype move = UP | DOWN | LEFT | RIGHT;

	exception Keypad of char;

	val debug = false;
	val rowLength = 5;

	fun debugPrint s = if (debug) then (print s) else ();

	(* (0, 0) is top left *)
	val keypad = [
		[#"0", #"0", #"1", #"0", #"0"],
		[#"0", #"2", #"3", #"4", #"0"],
		[#"5", #"6", #"7", #"8", #"9"],
		[#"0", #"A", #"B", #"C", #"0"],
		[#"0", #"0", #"D", #"0", #"0"]
	];
	fun getCode (posX, posY) = List.nth (List.nth(keypad, posY), posX);
	fun getIndexHelper acc x [] = NONE
	|   getIndexHelper acc x (y :: L) = if (x=y) then (SOME acc) else (getIndexHelper (acc+1) x L);
	fun getIndex x L = getIndexHelper 0 x L;
	fun getRow acc x [] = NONE
	|   getRow acc x (y :: L) = if (List.exists (fn z => x=z) y) then (SOME (acc, y)) else (getRow (acc+1) x L);
	fun getPos code =
	let
		val _ = debugPrint ("Looking for code: " ^ (Char.toString code) ^ "\n")
		val (rows, rowL) = valOf (getRow 0 code keypad)
		val cols = valOf (getIndex code rowL)
	in
		(cols, rows)
	end;

	fun boundsCheck (pos as (posX,posY)) = not((posX < 0) orelse (posX >= rowLength) orelse (posY < 0) orelse (posY >= rowLength) orelse ((getCode pos) = #"0"));

	fun getDelta LEFT = (~1, 0)
	|   getDelta RIGHT = (1, 0)
	|   getDelta UP = (0, ~1)
	|   getDelta DOWN = (0, 1);
	fun applyMove (pos as (posX, posY), move) =
	let
		val (dx, dy) = getDelta move
		val newPos = (posX + dx, posY + dy)
	in
		if (boundsCheck newPos)
		then
			newPos
		else
			pos
	end;

	fun solveLine pos [] = getCode pos
	|   solveLine pos (move :: L) = solveLine (applyMove (pos, move)) L;
	(* val parse : string -> taximove list *)
	fun parseLine [] = []
	|   parseLine (#"U" :: L) = UP :: (parseLine L)
	|   parseLine (#"D" :: L) = DOWN :: (parseLine L)
	|   parseLine (#"L" :: L) = LEFT :: (parseLine L)
	|   parseLine (#"R" :: L) = RIGHT :: (parseLine L)
	|   parseLine (c :: L) = if (Char.isSpace c) then (parseLine L) else raise Keypad(c);
	fun getAllLines (acc, ins) = (case (TextIO.inputLine ins) of
		NONE => List.rev acc
	|	SOME(line) => getAllLines (line :: acc, ins)
	);
	fun parse filename =
	let
		val ins = TextIO.openIn filename
		val moveStrings = getAllLines ([], ins)
		val moveList = List.map (parseLine o String.explode) moveStrings
	in
		moveList
	end;

	fun solve filename =
	let
		fun codeFold (moves, []) = [solveLine (0,2) moves]
		|   codeFold (moves, L as lastCode :: _) = (debugPrint ("lastCode = " ^ (Char.toString lastCode) ^ "\n"); (solveLine (getPos lastCode) moves) :: L)
		val revCodeList = List.foldl (codeFold) [] (parse filename) handle Keypad(c) => (print (Char.toString c); [])
		val codeList = List.rev revCodeList
		val code = String.implode codeList
	in
		(if (debug) then (print ("(" ^ ")\n")) else ();
		code
	)
	end;
end
