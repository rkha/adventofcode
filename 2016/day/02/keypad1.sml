structure Keypad1 =
struct
	datatype move = UP | DOWN | LEFT | RIGHT;

	exception Keypad of char;

	val debug = false;
	val rowLength = 3;

	fun getCode (posX, posY) = (rowLength*posY) + (posX+1);
	fun getPos code =
	let
		val rows = Int.div(code, rowLength)
		val cols = code - (rowLength * rows)
	in
		(cols, rows)
	end;

	fun boundsCheck (posX, posY) =
	let
		val newX = if (posX < 0) then 0 else if (posX >= rowLength) then (rowLength-1) else posX
		val newY = if (posY < 0) then 0 else if (posY >= rowLength) then (rowLength-1) else posY
	in
		(newX, newY)
	end;
	fun applyMove ((posX,posY), LEFT) = (posX-1, posY)
	|   applyMove ((posX,posY), RIGHT) = (posX+1, posY)
	|   applyMove ((posX,posY), UP) = (posX, posY-1)
	|   applyMove ((posX,posY), DOWN) = (posX, posY+1);

	fun solveLine pos [] = getCode pos
	|   solveLine pos (move :: L) = solveLine (boundsCheck (applyMove (pos, move))) L;
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
		fun codeFold (moves, []) = [solveLine (1,1) moves]
		|   codeFold (moves, lastCode :: L) = (solveLine (getPos lastCode) moves) :: lastCode :: L
		val revCodeList = List.foldl (codeFold) [] (parse filename) handle Keypad(c) => (print (Char.toString c); [])
		val codeList = List.rev revCodeList
		val code = List.foldl (fn (x,z) => (z*10) + x) 0 codeList
	in
		(if (debug) then (print ("(" ^ ")\n")) else ();
		code
	)
	end;
end
