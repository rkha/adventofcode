structure Solver1 :> SOLVER =
struct
	type move = char list;

	exception Solver;

	val debug = true;
	fun debugPrint s = if (debug) then (print s) else ();
	fun debugPrintln s = if (debug) then (debugPrint (s ^ "\n")) else ();

	fun seq i j = if (i < j) then i :: (seq (i+1) j) else [];

	fun parseLine L = L;

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

	fun day25Inner a =
	let
		val a' = Int.div(a,2)
		val c = if ((Int.mod(a,2))=0) then 2 else 1

		val b = 2 - c
		(* val _ = debugPrintln ("Output signal: " ^ (Int.toString b)) *)
	in
		if (not(a'=0))
		then
			b :: (day25Inner a')
		else
			[b]
	end
	and day25Outer accLength d =
	let
		val signal = day25Inner d

		val signalLength = List.length signal
	in
		if ((accLength + signalLength) < 20)
		then
			signal @ (day25Outer (accLength + signalLength) d)
		else
			signal
	end;

	fun printIntList [x] = (Int.toString x) ^ "]"
	|   printIntList (x :: L) = (Int.toString x) ^ "," ^ (printIntList L)
	|   printIntList [] = raise Solver;
	fun day25 a =
	let
		val d = a + (231*11)
		val output = day25Outer 0 d
		val _ = debugPrintln("Input a = " ^ (Int.toString a) ^ " generated the following signal: [" ^ (printIntList output))
	in
		output
	end;

	fun isValidSignal 1 (0 :: L) = isValidSignal 0 L
	|   isValidSignal 0 (1 :: L) = isValidSignal 1 L
	|   isValidSignal _ [] = true
	|   isValidSignal _ _ = false;

	fun solve filename =
	let
		val lastMax = 0
		val newMax = 10000
		val _ = parse filename
		val signals = List.map (fn x => (x,day25 x)) (seq (lastMax+1) newMax)
		val validSignals = List.filter (fn (x,signal) => isValidSignal 1 signal) signals

		val (firstInput, firstSignal) = hd validSignals
	in
		(debugPrint ("(" ^ (Int.toString 0) ^ ")\n");
		firstInput
		)
	end;
end
