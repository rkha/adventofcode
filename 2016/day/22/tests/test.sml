val tests1 = [
	("tests/input1", 7)
];

val tests2 = [
	("tests/input1", 7)
];

List.map (fn (x,y) => (Solver1.solve x) = y) tests1;
List.map (fn (x,y) => (Solver2.solve x) = y) tests2;
