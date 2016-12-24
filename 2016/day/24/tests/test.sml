val tests1 = [
	("tests/input1", 14)
];

val tests2 = [
	("tests/input1", 20)
];

List.map (fn (x,y) => (Solver1.solve x) = y) tests1;
List.map (fn (x,y) => (Solver2.solve x) = y) tests2;
