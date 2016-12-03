val tests1 = [
	("input", 0)
];

val tests2 = [
	("input", 0)
];

List.map (fn (x,y) => (Solver1.solve x) = y) tests1;
List.map (fn (x,y) => (Solver2.solve x) = y) tests2;
