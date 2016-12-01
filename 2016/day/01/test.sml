val tests1 = [
	("input1", 5),
	("input2", 2),
	("input3", 12),
	("input3c", 15)
];

val test2 = [
	("input4", 4)
];

List.map (fn (x,y) => (Taxi1.solve x) = y) tests1;
