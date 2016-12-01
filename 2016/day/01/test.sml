val tests = [
	("input1", 5),
	("input2", 2),
	("input3", 12),
	("input3c", 15)
];

List.map (fn (x,y) => (Taxi.solve x) = y) tests;
