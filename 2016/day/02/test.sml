val tests1 = [
	("input1", 5),
	("input2", 2),
	("input3", 12),
	("input3c", 15)
];

val tests2 = [
	("input4", 4)
];

List.map (fn (x,y) => (???.solve x) = y) tests1;
List.map (fn (x,y) => (???.solve x) = y) tests2;
