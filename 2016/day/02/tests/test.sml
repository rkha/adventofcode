val testdir = "tests/";
val tests1 = [
	("input1", 1985)
];

val tests2 = [
	("input1", "5DB3")
];

List.map (fn (x,y) => (Keypad1.solve (testdir ^ x)) = y) tests1;
List.map (fn (x,y) => (Keypad2.solve (testdir ^ x)) = y) tests2;
