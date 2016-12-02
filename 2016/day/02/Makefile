all: sources.cm keypad1.sml keypad2.sml
	echo 'CM.make "sources.cm";' | sml

solve: all
	echo 'CM.make "sources.cm"; val sol02_1 = Keypad1.solve "input"; val sol01_2 = Keypad2.solve "input"' | sml

solve1: keypad1.sml input
	echo 'CM.make "sources.cm"; val sol02_1 = Keypad1.solve "input";' | sml

solve2: sources.cm input
	echo 'CM.make "sources.cm"; val sol02_2 = Keypad2.solve "input"' | sml

test: sources.cm keypad1.sml keypad2.sml tests/test.sml tests/input1 tests/output1 tests/output2
	echo 'CM.make "sources.cm"; use "tests/test.sml";' | sml
