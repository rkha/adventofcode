all: sources.cm keypad1.sml
	echo 'CM.make "sources.cm";' | sml

solve:
	echo 'CM.make "sources.cm"; val sol02_1 = Keypad1.solve "input"; val sol01_2 = Keypad2.solve "input"' | sml

solve1: sources.cm
	echo 'CM.make "sources.cm"; val sol02_1 = Keypad1.solve "input";' | sml

solve2: sources.cm
	echo 'CM.make "sources.cm"; val sol02_2 = Keypad2.solve "input"' | sml

test: sources.cm test.sml
	echo 'CM.make "sources.cm"; use "tests/test.sml";' | sml
