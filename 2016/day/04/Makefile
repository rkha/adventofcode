all: sources.cm src/*
	sml sources.cm

solve:
	echo 'val sol00_1 = Solver1.solve "input"; val sol00_2 = Solver2.solve "input"' | sml sources.cm

solve1: sources.cm src/*
	echo 'val sol00_1 = Solver1.solve "input";' | sml sources.cm

solve2: sources.cm src/*
	echo 'val sol00_2 = Solver2.solve "input"' | sml sources.cm

test: sources.cm src/* tests/test.sml
	sml sources.cm tests/test.sml
