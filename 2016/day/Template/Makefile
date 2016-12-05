SMLFLAGS=-m

all: sources.cm src/*
	sml $(SMLFLAGS) sources.cm

solve:
	echo 'val sol00_1 = Solver1.solve "input"; val sol00_2 = Solver2.solve "input"' | sml $(SMLFLAGS) sources.cm

solve1: sources.cm src/*
	echo 'val sol00_1 = Solver1.solve "input";' | sml $(SMLFLAGS) sources.cm

solve2: sources.cm src/*
	echo 'val sol00_2 = Solver2.solve "input"' | sml $(SMLFLAGS) sources.cm

test: sources.cm src/* tests/test.sml
	sml $(SMLFLAGS) sources.cm tests/test.sml
