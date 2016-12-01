all: sources.cm taxi1.sig taxi1.sml taxi2.sig taxi2.sml
	echo 'CM.make "sources.cm";' | sml

solve:
	echo 'CM.make "sources.cm"; val sol01_1 = Taxi1.solve "input"; val sol01_2 = Taxi2.solve "input"' | sml

test:
	echo 'CM.make "sources.cm"; use "test.sml";' | sml
