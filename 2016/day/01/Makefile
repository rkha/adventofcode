all: sources.cm taxi.sig taxi1.sml taxi2.sml
	echo 'CM.make "sources.cm";' | sml

solve:
	echo 'CM.make "sources.cm"; Taxi1.solve "input";' | sml

test:
	echo 'CM.make "sources.cm"; use "test.sml";' | sml
