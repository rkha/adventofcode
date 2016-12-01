all: sources.cm taxi.sig taxi.sml
	echo 'CM.make "sources.cm";' | sml

solve:
	echo 'CM.make "sources.cm"; Taxi1.solve "input";' | sml

test:
	echo 'CM.make "sources.cm"; use "test.sml";' | sml
