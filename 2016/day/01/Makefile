all: sources.cm taxi.sig taxi.sml
	echo 'CM.make "sources.cm";' | sml

solve:
	echo 'CM.make "sources.cm"; Taxi.solve "input";' | sml

test:
	echo 'CM.make "sources.cm"; (Taxi.solve "input1") = 5; (Taxi.solve "input2") = 2; (Taxi.solve "input3") = 12; (Taxi.solve "input3c") = 15;' | sml
