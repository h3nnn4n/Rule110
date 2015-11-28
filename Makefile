COMP=ghc
FLAGS=-O3 -Wall

.PHONY: clean

all:
	$(COMP) Rule110.hs -o rule110

clean:
	-@rm -v rule110 *.hi *.o 2> /dev/null | true
