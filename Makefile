COMPILER=ghc
FILES=hgrep
all:
	$(COMPILER) $(FILES).hs
clean:
	rm *.o *.hi $(FILES)
