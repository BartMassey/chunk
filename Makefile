HC = ghc
HCFLAGS = -Wall -O2

all: criterion.html doc/index.html

criterion: Chunk.hs criterion.hs
	$(HC) $(HCFLAGS) --make criterion.hs

hunit: Chunk.hs hunit.hs
	$(HC) $(HCFLAGS) --make hunit.hs

criterion.html: criterion
	./criterion --output criterion.html

doc/index.html: Chunk.hs
	cd doc && haddock -h ../Chunk.hs

clean:
	-rm -rf criterion *.o *.hi criterion.html doc
