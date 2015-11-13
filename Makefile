HC = ghc
HCFLAGS = -Wall -O2

all: criterion.html doc/index.html hunit.txt

criterion: Chunk.hs criterion.hs
	$(HC) $(HCFLAGS) --make criterion.hs

hunit: Chunk.hs hunit.hs
	$(HC) $(HCFLAGS) --make hunit.hs

criterion.html: criterion
	./criterion --output criterion.html

hunit.txt: hunit
	./hunit 2>&1 | tee hunit.txt

doc/index.html: Chunk.hs
	-@mkdir doc
	cd doc && haddock -h ../Chunk.hs

clean:
	-rm -rf criterion hunit *.o *.hi criterion.html hunit.txt doc
