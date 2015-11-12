all: criterion.html doc/index.html

criterion: Chunk.hs criterion.hs
	ghc -Wall -O2 --make criterion.hs

criterion.html: criterion
	./criterion --output criterion.html

doc/index.html: Chunk.hs
	cd doc && haddock -h ../Chunk.hs

clean:
	-rm -rf criterion *.o *.hi criterion.html doc
