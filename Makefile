all: chunk chunk.html doc/index.html

chunk: chunk.hs
	ghc -Wall -O2 --make chunk.hs

chunk.html: chunk
	./chunk --output chunk.html

doc/index.html: chunk.hs
	cd doc && haddock -h ../chunk.hs

clean:
	-rm -rf chunk chunk.o chunk.hi chunk.html doc
