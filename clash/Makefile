clean:
	rm src/*.hi src/*.o src/Graph/*.hi src/Graph/*.o src/Filter/*.hi src/Filter/*.o
	rm test/*.hi test/*.o test/Graph/*.hi test/Graph/*.o test/Filter/*.hi test/Filter/*.o

%: %.hs
	ghc $< -O2 -fexcess-precision -optc-O3 -optc-ffast-math  -fforce-recomp

