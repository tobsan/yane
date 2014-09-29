GHC="/chalmers/sw/unsup/ghc-6.10.1/bin/ghc"

runO1:
	$(GHC) --make -threaded Run.hs -O -o $@
prof:
	$(GHC) --make -threaded Run.hs -O -prof -auto-all -caf-all -fforce-recomp -o runPO1
#	./runPO1 +RTS -hc -p -sstderr
#	hp2ps -e8in -c runPO1.hp
clean:
	-rm -f *.hi *.o

distclean: clean
	-rm -f *.aux *.prof *.ps *.hp *.pdf
