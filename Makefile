#GHC="/chalmers/sw/unsup/ghc-6.10.1/bin/ghc"
#GHC="ghc-6.8.2"
GHC="ghc"

runO1:
	$(GHC) --make Enterprise.hs -O2 -Wall -o $@
prof:
	$(GHC) --make Enterprise.hs -O2 -prof -auto-all -caf-all -fforce-recomp -o runPO1
#	./runPO1 +RTS -hc -p -sstderr
#	hp2ps -e8in -c runPO1.hp
clean:
	-rm -f *.hi *.o

distclean: clean
	-rm -f *.aux *.prof *.ps *.hp *.pdf
