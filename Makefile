GHC=ghc -cpp --make -Wall -O2 -funbox-strict-fields
#GHC=ghc -cpp --make -Wall -O2 -prof -auto-all -caf-all --make

all: draw dna2rna rna2gbc

draw: draw.c list.h list.c
	gcc -Wall -g -O2 draw.c list.c -o draw -lm

dna2rna: dna2rna.hs FastString3.hs
	$(GHC) dna2rna.hs -o dna2rna

rna2gbc: rna2gbc.hs FastString.hs GInstr.hs
	$(GHC) rna2gbc.hs -o rna2gbc

FastStringTest: FastString.hs FastStringTest.hs FastString2.hs FastString3.hs
	$(GHC) FastStringTest.hs -o FastStringTest

FastStringBench: FastString.hs FastStringBench.hs
	$(GHC) --make FastStringBench.hs -o FastStringBench

FastStringVisualise: FastString.hs FastStringVisualise.hs
	$(GHC) --make FastStringVisualise.hs -o FastStringVisualise

clean:
	rm -f *.o draw dna2rna rna2gbc FastStringTest
