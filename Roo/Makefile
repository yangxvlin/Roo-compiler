Roo: RooAST.hs RooParser PrettyRoo.hs Roo.hs
	ghc Roo.hs

RooParser: RooParser.hs RooAST.hs
	ghc RooParser.hs

RooScanner: RooScanner.x
	alex RooScanner.x
	ghc RooScanner.hs

clean:
	rm -f *.o *.hi
	rm -f RooParser RooScanner RooScanner.hs Roo

