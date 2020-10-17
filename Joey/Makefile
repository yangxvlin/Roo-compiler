Joey: JoeyAST.hs JoeyParser.hs PrettyJoey.hs Joey.hs
	ghc Joey.hs

JoeyParser: JoeyParser.hs JoeyAST.hs
	ghc JoeyParser.hs

JoeyScanner: JoeyScanner.x
	alex JoeyScanner.x
	ghc JoeyScanner.hs

clean:
	rm -f *.o *.hi
	rm -f JoeyParser JoeyScanner JoeyScanner.hs Joey

