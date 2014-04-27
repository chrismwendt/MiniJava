all:
	ghc Main.hs Lexer.hs Parser.hs SSACompiler.hs

clean:
	rm -f *.o *.hi Main Lexer Parser SSACompiler
