all: lexer parser TestCompiler.hs Parser.hs Lexer.hs ARM.hs AST.hs
	ghc TestCompiler

lexer: Lexer.x
	alex Lexer.x

parser: Parser.y
	happy Parser.y -ilog

clean:
	rm -f *.o *.hi log Lexer.hs Parser.hs TestCompiler

