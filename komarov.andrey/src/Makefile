all: lexer parser TestCompiler.hs Parser.hs Lexer.hs ARM.hs AST.hs
	ghc TestCompiler

lexer: Lexer.x
	alex Lexer.x

parser: Parser.y
	happy Parser.y

clean:
	rm -f *.o *.hi Lexer.hs Parser.hs TestCompiler

