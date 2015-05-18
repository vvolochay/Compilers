all: lexer parser FCC/Parser.hs FCC/Lexer.hs
	echo hi

lexer: FCC/Lexer.x
	alex FCC/Lexer.x

parser: FCC/Parser.y
	happy FCC/Parser.y -ilog

clean:
	rm -f *.o *.hi log Lexer.hs Parser.hs TestCompiler

