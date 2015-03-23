
hello: example.o
	gcc -g hello.c example.o -o hello

example.o: example.S
	as example.S -o example.o

clean:
	rm -f *.o hello
