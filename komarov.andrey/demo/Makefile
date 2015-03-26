all:
	make -C ../src
	../src/TestCompiler < callc.l > callc.S
	../src/TestCompiler < isPrime.l > prime.S
	../src/TestCompiler < fact.l > fact.S
	../src/TestCompiler < global.l > global.S

compile:
	gcc callc.S call.c -o call
	gcc fact.S fact.c -o fact
	gcc global.S global.c -o global
	gcc prime.S prime.c -o prime

