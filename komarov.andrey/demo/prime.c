#include <stdio.h>

int isPrime(int n);
int numPrimes(int from, int to);

int main()
{
    int i;
    for (i = 2; i < 20; i++)
    {
        printf("%d is %s\n", i, isPrime(i) ? "prime" : "not prime");
    }
    printf("%d primes between 100 and 5000\n", numPrimes(100, 5000));
}

