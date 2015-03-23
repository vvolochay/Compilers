#include <stdio.h>

int f();

int fact(int x);

int add(int a, int b)
{
    int c = a + b;
    return c;
}

int main()
{
    printf("hello\n");
    int i;
    for (i = 0; i < 10; i++)
        printf("%d ", f());
    printf("\n");
    printf("%d\n", fact(5));
}
