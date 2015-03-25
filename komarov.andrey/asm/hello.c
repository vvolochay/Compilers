#include <stdio.h>

int f();

int fact(int x);

int ret5(int a, int b, int c, int d, int e);

int ret6(int a, int b, int c, int d, int e, int f);

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
    printf("%d\n", ret5(1, 2, 3, 4, 5));
    printf("%d\n", ret6(1, 2, 3, 4, 5, 6));
}
