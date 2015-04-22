int div(int a, int b);
int mod(int a, int b);

bool isPrime(int n)
{
    int i;
    i = 2;
    while (i * i <= n)
    {
        if (mod(n, i) == 0)
            return false;
        else {}
        i = i + 1;
    }
    return true;
}

int numPrimes(int from, int to)
{
    int i;
    i = from;
    int ans;
    ans = 0;
    while (i < to)
    {
        if (isPrime(i))
            ans = ans + 1;
        else {}
        i = i + 1;
    }
    return ans;
}

int mod(int a, int b)
{
    return a - b * div(a, b);
}

int div(int a, int b)
{
    int c;
    c = 0;
    while (a >= 0)
    {
        c = c + 1;
        a = a - b;
    }
    return c - 1;
}

