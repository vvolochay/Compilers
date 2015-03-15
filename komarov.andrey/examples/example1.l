
int summ(int n)
{
    int i, sum;
    i = 0;
    sum = 0;
    while (i < n)
    {
        sum = sum + i;
        i = i + 1;
    }
    return sum;
}
