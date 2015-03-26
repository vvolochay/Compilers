int notreallymany(int a, int b, int c, int d)
{
  return a * 1 + b * 10 + c * 100 + d * 1000;
}

int many(int a, int b, int c, int d, int e, int f, int g, int h)
{
  return a * 1 + b * 10 + c * 100 + d * 1000 + e * 10000 + f * 100000 + g * 1000000 + h * 10000000;
}

int one()
{
  return many(1, 2, 3, 4, 5, 6, 7, 8);
}
